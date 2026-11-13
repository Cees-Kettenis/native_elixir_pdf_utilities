defmodule NativeElixirPdfUtilities.HtmlToPdf.ChromiumReadinessBrowserParityTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.TestSupport.{ChromiumPool, Paths}
  @moduletag :browser_parity

  setup do
    on_exit(fn -> ChromiumPool.stop() end)

    :ok
  end

  test "successive captures have independent browser profiles and clean them afterward" do
    url = serve(fn _path -> {200, "text/html", "<p>Independent capture</p>"} end)
    directory = Paths.project("tmp/browser_parity/chromium_readiness/isolation")
    File.mkdir_p!(directory)
    browser = System.get_env("CHROMIUM_BIN") || System.find_executable("chromium")
    wrapper = Path.join(directory, "browser-stdio-#{System.unique_integer([:positive])}.sh")
    quoted_browser = "'" <> String.replace(browser, "'", "'\"'\"'") <> "'"

    # Reuse the first capture to prove ordinary browser output cannot corrupt
    # the control pipe. Both captures still run the same browser and source.
    File.write!(
      wrapper,
      "#!/bin/sh\nprintf '%s\\n' 'ordinary browser output'\nexec #{quoted_browser} \"$@\"\n"
    )

    File.chmod!(wrapper, 0o700)
    on_exit(fn -> File.rm(wrapper) end)

    profiles =
      for {executable, index} <- [{wrapper, 1}, {browser, 2}] do
        output = Path.join(directory, "capture-#{index}.pdf")
        assert :ok = ChromiumPool.print_pdf!(executable, url, output)
        metadata = output |> Kernel.<>(".browser.json") |> File.read!() |> :json.decode()
        refute File.exists?(metadata["profile"])
        assert {text, 0} = System.cmd("pdftotext", [output, "-"])
        assert text =~ "Independent capture"
        metadata["profile"]
      end

    assert length(Enum.uniq(profiles)) == 2
  end

  @tag timeout: 15_000
  test "an inactive tiny SVG page becomes drawable before its screenshot" do
    # Two fresh captures compare foreground and deliberately backgrounded tabs.
    # Sharing a reference would skip the browser activation behavior under test.
    owner = self()
    svg = File.read!(Paths.fixture("html_to_pdf/svg/small_canvas_stroke.svg"))

    html =
      "<style>@page{size:48pt 3pt;margin:0}html,body{margin:0}svg{display:block}</style>" <> svg

    url =
      serve(fn path ->
        if path == "/hold" do
          send(owner, {:held_image, self()})
          receive do: (:release -> {404, "text/plain", "Missing"})
        else
          {200, "text/html", html <> ~s|<img src="/hold" style="display:none">|}
        end
      end)

    directory = Paths.project("tmp/browser_parity/chromium_readiness/active_svg")
    File.mkdir_p!(directory)
    fixture = Path.join(directory, "original.html")
    File.write!(fixture, html)
    browser = System.get_env("CHROMIUM_BIN") || System.find_executable("chromium")
    reference = %{width: 64, height: 4, scale: 1}
    foreground = Path.join(directory, "foreground")
    background = Path.join(directory, "background")
    File.mkdir_p!(foreground)
    File.mkdir_p!(background)

    assert :ok =
             ChromiumPool.print_pdf!(
               browser,
               fixture,
               Path.join(foreground, "chromium.pdf"),
               reference
             )

    output = Path.join(background, "chromium.pdf")

    capture =
      Task.async(fn ->
        ChromiumPool.print_pdf!(browser, url, output, reference,
          browser_args: ["--remote-debugging-port=0"],
          timeout: 4_000
        )
      end)

    assert_receive {:held_image, request}, 10_000
    metadata = output |> Kernel.<>(".browser.json") |> File.read!() |> :json.decode()

    activate_other_tab!(metadata["profile"])
    send(request, :release)
    assert :ok = Task.await(capture, 10_000)

    assert [%{"focused" => true}] = :json.decode(File.read!(output <> ".readiness.json"))

    png = File.read!(Path.join(foreground, "chromium-svg-0.png"))
    assert <<137, 80, 78, 71, 13, 10, 26, 10, 13::32, "IHDR", 64::32, 1::32, _::binary>> = png
    assert png == File.read!(Path.join(background, "chromium-svg-0.png"))
  end

  test "caller death closes an active browser and removes its profile" do
    owner = self()

    url =
      serve(fn _path ->
        send(owner, {:capture_request, self()})

        receive do
          :finish -> {200, "text/html", "<p>Unreachable</p>"}
        end
      end)

    directory = Paths.project("tmp/browser_parity/chromium_readiness/owner_exit")
    File.mkdir_p!(directory)
    output = Path.join(directory, "abandoned.pdf")
    File.rm(output)
    browser = System.get_env("CHROMIUM_BIN") || System.find_executable("chromium")
    caller = spawn(fn -> ChromiumPool.print_pdf!(browser, url, output) end)
    on_exit(fn -> Process.exit(caller, :kill) end)
    assert_receive {:capture_request, request}, 10_000

    metadata = output |> Kernel.<>(".browser.json") |> File.read!() |> :json.decode()
    worker = metadata["worker"] |> String.to_charlist() |> :erlang.list_to_pid()
    monitor = Process.monitor(worker)
    assert {:monitored_by, monitors} = Process.info(worker, :monitored_by)
    assert self() in monitors
    Process.exit(caller, :kill)
    assert_receive {:DOWN, ^monitor, :process, ^worker, :normal}, 10_000
    send(request, :finish)
    refute File.exists?(metadata["profile"])
    refute File.exists?(output)
    assert File.read!(output <> ".readiness-error.txt") =~ "capture owner exited"
  end

  test "capture waits for nested documents fonts and images while author scripts stay disabled" do
    font = File.read!(Paths.fixture("html_to_pdf/browser_parity/assets/math-glyph-reference.ttf"))
    image = File.read!(Paths.fixture("html_to_pdf/browser_parity/assets/embedded-neutral.jpg"))

    parent = """
    <style>@page{size:300pt 300pt;margin:0}body{margin:0}</style>
    <p>Parent ready</p><iframe src="/child"></iframe><object data="/child" type="text/html"></object>
    <embed src="/child" type="text/html"><iframe src="about:blank"></iframe>
    <embed src="about:blank"><img src="/image" width="8" height="8"><img src="/broken" alt="Broken image">
    <script>document.body.textContent='Author script ran';</script>
    """

    child = """
    <style>@font-face{font-family:Delayed;src:url(/font)}body{margin:0;font-family:Delayed}</style>
    <p>Child ready <span>𝑥</span></p><iframe src="/leaf"></iframe><object data="/vector" type="image/svg+xml"></object>
    """

    url =
      serve(fn path ->
        case path do
          "/" ->
            {200, "text/html", parent}

          "/child" ->
            Process.sleep(100)
            {200, "text/html", child}

          "/leaf" ->
            Process.sleep(100)
            {200, "text/html", "<p>Nested leaf ready</p>"}

          "/vector" ->
            Process.sleep(100)

            {200, "image/svg+xml",
             ~s|<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20"><rect width="20" height="20" fill="blue"/></svg>|}

          "/font" ->
            Process.sleep(100)
            {200, "font/ttf", font}

          "/image" ->
            Process.sleep(100)
            {200, "image/jpeg", image}

          _ ->
            {404, "text/plain", "Missing"}
        end
      end)

    output = Paths.project("tmp/browser_parity/chromium_readiness/delayed.pdf")
    File.mkdir_p!(Path.dirname(output))

    assert :ok =
             ChromiumPool.print_pdf!(
               System.get_env("CHROMIUM_BIN") || System.find_executable("chromium"),
               url,
               output
             )

    {text, 0} = System.cmd(System.find_executable("pdftotext"), ["-layout", output, "-"])
    assert text =~ "Parent ready"
    assert length(Regex.scan(~r/Child ready/, text)) == 3
    assert length(Regex.scan(~r/Nested leaf ready/, text)) == 3
    refute text =~ "Author script ran"
    {fonts, 0} = System.cmd(System.find_executable("pdffonts"), [output])
    assert fonts =~ ~r/\b[A-Z]{6}\+MathGlyphTestReference\b/

    frames = output |> Kernel.<>(".readiness.json") |> File.read!() |> :json.decode()
    assert Enum.all?(frames, &(&1["ready"] == "complete" and &1["fonts"] == "loaded"))
    assert Enum.count(frames, &String.ends_with?(&1["url"], "/child")) == 3
    assert Enum.count(frames, &String.ends_with?(&1["url"], "/leaf")) == 3
    assert Enum.count(frames, &String.ends_with?(&1["url"], "/vector")) == 3
    images = frames |> Enum.flat_map(& &1["images"])
    assert Enum.any?(images, &(&1["complete"] and &1["width"] > 0))
    assert Enum.any?(images, &(&1["complete"] and &1["width"] == 0))
  end

  test "parser yields preserve object documents and failed-resource fallback without retries" do
    observer = self()

    objects =
      Enum.map_join(1..12, fn index ->
        ~s|<object data="/child-#{index}" type="text/html"><p>Unused fallback #{index}</p></object>|
      end)

    url =
      serve(fn path ->
        send(observer, {:resource_request, path})

        cond do
          path == "/" ->
            {200, "text/html",
             """
             <html><head><style>object{display:block;width:280px;height:100px}</style></head><body>
             #{objects}
             <object data="/missing" type="text/html"><p>Missing object fallback</p></object>
             <script>document.body.textContent='Author script ran';</script>
             </body></html>
             """}

          String.starts_with?(path, "/child-") ->
            {200, "text/html",
             """
             <p>Object child #{path}</p>
             <object data="/leaf#{path}" type="text/html"><p>Unused nested fallback</p></object>
             """}

          String.starts_with?(path, "/leaf/child-") ->
            {200, "text/html", "<p>Nested object leaf #{path}</p>"}

          true ->
            {404, "text/plain", "Missing"}
        end
      end)

    output = Paths.project("tmp/browser_parity/chromium_readiness/parser_yields.pdf")
    File.mkdir_p!(Path.dirname(output))
    browser = System.get_env("CHROMIUM_BIN") || System.find_executable("chromium")

    assert :ok =
             ChromiumPool.print_pdf!(browser, url, output, nil,
               browser_args: ["--enable-blink-features=HTMLParserYieldAndDelayOftenForTesting"]
             )

    assert {text, 0} = System.cmd("pdftotext", [output, "-"])
    assert length(Regex.scan(~r/Object child/, text)) == 12
    assert length(Regex.scan(~r/Nested object leaf/, text)) == 12
    assert text =~ "Missing object fallback"
    refute text =~ "Unused"
    refute text =~ "Author script ran"

    frames = output |> Kernel.<>(".readiness.json") |> File.read!() |> :json.decode()
    assert Enum.count(frames, &String.contains?(&1["url"], "/child-")) == 24
    assert Enum.all?(frames, &(&1["ready"] == "complete" and &1["fonts"] == "loaded"))

    for path <- ["/missing" | Enum.flat_map(1..12, &["/child-#{&1}", "/leaf/child-#{&1}"])] do
      assert_received {:resource_request, ^path}
      refute_received {:resource_request, ^path}
    end
  end

  @tag timeout: 15_000
  test "unfinished child loads fail with pending resource diagnostics when the deadline expires" do
    url =
      serve(fn path ->
        case path do
          "/hang" ->
            receive do
              :never -> {200, "text/html", "unreachable"}
            end

          "/" ->
            {200, "text/html", ~s|<iframe src="/hang"></iframe>|}

          _ ->
            {200, "text/html", "Ready"}
        end
      end)

    directory = Paths.project("tmp/browser_parity/chromium_readiness")
    File.mkdir_p!(directory)
    browser = System.get_env("CHROMIUM_BIN") || System.find_executable("chromium")

    output = Path.join(directory, "unfinished.pdf")
    File.rm(output)
    clock = :atomics.new(1, [])
    observer = self()

    capture =
      Task.async(fn ->
        assert_raise ExUnit.AssertionError, ~r/deadline exceeded.*hang/s, fn ->
          ChromiumPool.print_pdf!(browser, url, output, nil,
            timeout: 2_000,
            clock: fn -> :atomics.get(clock, 1) end,
            request_observer: observer
          )
        end
      end)

    hanging_url = url <> "hang"
    assert_receive {:chromium_request, worker, ^hanging_url}, 10_000
    :atomics.put(clock, 1, 2_000)
    send(worker, :clock_advanced)
    Task.await(capture, 10_000)

    assert File.read!(output <> ".readiness-error.txt") =~ "/hang"
    refute File.exists?(output)
  end

  defp activate_other_tab!(profile) do
    [port, path | _] = File.read!(Path.join(profile, "DevToolsActivePort")) |> String.split("\n")

    assert {:ok, socket} =
             :gen_tcp.connect(
               ~c"127.0.0.1",
               String.to_integer(port),
               [:binary, active: false, packet: :http_bin],
               5_000
             )

    try do
      key = Base.encode64(:crypto.strong_rand_bytes(16))

      assert :ok =
               :gen_tcp.send(
                 socket,
                 "GET #{path} HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: #{key}\r\nSec-WebSocket-Version: 13\r\n\r\n"
               )

      assert {:ok, {:http_response, _version, 101, _reason}} = :gen_tcp.recv(socket, 0, 5_000)

      Stream.repeatedly(fn -> :gen_tcp.recv(socket, 0, 5_000) end)
      |> Enum.reduce_while(nil, fn
        {:ok, :http_eoh}, _ -> {:halt, nil}
        {:ok, {:http_header, _, _, _, _}}, _ -> {:cont, nil}
      end)

      :ok = :inet.setopts(socket, packet: :raw)

      command = fn id, method, params ->
        payload = :json.encode(%{id: id, method: method, params: params}) |> IO.iodata_to_binary()
        size = byte_size(payload)
        header = if size < 126, do: <<0x81, 0x80 + size>>, else: <<0x81, 0xFE, size::16>>
        mask = :crypto.strong_rand_bytes(4)

        masked =
          for {byte, index} <- Enum.with_index(:binary.bin_to_list(payload)),
              into: <<>>,
              do: <<Bitwise.bxor(byte, :binary.at(mask, rem(index, 4)))>>

        :ok = :gen_tcp.send(socket, [header, mask, masked])
        assert {:ok, <<0x81, size>>} = :gen_tcp.recv(socket, 2, 5_000)

        size =
          case size do
            126 ->
              assert {:ok, <<length::16>>} = :gen_tcp.recv(socket, 2, 5_000)
              length

            127 ->
              assert {:ok, <<length::64>>} = :gen_tcp.recv(socket, 8, 5_000)
              length

            length ->
              length
          end

        assert {:ok, response} = :gen_tcp.recv(socket, size, 5_000)
        response = :json.decode(response)
        assert response["id"] == id
        refute response["error"]
        response["result"]
      end

      # The held response can precede publication of the navigation URL in
      # TargetInfo. The capture is the only attached page in this fresh browser.
      [info] =
        command.(1, "Target.getTargets", %{})["targetInfos"]
        |> Enum.filter(&(&1["type"] == "page" and &1["attached"]))

      assert is_binary(info["browserContextId"])

      assert %{"targetId" => _} =
               command.(2, "Target.createTarget", %{
                 url: "about:blank",
                 browserContextId: info["browserContextId"]
               })
    after
      :gen_tcp.close(socket)
    end
  end

  defp serve(response) do
    {:ok, listener} =
      :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true, ip: {127, 0, 0, 1}])

    {:ok, {_, port}} = :inet.sockname(listener)

    server =
      spawn(fn ->
        accept = fn accept ->
          case :gen_tcp.accept(listener) do
            {:ok, socket} ->
              worker =
                spawn_link(fn ->
                  receive do
                    {:socket, socket} ->
                      case :gen_tcp.recv(socket, 0, 5_000) do
                        {:ok, request} ->
                          [_, path | _] = String.split(request, " ")
                          {status, type, body} = response.(path)

                          :gen_tcp.send(socket, [
                            "HTTP/1.1 #{status} Response\r\nContent-Type: #{type}\r\nContent-Length: #{byte_size(body)}\r\nConnection: close\r\n\r\n",
                            body
                          ])

                        {:error, :closed} ->
                          :ok
                      end

                      :gen_tcp.close(socket)
                  end
                end)

              :ok = :gen_tcp.controlling_process(socket, worker)
              send(worker, {:socket, socket})
              accept.(accept)

            {:error, :closed} ->
              :ok
          end
        end

        accept.(accept)
      end)

    on_exit(fn ->
      Process.exit(server, :kill)
      :gen_tcp.close(listener)
    end)

    "http://127.0.0.1:#{port}/"
  end
end
