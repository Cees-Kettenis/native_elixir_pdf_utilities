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

    profiles =
      for index <- 1..2 do
        output = Path.join(directory, "capture-#{index}.pdf")
        assert :ok = ChromiumPool.print_pdf!(browser, url, output)
        metadata = output |> Kernel.<>(".browser.json") |> File.read!() |> :json.decode()
        refute File.exists?(metadata["profile"])
        assert {text, 0} = System.cmd("pdftotext", [output, "-"])
        assert text =~ "Independent capture"
        metadata["profile"]
      end

    assert length(Enum.uniq(profiles)) == 2
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
                      {:ok, request} = :gen_tcp.recv(socket, 0, 5_000)
                      [_, path | _] = String.split(request, " ")
                      {status, type, body} = response.(path)

                      :gen_tcp.send(socket, [
                        "HTTP/1.1 #{status} Response\r\nContent-Type: #{type}\r\nContent-Length: #{byte_size(body)}\r\nConnection: close\r\n\r\n",
                        body
                      ])

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
