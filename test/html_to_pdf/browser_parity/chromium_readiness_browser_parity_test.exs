defmodule NativeElixirPdfUtilities.HtmlToPdf.ChromiumReadinessBrowserParityTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.TestSupport.{ChromiumPool, Paths}
  @moduletag :browser_parity

  setup do
    timeout = ExUnit.configuration()[:timeout]

    on_exit(fn ->
      ExUnit.configure(timeout: timeout)
      ChromiumPool.stop()
    end)

    :ok
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
  test "unfinished child loads fail with pending resource diagnostics within the suite deadline" do
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

    assert :ok =
             ChromiumPool.print_pdf!(browser, url <> "ready", Path.join(directory, "ready.pdf"))

    ExUnit.configure(timeout: 2_000)
    output = Path.join(directory, "unfinished.pdf")
    File.rm(output)

    assert_raise ExUnit.AssertionError, ~r/deadline exceeded.*hang/s, fn ->
      ChromiumPool.print_pdf!(browser, url, output)
    end

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
