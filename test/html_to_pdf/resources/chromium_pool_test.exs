defmodule NativeElixirPdfUtilities.HtmlToPdf.ChromiumPoolTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.{ChromiumPool, Paths}

  test "an executable exit before the print call still returns its actionable failure" do
    directory =
      Paths.project("tmp/chromium-pool-early-exit-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)
    output = Path.join(directory, "chromium.pdf")
    port = make_ref()

    state = %{
      port: port,
      exit_status: nil,
      profile: directory,
      log: Path.join(directory, "browser.log"),
      clock: fn -> 0 end,
      timeout: 1,
      deadline: nil,
      pending: %{},
      session: nil,
      loading: MapSet.new(),
      readiness: []
    }

    # Deliver the exit first, deterministically. This was previously a normal
    # worker stop, so the later GenServer.call exited instead of reporting why.
    assert {:noreply, waiting} = ChromiumPool.handle_info({port, {:exit_status, 1}}, state)

    assert {:stop, :normal, {:error, message}, %{exit_status: 1}} =
             ChromiumPool.handle_call({:pdf, "fixture.html", output, nil}, nil, waiting)

    assert message =~ "Chromium PDF rendering failed"
    assert message =~ "Chrome pipe exited with status 1"
    assert message =~ state.log
    assert File.read!(output <> ".readiness-error.txt") == "Chrome pipe exited with status 1"
  end
end
