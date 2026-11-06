defmodule NativeElixirPdfUtilities.HtmlToPdf.BrowserParityTelemetryTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.TestSupport.BrowserParityTelemetry, as: Telemetry

  setup do
    directory = Path.expand("tmp/browser_parity/telemetry-#{System.unique_integer([:positive])}")
    previous = System.get_env("BROWSER_PARITY_TELEMETRY")
    System.put_env("BROWSER_PARITY_TELEMETRY", directory)

    on_exit(fn ->
      if previous,
        do: System.put_env("BROWSER_PARITY_TELEMETRY", previous),
        else: System.delete_env("BROWSER_PARITY_TELEMETRY")

      File.rm_rf!(directory)
    end)

    %{directory: directory}
  end

  test "nested operations retain their owner, geometry and parent without changing results", %{
    directory: directory
  } do
    assert %{pages: [%{regions: [%{bounds: {1, 2, 3, 4}}]}]} =
             Telemetry.measure(:comparison, %{fixture: "example.html"}, fn ->
               assert :result = Telemetry.measure(:native_render, %{}, fn -> :result end)
               %{pages: [%{regions: [%{bounds: {1, 2, 3, 4}}]}]}
             end)

    events =
      directory
      |> Path.join("*.json")
      |> Path.wildcard()
      |> Enum.map(&:json.decode(File.read!(&1)))

    assert length(events) == 2
    parent = Enum.find(events, &(&1["phase"] == "comparison"))
    child = Enum.find(events, &(&1["phase"] == "native_render"))
    assert parent["status"] == "ok" and child["status"] == "ok"
    assert child["parent_id"] == parent["id"]
    assert child["owner"] == parent["owner"]

    assert get_in(parent, ["pages", Access.at(0), "regions", Access.at(0), "bounds"]) == [
             1,
             2,
             3,
             4
           ]

    assert child["elapsed_us"] >= 0
    assert Telemetry.context() == %{}
  end

  test "failed operations retain their original exception and restore the parent context", %{
    directory: directory
  } do
    assert_raise ArgumentError, "intentional probe", fn ->
      Telemetry.measure(:native_render, %{}, fn -> raise ArgumentError, "intentional probe" end)
    end

    [path] = Path.wildcard(Path.join(directory, "*.json"))
    event = :json.decode(File.read!(path))
    assert event["status"] == "error"
    assert event["failure"] =~ "intentional probe"
    assert Telemetry.context() == %{}
  end

  test "an interrupted worker still leaves an invocation record", %{directory: directory} do
    parent = self()

    {pid, monitor} =
      spawn_monitor(fn ->
        Telemetry.measure(:browser_capture, %{}, fn ->
          send(parent, :capture_started)
          receive do: (:finish -> :ok)
        end)
      end)

    assert_receive :capture_started
    Process.exit(pid, :kill)
    assert_receive {:DOWN, ^monitor, :process, ^pid, :killed}
    [path] = Path.wildcard(Path.join(directory, "*.json"))
    assert :json.decode(File.read!(path))["status"] == "started"
  end
end
