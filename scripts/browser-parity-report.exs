[directory] = System.argv()

events =
  directory
  |> Path.join("**/[0-9]*-[0-9]*.json")
  |> Path.wildcard()
  |> Enum.map(&:json.decode(File.read!(&1)))

phases =
  events
  |> Enum.group_by(& &1["phase"])
  |> Map.new(fn {phase, entries} ->
    {phase,
     %{
       invocations: length(entries),
       completed: Enum.count(entries, &(&1["status"] == "ok")),
       errors: Enum.count(entries, &(&1["status"] == "error")),
       interrupted: Enum.count(entries, &(&1["status"] == "started")),
       seconds:
         Enum.sum(for entry <- entries, entry["status"] != "started", do: entry["elapsed_us"]) /
           1_000_000
     }}
  end)

owners =
  events
  |> Enum.filter(&(&1["phase"] in ["comparison", "shared_reference", "browser_capture"]))
  |> Enum.group_by(&{&1["owner"], &1["fixture"] || &1["browser_input"]})
  |> Enum.map(fn {{owner, fixture}, entries} ->
    %{
      owner: owner,
      fixture: fixture,
      comparisons: Enum.count(entries, &(&1["phase"] == "comparison")),
      shared_references: Enum.count(entries, &(&1["phase"] == "shared_reference")),
      captures: Enum.count(entries, &(&1["phase"] == "browser_capture")),
      source_sha256:
        entries |> Enum.map(& &1["source_sha256"]) |> Enum.reject(&is_nil/1) |> Enum.uniq()
    }
  end)
  |> Enum.sort_by(&{&1.owner, &1.fixture})

intervals =
  events
  |> Enum.filter(&(&1["phase"] in ["browser_launch", "browser_cleanup"]))
  |> Enum.group_by(& &1["parent_id"])
  |> Enum.flat_map(fn {_capture, entries} ->
    launch = Enum.find(entries, &(&1["phase"] == "browser_launch" and &1["status"] == "ok"))
    cleanup = Enum.find(entries, &(&1["phase"] == "browser_cleanup" and &1["status"] == "ok"))

    if launch && cleanup do
      [
        {launch["run_id"], launch["started_us"], 1},
        {cleanup["run_id"], cleanup["started_us"] + cleanup["elapsed_us"], -1}
      ]
    else
      []
    end
  end)
  |> Enum.sort()

peaks =
  intervals
  |> Enum.group_by(&elem(&1, 0))
  |> Enum.map(fn {_run, entries} ->
    {_active, peak} =
      Enum.reduce(entries, {0, 0}, fn {_run, _time, change}, {active, peak} ->
        {active + change, max(peak, active + change)}
      end)

    peak
  end)

peak = Enum.max(peaks, fn -> 0 end)

report = %{
  phases: phases,
  owners: owners,
  peak_completed_browser_captures: peak,
  raster_pages:
    Enum.sum(for event <- events, event["phase"] == "rasterization", do: event["page_count"] || 0),
  raster_pixels:
    Enum.sum(
      for event <- events,
          event["phase"] == "rasterization",
          page <- event["raster_pages"] || [],
          do: page["pixels"]
    ),
  compared_pixels:
    Enum.sum(
      for event <- events,
          event["phase"] in ["pixel_comparison", "strict_pixel_comparison"],
          do: event["reference_pixels"] || 0
    ),
  runtimes: events |> Enum.map(&%{elixir: &1["elixir"], otp: &1["otp"]}) |> Enum.uniq(),
  note:
    "Phase timings are nested and must not be added together. Raw errors include intentional failure probes; ExUnit determines suite success. Peak concurrency covers captures with completed launch and cleanup records. Command elapsed time comes from the invoking runner."
}

IO.puts(:json.encode(report))
