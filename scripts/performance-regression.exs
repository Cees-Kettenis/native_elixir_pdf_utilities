alias NativeElixirPdfUtilities.HtmlToPdf
alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
alias NativeElixirPdfUtilities.Text

Code.require_file("../test/support/png_fixture.ex", __DIR__)
alias NativeElixirPdfUtilities.TestSupport.PngFixture

maximum_reduction_regression_percent = 10
maximum_time_regression_percent = 10
runtime_version = System.version() |> String.split(".") |> Enum.take(2) |> Enum.join(".")
report_path = System.get_env("PERFORMANCE_REPORT")
baseline_path = System.get_env("PERFORMANCE_BASELINE_REPORT")

if baseline_path && !report_path do
  raise "PERFORMANCE_BASELINE_REPORT requires PERFORMANCE_REPORT to retain comparison evidence"
end

Process.put(:performance_results, [])
Process.put(:performance_failures, [])

# Measure every original workload and retain its evidence before failing the run.
record_failure = fn message ->
  IO.puts("PERFORMANCE_FAILURE #{message}")
  Process.put(:performance_failures, [message | Process.get(:performance_failures)])
end

# Memory sampling runs separately so it cannot distort the existing timed samples.
# This is sampled total BEAM memory, including shared caches and binaries. Native
# allocations require an additional OS profiler, such as /usr/bin/time -v.
measure_memory = fn operation ->
  owner = self()
  reference = make_ref()
  :erlang.garbage_collect()

  sampler =
    spawn_link(fn ->
      initial = :erlang.memory(:total)
      send(owner, {reference, :ready})

      sample = fn sample, peak ->
        receive do
          :stop -> send(owner, {reference, :peak, max(peak, :erlang.memory(:total))})
        after
          10 -> sample.(sample, max(peak, :erlang.memory(:total)))
        end
      end

      sample.(sample, initial)
    end)

  receive do
    {^reference, :ready} -> :ok
  end

  result = operation.()
  send(sampler, :stop)

  receive do
    {^reference, :peak, peak} ->
      case result do
        {:ok, _value} -> peak
        failure -> raise "memory sample failed: #{inspect(failure)}"
      end
  end
end

measure = fn name, operation, limits ->
  Enum.each(1..2, fn _ ->
    case operation.() do
      {:ok, _result} -> :ok
      failure -> raise "#{name} warmup failed: #{inspect(failure)}"
    end
  end)

  samples =
    Enum.map(1..5, fn _iteration ->
      :erlang.garbage_collect()
      # Measure work in the operation worker, and elapsed time around the full
      # boundary so worker startup and result delivery remain part of latency.
      {microseconds, {result, reductions}} =
        :timer.tc(fn ->
          NativeElixirPdfUtilities.Operation.run(__MODULE__, :performance_sample, fn ->
            {:reductions, before_work} = Process.info(self(), :reductions)
            result = operation.()
            {:reductions, after_work} = Process.info(self(), :reductions)
            {result, after_work - before_work}
          end)
        end)

      value =
        case result do
          {:ok, value} -> value
          failure -> raise "#{name} sample failed: #{inspect(failure)}"
        end

      %{
        microseconds: microseconds,
        reductions: reductions,
        value: value
      }
    end)

  median_microseconds =
    samples
    |> Enum.map(& &1.microseconds)
    |> Enum.sort()
    |> Enum.at(2)

  median_reductions =
    samples
    |> Enum.map(& &1.reductions)
    |> Enum.sort()
    |> Enum.at(2)

  baseline_reductions =
    limits
    |> Keyword.fetch!(:baseline_reductions)
    |> Map.fetch!(runtime_version)

  maximum_reductions =
    div(baseline_reductions * (100 + maximum_reduction_regression_percent), 100)

  latest_value = samples |> List.last() |> Map.fetch!(:value)
  result_bytes = if is_binary(latest_value), do: byte_size(latest_value), else: nil

  result = %{
    name: name,
    median_ms: Float.round(median_microseconds / 1_000, 3),
    median_reductions: median_reductions,
    baseline_reductions: baseline_reductions,
    reduction_change_percent: Float.round((median_reductions / baseline_reductions - 1) * 100, 2),
    maximum_reductions: maximum_reductions,
    result_bytes: result_bytes
  }

  IO.inspect(result, label: "PERFORMANCE_REGRESSION")

  if median_reductions > maximum_reductions do
    record_failure.("""
    #{name} exceeded the reductions limit
    measured: #{median_reductions}
    baseline: #{baseline_reductions}
    allowed: #{maximum_reductions} (+#{maximum_reduction_regression_percent}%)
    optimize the regression before treating the pull request as complete
    """)
  end

  if median_microseconds > Keyword.fetch!(limits, :maximum_microseconds) do
    record_failure.("""
    #{name} exceeded the wall-clock limit
    measured: #{Float.round(median_microseconds / 1_000, 3)} ms
    allowed: #{Keyword.fetch!(limits, :maximum_microseconds) / 1_000} ms
    rerun on an idle host before deciding whether this is a regression
    """)
  end

  case Keyword.fetch(limits, :maximum_result_bytes) do
    {:ok, maximum} when is_integer(result_bytes) and result_bytes > maximum ->
      record_failure.("""
      #{name} exceeded the output-size limit
      measured: #{result_bytes} bytes
      allowed: #{maximum} bytes
      """)

    _ ->
      :ok
  end

  if report_path do
    memory_samples = Enum.map(1..3, fn _ -> measure_memory.(operation) end)

    evidence =
      Map.merge(result, %{
        result_bytes: result_bytes || :null,
        samples: Enum.map(samples, &Map.take(&1, [:microseconds, :reductions])),
        sampled_peak_vm_bytes: Enum.max(memory_samples),
        memory_samples: memory_samples
      })

    Process.put(:performance_results, [evidence | Process.get(:performance_results)])
  end

  latest_value
end

fixture_directory = Path.expand("../test/fixtures/html_to_pdf/documents", __DIR__)

fixtures = [
  {"invoice_012.html", [page_size: :a4], %{"1.19" => 2_470_000, "1.20" => 2_400_000}, 162_436},
  {"statement_012.html", [page_size: :a4], %{"1.19" => 2_350_000, "1.20" => 2_290_000}, 161_282},
  {"multi_page_report_012.html", [page_size: :a4], %{"1.19" => 2_700_000, "1.20" => 2_600_000},
   162_795},
  {"purchase_order.html", [page_size: :a4], %{"1.19" => 3_640_000, "1.20" => 3_500_000}, 124_904},
  {"material_requisition.html", [page_size: :a4], %{"1.19" => 4_630_000, "1.20" => 4_450_000},
   126_232},
  # Approved size baseline: retain the full source font for later field editing.
  # The 2026-10-15 capture is 688,413 bytes, including a 382,174-byte font snapshot.
  # Timing and reductions retain their original pre-milestone baselines.
  {"government_application_form.html", [page_size: :a4],
   %{"1.19" => 3_850_000, "1.20" => 3_680_000}, 688_413},
  {"stock_sticker.html", [page_size: {4.92126, 1.49606}, margin: 0],
   %{"1.19" => 1_410_000, "1.20" => 1_400_000}, 59_392},
  {"trim_card.html", [page_size: {11.6929, 8.2677}, margin: 0],
   %{"1.19" => 3_810_000, "1.20" => 3_720_000}, 163_606}
]

# These IDs belong to the fixed pre-milestone capture. File placement can change
# without changing workload identity, source bytes, thresholds or input hashes.
fixture_sources = %{
  "invoice_012.html" => "invoice.html",
  "statement_012.html" => "statement.html",
  "multi_page_report_012.html" => "multi_page_report.html"
}

Enum.each(fixtures, fn {file, opts, baseline_reductions, baseline_bytes} ->
  source = Map.get(fixture_sources, file, file)
  html = fixture_directory |> Path.join(source) |> File.read!()

  measure.(
    "fixture #{file}",
    fn -> HtmlToPdf.render(html, opts) end,
    baseline_reductions: baseline_reductions,
    maximum_microseconds: 3_000_000,
    maximum_result_bytes: div(baseline_bytes * 105, 100)
  )
end)

rows =
  Enum.map_join(1..100, fn item ->
    """
    <tr><td style="padding: 3pt; width: 30pt">#{item}</td>
    <td style="padding: 3pt"><table><tr><td style="padding: 3pt">Part #{item}</td></tr>
    <tr><td style="padding: 3pt">Synthetic component with a longer description for wrapping</td></tr></table></td>
    <td style="padding: 3pt; text-align: right">12</td>
    <td style="padding: 3pt; text-align: right">123.45</td>
    <td style="padding: 3pt; text-align: right">1,481.40</td></tr>
    """
  end)

synthetic_html = """
<html><head><style>
@page { size: A4; margin: 24pt; }
body { font-family: DejaVu Sans; font-size: 10pt; }
table { width: 100%; border-collapse: collapse; }
th { text-align: left; padding: 3pt; }
</style></head><body>
<h2>Synthetic purchase order</h2><p>Example company<br>Example address</p>
<table><thead><tr><th>Item</th><th>Description</th><th>Qty</th><th>Price</th><th>Total</th></tr></thead>
<tbody>#{rows}</tbody></table>
</body></html>
"""

synthetic_pdf =
  measure.(
    "100-row HTML render",
    fn -> HtmlToPdf.render(synthetic_html) end,
    baseline_reductions: %{"1.19" => 13_270_000, "1.20" => 11_840_000},
    maximum_microseconds: 2_000_000,
    maximum_result_bytes: div(166_852 * 105, 100)
  )

measure.(
  "100-row uncompressed full-font render",
  fn -> HtmlToPdf.render(synthetic_html, subset_fonts: false, compress_streams: false) end,
  baseline_reductions: %{"1.19" => 12_710_000, "1.20" => 11_270_000},
  maximum_microseconds: 3_000_000,
  maximum_result_bytes: div(1_632_177 * 105, 100)
)

measure.(
  "100-row PDF text extraction",
  fn -> Text.extract(synthetic_pdf, layout: false) end,
  baseline_reductions: %{"1.19" => 5_090_000, "1.20" => 4_600_000},
  maximum_microseconds: 2_000_000
)

measure.(
  "100-row PDF visual text extraction",
  fn -> Text.extract(synthetic_pdf, layout: true) end,
  baseline_reductions: %{"1.19" => 5_420_000, "1.20" => 4_920_000},
  maximum_microseconds: 2_000_000
)

repeated_text =
  Enum.map_join(1..80, fn _ ->
    "<p>Repeated synthetic component description with inspection, packaging, and delivery notes.</p>"
  end)

measure.(
  "repeated text layout",
  fn -> HtmlToPdf.render("<html><body>#{repeated_text}</body></html>") end,
  baseline_reductions: %{"1.19" => 3_290_000, "1.20" => 3_250_000},
  maximum_microseconds: 2_000_000,
  maximum_result_bytes: div(82_140 * 105, 100)
)

long_paragraph =
  1..220
  |> Enum.map_join(" ", fn item ->
    "synthetic-component-#{rem(item, 17)} requires inspection and careful wrapping"
  end)

measure.(
  "long paragraph layout",
  fn -> HtmlToPdf.render("<html><body><p>#{long_paragraph}</p></body></html>") end,
  baseline_reductions: %{"1.19" => 6_350_000, "1.20" => 6_250_000},
  maximum_microseconds: 2_000_000,
  maximum_result_bytes: div(85_076 * 105, 100)
)

png =
  PngFixture.build(128, 128, 6, 16, 1, fn x, y -> [x * 127, y * 127, 32_768, 40_000] end,
    filters: true
  )

images = String.duplicate("<img src=\"sample\" style=\"width:128px;height:128px\">", 5)

measure.(
  "five repeated PNG images",
  fn ->
    HtmlToPdf.render("<html><body>#{images}</body></html>", assets: %{"sample" => {:bytes, png}})
  end,
  baseline_reductions: %{"1.19" => 1_860_000, "1.20" => 1_860_000},
  maximum_microseconds: 3_000_000,
  maximum_result_bytes: div(37_811 * 105, 100)
)

large_png =
  PngFixture.build(
    512,
    512,
    6,
    16,
    1,
    fn x, y ->
      [x * 127, y * 127, 32_768, 40_000]
    end,
    filters: true
  )

large_images = String.duplicate("<img src=\"large\">", 10)

measure.(
  "ten repeated 512px PNG images",
  fn ->
    HtmlToPdf.render("<html><body>#{large_images}</body></html>",
      assets: %{"large" => {:bytes, large_png}}
    )
  end,
  baseline_reductions: %{"1.19" => 26_180_000, "1.20" => 26_150_000},
  maximum_microseconds: 3_000_000,
  maximum_result_bytes: div(634_859 * 105, 100)
)

adversarial_paragraph =
  1..800
  |> Enum.map_join(" ", fn index -> "synthetic#{rem(index, 11)}" end)
  |> then(&"<p style='width:180pt'>#{&1}</p>")

measure.(
  "800-word inline paragraph",
  fn -> HtmlToPdf.render(adversarial_paragraph) end,
  baseline_reductions: %{"1.19" => 4_140_000, "1.20" => 4_090_000},
  maximum_microseconds: 2_000_000,
  maximum_result_bytes: div(84_761 * 105, 100)
)

many_table_headers =
  Enum.map_join(1..160, fn index ->
    "<table><thead><tr><th>Section #{index}</th></tr></thead><tbody><tr><td>Value</td></tr></tbody></table>"
  end)

measure.(
  "160 table headings",
  fn -> HtmlToPdf.render(many_table_headers) end,
  baseline_reductions: %{"1.19" => 5_890_000, "1.20" => 5_700_000},
  maximum_microseconds: 2_000_000,
  maximum_result_bytes: div(155_020 * 105, 100)
)

many_boxes =
  Enum.map(1..4_000, fn index ->
    %{
      type: :text,
      text: "B#{index}",
      x: 0,
      y: index / 10,
      font: "Helvetica",
      font_size: 10,
      color: {0, 0, 0}
    }
  end)

measure.(
  "4,000 boxes on one page",
  fn -> PdfWriter.render([%{size: {500, 500}, boxes: many_boxes}]) end,
  baseline_reductions: %{"1.19" => 1_970_000, "1.20" => 1_370_000},
  maximum_microseconds: 2_000_000,
  maximum_result_bytes: div(21_226 * 105, 100)
)

if report_path do
  evidence_files =
    [
      __ENV__.file,
      Path.expand("../test/support/png_fixture.ex", __DIR__),
      Path.expand("../mix.lock", __DIR__)
    ] ++
      Enum.map(fixtures, fn {file, _opts, _reductions, _bytes} ->
        Path.join(fixture_directory, Map.get(fixture_sources, file, file))
      end) ++ Path.wildcard(Path.expand("../priv/fonts/**/*.ttf", __DIR__))

  input_sources =
    Map.new(fixtures, fn {file, _opts, _reductions, _bytes} ->
      {"test/fixtures/html_to_pdf/" <> file,
       "test/fixtures/html_to_pdf/documents/" <> Map.get(fixture_sources, file, file)}
    end)

  logical_input_names = Map.new(input_sources, fn {identity, source} -> {source, identity} end)

  file_hashes = fn files ->
    Map.new(files, fn path ->
      relative = Path.relative_to(path, Path.expand("..", __DIR__))
      {Map.get(logical_input_names, relative, relative),
       :crypto.hash(:sha256, File.read!(path)) |> Base.encode16(case: :lower)}
    end)
  end

  report = %{
    schema_version: 1,
    revision: System.get_env("PERFORMANCE_REVISION", "working tree"),
    elixir: System.version(),
    otp: :erlang.system_info(:otp_release) |> List.to_string(),
    erts: :erlang.system_info(:version) |> List.to_string(),
    architecture: :erlang.system_info(:system_architecture) |> List.to_string(),
    schedulers: :erlang.system_info(:schedulers_online),
    input_hashes: file_hashes.(evidence_files),
    input_sources: input_sources,
    library_hashes: file_hashes.(Path.wildcard(Path.expand("../lib/**/*.ex", __DIR__))),
    guard_failures: Enum.reverse(Process.get(:performance_failures)),
    warmups: 2,
    timed_samples: 5,
    memory_sample_interval_ms: 10,
    memory_runs: 3,
    memory_scope:
      "sampled total BEAM memory; excludes native allocations and may miss brief peaks",
    workloads: Enum.reverse(Process.get(:performance_results))
  }

  File.mkdir_p!(Path.dirname(report_path))
  File.write!(report_path, [:json.encode(report), "\n"])
  IO.puts("PERFORMANCE_REPORT path=#{report_path}")

  if baseline_path do
    baseline_file = baseline_path |> File.read!() |> :json.decode()

    baseline =
      case baseline_file do
        %{"reports" => reports} ->
          Map.fetch!(
            reports,
            System.get_env("PERFORMANCE_BASELINE_KEY", "elixir-#{runtime_version}.json")
          )

        single_report ->
          single_report
      end

    for key <- [:elixir, :otp, :erts, :architecture, :schedulers] do
      if Map.fetch!(baseline, Atom.to_string(key)) != Map.fetch!(report, key) do
        raise "fixed performance baseline has different #{key}; use the same runtime and VM settings"
      end
    end

    # The harness itself may gain checks. Workload fixtures, fonts, PNG builder
    # and dependency lock must retain their bytes across milestone comparisons.
    baseline_inputs =
      Map.fetch!(baseline, "input_hashes") |> Map.delete("scripts/performance-regression.exs")

    candidate_inputs = Map.delete(report.input_hashes, "scripts/performance-regression.exs")

    if baseline_inputs != candidate_inputs do
      raise "fixed performance baseline inputs differ; preserve fixtures, assets and dependencies"
    end

    baseline_workloads = Map.new(Map.fetch!(baseline, "workloads"), &{Map.fetch!(&1, "name"), &1})

    if Enum.sort(Map.keys(baseline_workloads)) != Enum.sort(Enum.map(report.workloads, & &1.name)) do
      raise "fixed performance baseline workloads differ; do not remove or replace measured cases"
    end

    for workload <- report.workloads do
      baseline_workload = Map.fetch!(baseline_workloads, workload.name)
      baseline_ms = Map.fetch!(baseline_workload, "median_ms")
      maximum_ms = baseline_ms * (100 + maximum_time_regression_percent) / 100
      change_percent = (workload.median_ms / baseline_ms - 1) * 100

      IO.inspect(
        %{
          name: workload.name,
          baseline_ms: baseline_ms,
          candidate_ms: workload.median_ms,
          time_change_percent: Float.round(change_percent, 3),
          maximum_ms: maximum_ms
        },
        label: "PERFORMANCE_MILESTONE"
      )

      if workload.median_ms > maximum_ms do
        record_failure.(
          "#{workload.name} exceeded the cumulative wall-clock allowance: " <>
            "#{workload.median_ms} ms versus fixed #{baseline_ms} ms, allowed +10%"
        )
      end
    end

  end
end

case Enum.reverse(Process.get(:performance_failures)) do
  [] ->
    IO.puts("PERFORMANCE_REGRESSION status=PASS")

    if baseline_path do
      IO.puts("PERFORMANCE_MILESTONE status=PASS allowance_percent=10 baseline=#{baseline_path}")
    end

  failures ->
    IO.puts("PERFORMANCE_REGRESSION status=FAIL failures=#{length(failures)}")
    raise Enum.join(failures, "\n\n")
end
