alias NativeElixirPdfUtilities.HtmlToPdf
alias NativeElixirPdfUtilities.Text

Code.require_file("../test/support/png_fixture.ex", __DIR__)
alias NativeElixirPdfUtilities.TestSupport.PngFixture

maximum_reduction_regression_percent = 5

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
      {:reductions, reductions_before} = Process.info(self(), :reductions)
      {microseconds, result} = :timer.tc(operation)
      {:reductions, reductions_after} = Process.info(self(), :reductions)

      value =
        case result do
          {:ok, value} -> value
          failure -> raise "#{name} sample failed: #{inspect(failure)}"
        end

      %{
        microseconds: microseconds,
        reductions: reductions_after - reductions_before,
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

  baseline_reductions = Keyword.fetch!(limits, :baseline_reductions)

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
    raise """
    #{name} exceeded the reductions limit
    measured: #{median_reductions}
    baseline: #{baseline_reductions}
    allowed: #{maximum_reductions} (+#{maximum_reduction_regression_percent}%)
    optimize the regression before treating the pull request as complete
    """
  end

  if median_microseconds > Keyword.fetch!(limits, :maximum_microseconds) do
    raise """
    #{name} exceeded the wall-clock limit
    measured: #{Float.round(median_microseconds / 1_000, 3)} ms
    allowed: #{Keyword.fetch!(limits, :maximum_microseconds) / 1_000} ms
    rerun on an idle host before deciding whether this is a regression
    """
  end

  case Keyword.fetch(limits, :maximum_result_bytes) do
    {:ok, maximum} when is_integer(result_bytes) and result_bytes > maximum ->
      raise """
      #{name} exceeded the output-size limit
      measured: #{result_bytes} bytes
      allowed: #{maximum} bytes
      """

    _ ->
      :ok
  end

  latest_value
end

fixture_directory = Path.expand("../test/fixtures/html_to_pdf", __DIR__)

fixtures = [
  {"invoice_012.html", [page_size: :a4], 3_930_000, 1_482_140},
  {"statement_012.html", [page_size: :a4], 3_950_000, 1_480_565},
  {"multi_page_report_012.html", [page_size: :a4], 4_650_000, 1_484_879},
  {"purchase_order.html", [page_size: :a4], 6_300_000, 851_598},
  {"material_requisition.html", [page_size: :a4], 8_350_000, 856_939},
  {"government_application_form.html", [page_size: :a4], 6_230_000, 2_257_618},
  {"stock_sticker.html", [page_size: {4.92126, 1.49606}, margin: 0], 2_530_000, 415_886},
  {"trim_card.html", [page_size: {11.6929, 8.2677}, margin: 0], 7_540_000, 1_486_342}
]

Enum.each(fixtures, fn {file, opts, baseline_reductions, baseline_bytes} ->
  html = fixture_directory |> Path.join(file) |> File.read!()

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
    baseline_reductions: 53_200_000,
    maximum_microseconds: 2_000_000,
    maximum_result_bytes: div(1_632_177 * 105, 100)
  )

measure.(
  "100-row PDF text extraction",
  fn -> Text.extract(synthetic_pdf, layout: false) end,
  baseline_reductions: 9_830_000,
  maximum_microseconds: 2_000_000
)

measure.(
  "100-row PDF visual text extraction",
  fn -> Text.extract(synthetic_pdf, layout: true) end,
  baseline_reductions: 10_380_000,
  maximum_microseconds: 2_000_000
)

repeated_text =
  Enum.map_join(1..80, fn _ ->
    "<p>Repeated synthetic component description with inspection, packaging, and delivery notes.</p>"
  end)

measure.(
  "repeated text layout",
  fn -> HtmlToPdf.render("<html><body>#{repeated_text}</body></html>") end,
  baseline_reductions: 8_840_000,
  maximum_microseconds: 2_000_000,
  maximum_result_bytes: div(796_174 * 105, 100)
)

long_paragraph =
  1..220
  |> Enum.map_join(" ", fn item ->
    "synthetic-component-#{rem(item, 17)} requires inspection and careful wrapping"
  end)

measure.(
  "long paragraph layout",
  fn -> HtmlToPdf.render("<html><body><p>#{long_paragraph}</p></body></html>") end,
  baseline_reductions: 15_850_000,
  maximum_microseconds: 2_000_000,
  maximum_result_bytes: div(832_420 * 105, 100)
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
  baseline_reductions: 6_850_000,
  maximum_microseconds: 3_000_000,
  maximum_result_bytes: div(37_811 * 105, 100)
)

IO.puts("PERFORMANCE_REGRESSION status=PASS")
