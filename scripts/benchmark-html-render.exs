# Run with: mise exec -- mix run scripts/benchmark-html-render.exs [private.html]
# With no argument, uses a synthetic 250-item purchase-order-shaped document.
# Only aggregate measurements are printed. HTML and PDF content are never written.
alias NativeElixirPdfUtilities.HtmlToPdf

alias NativeElixirPdfUtilities.HtmlToPdf.{
  CssParser,
  FontFallback,
  HtmlParser,
  Layout,
  PageGeometry,
  Pagination,
  PdfWriter,
  Style
}

html =
  case System.argv() do
    [path] ->
      File.read!(path)

    [] ->
      groups =
        1..250
        |> Enum.chunk_every(15)
        |> Enum.map_join(fn items ->
          rows =
            Enum.map_join(items, fn item ->
              """
              <tr><td style="padding: 3pt; width: 30pt">#{item}</td>
              <td style="padding: 3pt"><table><tr><td style="padding: 3pt">Part #{item}</td></tr>
              <tr><td style="padding: 3pt">Synthetic component with a longer description for wrapping</td></tr></table></td>
              <td style="padding: 3pt; text-align: right">12</td>
              <td style="padding: 3pt; text-align: right">123.45</td>
              <td style="padding: 3pt; text-align: right">1,481.40</td></tr>
              """
            end)

          "<section><h2>Synthetic purchase order</h2><p>Example company<br>Example address</p>" <>
            "<table><thead><tr><th>Item</th><th>Description</th><th>Qty</th><th>Price</th><th>Total</th></tr></thead>" <>
            "<tbody>#{rows}</tbody></table></section>"
        end)

      """
      <html><head><style>
      @page { size: A4; margin: 24pt; }
      body { font-family: DejaVu Sans; font-size: 10pt; }
      table { width: 100%; border-collapse: collapse; }
      th { text-align: left; padding: 3pt; }
      section { break-before: page; }
      </style></head><body>#{groups}</body></html>
      """

    _ ->
      raise "expected at most one HTML path"
  end

measure = fn name, fun ->
  {:reductions, before} = Process.info(self(), :reductions)
  {microseconds, result} = :timer.tc(fun)
  {:reductions, after_count} = Process.info(self(), :reductions)

  case result do
    {:ok, value} -> {value, {name, Float.round(microseconds / 1_000, 2), after_count - before}}
    _ -> raise "benchmark stage #{name} failed"
  end
end

run = fn ->
  {dom, parsing} = measure.(:parsing, fn -> HtmlParser.parse_detailed(html) end)
  {:ok, entries} = Style.load_stylesheets(dom, [])

  opts =
    Enum.reduce(entries, [], fn entry, opts ->
      {:ok, page_opts} = CssParser.page_options(entry.css)
      PageGeometry.merge_page_options(opts, page_opts)
    end)

  {styled, styling} = measure.(:styling, fn -> Style.compute_detailed(dom, opts) end)
  {resolved, fallback} = measure.(:fallback, fn -> FontFallback.resolve(styled) end)
  {layout, layout_time} = measure.(:layout, fn -> Layout.layout(resolved, opts) end)
  {pages, pagination} = measure.(:pagination, fn -> Pagination.paginate(layout, opts) end)
  {_pdf, writing} = measure.(:writing, fn -> PdfWriter.render(pages, opts) end)
  {pdf, total} = measure.(:public_render, fn -> HtmlToPdf.render(html) end)

  %{
    stages: [parsing, styling, fallback, layout_time, pagination, writing],
    public_render: total,
    pages: length(pages),
    pdf_bytes: byte_size(pdf),
    pdf_sha256: Base.encode16(:crypto.hash(:sha256, pdf), case: :lower)
  }
end

IO.puts(
  "Times are milliseconds; reductions are per calling process. Stage totals exclude option preparation."
)

IO.puts(
  "Peak memory is sampled VM memory, including binaries and shared caches, not an exact allocation peak."
)

IO.inspect(run.(), label: "cold")

for iteration <- 1..3 do
  :erlang.garbage_collect()
  owner = self()

  sampler =
    spawn_link(fn ->
      sample = fn sample, peak ->
        peak = max(peak, :erlang.memory(:total))

        receive do
          :stop -> send(owner, {:peak_memory, peak})
        after
          10 -> sample.(sample, peak)
        end
      end

      sample.(sample, 0)
    end)

  result = run.()
  send(sampler, :stop)

  receive do
    {:peak_memory, peak} ->
      IO.inspect(Map.put(result, :sampled_peak_vm_bytes, peak), label: "warm #{iteration}")
  end
end
