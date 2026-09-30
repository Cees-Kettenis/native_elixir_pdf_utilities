# Run with: mix run scripts/generate-html-to-pdf-examples.exs
# The guide is the source for both the downloadable HTML and the rendered output.

repository = Path.expand("..", __DIR__)
guide_path = Path.join(repository, "docs/html-to-pdf-examples.md")
output_dir = Path.join(repository, "docs/assets/html-to-pdf-examples")
guide = File.read!(guide_path)
File.mkdir_p!(output_dir)

for [_, filename, content] <-
      Regex.scan(~r/Save as `([^`]+)`\.\n\n```(?:html|css)\n(.*?)\n```/s, guide) do
  File.write!(Path.join(output_dir, filename), content <> "\n")
end

File.cd!(output_dir, fn ->
  for name <- ["logo", "product"] do
    svg = File.read!("#{name}.svg")
    {:ok, png} = NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer.rasterize(svg, [], nil)
    File.write!("#{name}.png", png)
  end

  for {[_, code], index} <- Enum.with_index(Regex.scan(~r/```elixir\n(.*?)\n```/s, guide), 1) do
    Code.eval_string(code, [], file: "#{guide_path}:example-#{index}")
    IO.puts("Example #{index}: rendered")
  end

  pdftoppm =
    System.find_executable("pdftoppm") || raise "Install Poppler to generate PDF previews"

  for path <- Path.wildcard("*.pdf") do
    pdf = File.read!(path)
    {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    {:ok, page_count} = NativeElixirPdfUtilities.Info.page_count(pdf)
    basename = Path.rootname(path)
    dpi = if basename == "stock-labels", do: "300", else: "90"

    {_, 0} =
      System.cmd(pdftoppm, [
        "-f",
        "1",
        "-singlefile",
        "-r",
        dpi,
        "-png",
        path,
        "#{basename}-page-1"
      ])

    if page_count > 1 do
      {_, 0} =
        System.cmd(pdftoppm, [
          "-f",
          to_string(page_count),
          "-singlefile",
          "-r",
          dpi,
          "-png",
          path,
          "#{basename}-last-page"
        ])
    end

    IO.puts("#{path}: #{page_count} page(s), preview generated")
  end
end)
