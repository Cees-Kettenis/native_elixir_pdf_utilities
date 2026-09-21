# Run with mise exec -- mix run scripts/benchmark-images.exs MODE
# Optionally wrap with an OS memory profiler.
# The OS peak includes the BEAM, native renderer, and compilation startup.
Code.require_file("../test/support/png_fixture.ex", __DIR__)
alias NativeElixirPdfUtilities.TestSupport.PngFixture, as: PNG
alias NativeElixirPdfUtilities.HtmlToPdf

mode = List.first(System.argv()) || "svg"

{html, opts} =
  case mode do
    "png" ->
      png =
        PNG.build(512, 512, 6, 16, 1, fn x, y -> [x * 127, y * 127, 32768, 40000] end,
          filters: true
        )

      {"<img src=\"sample\">", [assets: %{"sample" => {:bytes, png}}]}

    _ ->
      content =
        if mode == "filters" do
          filters =
            Enum.map_join(1..32, fn i -> "<feOffset dx=\"1\" dy=\"1\" result=\"r#{i}\"/>" end)

          "<defs><filter id=\"f\">#{filters}</filter></defs><rect width=\"256\" height=\"256\" fill=\"red\" filter=\"url(#f)\"/>"
        else
          Enum.map_join(1..2000, fn i ->
            "<rect x=\"#{rem(i, 256)}\" y=\"#{rem(div(i, 256), 256)}\" width=\"1\" height=\"1\" fill=\"red\"/>"
          end)
        end

      svg =
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"256\" height=\"256\">#{content}</svg>"

      {"<img src=\"data:image/svg+xml;base64,#{Base.encode64(svg)}\">", []}
  end

{microseconds, results} =
  :timer.tc(fn ->
    if mode == "concurrent" do
      1..4
      |> Task.async_stream(fn _ -> HtmlToPdf.render(html, opts) end,
        max_concurrency: 4,
        timeout: 30_000
      )
      |> Enum.map(fn {:ok, result} -> result end)
    else
      [HtmlToPdf.render(html, opts)]
    end
  end)

IO.inspect(%{
  mode: mode,
  milliseconds: microseconds / 1000,
  outputs:
    Enum.map(results, fn result ->
      case result do
        {:ok, pdf} -> {:ok, byte_size(pdf)}
        error -> error
      end
    end)
})
