alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.PageFurnitureNestedBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity

  test "MathML and embedded paint stay in their running margins on every page" do
    fixture = Paths.fixture("html_to_pdf/browser_parity/page_furniture_nested_paint.html")
    artifacts = "tmp/browser_parity/page_furniture_nested_paint"

    # Chromium paints the original explicit page sections. Native rendering uses
    # the same header/footer markup through the running-furniture API, avoiding
    # Chromium's fixed-container print placement for nested MathML and iframes.
    stats =
      PdfVisualCompare.with_embedded_font_configuration(fixture, fn ->
        PdfVisualCompare.assert_browser_match!(fixture,
          artifact_dir: artifacts,
          native_renderer: fn html, opts ->
            [_, header] = Regex.run(~r/<header>(.*?)<\/header>/s, html)
            [_, footer] = Regex.run(~r/<footer>(.*?)<\/footer>/s, html)

            body =
              Enum.map_join(Regex.scan(~r/<main>(.*?)<\/main>/s, html), fn [_, content] ->
                "<section>#{content}</section>"
              end)

            source = """
            <style>
            @page {size:300pt 240pt;margin:40pt}
            html,body {margin:0;padding:0;font-family:'DejaVu Sans';font-size:10pt;line-height:14pt}
            section {height:140pt;break-after:page}
            section:last-child {break-after:auto}
            </style>#{body}
            """

            File.write!(Path.join(artifacts, "native-input.html"), source)

            PdfVisualCompare.render_embedded_font_bytes(
              source,
              opts
              |> Keyword.put(:margin, 40)
              |> Keyword.put(:page_furniture, header: header, footer: footer),
              fixture
            )
          end,
          regions: [
            {"header margin", {0, 0, 300, 40}},
            {"header child", {132, 0, 120, 40}},
            {"footer margin", {0, 200, 300, 40}},
            {"footer child", {132, 200, 120, 40}}
          ],
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )
      end)

    assert stats.page_count == 2
  end
end
