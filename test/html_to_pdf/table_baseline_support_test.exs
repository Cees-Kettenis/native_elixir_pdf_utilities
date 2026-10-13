defmodule NativeElixirPdfUtilities.HtmlToPdf.TableBaselineSupportTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "effective baseline cells diagnose the missing row geometry after authored overrides" do
    for baseline <- ["valign=baseline", "style='vertical-align:baseline'"],
        tag <- ["tbody", "tr", "td"] do
      attributes = if tag == "td", do: baseline, else: ""
      row = if tag == "tr", do: baseline, else: ""
      group = if tag == "tbody", do: baseline, else: ""

      html =
        "<table><tbody #{group}><tr #{row}><td #{attributes} style='font-size:8pt'>Small</td><td style='font-size:16pt'>Large</td></tr></tbody></table>"

      html =
        if tag == "td" and String.starts_with?(baseline, "style="),
          do:
            String.replace(
              html,
              "#{baseline} style='font-size:8pt'",
              "style='vertical-align:baseline;font-size:8pt'"
            ),
          else: html

      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :style and diagnostic.reason == :unsupported_html
      assert diagnostic.message =~ "row baseline geometry"
      assert diagnostic.source == "td effective vertical-align=baseline"
      assert is_integer(diagnostic.line) and is_integer(diagnostic.column)
    end

    assert {:ok, _pdf} =
             HtmlToPdf.render(
               "<table><tbody valign=baseline><tr><td style='vertical-align:top;font-size:8pt'>Small</td><td style='vertical-align:top;font-size:16pt'>Large</td></tr></tbody></table>"
             )

    assert {:ok, _pdf} =
             HtmlToPdf.render(
               "<table><tr><td valign=baseline style='vertical-align:top'>Override</td><td valign=baseline style='display:none'>Hidden</td></tr></table>"
             )
  end

  test "direct prepared cells delegate effective baseline validation without raising" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<table><tr><td>Cell</td></tr></table>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    [table] = styled.children
    [row] = table.children
    [cell] = row.children

    altered = %{
      styled
      | children: [
          %{
            table
            | children: [
                %{
                  row
                  | children: [%{cell | style: Map.put(cell.style, :vertical_align, :baseline)}]
                }
              ]
          }
        ]
    }

    assert {:error, {:unsupported_html, diagnostic}} = Layout.layout(altered)
    assert diagnostic.stage == :layout and diagnostic.operation == :layout

    assert diagnostic.reason == :unsupported_html and
             diagnostic.source == "td effective vertical-align=baseline"

    assert diagnostic.message =~ "vertical-align:top"
  end
end
