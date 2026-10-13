defmodule NativeElixirPdfUtilities.HtmlToPdf.TableBaselineSupportTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "effective baseline hints preserve computed styles and use actual fragmented print geometry" do
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

      assert {:ok, _pdf} = HtmlToPdf.render(html)
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      [table] = styled.children

      assert {:ok, %{rows: [row]}} =
               NativeElixirPdfUtilities.Validators.HtmlTableValidator.form(
                 table.children,
                 table.style
               )

      assert hd(row.cells).cell.style.vertical_align == :baseline
      assert {:ok, layout} = Layout.layout(styled)
      small = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Small"))
      large = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Large"))
      assert small.y > large.y
      assert is_integer(small.table_cell.source_metadata.line)
      assert is_integer(small.table_cell.source_metadata.column)
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

  test "direct prepared cells delegate effective baseline measurement after an authored override" do
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

    assert {:ok, layout} = Layout.layout(altered)
    assert {:ok, surface} = Layout.layout_viewport(altered, 200.0, 100.0)
    assert Enum.any?(layout.boxes, &(Map.get(&1, :text) == "Cell"))
    assert Enum.any?(surface.boxes, &(Map.get(&1, :text) == "Cell"))
    assert hd(hd(hd(altered.children).children).children).style.vertical_align == :baseline
  end

  test "allocation-dependent baseline descendants retain actionable shared failure and authored override" do
    html =
      "<table><tr><td style='vertical-align:baseline'><div style='height:50%'>Pending</div></td></tr></table>"

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.stage == :layout
    assert diagnostic.reason == :unsupported_html
    assert diagnostic.operation == :layout
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlTableValidator
    assert diagnostic.source == "td baseline percentage-height descendant"
    assert diagnostic.message =~ "allocation-dependent baseline geometry"
    assert diagnostic.message =~ "vertical-align:top"
    assert is_integer(diagnostic.line) and is_integer(diagnostic.column)

    assert {:ok, _pdf} =
             HtmlToPdf.render(
               String.replace(html, "vertical-align:baseline", "vertical-align:top")
             )
  end
end
