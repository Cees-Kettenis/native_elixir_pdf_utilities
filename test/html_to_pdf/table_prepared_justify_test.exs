defmodule NativeElixirPdfUtilities.HtmlToPdf.TablePreparedJustifyTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator

  test "a prepared cell without source attributes diagnoses effective inherited justification" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed("<table><tr><td>Prepared cell</td></tr></table>")

    assert {:ok, styled} = Style.compute_detailed(dom)

    prepared =
      update_in(
        styled.children,
        [Access.at(0), :children, Access.at(0), :children, Access.at(0)],
        fn cell ->
          cell
          |> Map.delete(:attributes)
          |> Map.update!(:style, &Map.put(&1, :text_align, :justify))
        end
      )

    [table] = prepared
    [row] = table.children
    [cell] = row.children
    refute Map.has_key?(cell, :attributes)

    assert {:error, {:unsupported_html, diagnostic}} =
             Layout.layout(%{styled | children: prepared})

    assert diagnostic.stage == :layout and diagnostic.operation == :layout
    assert diagnostic.reason == :unsupported_html and diagnostic.module == HtmlTableValidator
    assert diagnostic.source == "td inherited text-align=justify"
    assert diagnostic.message =~ "text justification"
    assert diagnostic.message =~ "left, center or right"
  end
end
