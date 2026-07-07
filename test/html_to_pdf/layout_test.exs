defmodule NativeElixirPdfUtilities.HtmlToPdf.LayoutTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.Layout

  test "layout positions a paragraph text box on the first page" do
    styled_tree = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          style: %{
            color: {0, 0, 0},
            font_family: "Helvetica",
            font_size: 12.0,
            line_height: 14.4
          },
          children: [%{type: :text, text: "Hello"}]
        }
      ]
    }

    assert {:ok, layout_tree} = Layout.layout(styled_tree, margin: "20mm")
    assert layout_tree.page_size == {595.28, 841.89}
    assert_in_delta layout_tree.margin, 56.6929, 0.0001

    [box] = layout_tree.boxes
    assert box.text == "Hello"
    assert_in_delta box.x, 56.6929, 0.0001
    assert_in_delta box.y, 773.1971, 0.0001
    assert box.font == "Helvetica"
  end

  test "layout rejects invalid options and unsupported trees" do
    assert Layout.layout(%{tag: "p", style: %{}}, []) == {:error, :invalid_layout}

    assert Layout.layout(%{type: :document, children: []}, page_size: :unknown) ==
             {:error, :invalid_layout}
  end
end
