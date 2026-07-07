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
            display: :block,
            font_family: "Helvetica",
            font_size: 12.0,
            font_style: :normal,
            font_weight: 400,
            line_height: 14.4,
            margin_after: 12.0
          },
          children: [
            %{
              type: :text,
              text: "Hello",
              style: %{
                color: {0, 0, 0},
                font_family: "Helvetica",
                font_size: 12.0,
                font_style: :normal,
                font_weight: 400,
                line_height: 14.4
              }
            }
          ]
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

  test "layout creates separate text boxes for inline styles" do
    styled_tree = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          style: %{
            color: {0, 0, 0},
            display: :block,
            font_family: "Helvetica",
            font_size: 12.0,
            font_style: :normal,
            font_weight: 400,
            line_height: 14.4,
            margin_after: 12.0
          },
          children: [
            %{
              type: :text,
              text: "A ",
              style: %{
                color: {0, 0, 0},
                font_family: "Helvetica",
                font_size: 12.0,
                font_style: :normal,
                font_weight: 400,
                line_height: 14.4
              }
            },
            %{
              type: :element,
              tag: "strong",
              style: %{
                color: {1, 0, 0},
                display: :inline,
                font_family: "Helvetica",
                font_size: 12.0,
                font_style: :normal,
                font_weight: 700,
                line_height: 14.4
              },
              children: [
                %{
                  type: :text,
                  text: "bold",
                  style: %{
                    color: {1, 0, 0},
                    font_family: "Helvetica",
                    font_size: 12.0,
                    font_style: :normal,
                    font_weight: 700,
                    line_height: 14.4
                  }
                }
              ]
            },
            %{
              type: :element,
              tag: "em",
              style: %{
                color: {0, 0, 1},
                display: :inline,
                font_family: "Helvetica",
                font_size: 12.0,
                font_style: :italic,
                font_weight: 400,
                line_height: 14.4
              },
              children: [
                %{
                  type: :text,
                  text: "italic",
                  style: %{
                    color: {0, 0, 1},
                    font_family: "Helvetica",
                    font_size: 12.0,
                    font_style: :italic,
                    font_weight: 400,
                    line_height: 14.4
                  }
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, layout_tree} = Layout.layout(styled_tree, margin: 10)
    [plain, bold, italic] = layout_tree.boxes

    assert plain.text == "A "
    assert plain.font == "Helvetica"
    assert bold.text == "bold"
    assert bold.font == "Helvetica-Bold"
    assert bold.color == {1, 0, 0}
    assert italic.text == "italic"
    assert italic.font == "Helvetica-Oblique"
    assert italic.color == {0, 0, 1}
    assert bold.x > plain.x
    assert italic.x > bold.x
  end

  test "layout rejects invalid options and unsupported trees" do
    assert Layout.layout(%{tag: "p", style: %{}}, []) == {:error, :invalid_layout}

    assert Layout.layout(%{type: :document, children: []}, page_size: :unknown) ==
             {:error, :invalid_page_size}
  end
end
