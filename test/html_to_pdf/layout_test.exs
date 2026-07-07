defmodule NativeElixirPdfUtilities.HtmlToPdf.LayoutTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.Layout
  alias NativeElixirPdfUtilities.HtmlToPdf.Style

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

  test "layout accounts for margin padding border and background dimensions" do
    styled_tree = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          style: %{
            background_color: {0.9, 0.9, 0.9},
            border_color: {1, 0, 0},
            border_radius: 2.0,
            border_widths: %{top: 1.0, right: 1.0, bottom: 1.0, left: 1.0},
            color: {0, 0, 0},
            display: :block,
            font_family: "Helvetica",
            font_size: 12.0,
            font_style: :normal,
            font_weight: 400,
            line_height: 14.4,
            margin: %{top: 2.0, right: 4.0, bottom: 6.0, left: 8.0},
            padding: %{top: 3.0, right: 5.0, bottom: 3.0, left: 5.0}
          },
          children: [
            %{
              type: :text,
              text: "Boxed",
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

    assert {:ok, layout_tree} = Layout.layout(styled_tree, margin: 10)
    [background, text] = layout_tree.boxes

    assert background.type == :rect
    assert_in_delta background.x, 18.0, 0.0001
    assert_in_delta background.y, 807.49, 0.0001
    assert_in_delta background.width, 563.28, 0.0001
    assert_in_delta background.height, 22.4, 0.0001
    assert background.fill_color == {0.9, 0.9, 0.9}
    assert background.stroke_color == {1, 0, 0}
    assert background.stroke_width == 1.0
    assert background.border_radius == 2.0

    assert text.type == :text
    assert_in_delta text.x, 24.0, 0.0001
    assert_in_delta text.y, 813.89, 0.0001
    assert_in_delta text.width, 551.28, 0.0001
  end

  test "layout creates list markers and link annotation bounds" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "ol",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "li",
              attributes: %{},
              children: [
                %{type: :text, text: "Read "},
                %{
                  type: :element,
                  tag: "a",
                  attributes: %{"href" => "https://example.com"},
                  children: [%{type: :text, text: "docs"}]
                }
              ]
            },
            %{
              type: :element,
              tag: "li",
              attributes: %{},
              children: [%{type: :text, text: "Ship"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    assert {:ok, layout_tree} = Layout.layout(styled_tree, margin: 10)
    [first_marker, plain, link, second_marker, second_text] = layout_tree.boxes

    assert first_marker.text == "1."
    assert_in_delta first_marker.x, 34.0, 0.0001
    assert plain.text == "Read "
    assert_in_delta plain.x, 52.0, 0.0001
    assert link.text == "docs"
    assert link.link_url == "https://example.com"
    assert_in_delta link.x, 88.0, 0.0001
    assert_in_delta link.annotation_width, 28.8, 0.0001
    assert second_marker.text == "2."
    assert second_text.text == "Ship"
    assert second_text.y < plain.y
  end

  test "layout creates deterministic table columns captions borders and header cells" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "table",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "caption",
              attributes: %{},
              children: [%{type: :text, text: "Summary"}]
            },
            %{
              type: :element,
              tag: "thead",
              attributes: %{},
              children: [
                %{
                  type: :element,
                  tag: "tr",
                  attributes: %{},
                  children: [
                    %{
                      type: :element,
                      tag: "th",
                      attributes: %{},
                      children: [%{type: :text, text: "Name"}]
                    },
                    %{
                      type: :element,
                      tag: "th",
                      attributes: %{},
                      children: [%{type: :text, text: "Count"}]
                    }
                  ]
                }
              ]
            },
            %{
              type: :element,
              tag: "tbody",
              attributes: %{},
              children: [
                %{
                  type: :element,
                  tag: "tr",
                  attributes: %{},
                  children: [
                    %{
                      type: :element,
                      tag: "td",
                      attributes: %{},
                      children: [%{type: :text, text: "Alpha"}]
                    },
                    %{
                      type: :element,
                      tag: "td",
                      attributes: %{},
                      children: [%{type: :text, text: "2"}]
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    assert {:ok, layout_tree} = Layout.layout(styled_tree, margin: 10)

    [
      caption,
      first_header_cell,
      first_header_text,
      second_header_cell,
      second_header_text,
      first_data_cell,
      first_data_text,
      second_data_cell,
      second_data_text
    ] = layout_tree.boxes

    assert caption.text == "Summary"
    assert_in_delta caption.x, 272.44, 0.0001

    assert first_header_cell.type == :rect
    assert_in_delta first_header_cell.x, 10.0, 0.0001
    assert_in_delta first_header_cell.y, 789.09, 0.0001
    assert_in_delta first_header_cell.width, 287.64, 0.0001
    assert_in_delta first_header_cell.height, 24.4, 0.0001
    assert first_header_cell.fill_color == {0.9333333333, 0.9333333333, 0.9333333333}
    assert first_header_cell.stroke_width == 1.0

    assert first_header_text.text == "Name"
    assert first_header_text.font == "Helvetica-Bold"
    assert_in_delta first_header_text.x, 139.42, 0.0001
    assert_in_delta first_header_text.y, 796.49, 0.0001

    assert_in_delta second_header_cell.x, 297.64, 0.0001
    assert second_header_text.text == "Count"
    assert second_header_text.font == "Helvetica-Bold"
    assert second_header_text.x > second_header_cell.x

    assert_in_delta first_data_cell.y, 764.69, 0.0001
    assert first_data_text.text == "Alpha"
    assert_in_delta first_data_text.x, 15.0, 0.0001
    assert_in_delta first_data_text.y, 772.09, 0.0001
    assert second_data_cell.x > first_data_cell.x
    assert second_data_text.text == "2"
  end

  test "layout rejects invalid options and unsupported trees" do
    assert Layout.layout(%{tag: "p", style: %{}}, []) == {:error, :invalid_layout}

    assert Layout.layout(%{type: :document, children: []}, page_size: :unknown) ==
             {:error, :invalid_page_size}
  end
end
