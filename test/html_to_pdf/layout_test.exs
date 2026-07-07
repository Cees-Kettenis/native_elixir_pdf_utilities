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

  test "layout positions row flex items with order gap justify-content and align-items" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{
            "style" =>
              "display: flex; width: 120pt; height: 40pt; gap: 10pt; justify-content: center; align-items: center"
          },
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "order: 2; width: 20pt; height: 20pt"},
              children: [%{type: :text, text: "A"}]
            },
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "order: 1; width: 20pt; height: 20pt"},
              children: [%{type: :text, text: "B"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    assert {:ok, layout_tree} = Layout.layout(styled_tree, page_size: {200, 100}, margin: 10)

    [first, second] = layout_tree.boxes

    assert first.text == "B"
    assert second.text == "A"
    assert_in_delta first.x, 45.0, 0.0001
    assert_in_delta second.x, 75.0, 0.0001
    assert_in_delta first.y, 68.0, 0.0001
    assert_in_delta second.y, 68.0, 0.0001
  end

  test "layout grows flex items and wraps rows deterministically" do
    grow_dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{"style" => "display: flex; width: 90pt; gap: 10pt"},
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "flex: 1 1 20pt"},
              children: [%{type: :text, text: "A"}]
            },
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "flex: 2 1 20pt"},
              children: [%{type: :text, text: "B"}]
            }
          ]
        }
      ]
    }

    assert {:ok, grow_tree} = Style.compute(grow_dom, [])
    assert {:ok, grow_layout} = Layout.layout(grow_tree, page_size: {200, 100}, margin: 10)
    [first, second] = grow_layout.boxes

    assert_in_delta first.width, 33.3333, 0.0001
    assert_in_delta second.x, 53.3333, 0.0001
    assert_in_delta second.width, 46.6667, 0.0001

    wrap_dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{"style" => "display: flex; flex-wrap: wrap; width: 50pt; gap: 10pt 5pt"},
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "width: 20pt"},
              children: [%{type: :text, text: "A"}]
            },
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "width: 20pt"},
              children: [%{type: :text, text: "B"}]
            },
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "width: 20pt"},
              children: [%{type: :text, text: "C"}]
            }
          ]
        }
      ]
    }

    assert {:ok, wrap_tree} = Style.compute(wrap_dom, [])
    assert {:ok, wrap_layout} = Layout.layout(wrap_tree, page_size: {200, 100}, margin: 10)
    [a, b, c] = wrap_layout.boxes

    assert a.text == "A"
    assert b.text == "B"
    assert c.text == "C"
    assert_in_delta a.x, 10.0, 0.0001
    assert_in_delta b.x, 35.0, 0.0001
    assert_in_delta c.x, 10.0, 0.0001
    assert c.y < a.y
  end

  test "layout positions column flex items with cross-axis alignment" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{
            "style" =>
              "display: flex; flex-direction: column; width: 60pt; gap: 4pt; align-items: flex-end"
          },
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "width: 20pt; height: 20pt"},
              children: [%{type: :text, text: "A"}]
            },
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "width: 30pt; height: 20pt; align-self: center"},
              children: [%{type: :text, text: "B"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    assert {:ok, layout_tree} = Layout.layout(styled_tree, page_size: {200, 100}, margin: 10)

    [first, second] = layout_tree.boxes

    assert first.text == "A"
    assert second.text == "B"
    assert_in_delta first.x, 50.0, 0.0001
    assert_in_delta second.x, 25.0, 0.0001
    assert second.y < first.y
  end

  test "layout positions grid items with explicit placement gaps and alignment" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{
            "style" =>
              "display: grid; width: 120pt; height: 60pt; grid-template-columns: 30pt 30pt; grid-template-rows: 20pt 20pt; gap: 10pt; justify-content: center; align-content: center; justify-items: center; align-items: end"
          },
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "grid-column: 2 / 3; grid-row: 1 / 2"},
              children: [%{type: :text, text: "A"}]
            },
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "grid-area: 2 / 1 / 3 / 2; align-self: start"},
              children: [%{type: :text, text: "B"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    assert {:ok, layout_tree} = Layout.layout(styled_tree, page_size: {200, 120}, margin: 10)

    [first, second] = layout_tree.boxes

    assert first.text == "A"
    assert second.text == "B"
    assert_in_delta first.x, 86.4, 0.0001
    assert_in_delta first.y, 87.4, 0.0001
    assert_in_delta second.x, 46.4, 0.0001
    assert_in_delta second.y, 63.0, 0.0001
  end

  test "layout auto places grid items and adds implicit tracks deterministically" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{
            "style" =>
              "display: grid; width: 80pt; grid-template-columns: 20pt 20pt; grid-auto-columns: 15pt; grid-auto-rows: 15pt; gap: 5pt"
          },
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{},
              children: [%{type: :text, text: "A"}]
            },
            %{
              type: :element,
              tag: "span",
              attributes: %{},
              children: [%{type: :text, text: "B"}]
            },
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "grid-column: 3 / 4"},
              children: [%{type: :text, text: "C"}]
            },
            %{
              type: :element,
              tag: "span",
              attributes: %{},
              children: [%{type: :text, text: "D"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    assert {:ok, layout_tree} = Layout.layout(styled_tree, page_size: {200, 120}, margin: 10)

    [a, b, c, d] = layout_tree.boxes

    assert a.text == "A"
    assert b.text == "B"
    assert c.text == "C"
    assert d.text == "D"
    assert_in_delta a.x, 10.0, 0.0001
    assert_in_delta b.x, 35.0, 0.0001
    assert_in_delta c.x, 60.0, 0.0001
    assert_in_delta d.x, 10.0, 0.0001
    assert d.y < a.y
  end

  test "layout rejects invalid options and unsupported trees" do
    assert Layout.layout(%{tag: "p", style: %{}}, []) == {:error, :invalid_layout}

    assert Layout.layout(%{type: :document, children: []}, page_size: :unknown) ==
             {:error, :invalid_page_size}
  end
end
