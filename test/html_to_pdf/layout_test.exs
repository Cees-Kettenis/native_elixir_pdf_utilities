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

  test "layout measures embedded font text with TTF glyph widths" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "font-family: 'Fixture Sans'"},
          children: [
            %{type: :text, text: "iiii"},
            %{
              type: :element,
              tag: "span",
              attributes: %{},
              children: [%{type: :text, text: "WWWW"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} =
             Style.compute(dom, fonts: [%{family: "Fixture Sans", path: ttf_font_path!()}])

    assert {:ok, layout_tree} = Layout.layout(styled_tree, margin: 10)
    [narrow, wide] = layout_tree.boxes

    assert narrow.font =~ "Embedded-"
    assert wide.font == narrow.font
    assert narrow.annotation_width < wide.annotation_width
    assert wide.x > narrow.x
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

  test "layout sizes and positions image boxes with CSS dimensions" do
    styled_tree = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "img",
          style: %{
            background_color: {0.9, 0.9, 0.9},
            border_color: {0, 0, 1},
            border_radius: 0.0,
            border_widths: %{top: 1.0, right: 1.0, bottom: 1.0, left: 1.0},
            color: {0, 0, 0},
            display: :image,
            font_family: "Helvetica",
            font_size: 12.0,
            font_style: :normal,
            font_weight: 400,
            image: image_fixture(20.0, 10.0),
            line_height: 14.4,
            margin: %{top: 2.0, right: 0.0, bottom: 4.0, left: 3.0},
            padding: %{top: 2.0, right: 2.0, bottom: 2.0, left: 2.0},
            width: 30.0
          },
          children: []
        }
      ]
    }

    assert {:ok, layout_tree} = Layout.layout(styled_tree, page_size: {100, 100}, margin: 10)
    [background, image] = layout_tree.boxes

    assert background.type == :rect
    assert_in_delta background.x, 13.0, 0.0001
    assert_in_delta background.y, 67.0, 0.0001
    assert_in_delta background.width, 36.0, 0.0001
    assert_in_delta background.height, 21.0, 0.0001
    assert background.fill_color == {0.9, 0.9, 0.9}
    assert background.stroke_color == {0, 0, 1}

    assert image.type == :image
    assert_in_delta image.x, 16.0, 0.0001
    assert_in_delta image.y, 70.0, 0.0001
    assert_in_delta image.width, 30.0, 0.0001
    assert_in_delta image.height, 15.0, 0.0001
    assert image.image.format == :png
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

  test "layout supports default options letter pages and margin units" do
    assert {:ok, default_layout} = Layout.layout(document([paragraph("Default")]))
    assert default_layout.page_size == {595.28, 841.89}

    assert {:ok, letter_layout} =
             Layout.layout(document([paragraph("Letter")]), page_size: :letter, margin: "10px")

    assert letter_layout.page_size == {612.0, 792.0}
    assert_in_delta letter_layout.margin, 7.5, 0.0001

    assert {:ok, cm_layout} =
             Layout.layout(document([paragraph("Centimeter")]),
               page_size: {100, 80},
               margin: "1cm"
             )

    assert cm_layout.page_size == {100.0, 80.0}
    assert_in_delta cm_layout.margin, 72.0 / 2.54, 0.0001

    assert {:ok, in_layout} =
             Layout.layout(document([paragraph("Inch")]), page_size: {100, 80}, margin: "1in")

    assert_in_delta in_layout.margin, 72.0, 0.0001
  end

  test "layout handles image sizing variants and display none blocks" do
    base_style = image_style(image_fixture(20.0, 10.0))

    assert {:ok, both_layout} =
             Layout.layout(
               document([
                 %{type: :element, style: Map.merge(base_style, %{width: 8.0, height: 6.0})}
               ]),
               page_size: {100, 100},
               margin: 10
             )

    assert [%{type: :image, width: 8.0, height: 6.0}] = both_layout.boxes

    assert {:ok, height_layout} =
             Layout.layout(
               document([%{type: :element, style: Map.merge(base_style, %{height: 5.0})}]),
               page_size: {100, 100},
               margin: 10
             )

    assert [%{type: :image, width: 10.0, height: 5.0}] = height_layout.boxes

    assert {:ok, natural_layout} =
             Layout.layout(
               document([
                 %{type: :element, style: %{display: :none}},
                 %{type: :element, style: base_style}
               ]),
               page_size: {100, 100},
               margin: 10
             )

    assert [%{type: :image, width: 20.0, height: 10.0}] = natural_layout.boxes
  end

  test "layout covers flex empty skipped image shrink and distribution branches" do
    assert {:ok, empty_tree} =
             Style.compute(%{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "div",
                   attributes: %{"style" => "display: flex; width: 40pt; background-color: red"},
                   children: []
                 }
               ]
             })

    assert {:ok, empty_layout} = Layout.layout(empty_tree, page_size: {100, 100}, margin: 10)
    assert [%{type: :rect, height: height}] = empty_layout.boxes
    assert_in_delta height, 0.0, 0.0001

    assert {:ok, shrink_tree} =
             Style.compute(%{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "div",
                   attributes: %{
                     "style" =>
                       "display: flex; width: 30pt; gap: 2pt; justify-content: flex-end; align-items: stretch"
                   },
                   children: [
                     %{type: :text, text: " "},
                     %{
                       type: :element,
                       tag: "span",
                       attributes: %{"style" => "display: none"},
                       children: [%{type: :text, text: "Hidden"}]
                     },
                     %{
                       type: :element,
                       tag: "span",
                       attributes: %{"style" => "width: 40pt; flex-shrink: 1"},
                       children: [%{type: :text, text: "Wide"}]
                     },
                     %{
                       type: :element,
                       tag: "img",
                       attributes: %{
                         "src" => "data:image/png;base64,#{Base.encode64(png_fixture())}",
                         "style" => "width: 12pt"
                       },
                       children: []
                     }
                   ]
                 }
               ]
             })

    assert {:ok, shrink_layout} = Layout.layout(shrink_tree, page_size: {100, 100}, margin: 10)
    assert Enum.any?(shrink_layout.boxes, &(&1.type == :image))
    assert Enum.any?(shrink_layout.boxes, &(&1.type == :text and &1.text == "Wide"))

    for justify <- ["space-around", "space-evenly"] do
      assert {:ok, tree} =
               Style.compute(%{
                 type: :document,
                 children: [
                   %{
                     type: :element,
                     tag: "div",
                     attributes: %{
                       "style" => "display: flex; width: 120pt; justify-content: #{justify}"
                     },
                     children: [
                       %{type: :text, text: "A"},
                       %{type: :text, text: "B"}
                     ]
                   }
                 ]
               })

      assert {:ok, layout} = Layout.layout(tree, page_size: {160, 100}, margin: 10)
      assert Enum.map(layout.boxes, & &1.text) == ["A", "B"]
    end
  end

  test "layout covers grid text image skipped placement and distribution branches" do
    assert {:ok, tree} =
             Style.compute(%{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "div",
                   attributes: %{
                     "style" =>
                       "display: grid; width: 140pt; height: 70pt; grid-template-columns: auto 1fr; grid-template-rows: auto 1fr; grid-auto-columns: 10pt; gap: 4pt; justify-content: space-around; align-content: space-evenly; justify-items: end; align-items: center"
                   },
                   children: [
                     %{type: :text, text: " "},
                     %{type: :text, text: "A"},
                     %{
                       type: :element,
                       tag: "span",
                       attributes: %{"style" => "display: none"},
                       children: [%{type: :text, text: "Hidden"}]
                     },
                     %{
                       type: :element,
                       tag: "span",
                       attributes: %{"style" => "grid-row: 2; grid-column: 2"},
                       children: [%{type: :text, text: "B"}]
                     },
                     %{
                       type: :element,
                       tag: "span",
                       attributes: %{"style" => "grid-row: 2; grid-column: span 2"},
                       children: [%{type: :text, text: "C"}]
                     },
                     %{
                       type: :element,
                       tag: "img",
                       attributes: %{
                         "src" => "data:image/png;base64,#{Base.encode64(png_fixture())}",
                         "style" => "grid-column: 1 / span 2; width: 8pt; height: 8pt"
                       },
                       children: []
                     }
                   ]
                 }
               ]
             })

    assert {:ok, layout} = Layout.layout(tree, page_size: {180, 120}, margin: 10)
    assert Enum.any?(layout.boxes, &(&1.type == :image))

    assert Enum.filter(layout.boxes, &(&1.type == :text)) |> Enum.map(& &1.text) == [
             "A",
             "B",
             "C"
           ]

    for justify <- ["flex-end", "space-between", "space-evenly"] do
      assert {:ok, distribution_tree} =
               Style.compute(%{
                 type: :document,
                 children: [
                   %{
                     type: :element,
                     tag: "div",
                     attributes: %{
                       "style" =>
                         "display: grid; width: 100pt; grid-template-columns: 10pt 10pt; justify-content: #{justify}"
                     },
                     children: [%{type: :text, text: "A"}, %{type: :text, text: "B"}]
                   }
                 ]
               })

      assert {:ok, distribution_layout} =
               Layout.layout(distribution_tree, page_size: {140, 100}, margin: 10)

      assert Enum.map(distribution_layout.boxes, & &1.text) == ["A", "B"]
    end
  end

  test "layout covers remaining grid sizing and invalid item branches" do
    assert {:ok, empty_grid_tree} =
             Style.compute(%{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "div",
                   attributes: %{
                     "style" =>
                       "display: grid; width: 40pt; height: 20pt; grid-template-columns: 0fr; grid-template-rows: 0fr; background-color: red"
                   },
                   children: []
                 }
               ]
             })

    assert {:ok, empty_grid_layout} =
             Layout.layout(empty_grid_tree, page_size: {100, 100}, margin: 10)

    assert [%{type: :rect}] = empty_grid_layout.boxes

    assert {:ok, auto_column_tree} =
             Style.compute(%{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "div",
                   attributes: %{
                     "style" =>
                       "display: grid; width: 80pt; grid-template-columns: 20pt 20pt; gap: 5pt"
                   },
                   children: [
                     %{
                       type: :element,
                       tag: "span",
                       attributes: %{"style" => "grid-row: 1; grid-column: 1"},
                       children: [%{type: :text, text: "A"}]
                     },
                     %{
                       type: :element,
                       tag: "span",
                       attributes: %{"style" => "grid-row: 1"},
                       children: [%{type: :text, text: "B"}]
                     }
                   ]
                 }
               ]
             })

    assert {:ok, auto_column_layout} =
             Layout.layout(auto_column_tree, page_size: {120, 100}, margin: 10)

    [a, b] = auto_column_layout.boxes
    assert a.text == "A"
    assert b.text == "B"
    assert b.x > a.x

    invalid_grid = %{
      type: :element,
      style: %{display: :grid},
      children: [%{type: :invalid}]
    }

    assert Layout.layout(document([invalid_grid]), []) == {:error, :invalid_layout}

    invalid_inline_grid = %{
      type: :element,
      style: %{display: :grid},
      children: [
        %{
          type: :element,
          style: block_style(),
          children: [%{type: :element, style: %{display: :block}, children: []}]
        }
      ]
    }

    assert Layout.layout(document([invalid_inline_grid]), []) == {:error, :invalid_layout}
  end

  test "layout covers remaining flex column image shrink and ordering branches" do
    assert {:ok, column_image_tree} =
             Style.compute(%{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "div",
                   attributes: %{
                     "style" =>
                       "display: flex; flex-direction: column-reverse; width: 50pt; height: 80pt; justify-content: space-between; align-items: stretch"
                   },
                   children: [
                     %{
                       type: :element,
                       tag: "img",
                       attributes: %{
                         "src" => "data:image/png;base64,#{Base.encode64(png_fixture())}",
                         "style" => "width: 10pt; height: 10pt"
                       },
                       children: []
                     },
                     %{
                       type: :element,
                       tag: "span",
                       attributes: %{"style" => "width: 10pt; height: 10pt"},
                       children: [%{type: :text, text: "B"}]
                     }
                   ]
                 }
               ]
             })

    assert {:ok, column_image_layout} =
             Layout.layout(column_image_tree, page_size: {120, 120}, margin: 10)

    assert Enum.any?(column_image_layout.boxes, &(&1.type == :image))
    assert Enum.any?(column_image_layout.boxes, &(&1.type == :text and &1.text == "B"))

    assert {:ok, shrink_tree} =
             Style.compute(%{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "div",
                   attributes: %{"style" => "display: flex; width: 30pt"},
                   children: [
                     %{
                       type: :element,
                       tag: "span",
                       attributes: %{"style" => "width: 40pt; flex-shrink: 1"},
                       children: [%{type: :text, text: "A"}]
                     },
                     %{
                       type: :element,
                       tag: "span",
                       attributes: %{"style" => "width: 40pt; flex-shrink: 1"},
                       children: [%{type: :text, text: "B"}]
                     }
                   ]
                 }
               ]
             })

    assert {:ok, shrink_layout} = Layout.layout(shrink_tree, page_size: {100, 100}, margin: 10)
    [a, b] = shrink_layout.boxes
    assert a.text == "A"
    assert b.text == "B"
    assert b.x > a.x

    invalid_flex = %{
      type: :element,
      style: %{display: :flex},
      children: [%{type: :invalid}]
    }

    assert Layout.layout(document([invalid_flex]), []) == {:error, :invalid_layout}

    invalid_inline_flex = %{
      type: :element,
      style: %{display: :flex},
      children: [
        %{
          type: :element,
          style: block_style(),
          children: [%{type: :element, style: %{display: :block}, children: []}]
        }
      ]
    }

    assert Layout.layout(document([invalid_inline_flex]), []) == {:error, :invalid_layout}

    forced_shrink = %{
      type: :element,
      style:
        Map.merge(block_style(), %{
          display: :flex,
          width: 30.0,
          flex_direction: :row,
          flex_wrap: :nowrap,
          column_gap: 0.0,
          row_gap: 0.0,
          justify_content: :flex_start,
          align_items: :stretch
        }),
      children: [
        %{
          type: :element,
          style:
            Map.merge(text_style(), %{
              display: :inline,
              width: 40.0,
              flex_basis: 40.0,
              flex_shrink: 1.0
            }),
          children: [%{type: :text, text: "A", style: text_style()}]
        },
        %{
          type: :element,
          style:
            Map.merge(text_style(), %{
              display: :inline,
              width: 40.0,
              flex_basis: 40.0,
              flex_shrink: 1.0
            }),
          children: [%{type: :text, text: "B", style: text_style()}]
        }
      ]
    }

    assert {:ok, forced_shrink_layout} =
             Layout.layout(document([forced_shrink]), page_size: {100, 100}, margin: 10)

    assert Enum.map(forced_shrink_layout.boxes, & &1.text) == ["A", "B"]

    no_shrink = %{
      forced_shrink
      | children:
          Enum.map(forced_shrink.children, fn child ->
            put_in(child.style.flex_shrink, 0.0)
          end)
    }

    assert {:ok, no_shrink_layout} =
             Layout.layout(document([no_shrink]), page_size: {100, 100}, margin: 10)

    assert Enum.map(no_shrink_layout.boxes, & &1.text) == ["A", "B"]
  end

  test "layout rejects invalid nested structures through containers" do
    bad_inline = %{
      type: :element,
      style: block_style(),
      children: [%{type: :element, style: %{display: :block}, children: []}]
    }

    assert Layout.layout(document([bad_inline]), []) == {:error, :invalid_layout}

    bad_nested_inline = %{
      type: :element,
      style: block_style(),
      children: [
        %{
          type: :element,
          style: Map.merge(text_style(), %{display: :inline}),
          children: [%{type: :element, style: %{display: :block}, children: []}]
        }
      ]
    }

    assert Layout.layout(document([bad_nested_inline]), []) == {:error, :invalid_layout}

    bad_list = %{
      type: :element,
      style: %{display: :list, list_marker_type: :disc},
      children: [%{type: :element, style: %{display: :block}, children: []}]
    }

    assert Layout.layout(document([bad_list]), []) == {:error, :invalid_layout}

    bad_table = %{
      type: :element,
      style: table_style(),
      children: [%{type: :element, style: %{display: :block}, children: []}]
    }

    assert Layout.layout(document([bad_table]), []) == {:error, :invalid_layout}

    bad_row_group = %{
      type: :element,
      style: table_style(),
      children: [
        %{
          type: :element,
          style: %{display: :table_row_group, table_section: :body},
          children: [%{type: :element, style: %{display: :block}, children: []}]
        }
      ]
    }

    assert Layout.layout(document([bad_row_group]), []) == {:error, :invalid_layout}

    bad_row = %{
      type: :element,
      style: table_style(),
      children: [
        %{
          type: :element,
          style: %{display: :table_row},
          children: [%{type: :element, style: %{display: :block}, children: []}]
        }
      ]
    }

    assert Layout.layout(document([bad_row]), []) == {:error, :invalid_layout}

    empty_table = %{
      type: :element,
      style: table_style(),
      children: [
        %{
          type: :element,
          style: %{display: :table_row},
          children: []
        }
      ]
    }

    assert Layout.layout(document([empty_table]), []) == {:error, :invalid_layout}

    bad_caption = %{
      type: :element,
      style: table_style(),
      children: [
        %{type: :element, style: %{display: :table_caption}, children: :bad},
        %{
          type: :element,
          style: %{display: :table_row},
          children: [
            %{
              type: :element,
              style: table_cell_style(),
              children: [%{type: :text, text: "x", style: text_style()}]
            }
          ]
        }
      ]
    }

    assert Layout.layout(document([bad_caption]), []) == {:error, :invalid_layout}

    bad_row_shape = %{
      type: :element,
      style: table_style(),
      children: [
        %{
          type: :element,
          style: %{display: :table_row_group},
          children: [%{type: :element, style: %{display: :table_row}, children: :bad}]
        }
      ]
    }

    assert Layout.layout(document([bad_row_shape]), []) == {:error, :invalid_layout}

    bad_cell_shape = %{
      type: :element,
      style: table_style(),
      children: [
        %{
          type: :element,
          style: %{display: :table_row},
          children: [%{type: :element, style: table_cell_style(), children: :bad}]
        }
      ]
    }

    assert Layout.layout(document([bad_cell_shape]), []) == {:error, :invalid_layout}

    bad_cell_inline = %{
      type: :element,
      style: table_style(),
      children: [
        %{
          type: :element,
          style: %{display: :table_row},
          children: [
            %{
              type: :element,
              style: table_cell_style(),
              children: [%{type: :element, style: %{display: :block}, children: []}]
            }
          ]
        },
        %{
          type: :element,
          style: %{display: :table_row},
          children: [
            %{
              type: :element,
              style: table_cell_style(),
              children: [%{type: :text, text: "x", style: text_style()}]
            }
          ]
        }
      ]
    }

    assert Layout.layout(document([bad_cell_inline]), []) == {:error, :invalid_layout}

    bad_table_after_error = %{
      type: :element,
      style: table_style(),
      children: [
        %{type: :element, style: %{display: :block}, children: []},
        %{type: :element, style: %{display: :table_row}, children: []}
      ]
    }

    assert Layout.layout(document([bad_table_after_error]), []) == {:error, :invalid_layout}
  end

  test "layout positions right-aligned table cell text" do
    assert {:ok, styled_tree} =
             Style.compute(%{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "table",
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
                           attributes: %{"style" => "text-align: right"},
                           children: [%{type: :text, text: "R"}]
                         }
                       ]
                     }
                   ]
                 }
               ]
             })

    assert {:ok, layout_tree} = Layout.layout(styled_tree, page_size: {100, 100}, margin: 10)
    [_cell, text] = layout_tree.boxes
    assert text.text == "R"
    assert text.x > 10.0
  end

  test "layout rejects invalid options and unsupported trees" do
    assert Layout.layout(%{tag: "p", style: %{}}, []) == {:error, :invalid_layout}

    assert Layout.layout(document([%{type: :invalid}, paragraph("After")]), []) ==
             {:error, :invalid_layout}

    assert Layout.layout(%{type: :document, children: []}, page_size: :unknown) ==
             {:error, :invalid_page_size}

    assert Layout.layout(%{type: :document, children: []}, margin: -1) ==
             {:error, :invalid_margin}

    assert Layout.layout(%{type: :document, children: []}, margin: "1em") ==
             {:error, :invalid_margin}

    assert {:ok, pt_margin_layout} = Layout.layout(document([paragraph("pt")]), margin: "10pt")
    assert_in_delta pt_margin_layout.margin, 10.0, 0.0001
  end

  defp ttf_font_path! do
    [
      "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
      "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
      "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf"
    ]
    |> Enum.find(&File.exists?/1)
    |> case do
      nil -> flunk("No local TTF font fixture found")
      path -> path
    end
  end

  defp image_fixture(width, height) do
    %{
      format: :png,
      data: <<255, 0, 0>>,
      width_px: round(width / 0.75),
      height_px: round(height / 0.75),
      width: width,
      height: height,
      color_space: :device_rgb,
      bits_per_component: 8
    }
  end

  defp document(children) do
    %{type: :document, children: children}
  end

  defp paragraph(text) do
    %{
      type: :element,
      style: block_style(),
      children: [
        %{
          type: :text,
          text: text,
          style: text_style()
        }
      ]
    }
  end

  defp block_style do
    Map.merge(text_style(), %{
      display: :block,
      line_height: 14.4,
      margin_after: 12.0
    })
  end

  defp image_style(image) do
    Map.merge(text_style(), %{
      display: :image,
      image: image,
      margin_after: 0.0
    })
  end

  defp table_style do
    Map.merge(text_style(), %{display: :table, margin_after: 0.0})
  end

  defp table_cell_style do
    Map.merge(text_style(), %{
      display: :table_cell,
      line_height: 14.4,
      padding: %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0},
      border_widths: %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    })
  end

  defp text_style do
    %{
      color: {0, 0, 0},
      font_family: "Helvetica",
      font_size: 12.0,
      font_style: :normal,
      font_weight: 400,
      line_height: 14.4
    }
  end

  defp png_fixture do
    row = <<0, 255, 0, 0>>

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<1::32, 1::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(row)) <>
      png_chunk("IEND", "")
  end

  defp png_chunk(type, data) do
    crc = :erlang.crc32(type <> data)
    <<byte_size(data)::32, type::binary, data::binary, crc::32>>
  end
end
