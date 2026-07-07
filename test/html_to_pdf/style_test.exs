defmodule NativeElixirPdfUtilities.HtmlToPdf.StyleTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.Style

  test "compute applies default paragraph styles" do
    dom = %{
      type: :document,
      children: [
        %{type: :element, tag: "p", attributes: %{}, children: [%{type: :text, text: "Hello"}]}
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.display == :block
    assert paragraph.style.font_family == "Helvetica"
    assert paragraph.style.font_size == 12.0
    assert paragraph.style.color == {0, 0, 0}
  end

  test "compute applies heading defaults and inline style colors" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "h1",
          attributes: %{"style" => "color: #336699"},
          children: [%{type: :text, text: "Title"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [heading] = styled_tree.children
    [title] = heading.children

    assert heading.style.display == :block
    assert heading.style.font_size == 24.0
    assert heading.style.font_weight == 700
    assert heading.style.color == {0.2, 0.4, 0.6}
    assert title.style.color == {0.2, 0.4, 0.6}
  end

  test "compute applies inline bold italic and inherited color" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "color: red"},
          children: [
            %{type: :text, text: "Hello "},
            %{
              type: :element,
              tag: "strong",
              attributes: %{},
              children: [%{type: :text, text: "bold"}]
            },
            %{
              type: :element,
              tag: "em",
              attributes: %{},
              children: [%{type: :text, text: "italic"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children
    [plain, strong, em] = paragraph.children
    [bold] = strong.children
    [italic] = em.children

    assert plain.style.color == {1, 0, 0}
    assert strong.style.font_weight == 700
    assert bold.style.font_weight == 700
    assert bold.style.color == {1, 0, 0}
    assert em.style.font_style == :italic
    assert italic.style.font_style == :italic
  end

  test "compute applies block box styles from inline declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{
            "style" =>
              "margin: 2pt 4pt 6pt 8pt; padding: 3pt 5pt; border: 1pt solid #336699; border-radius: 2pt; background-color: #eeeeee"
          },
          children: [%{type: :text, text: "Boxed"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.margin == %{top: 2.0, right: 4.0, bottom: 6.0, left: 8.0}
    assert paragraph.style.margin_after == 6.0
    assert paragraph.style.padding == %{top: 3.0, right: 5.0, bottom: 3.0, left: 5.0}
    assert paragraph.style.border_widths == %{top: 1.0, right: 1.0, bottom: 1.0, left: 1.0}
    assert paragraph.style.border_color == {0.2, 0.4, 0.6}
    assert paragraph.style.border_radius == 2.0
    assert_in_delta elem(paragraph.style.background_color, 0), 0.9333, 0.0001
  end

  test "compute applies manual page break declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "break-before: page; page-break-after: always"},
          children: [%{type: :text, text: "Break"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.break_before == :page
    assert paragraph.style.break_after == :page

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{"style" => "break-before: left"},
                   children: [%{type: :text, text: "Bad"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}
  end

  test "compute applies list defaults and propagates link URLs to text" do
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
                  attributes: %{"href" => "mailto:team@example.com"},
                  children: [%{type: :text, text: "email"}]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [list] = styled_tree.children
    [item] = list.children
    [_plain, link] = item.children
    [link_text] = link.children

    assert list.style.display == :list
    assert list.style.list_marker_type == :decimal
    assert item.style.display == :list_item
    assert link.style.display == :inline
    assert link.style.color == {0, 0, 1}
    assert link.style.link_url == "mailto:team@example.com"
    assert link_text.style.link_url == "mailto:team@example.com"
  end

  test "compute applies table defaults and header cell behavior" do
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
                      attributes: %{"style" => "padding: 2pt; border: 2pt solid blue"},
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
    [table] = styled_tree.children
    [caption, head, body] = table.children
    [header_row] = head.children
    [name, count] = header_row.children
    [body_row] = body.children
    [alpha, amount] = body_row.children

    assert table.style.display == :table
    assert caption.style.display == :table_caption
    assert caption.style.text_align == :center
    assert head.style.display == :table_row_group
    assert head.style.table_section == :head
    assert body.style.table_section == :body
    assert name.style.display == :table_cell
    assert name.style.font_weight == 700
    assert name.style.text_align == :center
    assert count.style.background_color == {0.9333333333, 0.9333333333, 0.9333333333}
    assert alpha.style.padding == %{top: 2.0, right: 2.0, bottom: 2.0, left: 2.0}
    assert alpha.style.border_widths == %{top: 2.0, right: 2.0, bottom: 2.0, left: 2.0}
    assert alpha.style.border_color == {0, 0, 1}
    assert amount.style.text_align == :left
  end

  test "compute applies configured stylesheets before style tag rules" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [%{type: :text, text: "p { color: green; }"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "Hello"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, stylesheets: ["p { color: red; }"])
    [paragraph] = styled_tree.children
    [text] = paragraph.children

    assert paragraph.style.color == {0, 0.5019607843, 0}
    assert text.style.color == {0, 0.5019607843, 0}
  end

  test "compute applies configured stylesheet files" do
    stylesheet_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-style-test.css")
    File.write!(stylesheet_path, "p { color: #336699; }")

    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "Hello"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, stylesheets: [stylesheet_path])
    [paragraph] = styled_tree.children

    assert paragraph.style.color == {0.2, 0.4, 0.6}
  after
    stylesheet_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-style-test.css")
    File.rm(stylesheet_path)
  end

  test "compute resolves font-family fallback against embedded fonts" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "font-family: Missing, 'Fixture Sans', Helvetica"},
          children: [%{type: :text, text: "Hello"}]
        }
      ]
    }

    assert {:ok, styled_tree} =
             Style.compute(dom,
               fonts: [%{family: "Fixture Sans", path: ttf_font_path!()}],
               default_font: "Missing, Helvetica"
             )

    [paragraph] = styled_tree.children
    [text] = paragraph.children

    assert paragraph.style.font_family == "Fixture Sans"
    assert paragraph.style.font_families == ["Missing", "Fixture Sans", "Helvetica"]
    assert paragraph.style.font_face.type == :embedded
    assert text.style.font_face.id == paragraph.style.font_face.id
  end

  test "compute applies selector specificity before source order" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{
              type: :text,
              text: """
              #intro { color: blue; }
              p.copy { color: green; }
              p { color: red; }
              """
            }
          ]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"id" => "intro", "class" => "copy"},
          children: [%{type: :text, text: "Specific"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.color == {0, 0, 1}
  end

  test "compute applies source order when specificity matches" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [%{type: :text, text: "p { color: red; } p { color: blue; }"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "Later wins"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.color == {0, 0, 1}
  end

  test "compute applies descendant and child selectors with inheritance" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{
              type: :text,
              text: "body > p .note { color: #336699; font-size: 16pt; font-weight: bold; }"
            }
          ]
        },
        %{
          type: :element,
          tag: "body",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "p",
              attributes: %{},
              children: [
                %{
                  type: :element,
                  tag: "span",
                  attributes: %{"class" => "note"},
                  children: [%{type: :text, text: "Inherited"}]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children
    [span] = paragraph.children
    [text] = span.children

    assert span.style.color == {0.2, 0.4, 0.6}
    assert span.style.font_size == 16.0
    assert span.style.font_weight == 700
    assert span.style.line_height == 19.2
    assert text.style.color == {0.2, 0.4, 0.6}
    assert text.style.font_size == 16.0
  end

  test "compute ignores child selectors when the required parent is absent" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [%{type: :text, text: "div > p { color: red; }"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "No parent"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children
    assert paragraph.style.color == {0, 0, 0}
  end

  test "compute gives inline style declarations the highest priority" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [%{type: :text, text: "#intro { color: blue; }"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"id" => "intro", "style" => "color: red"},
          children: [%{type: :text, text: "Inline"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.color == {1, 0, 0}
  end

  test "compute applies flex container and item declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{
            "style" =>
              "display: flex; flex-direction: row-reverse; flex-wrap: wrap; gap: 6pt; justify-content: space-between; align-items: center"
          },
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{
                "style" =>
                  "order: 2; flex-grow: 1; flex-shrink: 0; flex-basis: 20pt; align-self: flex-end"
              },
              children: [%{type: :text, text: "A"}]
            },
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "order: 1; flex: 2 1 10pt"},
              children: [%{type: :text, text: "B"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [container] = styled_tree.children
    [first, second] = container.children

    assert container.style.display == :flex
    assert container.style.flex_direction == :row_reverse
    assert container.style.flex_wrap == :wrap
    assert container.style.row_gap == 6.0
    assert container.style.column_gap == 6.0
    assert container.style.justify_content == :space_between
    assert container.style.align_items == :center

    assert first.style.order == 2
    assert first.style.flex_grow == 1.0
    assert first.style.flex_shrink == 0.0
    assert first.style.flex_basis == 20.0
    assert first.style.align_self == :flex_end

    assert second.style.order == 1
    assert second.style.flex_grow == 2.0
    assert second.style.flex_shrink == 1.0
    assert second.style.flex_basis == 10.0
  end

  test "compute applies grid container and item declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{
            "style" =>
              "display: grid; grid-template-columns: 30pt 1fr; grid-template-rows: 20pt auto; grid-auto-columns: 12pt; grid-auto-rows: 18pt; gap: 4pt 6pt; justify-items: center; align-items: end; justify-content: space-between; align-content: center"
          },
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "grid-column: 2 / span 2; grid-row: 1 / 3"},
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
    [container] = styled_tree.children
    [first, second] = container.children

    assert container.style.display == :grid
    assert container.style.grid_template_columns == [{:length, 30.0}, {:fr, 1.0}]
    assert container.style.grid_template_rows == [{:length, 20.0}, :auto]
    assert container.style.grid_auto_columns == {:length, 12.0}
    assert container.style.grid_auto_rows == {:length, 18.0}
    assert container.style.row_gap == 4.0
    assert container.style.column_gap == 6.0
    assert container.style.justify_items == :center
    assert container.style.align_items == :flex_end
    assert container.style.justify_content == :space_between
    assert container.style.align_content == :center

    assert first.style.grid_column_start == 2
    assert first.style.grid_column_end == {:span, 2}
    assert first.style.grid_row_start == 1
    assert first.style.grid_row_end == 3

    assert second.style.grid_row_start == 2
    assert second.style.grid_column_start == 1
    assert second.style.grid_row_end == 3
    assert second.style.grid_column_end == 2
    assert second.style.align_self == :flex_start
  end

  test "compute loads data URI PNG and base_url relative JPEG images" do
    base_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-image-style-test")
    File.mkdir_p!(base_dir)
    File.write!(Path.join(base_dir, "photo.jpg"), jpeg_fixture(3, 2))

    png_src = "data:image/png;base64,#{Base.encode64(png_fixture(2, 1))}"

    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "img",
          attributes: %{"src" => png_src, "style" => "width: 20pt"},
          children: []
        },
        %{
          type: :element,
          tag: "img",
          attributes: %{"src" => "photo.jpg", "style" => "height: 12pt"},
          children: []
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, base_url: base_dir)
    [png, jpeg] = styled_tree.children

    assert png.style.display == :image
    assert png.style.width == 20.0
    assert png.style.image.format == :png
    assert png.style.image.width_px == 2
    assert png.style.image.height_px == 1
    assert png.style.image.color_space == :device_rgb

    assert jpeg.style.display == :image
    assert jpeg.style.height == 12.0
    assert jpeg.style.image.format == :jpeg
    assert jpeg.style.image.width_px == 3
    assert jpeg.style.image.height_px == 2
  after
    base_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-image-style-test")
    File.rm_rf(base_dir)
  end

  test "compute rejects missing sources unsupported formats and unsafe relative images" do
    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{type: :element, tag: "img", attributes: %{}, children: []}
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "img",
                   attributes: %{"src" => "data:image/gif;base64,R0lGODlhAQABAAAAACw="},
                   children: []
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "img",
                   attributes: %{"src" => "../photo.png"},
                   children: []
                 }
               ]
             },
             base_url: System.tmp_dir!()
           ) == {:error, :invalid_document}
  end

  test "compute rejects unsupported document trees" do
    assert Style.compute(%{tag: "p"}, []) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{"style" => "background: red"},
                   children: [%{type: :text, text: "Hello"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "style",
                   attributes: %{},
                   children: [%{type: :text, text: "p { background: red; }"}]
                 },
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{},
                   children: [%{type: :text, text: "Hello"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{"style" => "border: 1pt dashed red"},
                   children: [%{type: :text, text: "Hello"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{},
                   children: [
                     %{
                       type: :element,
                       tag: "a",
                       attributes: %{"href" => "javascript:alert(1)"},
                       children: [%{type: :text, text: "bad"}]
                     }
                   ]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "div",
                   attributes: %{"style" => "display: flex; justify-content: baseline"},
                   children: [%{type: :text, text: "Bad"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "div",
                   attributes: %{
                     "style" => "display: grid; grid-template-columns: repeat(2, 1fr)"
                   },
                   children: [%{type: :text, text: "Bad"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}
  end

  test "compute accepts wrapper elements heading levels and inline aliases" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "html",
          attributes: %{},
          children: [
            %{type: :element, tag: "head", attributes: %{}, children: []},
            %{
              type: :element,
              tag: "body",
              attributes: %{},
              children:
                Enum.map(["h2", "h3", "h4", "h5", "h6"], fn tag ->
                  %{
                    type: :element,
                    tag: tag,
                    attributes: %{},
                    children: [%{type: :text, text: tag}]
                  }
                end) ++
                  [
                    %{
                      type: :element,
                      tag: "p",
                      attributes: %{},
                      children: [
                        %{
                          type: :element,
                          tag: "b",
                          attributes: %{},
                          children: [%{type: :text, text: "bold"}]
                        },
                        %{
                          type: :element,
                          tag: "i",
                          attributes: %{},
                          children: [%{type: :text, text: "italic"}]
                        }
                      ]
                    },
                    %{
                      type: :element,
                      tag: "table",
                      attributes: %{},
                      children: [
                        %{
                          type: :element,
                          tag: "tfoot",
                          attributes: %{},
                          children: []
                        }
                      ]
                    }
                  ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom)
    headings = Enum.take(styled_tree.children, 5)
    paragraph = Enum.at(styled_tree.children, 5)
    table = Enum.at(styled_tree.children, 6)

    assert Enum.map(headings, & &1.style.font_size) == [20.0, 16.0, 14.0, 12.0, 10.0]
    [bold, italic] = paragraph.children
    assert bold.style.font_weight == 700
    assert italic.style.font_style == :italic
    [foot] = table.children
    assert foot.style.table_section == :foot
  end

  test "compute accepts supported display and box value variants" do
    assertions = [
      {"display: block", :display, :block},
      {"display: inline", :display, :inline},
      {"display: none", :display, :none},
      {"display: inline-flex", :display, :inline_flex},
      {"display: inline-grid", :display, :inline_grid},
      {"margin: 1pt 2pt 3pt", :margin, %{top: 1.0, right: 2.0, bottom: 3.0, left: 2.0}},
      {"padding-top: 2pt", :padding, %{top: 2.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"padding-right: 2pt", :padding, %{top: 0.0, right: 2.0, bottom: 0.0, left: 0.0}},
      {"padding-bottom: 2pt", :padding, %{top: 0.0, right: 0.0, bottom: 2.0, left: 0.0}},
      {"padding-left: 2pt", :padding, %{top: 0.0, right: 0.0, bottom: 0.0, left: 2.0}},
      {"margin-left: 2pt", :margin, %{top: 0.0, right: 0.0, bottom: 12.0, left: 2.0}},
      {"border-width: 2pt", :border_widths, %{top: 2.0, right: 2.0, bottom: 2.0, left: 2.0}},
      {"border: none", :border_widths, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"color: #abc", :color, {0.6666666667, 0.7333333333, 0.8}},
      {"font-size: 10px", :font_size, 7.5},
      {"width: 10mm", :width, 72.0 / 25.4 * 10},
      {"height: 1cm", :height, 72.0 / 2.54},
      {"width: 1in", :width, 72.0},
      {"height: 0", :height, 0.0},
      {"text-align: left", :text_align, :left},
      {"text-align: center", :text_align, :center},
      {"text-align: right", :text_align, :right},
      {"font-weight: normal", :font_weight, 400},
      {"font-weight: 900", :font_weight, 900},
      {"font-style: normal", :font_style, :normal},
      {"font-style: italic", :font_style, :italic},
      {"border-color: red", :border_color, {1, 0, 0}},
      {"margin-bottom: 5pt", :margin_after, 5.0}
    ]

    Enum.each(assertions, fn {style, key, expected} ->
      assert {:ok, computed} = style_for("p", style)
      assert_style_value(Map.fetch!(computed, key), expected)
    end)
  end

  test "compute accepts supported flex and grid value variants" do
    assertions = [
      {"display: flex; flex-direction: row", :flex_direction, :row},
      {"display: flex; flex-direction: column-reverse", :flex_direction, :column_reverse},
      {"display: flex; flex-wrap: nowrap", :flex_wrap, :nowrap},
      {"display: flex; justify-content: start", :justify_content, :flex_start},
      {"display: flex; justify-content: end", :justify_content, :flex_end},
      {"display: flex; justify-content: stretch", :justify_content, :stretch},
      {"display: flex; justify-content: space-around", :justify_content, :space_around},
      {"display: flex; justify-content: space-evenly", :justify_content, :space_evenly},
      {"display: flex; align-items: stretch", :align_items, :stretch},
      {"display: flex; align-items: start", :align_items, :flex_start},
      {"display: flex; align-self: auto", :align_self, :auto},
      {"display: flex; align-self: stretch", :align_self, :stretch},
      {"display: flex; align-self: start", :align_self, :flex_start},
      {"display: flex; align-self: end", :align_self, :flex_end},
      {"display: flex; flex-basis: auto", :flex_basis, :auto},
      {"display: flex; flex: none", :flex_grow, 0.0},
      {"display: flex; flex: auto", :flex_grow, 1.0},
      {"display: flex; flex: initial", :flex_grow, 0.0},
      {"display: flex; flex: 2", :flex_grow, 2.0},
      {"display: flex; flex: 2 0", :flex_shrink, 0.0},
      {"display: flex; row-gap: 2pt", :row_gap, 2.0},
      {"display: flex; column-gap: 3pt", :column_gap, 3.0},
      {"display: grid; grid-template-columns: auto", :grid_template_columns, [:auto]},
      {"display: grid; grid-column-start: auto", :grid_column_start, :auto},
      {"display: grid; grid-column-end: span 2", :grid_column_end, {:span, 2}},
      {"display: grid; grid-row-start: 2", :grid_row_start, 2},
      {"display: grid; grid-row-end: 3", :grid_row_end, 3},
      {"display: grid; grid-column: 2", :grid_column_start, 2},
      {"display: grid; justify-items: stretch", :justify_items, :stretch},
      {"display: grid; justify-items: start", :justify_items, :flex_start},
      {"display: grid; justify-items: end", :justify_items, :flex_end}
    ]

    Enum.each(assertions, fn {style, key, expected} ->
      assert {:ok, computed} = style_for("div", style)
      assert_style_value(Map.fetch!(computed, key), expected)
    end)
  end

  test "compute rejects invalid style branches" do
    invalid_styles = [
      "display: contents",
      "flex-direction: sideways",
      "flex-wrap: reverse",
      "align-items: baseline",
      "align-self: baseline",
      "justify-items: baseline",
      "order: first",
      "flex-grow: -1",
      "flex: 1 1 1pt extra",
      "grid-template-columns: bad",
      "grid-column: 1 / 2 / 3",
      "grid-area: 1 / 2 / 3",
      "text-align: justify",
      "font-weight: 1000",
      "font-weight: heavy",
      "font-style: oblique",
      "gap: 1pt 2pt 3pt",
      "padding: 1pt 2pt 3pt 4pt 5pt",
      "width: 2em",
      "border: nonsense"
    ]

    Enum.each(invalid_styles, fn style ->
      assert style_for("div", style) == {:error, :invalid_document}
    end)

    assert style_for("div", "break-before: auto") ==
             {:ok, style_for!("div", "break-before: auto")}

    assert style_for("div", "flex: 1 1 auto") == {:ok, style_for!("div", "flex: 1 1 auto")}
    assert style_for("div", "grid-template-columns: ") == {:error, :invalid_document}
    assert style_for("div", "grid-column: x") == {:error, :invalid_document}
    assert style_for("div", "font-family: Missing") == {:error, :invalid_document}
    assert style_for("div", "background-color: nope") == {:error, :invalid_document}
  end

  test "compute loads file URI and absolute images and rejects malformed image sources" do
    base_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-image-style-branches")
    File.mkdir_p!(base_dir)
    png_path = Path.join(base_dir, "photo.png")
    jpg_path = Path.join(base_dir, "photo.jpg")
    File.write!(png_path, png_rgba_fixture(1, 1, 4))
    File.write!(jpg_path, jpeg_fixture(4, 3, 1))
    cmyk_path = Path.join(base_dir, "cmyk.jpg")
    File.write!(cmyk_path, jpeg_fixture(2, 2, 4))
    chunked_png_path = Path.join(base_dir, "chunked.png")
    File.write!(chunked_png_path, png_with_extra_chunk_fixture())

    assert {:ok, png_style} = image_style(png_path, [])
    assert png_style.image.format == :png
    assert png_style.image.data == <<255, 0, 0>>

    assert {:ok, jpeg_style} = image_style("photo.jpg", base_url: "file://" <> base_dir)
    assert jpeg_style.image.format == :jpeg
    assert jpeg_style.image.color_space == :device_gray

    assert {:ok, cmyk_style} = image_style("cmyk.jpg", base_url: base_dir)
    assert cmyk_style.image.color_space == :device_cmyk

    assert {:ok, chunked_png_style} = image_style("chunked.png", base_url: base_dir)
    assert chunked_png_style.image.format == :png

    for filter <- [1, 2, 3] do
      filter_path = Path.join(base_dir, "filter-#{filter}.png")
      File.write!(filter_path, png_rgba_fixture(1, 2, filter))

      assert {:ok, filter_style} = image_style("filter-#{filter}.png", base_url: base_dir)
      assert filter_style.image.format == :png
    end

    for {name, fixture} <- [
          {"paeth-up.png", paeth_up_png_fixture()},
          {"paeth-up-left.png", paeth_up_left_png_fixture()}
        ] do
      File.write!(Path.join(base_dir, name), fixture)

      assert {:ok, paeth_style} = image_style(name, base_url: base_dir)
      assert paeth_style.image.format == :png
    end

    data_jpeg_src = "data:image/jpeg;base64,#{Base.encode64(jpeg_fixture(1, 1))}"
    assert {:ok, data_jpeg_style} = image_style(data_jpeg_src, [])
    assert data_jpeg_style.image.format == :jpeg

    restart_jpeg_src = "data:image/jpeg;base64,#{Base.encode64(restart_jpeg_fixture())}"
    assert {:ok, restart_jpeg_style} = image_style(restart_jpeg_src, [])
    assert restart_jpeg_style.image.format == :jpeg

    malformed_sources = [
      {"data:image/png;base64,%%%"},
      {"data:image/png;base64,#{Base.encode64(jpeg_fixture(1, 1))}"},
      {"data:image/png;base64,#{Base.encode64("not png")}"},
      {"data:image/png;base64,#{Base.encode64(<<137, 80, 78, 71, 13, 10, 26, 10, 1, 2, 3>>)}"},
      {"data:image/png;base64,#{Base.encode64(bad_idat_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(malformed_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(short_row_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(invalid_filter_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(trailing_png_fixture())}"},
      {"data:image/jpeg;base64,#{Base.encode64(<<1, 2, 3>>)}"},
      {"data:image/jpeg;base64,#{Base.encode64(<<255, 216, 255, 192, 0, 17, 7, 0, 1, 0, 1, 3>>)}"},
      {"data:image/jpeg;base64,#{Base.encode64(<<255, 216, 255, 217>>)}"},
      {"missing.png"},
      {"bad\u0000name.png"}
    ]

    Enum.each(malformed_sources, fn {src} ->
      assert image_style(src, base_url: base_dir) == {:error, :invalid_document}
    end)

    assert image_style("relative.png", []) == {:error, :invalid_document}

    assert image_style("photo.jpg", base_url: "https://example.com") ==
             {:error, :invalid_document}
  after
    base_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-image-style-branches")
    File.rm_rf(base_dir)
  end

  test "compute rejects malformed document children and stylesheet options" do
    assert Style.compute(%{type: :document, children: [%{type: :bogus}]}, []) ==
             {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{type: :element, tag: "unknown", attributes: %{}, children: []}
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert {:ok, link_tree} =
             Style.compute(
               %{
                 type: :document,
                 children: [
                   %{
                     type: :element,
                     tag: "a",
                     attributes: %{},
                     children: [%{type: :text, text: "x"}]
                   }
                 ]
               },
               []
             )

    [link] = link_tree.children
    assert link.style.display == :inline
    assert link.style.color == {0, 0, 1}
    refute Map.has_key?(link.style, :link_url)

    assert Style.compute(%{type: :document, children: []}, stylesheets: :bad) ==
             {:error, :invalid_document}

    assert Style.compute(%{type: :document, children: []}, stylesheets: ["missing-file.css"]) ==
             {:error, :invalid_document}

    assert Style.compute(%{type: :document, children: []}, stylesheets: [123]) ==
             {:error, :invalid_document}

    assert Style.compute(%{type: :document, children: []}, fonts: :bad) ==
             {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "a",
                   attributes: %{"href" => 123},
                   children: [%{type: :text, text: "bad"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert Style.compute(%{type: :document, children: []}, default_font: "Missing") ==
             {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "style",
                   attributes: %{},
                   children: [%{type: :element, tag: "span", attributes: %{}, children: []}]
                 },
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{"style" => ":"},
                   children: [%{type: :text, text: "Bad inline"}]
                 }
               ]
             },
             stylesheets: ["aside { color: red; }"]
           ) == {:error, :invalid_document}
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

  defp style_for(tag, style) do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: tag,
          attributes: %{"style" => style},
          children: [%{type: :text, text: "Styled"}]
        }
      ]
    }

    case Style.compute(dom, []) do
      {:ok, styled_tree} ->
        [element] = styled_tree.children
        {:ok, element.style}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp style_for!(tag, style) do
    assert {:ok, computed} = style_for(tag, style)
    computed
  end

  defp image_style(src, opts) do
    dom = %{
      type: :document,
      children: [
        %{type: :element, tag: "img", attributes: %{"src" => src}, children: []}
      ]
    }

    case Style.compute(dom, opts) do
      {:ok, styled_tree} ->
        [image] = styled_tree.children
        {:ok, image.style}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp assert_style_value(actual, expected) do
    case {actual, expected} do
      {{a, b, c}, {x, y, z}} ->
        assert_in_delta a, x, 0.0001
        assert_in_delta b, y, 0.0001
        assert_in_delta c, z, 0.0001

      _ ->
        assert actual == expected
    end
  end

  defp png_fixture(width, height) do
    png_fixture(width, height, 2, 0)
  end

  defp png_rgba_fixture(width, height, filter) do
    png_fixture(width, height, 6, filter)
  end

  defp png_fixture(width, height, color_type, filter) do
    pixel =
      case color_type do
        2 -> <<255, 0, 0>>
        6 -> <<255, 0, 0, 255>>
      end

    row = :binary.copy(pixel, width)
    rows = Enum.map_join(1..height, "", fn _index -> <<filter>> <> row end)

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<width::32, height::32, 8, color_type, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(rows)) <>
      png_chunk("IEND", "")
  end

  defp png_chunk(type, data) do
    crc = :erlang.crc32(type <> data)
    <<byte_size(data)::32, type::binary, data::binary, crc::32>>
  end

  defp png_with_extra_chunk_fixture do
    row = <<0, 255, 0, 0>>

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<1::32, 1::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("tEXt", "ignored") <>
      png_chunk("IDAT", :zlib.compress(row)) <>
      png_chunk("IEND", "")
  end

  defp malformed_png_fixture do
    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      <<1::32, "IHDR", 0, 0, 0, 0, 0::32>>
  end

  defp invalid_filter_png_fixture do
    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<1::32, 1::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(<<9, 255, 0, 0>>)) <>
      png_chunk("IEND", "")
  end

  defp short_row_png_fixture do
    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<2::32, 1::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(<<0, 255, 0, 0>>)) <>
      png_chunk("IEND", "")
  end

  defp paeth_up_png_fixture do
    rows =
      <<0, 100, 0, 0>> <>
        <<4, 0, 0, 0>>

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<1::32, 2::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(rows)) <>
      png_chunk("IEND", "")
  end

  defp paeth_up_left_png_fixture do
    rows =
      <<0, 25, 0, 0, 50, 0, 0>> <>
        <<4, 231, 0, 0, 0, 0, 0>>

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<2::32, 2::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(rows)) <>
      png_chunk("IEND", "")
  end

  defp bad_idat_png_fixture do
    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<1::32, 1::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", "not zlib") <>
      png_chunk("IEND", "")
  end

  defp trailing_png_fixture do
    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<1::32, 1::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(<<0, 255, 0, 0, 1>>)) <>
      png_chunk("IEND", "")
  end

  defp jpeg_fixture(width, height) do
    jpeg_fixture(width, height, 3)
  end

  defp jpeg_fixture(width, height, components) do
    <<255, 216, 255, 224, 0, 16, "JFIF", 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 255, 192, 0, 17, 8,
      height::16, width::16, components, 1, 17, 0, 2, 17, 0, 3, 17, 0, 255, 217>>
  end

  defp restart_jpeg_fixture do
    <<255, 216, 255, 208, 255, 192, 0, 17, 8, 1::16, 1::16, 3, 1, 17, 0, 2, 17, 0, 3, 17, 0, 255,
      217>>
  end
end
