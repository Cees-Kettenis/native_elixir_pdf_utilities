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
end
