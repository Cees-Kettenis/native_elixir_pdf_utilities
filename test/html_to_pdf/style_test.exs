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

  test "compute treats section and article as block containers" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "section",
          attributes: %{"style" => "display: flex"},
          children: [
            %{
              type: :element,
              tag: "article",
              attributes: %{},
              children: [%{type: :text, text: "Trim"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [section] = styled_tree.children
    [article] = section.children

    assert section.style.display == :flex
    assert article.style.display == :block
  end

  test "compute applies body inherited styles to document fragments" do
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
              text: "body { font-family: 'Fixture Sans', Helvetica; color: #102038; }"
            }
          ]
        },
        %{
          type: :element,
          tag: "section",
          attributes: %{},
          children: [%{type: :text, text: "Fragment"}]
        }
      ]
    }

    assert {:ok, styled_tree} =
             Style.compute(dom, fonts: [%{family: "Fixture Sans", path: ttf_font_path!()}])

    [section] = styled_tree.children
    [text] = section.children

    assert section.style.font_family == "Fixture Sans"
    assert text.style.font_face.id == section.style.font_face.id
    assert section.style.color == {0.06274509803921569, 0.12549019607843137, 0.2196078431372549}
  end

  test "compute honors display none on implicit fragment body" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [%{type: :text, text: "body { display: none; }"}]
        },
        %{
          type: :element,
          tag: "section",
          attributes: %{},
          children: [%{type: :text, text: "Hidden"}]
        }
      ]
    }

    assert {:ok, %{children: []}} = Style.compute(dom, [])
  end

  test "compute accepts print CSS properties and transformed text" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "section",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "style",
              attributes: %{},
              children: [
                %{
                  type: :text,
                  text:
                    "* { box-sizing: border-box; } .label:last-child { page-break-after: auto; } .label span:nth-child(2) { display: block; margin-bottom: 2px; }"
                }
              ]
            },
            %{
              type: :element,
              tag: "div",
              attributes: %{
                "class" => "label",
                "style" =>
                  "background: #ffffff; border: 1px dashed #ccd6e1; width: min(100%, 360px); word-break: break-word; white-space: pre-line; letter-spacing: 0.05em; page-break-inside: avoid; text-transform: uppercase"
              },
              children: [
                %{
                  type: :element,
                  tag: "span",
                  attributes: %{},
                  children: [%{type: :text, text: "one"}]
                },
                %{
                  type: :element,
                  tag: "span",
                  attributes: %{},
                  children: [%{type: :text, text: "two"}]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [section] = styled_tree.children
    [label] = section.children
    [first, second] = label.children
    [first_text] = first.children

    assert label.style.background_color == {1.0, 1.0, 1.0}
    assert label.style.border_widths == %{top: 0.75, right: 0.75, bottom: 0.75, left: 0.75}
    assert label.style.break_after == :auto
    assert label.style.line_break == :break_word
    assert label.style.width == {:min, [{:percent, 1.0}, 270.0]}
    assert first_text.text == "ONE"
    assert second.style.display == :block
    assert second.style.margin_after == 1.5
  end

  test "compute skips descendants for display none elements" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{type: :text, text: ".hidden { display: none; }"}
          ]
        },
        %{
          type: :element,
          tag: "div",
          attributes: %{"class" => "hidden"},
          children: [
            %{
              type: :element,
              tag: "img",
              attributes: %{"src" => "data:image/png;base64,#RIGHTITEMQRCODE#"},
              children: []
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [hidden] = styled_tree.children

    assert hidden.style.display == :none
    assert hidden.children == []

    hidden_body_dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "body",
          attributes: %{"style" => "display: none"},
          children: [
            %{
              type: :element,
              tag: "img",
              attributes: %{"src" => "data:image/png;base64,#RIGHTITEMQRCODE#"},
              children: []
            }
          ]
        }
      ]
    }

    assert {:ok, %{children: []}} = Style.compute(hidden_body_dom, [])
  end

  test "compute covers print CSS alternate values and invalid branches" do
    assert style_for!("div", "background: none").background_color == nil
    assert style_for!("div", "display: grid; justify-self: center").justify_self == :center
    assert style_for!("div", "letter-spacing: normal").letter_spacing == 0.0
    assert style_for!("div", "letter-spacing: 0").letter_spacing == 0.0
    assert style_for!("div", "letter-spacing: 2px").letter_spacing == 1.5
    assert style_for!("div", "font-size: 10pt; letter-spacing: 0.05em").letter_spacing == 0.5
    assert style_for!("div", "white-space: nowrap").line_break == :normal
    assert style_for!("div", "word-wrap: normal").line_break == :normal
    assert style_for!("div", "overflow-wrap: anywhere").line_break == :anywhere
    assert style_for!("div", "border: 0").border_widths == edges(0.0)
    assert style_for!("div", "border-right: 0").border_widths.right == 0.0
    assert style_for!("div", "display: grid; grid-template-columns: 1fr  auto").display == :grid

    assert style_for!("div", "width: min(100%, min(10px, 20px))").width ==
             {:min, [{:percent, 1.0}, {:min, [7.5, 15.0]}]}

    lowercase = text_for_style("text-transform: lowercase", "MiXeD")
    capitalized = text_for_style("text-transform: capitalize", "hello world")
    unchanged = text_for_style("text-transform: none", "MiXeD")

    assert lowercase == "mixed"
    assert capitalized == "Hello World"
    assert unchanged == "MiXeD"

    invalid_styles = [
      "box-sizing: padding-box",
      "text-transform: sideways",
      "letter-spacing: wide",
      "white-space: pre-wrap",
      "page-break-inside: always",
      "float: left",
      "display: grid; grid-template-columns: minmax()",
      "width: min(100%, nope)",
      "width: min()",
      "width: min",
      "background: linear-gradient(red, blue)"
    ]

    for style <- invalid_styles do
      assert style_for("div", style) == {:error, :invalid_document}
    end
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

  test "compute applies explicit line-height declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "font-size: 10pt; line-height: 12px"},
          children: [%{type: :text, text: "Absolute"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "line-height: 1.5; font-size: 10pt"},
          children: [%{type: :text, text: "Unitless"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "font-size: 10pt; line-height: normal"},
          children: [%{type: :text, text: "Normal"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [absolute, unitless, normal] = styled_tree.children

    assert absolute.style.line_height == 9.0
    assert unitless.style.line_height == 15.0
    assert normal.style.line_height == 12.0

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{"style" => "line-height: tight"},
                   children: [%{type: :text, text: "Invalid"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}
  end

  test "compute derives normal line-height from embedded font metrics" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{
            "style" => "font-family: 'Fixture Sans'; font-size: 10pt; line-height: normal"
          },
          children: [%{type: :text, text: "Metric"}]
        }
      ]
    }

    assert {:ok, styled_tree} =
             Style.compute(dom, fonts: [%{family: "Fixture Sans", path: ttf_font_path!()}])

    [paragraph] = styled_tree.children
    %{ascent: ascent, descent: descent, units_per_em: units_per_em} = paragraph.style.font_face
    expected_line_height = (ascent - descent) / units_per_em * 10.0

    assert_in_delta paragraph.style.line_height, expected_line_height, 0.0001
  end

  test "compute accepts normal line-break declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "line-break: normal"},
          children: [%{type: :text, text: "Normal break"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.line_break == :normal
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
              "margin: 2pt 4pt 6pt 8pt; padding: 3pt 5pt; border: 1pt solid #336699; border-radius: 2pt; background-color: #eeeeee; box-sizing: border-box"
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
    assert paragraph.style.border_colors == edges({0.2, 0.4, 0.6})
    assert paragraph.style.border_radius == 2.0
    assert paragraph.style.box_sizing == :border_box
    assert_in_delta elem(paragraph.style.background_color, 0), 0.9333, 0.0001
  end

  test "compute preserves side-specific border colors" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{
            "style" => "border-top: 4px solid #22344a; border-bottom: 1px solid #d7dfe9"
          },
          children: [%{type: :text, text: "Head"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [element] = styled_tree.children

    assert element.style.border_widths == %{top: 3.0, right: 0.0, bottom: 0.75, left: 0.0}

    assert element.style.border_colors.top ==
             {0.13333333333333333, 0.20392156862745098, 0.2901960784313726}

    assert element.style.border_colors.bottom ==
             {0.8431372549019608, 0.8745098039215686, 0.9137254901960784}
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
                      attributes: %{
                        "colspan" => "2",
                        "rowspan" => "3",
                        "style" => "padding: 2pt; border: 2pt solid blue"
                      },
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
    assert alpha.style.colspan == 2
    assert alpha.style.rowspan == 3
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

  test "compute loads local fonts declared by embedded font-face rules" do
    font_path = ttf_font_path!()

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
              @font-face {
                font-family: "CSS Fixture";
                src: url("#{font_path}") format("truetype");
              }
              @media print { p { font-family: "CSS Fixture"; color: red; } }
              @media screen { p { color: blue; } }
              """
            }
          ]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "Printed"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom)
    [paragraph] = styled_tree.children

    assert paragraph.style.font_family == "CSS Fixture"
    assert paragraph.style.font_face.type == :embedded
    assert paragraph.style.color == {1, 0, 0}
  end

  test "compute resolves font-face URLs relative to stylesheet files and base_url" do
    fixture_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-css-font-test")
    css_dir = Path.join(fixture_dir, "css")
    font_dir = Path.join(fixture_dir, "fonts")
    stylesheet_path = Path.join(css_dir, "print.css")
    copied_font_path = Path.join(font_dir, "fixture.ttf")
    File.mkdir_p!(css_dir)
    File.mkdir_p!(font_dir)
    File.cp!(ttf_font_path!(), copied_font_path)

    File.write!(
      stylesheet_path,
      "@font-face { font-family: FileFixture; src: url('../fonts/fixture.ttf'); } p { font-family: FileFixture; }"
    )

    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "From file"}]
        }
      ]
    }

    assert {:ok, styled_from_file} = Style.compute(dom, stylesheets: [stylesheet_path])
    [paragraph] = styled_from_file.children
    assert paragraph.style.font_face.type == :embedded

    inline_css =
      "@font-face { font-family: BaseFixture; src: url('fonts/fixture.ttf'); } p { font-family: BaseFixture; }"

    assert {:ok, styled_from_base} =
             Style.compute(dom, stylesheets: [inline_css], base_url: fixture_dir)

    [paragraph] = styled_from_base.children
    assert paragraph.style.font_face.type == :embedded

    assert {:ok, styled_from_file_url} =
             Style.compute(dom, stylesheets: [inline_css], base_url: "file://#{fixture_dir}")

    [paragraph] = styled_from_file_url.children
    assert paragraph.style.font_face.type == :embedded

    assert Style.compute(dom, stylesheets: [inline_css]) == {:error, :invalid_document}

    assert Style.compute(dom, stylesheets: [inline_css], base_url: "https://example.com") ==
             {:error, :invalid_document}

    invalid_face_css =
      "@font-face { font-family: Bad; src: url('font.woff2') format('woff2'); }"

    assert Style.compute(dom, stylesheets: [invalid_face_css], base_url: fixture_dir) ==
             {:error, :invalid_document}

    invalid_stylesheet_path = Path.join(css_dir, "invalid.css")
    File.write!(invalid_stylesheet_path, "p { color: chartreuse; }")

    assert {:error, {:invalid_css, %{stage: :css}}} =
             Style.compute_detailed(dom, stylesheets: [invalid_stylesheet_path])
  after
    File.rm_rf(Path.join(System.tmp_dir!(), "native-elixir-pdf-css-font-test"))
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

  test "compute applies first-child pseudo selectors" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [%{type: :text, text: "div p:first-child { color: red; }"}]
        },
        %{
          type: :element,
          tag: "div",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "p",
              attributes: %{},
              children: [%{type: :text, text: "First"}]
            },
            %{
              type: :element,
              tag: "p",
              attributes: %{},
              children: [%{type: :text, text: "Second"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [container] = styled_tree.children
    [first, second] = container.children

    assert first.style.color == {1, 0, 0}
    assert second.style.color == {0, 0, 0}
  end

  test "compute accepts root custom properties and table cell layout hints" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "html",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "style",
              attributes: %{},
              children: [
                %{
                  type: :text,
                  text:
                    ":root { --row-height: 8rem; } .none { display: none !important; } .approval-section { display: flex; } td { line-break: anywhere; vertical-align: top; } .item-row td { min-height: var(--row-height); height: var(--row-height); }"
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
                  tag: "div",
                  attributes: %{"class" => "none approval-section"},
                  children: [%{type: :text, text: "Hidden"}]
                },
                %{
                  type: :element,
                  tag: "table",
                  attributes: %{},
                  children: [
                    %{
                      type: :element,
                      tag: "tr",
                      attributes: %{"class" => "item-row"},
                      children: [
                        %{
                          type: :element,
                          tag: "td",
                          attributes: %{},
                          children: [%{type: :text, text: "Cell"}]
                        }
                      ]
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
    hidden = Enum.find(styled_tree.children, &match?(%{tag: "div"}, &1))
    table = Enum.find(styled_tree.children, &match?(%{tag: "table"}, &1))
    [row] = table.children
    [cell] = row.children

    assert hidden.style.display == :none
    assert table.style.display == :table
    assert cell.style.display == :table_cell
    assert cell.style.height == 96.0
    assert cell.style.line_break == :anywhere
    assert cell.style.min_height == 96.0
  end

  test "compute resolves custom properties inside supported compound values" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "html",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "style",
              attributes: %{},
              children: [
                %{
                  type: :text,
                  text:
                    ":root { --pad: 6pt; --accent: #0f766e; } section { padding: var(--pad); border-left: 3pt solid var(--accent); }"
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
                  tag: "section",
                  attributes: %{},
                  children: [%{type: :text, text: "Panel"}]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [section] = styled_tree.children

    assert section.style.padding == %{top: 6.0, right: 6.0, bottom: 6.0, left: 6.0}
    assert section.style.border_widths.left == 3.0
    assert_style_value(section.style.border_colors.left, {0.0588235294, 0.462745098, 0.431372549})
  end

  test "compute applies child selectors with first-child pseudo classes" do
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
              text:
                ".address-section > .section { color: #336699; } .address-section > .section p { margin-top: 4pt; } .address-section > .section p:first-child { margin-top: 0; color: red; }"
            }
          ]
        },
        %{
          type: :element,
          tag: "div",
          attributes: %{"class" => "address-section"},
          children: [
            %{
              type: :element,
              tag: "div",
              attributes: %{"class" => "section"},
              children: [
                %{
                  type: :element,
                  tag: "p",
                  attributes: %{},
                  children: [%{type: :text, text: "First"}]
                },
                %{
                  type: :element,
                  tag: "p",
                  attributes: %{},
                  children: [%{type: :text, text: "Second"}]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [section_wrapper] = styled_tree.children
    [section] = section_wrapper.children
    [first, second] = section.children

    assert section.style.color == {0.2, 0.4, 0.6}
    assert first.style.color == {1, 0, 0}
    assert first.style.margin.top == 0.0
    assert second.style.margin.top == 4.0
    assert second.style.color == {0.2, 0.4, 0.6}
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
                           attributes: %{"colspan" => 2},
                           children: [%{type: :text, text: "Bad span shape"}]
                         }
                       ]
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
                           attributes: %{"colspan" => "0"},
                           children: [%{type: :text, text: "Bad span"}]
                         }
                       ]
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
                   attributes: %{"style" => "background: linear-gradient(red, blue)"},
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
                   children: [
                     %{type: :text, text: "p { background: linear-gradient(red, blue); }"}
                   ]
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
                   attributes: %{"style" => "border: 1pt double red"},
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
                     "style" => "display: grid; grid-template-columns: repeat(0, 1fr)"
                   },
                   children: [%{type: :text, text: "Bad"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}
  end

  test "compute_detailed returns document diagnostics" do
    assert {:error,
            {:invalid_document,
             %{
               stage: :style,
               reason: :invalid_document,
               message: "document tree must be a parsed HTML document"
             }}} = Style.compute_detailed(%{tag: "p"})

    assert {:ok, %{type: :document}} =
             Style.compute_detailed(%{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{},
                   children: [%{type: :text, text: "Detailed"}]
                 }
               ]
             })

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               source: "display: table-row-group"
             }}} =
             Style.compute_detailed(%{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "style",
                   attributes: %{},
                   children: [%{type: :text, text: "body { display: table-row-group; }"}]
                 },
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{},
                   children: [%{type: :text, text: "Detailed"}]
                 }
               ]
             })
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

    assert Enum.map(headings, & &1.style.font_size) == [
             18.0,
             14.04,
             12.0,
             9.959999999999999,
             8.040000000000001
           ]

    [bold, italic] = paragraph.children
    assert bold.style.font_weight == 700
    assert italic.style.font_style == :italic
    [foot] = table.children
    assert foot.style.table_section == :foot
  end

  test "compute inherits body font size into document blocks tables and headings" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "html",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "head",
              attributes: %{},
              children: [
                %{
                  type: :element,
                  tag: "style",
                  attributes: %{},
                  children: [%{type: :text, text: "body { font-size: 10px; }"}]
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
                  children: [%{type: :text, text: "P"}]
                },
                %{
                  type: :element,
                  tag: "h2",
                  attributes: %{},
                  children: [%{type: :text, text: "H2"}]
                },
                %{
                  type: :element,
                  tag: "h4",
                  attributes: %{},
                  children: [%{type: :text, text: "H4"}]
                },
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
                          attributes: %{},
                          children: [%{type: :text, text: "Cell"}]
                        }
                      ]
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
    [paragraph, h2, h4, table] = styled_tree.children
    [row] = table.children
    [cell] = row.children

    assert paragraph.style.font_size == 7.5
    assert h2.style.font_size == 11.25
    assert h4.style.font_size == 7.5
    assert table.style.font_size == 7.5
    assert cell.style.font_size == 7.5
  end

  test "compute accepts supported display and box value variants" do
    assertions = [
      {"display: block", :display, :block},
      {"display: inline", :display, :inline},
      {"display: inline-block", :display, :block},
      {"display: none", :display, :none},
      {"display: inline-flex", :display, :inline_flex},
      {"display: inline-grid", :display, :inline_grid},
      {"box-sizing: content-box", :box_sizing, :content_box},
      {"margin: 1pt 2pt 3pt", :margin, %{top: 1.0, right: 2.0, bottom: 3.0, left: 2.0}},
      {"padding-top: 2pt", :padding, %{top: 2.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"padding-right: 2pt", :padding, %{top: 0.0, right: 2.0, bottom: 0.0, left: 0.0}},
      {"padding-bottom: 2pt", :padding, %{top: 0.0, right: 0.0, bottom: 2.0, left: 0.0}},
      {"padding-left: 2pt", :padding, %{top: 0.0, right: 0.0, bottom: 0.0, left: 2.0}},
      {"margin-left: 2pt", :margin, %{top: 0.0, right: 0.0, bottom: 12.0, left: 2.0}},
      {"border-width: 2pt", :border_widths, %{top: 2.0, right: 2.0, bottom: 2.0, left: 2.0}},
      {"border: none", :border_widths, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"border-right: none", :border_widths, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"border-left: 2pt solid red", :border_widths,
       %{top: 0.0, right: 0.0, bottom: 0.0, left: 2.0}},
      {"border-top-width: 3pt", :border_widths, %{top: 3.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"border-bottom-color: rgb(51, 102, 153)", :border_colors,
       %{top: {0, 0, 0}, right: {0, 0, 0}, bottom: {0.2, 0.4, 0.6}, left: {0, 0, 0}}},
      {"border-left-style: none", :border_widths,
       %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"border-style: none", :border_widths, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"border: 1pt solid", :border_color, {0, 0, 0}},
      {"border: 1pt solid transparent", :border_color, {0, 0, 0}},
      {"border-color: transparent", :border_color, {0, 0, 0}},
      {"border-top: 2pt solid transparent", :border_colors,
       %{top: {0, 0, 0}, right: {0, 0, 0}, bottom: {0, 0, 0}, left: {0, 0, 0}}},
      {"border-right-color: transparent", :border_colors,
       %{top: {0, 0, 0}, right: {0, 0, 0}, bottom: {0, 0, 0}, left: {0, 0, 0}}},
      {"color: #abc", :color, {0.6666666667, 0.7333333333, 0.8}},
      {"color: #00000070", :color, {0, 0, 0}},
      {"color: rgba(255, 128, 0, 0.5)", :color, {1.0, 0.5019607843, 0.0}},
      {"color: rgb(100%, 50%, 0%)", :color, {1.0, 0.5, 0.0}},
      {"color: currentColor", :color, {0, 0, 0}},
      {"color: transparent", :color, {0, 0, 0}},
      {"background-color: #33669980", :background_color, {0.2, 0.4, 0.6}},
      {"background-color: transparent", :background_color, nil},
      {"font-size: 10px", :font_size, 7.5},
      {"width: 10mm", :width, 72.0 / 25.4 * 10},
      {"width: 100%", :width, {:percent, 1.0}},
      {"min-width: 20pt", :min_width, 20.0},
      {"max-width: 30pt", :max_width, 30.0},
      {"height: 1cm", :height, 72.0 / 2.54},
      {"height: 25%", :height, {:percent, 0.25}},
      {"max-height: 40pt", :max_height, 40.0},
      {"width: 1in", :width, 72.0},
      {"height: 0", :height, 0.0},
      {"position: relative", :display, :block},
      {"overflow: hidden", :overflow, :hidden},
      {"overflow: visible", :overflow, :visible},
      {"aspect-ratio: 16 / 9", :aspect_ratio, 16 / 9},
      {"text-align: left", :text_align, :left},
      {"text-align: center", :text_align, :center},
      {"text-align: right", :text_align, :right},
      {"vertical-align: baseline", :vertical_align, :baseline},
      {"vertical-align: top", :vertical_align, :top},
      {"vertical-align: middle", :vertical_align, :middle},
      {"vertical-align: bottom", :vertical_align, :bottom},
      {"font-weight: normal", :font_weight, 400},
      {"font-weight: 900", :font_weight, 900},
      {"font-style: normal", :font_style, :normal},
      {"font-style: italic", :font_style, :italic},
      {"border-color: red", :border_color, {1, 0, 0}},
      {"border-top: 2pt solid red", :border_color, {1, 0, 0}},
      {"border-collapse: collapse", :border_collapse, :collapse},
      {"border-collapse: separate", :border_collapse, :separate},
      {"aspect-ratio: 1.5", :aspect_ratio, 1.5},
      {"margin-bottom: 5pt", :margin_after, 5.0},
      {"margin-top: -2pt", :margin, %{top: -2.0, right: 0.0, bottom: 12.0, left: 0.0}}
    ]

    Enum.each(assertions, fn {style, key, expected} ->
      assert {:ok, computed} = style_for("p", style)
      assert_style_value(Map.fetch!(computed, key), expected)
    end)
  end

  test "side border none keeps the existing border color" do
    assert {:ok, style} = style_for("td", "border: 1px solid #ccc; border-right: none")

    assert style.border_widths == %{top: 0.75, right: 0.0, bottom: 0.75, left: 0.75}
    assert_style_value(style.border_color, {0.8, 0.8, 0.8})
  end

  test "table cells default to browser middle vertical alignment" do
    assert {:ok, style} = style_for("td", "")

    assert style.vertical_align == :middle
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
      "padding-top: -1pt",
      "width: 2em",
      "width: -10%",
      "height: -1pt",
      "height: var(--missing)",
      "max-width: nope",
      "min-height: var(--missing)",
      "min-height: nope",
      "position: absolute",
      "overflow: scroll",
      "border-top-color: nope",
      "border-top-style: dotted",
      "color: rgb(1, 2)",
      "color: rgb()",
      "color: rgb(nope, 0, 0)",
      "vertical-align: super",
      "line-break: strict",
      "color: #0000007",
      "aspect-ratio: 0 / 1",
      "aspect-ratio: 1 / 2 / 3",
      "border-collapse: maybe",
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
    refute Map.has_key?(png_style.image, :alpha_data)

    transparent_png_path = Path.join(base_dir, "transparent.png")
    File.write!(transparent_png_path, png_rgba_fixture(1, 1, 0, <<0, 0, 0, 0>>))

    assert {:ok, transparent_png_style} = image_style(transparent_png_path, [])
    assert transparent_png_style.image.data == <<0, 0, 0>>
    assert transparent_png_style.image.alpha_data == <<0>>

    assert {:ok, jpeg_style} = image_style("photo.jpg", base_url: "file://" <> base_dir)
    assert jpeg_style.image.format == :jpeg
    assert jpeg_style.image.color_space == :device_gray

    assert {:ok, cmyk_style} = image_style("cmyk.jpg", base_url: base_dir)
    assert cmyk_style.image.color_space == :device_cmyk

    assert {:ok, chunked_png_style} = image_style("chunked.png", base_url: base_dir)
    assert chunked_png_style.image.format == :png

    large_png_src = "data:image/png;base64,#{Base.encode64(png_fixture(100, 100))}"
    assert {:ok, large_png_style} = image_style(large_png_src, [])
    assert large_png_style.image.width_px == 100
    assert large_png_style.image.height_px == 100

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

    svg_src =
      "data:image/svg+xml;base64," <>
        Base.encode64(
          ~s(<svg xmlns="http://www.w3.org/2000/svg" width="2" height="1"><rect width="2" height="1" fill="red"/></svg>)
        )

    assert {:ok, svg_style} = image_style(svg_src, [])
    assert svg_style.display == :image
    assert svg_style.image.format == :png
    assert svg_style.image.width_px == 2
    assert svg_style.image.height_px == 1

    transparent_svg_src =
      "data:image/svg+xml;base64," <>
        Base.encode64(
          ~s(<svg xmlns="http://www.w3.org/2000/svg" width="2" height="1"><rect width="1" height="1" fill="red"/></svg>)
        )

    assert {:ok, transparent_svg_style} = image_style(transparent_svg_src, [])
    assert byte_size(transparent_svg_style.image.alpha_data) == 2

    doctype_svg_src =
      "data:image/svg+xml;base64," <>
        Base.encode64("""
        <?xml version="1.0" standalone="no"?>
        <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 20010904//EN"
          "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">
        <svg version="1.0" xmlns="http://www.w3.org/2000/svg" width="3" height="2">
          <path d="M0 0h3v2H0z" fill="#b9251b"/>
        </svg>
        """)

    assert {:ok, doctype_svg_style} = image_style(doctype_svg_src, [])
    assert doctype_svg_style.image.format == :png
    assert doctype_svg_style.image.width_px == 3
    assert doctype_svg_style.image.height_px == 2

    sized_svg_src =
      "data:image/svg+xml;base64," <>
        Base.encode64(
          ~s(<svg xmlns="http://www.w3.org/2000/svg" width="1000" height="1000"><rect width="1000" height="1000" fill="red"/></svg>)
        )

    sized_svg_dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "img",
          attributes: %{"src" => sized_svg_src, "style" => "width: 10px; aspect-ratio: 1"},
          children: []
        }
      ]
    }

    assert {:ok, sized_svg_tree} = Style.compute(sized_svg_dom, [])
    [sized_svg] = sized_svg_tree.children
    assert sized_svg.style.image.format == :png
    assert sized_svg.style.image.width_px == 10
    assert sized_svg.style.image.height_px == 10

    height_sized_svg_dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "img",
          attributes: %{"src" => sized_svg_src, "style" => "height: 10px; aspect-ratio: 2"},
          children: []
        }
      ]
    }

    assert {:ok, height_sized_svg_tree} = Style.compute(height_sized_svg_dom, [])
    [height_sized_svg] = height_sized_svg_tree.children
    assert height_sized_svg.style.image.format == :png
    assert height_sized_svg.style.aspect_ratio == 2.0
    assert height_sized_svg.style.height == 7.5
    assert height_sized_svg.style.image.width_px == 10
    assert height_sized_svg.style.image.height_px == 10

    malformed_sources = [
      {"data:image/svg+xml;base64,#{Base.encode64("<svg></svg>")}"},
      {"data:image/svg+xml;base64,#{Base.encode64(<<255>>)}"},
      {"data:image/png;base64,%%%"},
      {"data:image/png;base64,#{Base.encode64(jpeg_fixture(1, 1))}"},
      {"data:image/png;base64,#{Base.encode64("not png")}"},
      {"data:image/png;base64,#{Base.encode64(<<137, 80, 78, 71, 13, 10, 26, 10, 1, 2, 3>>)}"},
      {"data:image/png;base64,#{Base.encode64(bad_idat_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(truncated_idat_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(malformed_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(short_row_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(invalid_filter_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(trailing_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(oversized_png_fixture())}"},
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

    assert Style.compute_detailed(%{type: :document, children: []}, [:not_options]) ==
             {:error,
              {:invalid_document,
               %{
                 stage: :style,
                 reason: :invalid_document,
                 message: "style options must be a keyword list"
               }}}

    assert Style.load_stylesheets(:not_a_document, []) == {:error, :invalid_document}

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

  defp text_for_style(style, text) do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{"style" => style},
          children: [%{type: :text, text: text}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [element] = styled_tree.children
    [text_node] = element.children
    text_node.text
  end

  defp edges(value) do
    %{top: value, right: value, bottom: value, left: value}
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

  defp png_rgba_fixture(width, height, filter, pixel) do
    png_fixture(width, height, 6, filter, pixel)
  end

  defp png_fixture(width, height, color_type, filter) do
    pixel =
      case color_type do
        2 -> <<255, 0, 0>>
        6 -> <<255, 0, 0, 255>>
      end

    png_fixture(width, height, color_type, filter, pixel)
  end

  defp png_fixture(width, height, color_type, filter, pixel) do
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

  defp truncated_idat_png_fixture do
    compressed = :zlib.compress(<<0, 255, 0, 0>>)
    truncated = binary_part(compressed, 0, byte_size(compressed) - 1)

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<1::32, 1::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", truncated) <>
      png_chunk("IEND", "")
  end

  defp trailing_png_fixture do
    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<1::32, 1::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(<<0, 255, 0, 0, 1>>)) <>
      png_chunk("IEND", "")
  end

  defp oversized_png_fixture do
    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<100_000::32, 100_000::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress("")) <>
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
