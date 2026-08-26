defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlParserTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser

  test "parse accepts a strict paragraph document" do
    assert HtmlParser.parse("<p>Hello &amp; PDF</p>") ==
             {:ok,
              %{
                type: :document,
                children: [
                  %{
                    type: :element,
                    tag: "p",
                    attributes: %{},
                    children: [%{type: :text, text: "Hello & PDF"}]
                  }
                ]
              }}
  end

  test "parse accepts headings and inline text elements with style attributes" do
    assert HtmlParser.parse(
             ~s(<h1 style="color: #336699">Title</h1><p>Hello <strong>bold</strong> and <em>italic</em></p>)
           ) ==
             {:ok,
              %{
                type: :document,
                children: [
                  %{
                    type: :element,
                    tag: "h1",
                    attributes: %{"style" => "color: #336699"},
                    children: [%{type: :text, text: "Title"}]
                  },
                  %{
                    type: :element,
                    tag: "p",
                    attributes: %{},
                    children: [
                      %{type: :text, text: "Hello "},
                      %{
                        type: :element,
                        tag: "strong",
                        attributes: %{},
                        children: [%{type: :text, text: "bold"}]
                      },
                      %{type: :text, text: " and "},
                      %{
                        type: :element,
                        tag: "em",
                        attributes: %{},
                        children: [%{type: :text, text: "italic"}]
                      }
                    ]
                  }
                ]
              }}
  end

  test "parse accepts style tags and CSS targeting attributes" do
    assert HtmlParser.parse(
             ~s(<style>p.copy { color: red; }</style><p id="intro" class="copy lead">Hello</p>)
           ) ==
             {:ok,
              %{
                type: :document,
                children: [
                  %{
                    type: :element,
                    tag: "style",
                    attributes: %{},
                    children: [%{type: :text, text: "p.copy { color: red; }"}]
                  },
                  %{
                    type: :element,
                    tag: "p",
                    attributes: %{"id" => "intro", "class" => "copy lead"},
                    children: [%{type: :text, text: "Hello"}]
                  }
                ]
              }}
  end

  test "parse keeps quoted tag delimiters inside attribute values" do
    assert {:ok, %{children: [paragraph]}} =
             HtmlParser.parse(~s(<p title="1 > 0" data-check='2 > 1'>Valid</p>))

    assert paragraph.attributes == %{"data-check" => "2 > 1", "title" => "1 > 0"}
    assert paragraph.children == [%{type: :text, text: "Valid"}]
  end

  test "parse preserves style raw text and decodes RCDATA entities" do
    style_text = ~s(p::before { content: "< &lt;"; })

    html =
      "<style>#{style_text}</style>" <>
        "<title>1 < 2 &amp; 3</title>" <>
        "<div><textarea>4 < 5 &amp; 6</textarea></div>"

    assert {:ok, %{children: [style, title, %{children: [textarea]}]}} = HtmlParser.parse(html)
    assert style.children == [%{type: :text, text: style_text}]
    assert title.children == [%{type: :text, text: "1 < 2 & 3"}]
    assert textarea.children == [%{type: :text, text: "4 < 5 & 6"}]

    assert {:ok, %{children: [%{children: [raw_text]}]}} =
             HtmlParser.parse("<style><span>No element</span></style>")

    assert raw_text == %{type: :text, text: "<span>No element</span>"}
  end

  test "parse accepts generated-content and attribute-selector metadata" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<p data-label="Status" aria-label="Current status" role="note" title="Ready">Complete</p>)
             )

    [paragraph] = dom.children

    assert paragraph.attributes == %{
             "aria-label" => "Current status",
             "data-label" => "Status",
             "role" => "note",
             "title" => "Ready"
           }
  end

  test "parse accepts structural html head and body tags" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<html><head><style>p { color: blue; }</style></head><body><p>Hello</p></body></html>)
             )

    [html] = dom.children
    [head, body] = html.children
    [style] = head.children
    [paragraph] = body.children

    assert html.tag == "html"
    assert style.tag == "style"
    assert paragraph.tag == "p"
  end

  test "parse accepts semantic block containers" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<main><header><p>Head</p></header><nav><p>Nav</p></nav><aside><p>Side</p></aside><footer><p>Foot</p></footer></main>)
             )

    [main] = dom.children
    assert main.tag == "main"
    assert Enum.map(main.children, & &1.tag) == ["header", "nav", "aside", "footer"]
  end

  test "parse accepts document metadata void tags and line breaks" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<!DOCTYPE html><html lang="en"><meta charset="utf-8" /><title>PO</title><body><p>One<br />Two<br>Three</p><img src="logo.png" /></body></html>)
             )

    [html] = dom.children
    [meta, title, body] = html.children
    [paragraph, image] = body.children

    assert html.attributes == %{"lang" => "en"}
    assert meta.tag == "meta"
    assert meta.attributes == %{"charset" => "utf-8"}
    assert title.children == [%{type: :text, text: "PO"}]

    assert Enum.map(paragraph.children, &Map.get(&1, :tag, :text)) == [
             :text,
             "br",
             :text,
             "br",
             :text
           ]

    assert image.children == []
  end

  test "parse accepts static form controls and boolean attributes" do
    html = """
    <div>
      <input type="text" value="Alice &amp; Bob">
      <input type="checkbox" checked disabled="disabled">
      <input type="radio" checked="checked">
      <select name="status">
        <option>Pending</option>
        <option value="approved" selected>Approved</option>
      </select>
      <textarea name="notes" value="Fallback"></textarea>
      <button type="button"><strong>Save</strong></button>
    </div>
    """

    assert {:ok, %{children: [%{children: controls}]}} = HtmlParser.parse(html)

    [text, checkbox, radio, select, textarea, button] =
      Enum.reject(controls, &match?(%{type: :text}, &1))

    assert text.attributes == %{"type" => "text", "value" => "Alice & Bob"}

    assert checkbox.attributes == %{
             "checked" => "",
             "disabled" => "disabled",
             "type" => "checkbox"
           }

    assert radio.attributes == %{"checked" => "checked", "type" => "radio"}
    assert Enum.map(select.children, & &1.tag) == ["option", "option"]
    assert Enum.at(select.children, 1).attributes == %{"selected" => "", "value" => "approved"}
    assert textarea.attributes == %{"name" => "notes", "value" => "Fallback"}
    assert [%{tag: "strong"}] = button.children
  end

  test "parse rejects invalid static form structures and attributes" do
    invalid_html = [
      ~s(<div><input type="email" value="a@example.test"></div>),
      ~s(<div><input type="text" checked></div>),
      ~s(<div><input type="checkbox">child</input></div>),
      ~s(<div><select></select></div>),
      ~s(<div><select><span>Wrong</span></select></div>),
      ~s(<div><select><option></option></select></div>),
      ~s(<div><select><option selected>One</option><option selected>Two</option></select></div>),
      ~s(<div><button><input type="text"></button></div>),
      ~s(<div><button><strong><input type="text"></strong></button></div>),
      ~s(<div><button type="menu">Wrong</button></div>),
      ~s(<div><input type="text" readonly></div>),
      ~s(<div class>Wrong</div>)
    ]

    Enum.each(invalid_html, fn html ->
      assert HtmlParser.parse(html) == {:error, :unsupported_html}
    end)
  end

  test "parse ignores structural whitespace and decodes HTML character references once" do
    assert {:ok, dom} =
             HtmlParser.parse("""

             <html>
               <head>
                 <style>p::before { content: &quot;x&quot;; }</style>
               </head>
               <body>
                 <ul>
                  <li>&lt;One&gt; &apos;Two&apos; &#39;Three&#39;&nbsp;Four &copy; &#169; &#x2122; &NotEqualTilde; &amp;lt;</li>
                 </ul>
                 <table>
                   <tr><td>A</td></tr>
                 </table>
               </body>
             </html>
             """)

    [html] = dom.children
    [_head, body] = html.children
    [list, table] = body.children
    [item] = list.children
    [text] = item.children
    [row] = table.children

    assert text.text == "<One> 'Two' 'Three'\u00A0Four © © ™ ≂̸ &lt;"
    assert row.tag == "tr"
  end

  test "parse applies legacy named-reference rules in text and attributes" do
    assert {:ok, dom} =
             HtmlParser.parse(~s(<p class="&copy; &copyy &amp=1">&copy &copyy &unknown;</p>))

    [paragraph] = dom.children
    [text] = paragraph.children

    assert paragraph.attributes == %{"class" => "© &copyy &amp=1"}
    assert text.text == "© ©y &unknown;"
  end

  test "parse normalizes invalid and legacy numeric character references" do
    assert {:ok, dom} =
             HtmlParser.parse("<p>&#x80; &#0; &#xD800; &#x110000; &#x; &#;</p>")

    [paragraph] = dom.children
    [text] = paragraph.children

    assert text.text == "€ � � � &#x; &#;"
  end

  test "parse accepts div containers for flex layouts" do
    assert {:ok, dom} =
             HtmlParser.parse(~s(<div class="row"><div>A</div><span>B</span></div>))

    [container] = dom.children
    [first, second] = container.children

    assert container.tag == "div"
    assert container.attributes == %{"class" => "row"}
    assert first.tag == "div"
    assert second.tag == "span"
  end

  test "parse accepts semantic section and article block containers" do
    assert {:ok, dom} =
             HtmlParser.parse(~s(<section class="sheet"><article><p>Trim</p></article></section>))

    [section] = dom.children
    [article] = section.children
    [paragraph] = article.children

    assert section.tag == "section"
    assert article.tag == "article"
    assert paragraph.tag == "p"
  end

  test "parse accepts strict image source attributes" do
    assert HtmlParser.parse(
             ~s(<img src="photo.png" alt="Product photo"><div><img src='nested.jpg'></div>)
           ) ==
             {:ok,
              %{
                type: :document,
                children: [
                  %{
                    type: :element,
                    tag: "img",
                    attributes: %{"alt" => "Product photo", "src" => "photo.png"},
                    children: []
                  },
                  %{
                    type: :element,
                    tag: "div",
                    attributes: %{},
                    children: [
                      %{
                        type: :element,
                        tag: "img",
                        attributes: %{"src" => "nested.jpg"},
                        children: []
                      }
                    ]
                  }
                ]
              }}
  end

  test "parse accepts strict lists and link href attributes" do
    assert HtmlParser.parse(
             ~s(<ul><li>Read <a href="https://example.com">docs</a></li><li>Ship</li></ul><ol><li>First</li></ol>)
           ) ==
             {:ok,
              %{
                type: :document,
                children: [
                  %{
                    type: :element,
                    tag: "ul",
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
                  },
                  %{
                    type: :element,
                    tag: "ol",
                    attributes: %{},
                    children: [
                      %{
                        type: :element,
                        tag: "li",
                        attributes: %{},
                        children: [%{type: :text, text: "First"}]
                      }
                    ]
                  }
                ]
              }}
  end

  test "parse accepts strict table markup" do
    assert HtmlParser.parse(
             ~s(<table><caption>Totals</caption><thead><tr><th>Name</th><th>Count</th></tr></thead><tbody><tr><td>Alpha</td><td><a href="https://example.com">2</a></td></tr></tbody><tfoot><tr><td>Total</td><td>2</td></tr></tfoot></table>)
           ) ==
             {:ok,
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
                        tag: "caption",
                        attributes: %{},
                        children: [%{type: :text, text: "Totals"}]
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
                                children: [
                                  %{
                                    type: :element,
                                    tag: "a",
                                    attributes: %{"href" => "https://example.com"},
                                    children: [%{type: :text, text: "2"}]
                                  }
                                ]
                              }
                            ]
                          }
                        ]
                      },
                      %{
                        type: :element,
                        tag: "tfoot",
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
                                children: [%{type: :text, text: "Total"}]
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
              }}
  end

  test "parse accepts cell spans and block table children inside cells" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<table><tr><td colspan="2" rowspan="3"><p>Address</p><table><tr><td>Nested</td></tr></table></td></tr></table>)
             )

    [table] = dom.children
    [row] = table.children
    [cell] = row.children
    [paragraph, nested_table] = cell.children

    assert cell.attributes == %{"colspan" => "2", "rowspan" => "3"}
    assert paragraph.tag == "p"
    assert nested_table.tag == "table"
  end

  test "parse accepts a table without a caption" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<table><thead><tr><th>Name</th></tr></thead><tbody><tr><td>Alpha</td></tr></tbody></table>)
             )

    [table] = dom.children
    [head, body] = table.children

    assert head.tag == "thead"
    assert body.tag == "tbody"
  end

  test "parse accepts standard scope values only on table headers" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <table>
               <tr>
                 <th scope="col">Column</th>
                 <th scope="row">Row</th>
                 <th scope="rowgroup">Row group</th>
                 <th scope="colgroup">Column group</th>
               </tr>
             </table>
             """)

    [table] = dom.children
    [row] = table.children

    assert Enum.map(row.children, & &1.attributes["scope"]) ==
             ~w(col row rowgroup colgroup)

    assert HtmlParser.parse("<table><tr><th scope=\"column\">Bad</th></tr></table>") ==
             {:error, :unsupported_html}

    assert HtmlParser.parse("<table><tr><td scope=\"col\">Bad</td></tr></table>") ==
             {:error, :unsupported_html}
  end

  test "parse accepts column groups, column spans, and strict table ordering" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <table>
               <caption>Inventory</caption>
               <colgroup><col span="2" /><col /></colgroup>
               <colgroup span="2"></colgroup>
               <tr><td>A</td><td>B</td><td>C</td><td>D</td><td>E</td></tr>
             </table>
             """)

    [table] = dom.children
    [caption, first_group, second_group, row] = table.children
    [spanned_column, column] = first_group.children

    assert caption.tag == "caption"
    assert spanned_column.attributes == %{"span" => "2"}
    assert column.attributes == %{}
    assert second_group.attributes == %{"span" => "2"}
    assert second_group.children == []
    assert row.tag == "tr"

    invalid_tables = [
      "<table><col /><tr><td>A</td></tr></table>",
      "<table><tr><td>A</td></tr><colgroup><col /></colgroup></table>",
      "<table><colgroup></colgroup><tr><td>A</td></tr></table>",
      "<table><colgroup span=\"2\"><col /></colgroup><tr><td>A</td></tr></table>"
    ]

    Enum.each(invalid_tables, fn html ->
      assert HtmlParser.parse(html) == {:error, :unsupported_html}
    end)
  end

  test "parse rejects unsupported markup" do
    assert HtmlParser.parse("") == {:error, :unsupported_html}
    assert HtmlParser.parse("<>") == {:error, :unsupported_html}
    assert HtmlParser.parse("<p>Hello") == {:error, :unsupported_html}
    assert HtmlParser.parse("<style>") == {:error, :unsupported_html}
    assert HtmlParser.parse("<style>missing closing tag") == {:error, :unsupported_html}
    assert HtmlParser.parse("<script></script>") == {:error, :unsupported_html}
    assert HtmlParser.parse("<p>Hello</script></p>") == {:error, :unsupported_html}
    assert HtmlParser.parse("<p>Hello</>") == {:error, :unsupported_html}
    assert HtmlParser.parse("<p style=color:red>Hello</p>") == {:error, :unsupported_html}
    assert HtmlParser.parse(~s(<p style="a" style="b">Hello</p>)) == {:error, :unsupported_html}
    assert HtmlParser.parse(~S|<p onclick="run()">Hello</p>|) == {:error, :unsupported_html}

    assert HtmlParser.parse(~s(<a href="https://example.com">No block</a>)) ==
             {:error, :unsupported_html}

    assert HtmlParser.parse(~s(<ul><p>No item</p></ul>)) == {:error, :unsupported_html}
    assert HtmlParser.parse(~s(<table><td>No row</td></table>)) == {:error, :unsupported_html}
    assert HtmlParser.parse(~s(<head><p>No paragraph</p></head>)) == {:error, :unsupported_html}

    assert HtmlParser.parse(
             ~s(<table><caption>One</caption><caption>Two</caption><tr><td>Cell</td></tr></table>)
           ) == {:error, :unsupported_html}

    assert HtmlParser.parse(~s(<table><caption>Late</caption></table>)) ==
             {:error, :unsupported_html}

    assert HtmlParser.parse(~s(<table><tbody></tbody></table>)) == {:error, :unsupported_html}

    assert HtmlParser.parse(~s(<table><tbody><td>No row</td></tbody></table>)) ==
             {:error, :unsupported_html}

    assert HtmlParser.parse(~s(<table><tbody><tr></tr></tbody></table>)) ==
             {:error, :unsupported_html}

    assert HtmlParser.parse(
             ~s(<p><a href="https://example.com"><a href="https://nested.example">Nested</a></a></p>)
           ) ==
             {:error, :unsupported_html}

    assert HtmlParser.parse("<p>Hello</strong></p>") == {:error, :unsupported_html}
    assert HtmlParser.parse(:not_html) == {:error, :invalid_html}
  end

  test "parse_detailed returns source details for malformed tags" do
    assert {:error,
            {:unsupported_html,
             %{
               stage: :html,
               reason: :unsupported_html,
               line: 1,
               column: 1,
               source: "<p>Hello",
               message: ~s(line 1: HTML source "<p>Hello" is unsupported or malformed)
             }}} = HtmlParser.parse_detailed("<p>Hello")

    assert {:error,
            {:unsupported_html,
             %{
               stage: :html,
               reason: :unsupported_html,
               line: 1,
               column: 1,
               source: "<p Hello",
               message: ~s(line 1: HTML source "<p Hello" is unsupported or malformed)
             }}} = HtmlParser.parse_detailed("<p Hello")
  end
end
