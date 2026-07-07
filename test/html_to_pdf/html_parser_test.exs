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

  test "parse rejects unsupported markup" do
    assert HtmlParser.parse("<div>Hello</div>") == {:error, :unsupported_html}
    assert HtmlParser.parse(~s(<p class="copy">Hello</p>)) == {:error, :unsupported_html}

    assert HtmlParser.parse(~s(<a href="https://example.com">No block</a>)) ==
             {:error, :unsupported_html}

    assert HtmlParser.parse(~s(<ul><p>No item</p></ul>)) == {:error, :unsupported_html}
    assert HtmlParser.parse(~s(<table><td>No row</td></table>)) == {:error, :unsupported_html}

    assert HtmlParser.parse(
             ~s(<table><caption>One</caption><caption>Two</caption><tr><td>Cell</td></tr></table>)
           ) == {:error, :unsupported_html}

    assert HtmlParser.parse(~s(<table><tbody></tbody></table>)) == {:error, :unsupported_html}

    assert HtmlParser.parse(
             ~s(<p><a href="https://example.com"><a href="https://nested.example">Nested</a></a></p>)
           ) ==
             {:error, :unsupported_html}

    assert HtmlParser.parse("<p>Hello</strong></p>") == {:error, :unsupported_html}
    assert HtmlParser.parse(:not_html) == {:error, :invalid_html}
  end
end
