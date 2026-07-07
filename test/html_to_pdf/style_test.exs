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
  end
end
