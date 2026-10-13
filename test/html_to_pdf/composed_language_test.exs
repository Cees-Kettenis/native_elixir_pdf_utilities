defmodule NativeElixirPdfUtilities.HtmlToPdf.ComposedLanguageTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.LanguageValidator

  test "assigned language uses the logical host while shadow descendants inherit their host" do
    html =
      "<style>span:lang(en){color:red}span:lang(fr){color:blue}</style><div lang=en><template shadowrootmode=open><style>span:lang(en){color:red}span:lang(fr){color:blue}</style><span>Shadow host language</span><slot lang=fr></slot><slot name=empty lang=fr><span>Fallback French</span></slot></template><span>Assigned English</span></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom)
    [host] = tree.children
    [shadow, assigned, fallback] = host.children
    assert shadow.style.color == {1, 0, 0}
    assert assigned.style.color == {1, 0, 0}
    assert fallback.style.color == {0, 0, 1}
  end

  test "document language pragmas exclude shadow metadata" do
    html =
      "<head><meta http-equiv=content-language content=en><style>p:lang(en){color:red}p:lang(fr){color:blue}</style></head><body><div><template shadowrootmode=open><meta http-equiv=content-language content=fr><p>Shadow</p></template></div><p>Document</p></body>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom)
    assert List.last(hd(tree.children).children).style.color == {1, 0, 0}
  end

  test "foreign and qualified language attributes follow namespace applicability" do
    for namespace <- [:html, :svg] do
      assert LanguageValidator.language_attribute(%{
               namespace: namespace,
               attributes: %{"lang" => "fr"}
             }) == {:ok, "fr"}
    end

    assert LanguageValidator.language_attribute(%{
             namespace: :mathml,
             attributes: %{"lang" => "fr"}
           }) == :inherit

    assert LanguageValidator.language_attribute(%{
             attributes: %{"lang" => "fr"},
             attribute_namespaces: %{"lang" => "urn:other"}
           }) == :inherit

    assert LanguageValidator.language_attribute(%{
             namespace: :mathml,
             attributes: %{"xml:lang" => "fr"},
             attribute_namespaces: %{"xml:lang" => "http://www.w3.org/XML/1998/namespace"}
           }) == {:ok, "fr"}

    qualified = %{
      type: :element,
      tag: "meta",
      attributes: %{"http-equiv" => "content-language", "content" => "fr"},
      attribute_namespaces: %{"http-equiv" => "urn:other"},
      children: []
    }

    assert LanguageValidator.document_default(%{children: [qualified]}) == ""
  end

  test "selected-content clones inherit language at their actual destination" do
    html =
      "<style>select::picker-icon{display:none}span:lang(en){color:red}span:lang(fr){color:blue}</style><select lang=en style='appearance:base-select'><button lang=fr><selectedcontent></selectedcontent></button><option selected><span>Clone</span></option></select>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom, forms: :static)
    [select] = tree.children
    [button] = select.children
    [clone] = button.children
    assert clone.style.color == {0, 0, 1}
    assert clone.attributes == %{}
    assert is_reference(clone.cloned_from_node_id)
  end
end
