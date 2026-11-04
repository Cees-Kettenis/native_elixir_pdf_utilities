defmodule NativeElixirPdfUtilities.Validators.HtmlMetadataTableLookupTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.HtmlMetadataValidator

  test "shared legacy lookup returns unchanged first elements without table or cell restrictions" do
    first = %{
      type: :element,
      tag: "p",
      attributes: %{"id" => "same", "hidden" => ""},
      children: []
    }

    later = %{type: :element, tag: "th", attributes: %{"id" => "same"}, children: []}

    foreign = %{
      type: :element,
      tag: "svg",
      namespace: :svg,
      attributes: %{"id" => "foreign"},
      children: []
    }

    text = %{type: :text, text: "Spacing"}

    assert HtmlMetadataValidator.first_tree_ids([text, first, later, foreign]) == %{
             {nil, nil, "same"} => first,
             {nil, nil, "foreign"} => foreign
           }
  end

  test "explicit logical scope excludes other light trees and keeps template elements without entering content" do
    cell = %{
      type: :element,
      tag: "th",
      attributes: %{"id" => "h"},
      children: [],
      document_id: 1,
      tree_scope_id: 2,
      node_id: 4,
      parent_id: 3
    }

    other = %{
      type: :element,
      tag: "div",
      attributes: %{"id" => "other"},
      children: [cell],
      document_id: 1,
      tree_scope_id: 1
    }

    template = %{
      type: :element,
      tag: "template",
      attributes: %{"id" => "template"},
      children: [],
      content: %{type: :document_fragment, children: [cell]},
      document_id: 1,
      tree_scope_id: 2
    }

    assert HtmlMetadataValidator.first_tree_ids([other, template, cell], {1, 2}) == %{
             {1, 2, "template"} => template,
             {1, 2, "h"} => cell
           }

    assert HtmlMetadataValidator.first_tree_ids([cell]) == %{{1, 2, "h"} => cell}
    assert HtmlMetadataValidator.first_tree_ids([other], {1, 2}) == %{}
  end

  test "empty and namespaced ID attributes do not block unnamespaced first IDs" do
    namespaced = %{
      type: :element,
      tag: "span",
      attributes: %{"id" => "h"},
      children: [],
      attribute_namespaces: %{"id" => "http://www.w3.org/XML/1998/namespace"}
    }

    empty = %{type: :element, tag: "p", attributes: %{"id" => ""}, children: []}
    real = %{type: :element, tag: "td", attributes: %{"id" => "h"}, children: []}

    assert HtmlMetadataValidator.first_tree_ids([namespaced, empty, real]) == %{
             {nil, nil, "h"} => real
           }
  end
end
