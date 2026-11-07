defmodule NativeElixirPdfUtilities.Validators.HtmlSelectorStateValidatorTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Validators.{HtmlSelectorStateValidator, CssSelectorValidator}
  alias NativeElixirPdfUtilities.HtmlToPdf.CssSelector

  test "directionality resolves inherited auto text and text control values independently of CSS direction" do
    parent = element("div", %{"dir" => "RTL"})
    assert HtmlSelectorStateValidator.direction(element("p"), [parent]) == "rtl"

    assert HtmlSelectorStateValidator.direction(element("p", %{"dir" => "ltr"}), [parent]) ==
             "ltr"

    assert HtmlSelectorStateValidator.direction(element("input", %{"type" => "tel"}), [parent]) ==
             "ltr"

    for {text, expected} <- [
          {"... abc", "ltr"},
          {"... אבג", "rtl"},
          {"... العربية", "rtl"},
          {"123", "rtl"}
        ] do
      auto = %{
        element("div", %{"dir" => "auto"})
        | children: [element("span", %{"dir" => "ltr"}), %{type: :text, text: text}]
      }

      assert HtmlSelectorStateValidator.direction(auto, [parent]) == expected
    end

    node = %{
      element("bdi")
      | children: [
          element("span", %{"dir" => "invalid"}, [%{type: :text, text: "abc"}]),
          %{type: :comment, text: "ignored"}
        ]
    }

    assert HtmlSelectorStateValidator.direction(node, [parent]) == "ltr"

    input =
      Map.put(element("input", %{"dir" => "auto", "value" => "אבג"}), :_initial_form_state, %{
        value: "abc"
      })

    assert HtmlSelectorStateValidator.direction(input, []) == "ltr"

    assert HtmlSelectorStateValidator.direction(
             element("textarea", %{"dir" => "auto", "value" => "אבג"}),
             []
           ) == "rtl"

    assert HtmlSelectorStateValidator.direction(element("p", %{"dir" => "auto"}), []) == "ltr"

    for {argument, expected} <- [{"RTL", true}, {"rtl", true}, {"unknown", false}] do
      assert {:ok, [selector]} = CssSelectorValidator.parse(":dir(" <> argument <> ")")
      assert CssSelector.matches?(Enum.reverse(selector.parts), parent, [], %{}) == expected
    end
  end

  test "links and default choices require their owning elements" do
    for tag <- ["a", "area"] do
      assert HtmlSelectorStateValidator.matches?(:link, element(tag, %{"href" => ""}), [], %{})
      refute HtmlSelectorStateValidator.matches?(:link, element(tag), [], %{})
    end

    assert HtmlSelectorStateValidator.matches?(
             :any_link,
             Map.put(element("a", %{"xlink:href" => "#x"}), :namespace, :svg),
             [],
             %{}
           )

    refute HtmlSelectorStateValidator.matches?(
             :any_link,
             element("div", %{"href" => "#x"}),
             [],
             %{}
           )

    assert HtmlSelectorStateValidator.matches?(
             :local_link,
             element("a", %{"href" => "#x"}),
             [],
             %{}
           )

    refute HtmlSelectorStateValidator.matches?(
             :local_link,
             element("a", %{"href" => "https://other.example/"}),
             [],
             %{}
           )

    for tag <- ["details", "dialog"] do
      assert HtmlSelectorStateValidator.matches?(:open, element(tag, %{"open" => ""}), [], %{})
      refute HtmlSelectorStateValidator.matches?(:open, element(tag), [], %{})
    end

    for {tag, attrs} <- [
          {"option", %{"selected" => ""}},
          {"input", %{"type" => "radio", "checked" => ""}},
          {"input", %{"type" => "checkbox", "checked" => ""}}
        ] do
      assert HtmlSelectorStateValidator.matches?(:default, element(tag, attrs), [], %{})
    end

    refute HtmlSelectorStateValidator.matches?(
             :default,
             element("p", %{"checked" => ""}),
             [],
             %{}
           )

    refute HtmlSelectorStateValidator.matches?(:default, element("input"), [], %{})
    refute HtmlSelectorStateValidator.matches?(:defined, element("x-unregistered"), [], %{})

    refute HtmlSelectorStateValidator.matches?(
             :defined,
             element("button", %{"is" => "x-unregistered"}),
             [],
             %{}
           )

    assert HtmlSelectorStateValidator.matches?(:defined, element("p"), [], %{})

    assert HtmlSelectorStateValidator.matches?(
             :defined,
             Map.put(element("font-face"), :namespace, :svg),
             [],
             %{}
           )

    for state <- [
          :visited,
          :hover,
          :focus,
          :active,
          :user_invalid,
          :user_valid,
          :modal,
          :autofill
        ],
        do: refute(HtmlSelectorStateValidator.matches?(state, element("input"), [], %{}))
  end

  defp element(tag, attrs \\ %{}, children \\ []) do
    %{type: :element, tag: tag, attributes: attrs, children: children, _selector_id: make_ref()}
  end
end
