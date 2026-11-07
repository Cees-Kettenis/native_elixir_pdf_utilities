defmodule NativeElixirPdfUtilities.Validators.CssPseudoValidatorTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Validators.{CssPseudoValidator, CssSelectorValidator}
  alias NativeElixirPdfUtilities.HtmlToPdf.CssSelector

  test "static control parts select their originating controls and retain separate gated identities" do
    for {selector, tag, attributes, state} <- [
          {"::marker", "li", %{}, %{}},
          {"::placeholder", "input", %{"placeholder" => "Hint"}, %{placeholder_shown: true}},
          {"::placeholder", "textarea", %{}, %{placeholder_shown: true}},
          {"::details-content", "details", %{}, %{}},
          {"::picker(select)", "select", %{}, %{}},
          {"::checkmark", "option", %{}, %{}},
          {"::file-selector-button", "input", %{"type" => "file"}, %{}},
          {"::color-swatch", "input", %{"type" => "color"}, %{}},
          {"::slider-thumb", "input", %{"type" => "range"}, %{}},
          {"::fill", "progress", %{}, %{}},
          {"::thumb", "input", %{"type" => "range"}, %{}},
          {"::step-up", "input", %{"type" => "number"}, %{}},
          {"::field-text", "input", %{}, %{}},
          {"::field-content", "textarea", %{}, %{}},
          {"::field-component", "input", %{"type" => "date"}, %{}},
          {"::checkmark", "input", %{"type" => "checkbox"}, %{}},
          {"::field-content", "input", %{"type" => "time"}, %{}}
        ] do
      node = %{
        type: :element,
        tag: tag,
        attributes: attributes,
        children: [],
        _selector_id: make_ref(),
        _initial_form_state: state
      }

      assert {:ok, selectors} = CssSelectorValidator.parse(selector)
      assert [owner] = CssSelector.pseudo_owners(selectors, node, [], %{})
      assert owner.owner == {node._selector_id, owner.pseudo}
      refute owner.paint_ready

      assert {:error, {:not_implemented, diagnostic}} =
               CssPseudoValidator.validate_paint([owner], node)

      assert diagnostic.stage == :css and
               diagnostic.source == selector |> String.split("(") |> hd()

      other = %{node | tag: "p", attributes: %{}, _initial_form_state: %{}}
      assert CssSelector.pseudo_owners(selectors, other, [], %{}) == []
    end

    node = %{tag: "div", attributes: %{}, _selector_id: make_ref()}
    assert CssPseudoValidator.owner?(:marker, node, %{display: :list_item})
    refute CssPseudoValidator.owner?(:backdrop, node, %{})
    assert CssPseudoValidator.paint_ready?(:before)
    assert CssPseudoValidator.paint_ready?(:after)
    assert CssPseudoValidator.paint_ready?(:picker_icon)
  end

  test "pseudo-element functions reject invalid arguments and trailing compounds" do
    for selector <- [
          "::picker()",
          "::picker(input)",
          "::picker(select)more",
          "::nth-fragment()",
          "::nth-fragment(1n+bad)",
          "::unknown",
          "::marker(extra)",
          "::prefix",
          "::suffix",
          "::first-letter::unknown",
          "::first-letter::."
        ] do
      assert {:error, :invalid_css} = CssSelectorValidator.parse(selector)
    end

    assert {:ok, selectors} = CssSelectorValidator.parse("p::before")
    node = %{tag: "p", attributes: %{}, _selector_id: make_ref()}
    assert [owner] = CssSelector.pseudo_owners(selectors, node, [], %{})
    assert owner.paint_ready
    assert :ok = CssPseudoValidator.validate_paint([owner], node)
  end

  test "first-letter prefix and suffix retain nested ownership and control parts reject inapplicable inputs" do
    node = %{tag: "p", attributes: %{}, _selector_id: make_ref()}

    for child <- [:prefix, :suffix] do
      selector = "p::first-letter::#{child}"
      assert {:ok, [%{specificity: {0, 0, 3}}] = selectors} = CssSelectorValidator.parse(selector)

      assert [%{pseudo: {:first_letter, ^child}} = owner] =
               CssSelector.pseudo_owners(selectors, node, [], %{})

      assert owner.owner == {node._selector_id, {:first_letter, child}}

      assert {:error, {:not_implemented, diagnostic}} =
               CssPseudoValidator.validate_paint([owner], node)

      assert diagnostic.source == "::first-letter::#{child}"
    end

    checkbox = %{node | tag: "input", attributes: %{"type" => "checkbox"}}
    refute CssPseudoValidator.owner?(:field_text, checkbox, %{})
    refute CssPseudoValidator.owner?(:field_component, checkbox, %{})
    refute CssPseudoValidator.owner?(:step_up, %{checkbox | attributes: %{"type" => "date"}}, %{})

    refute CssPseudoValidator.owner?(:step_up, %{checkbox | attributes: %{"type" => "number"}}, %{
             appearance: :textfield
           })

    assert CssPseudoValidator.owner?(
             :slider_thumb,
             %{checkbox | attributes: %{"type" => "checkbox", "switch" => ""}},
             %{}
           )
  end
end
