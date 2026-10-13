defmodule NativeElixirPdfUtilities.HtmlToPdf.InputApplicabilityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Forms}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlFormValidator, HtmlValidator}

  test "inactive input attributes retain source values without changing complete static or interactive PDFs" do
    cases = [
      {"text",
       "checked multiple min=9 max=1 step=bad alpha colorspace=display-p3 accept=none src='javascript:ignored' alt=Unused width=999 height=999",
       "Value"},
      {"email", "checked min=9 max=1 step=bad alpha colorspace=bad accept=none src='/never.png'",
       "a@example.test"},
      {"password", "checked multiple list=unused min=9 max=1 src='/never.png'", "Value"},
      {"number",
       "checked multiple size=999 pattern='[' minlength=999 maxlength=1 src='/never.png'", "2"},
      {"range",
       "checked multiple readonly required size=999 placeholder=Unused accept=none alpha colorspace=bad src='/never.png' width=999 height=999 pattern='['",
       "50"},
      {"checkbox",
       "readonly multiple size=999 placeholder=Unused min=9 max=1 step=bad pattern='[' src='/never.png'",
       "on"},
      {"radio",
       "readonly multiple size=999 placeholder=Unused min=9 max=1 step=bad pattern='[' src='/never.png'",
       "on"},
      {"button",
       "checked readonly multiple required size=999 placeholder=Unused list=unused min=9 max=1 src='/never.png'",
       "Button"},
      {"submit",
       "checked readonly multiple required size=999 placeholder=Unused list=unused min=9 max=1 src='/never.png'",
       "Submit"},
      {"hidden",
       "checked readonly multiple required size=999 placeholder=Unused list=unused min=9 max=1 src='/never.png'",
       "Hidden"}
    ]

    for {type, inactive, value} <- cases,
        mode <- if(type == "range", do: [:static], else: [:static, :interactive]) do
      opts = [
        forms: mode,
        asset_resolver: fn request -> flunk("Inactive source requested #{inspect(request)}") end
      ]

      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<input type=#{type} name=field value='#{value}' #{inactive}>",
                 opts
               )

      assert {:ok, reference} =
               HtmlToPdf.render("<input type=#{type} name=field value='#{value}'>", opts)

      assert pdf == reference, "#{type} #{mode}"
    end
  end

  test "native toggles use fixed intrinsic dimensions while authored dimensions win" do
    for type <- ["checkbox", "radio"], font_size <- [8, 24] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<input type=#{type} style='font-size:#{font_size}pt;width:auto;height:auto'>"
               )

      assert {:ok, %{children: [control]}} = Style.compute_detailed(dom)
      assert control.style.width == 9.75
      assert control.style.height == 9.75

      assert {:ok, dom} =
               HtmlParser.parse_detailed("<input type=#{type} style='width:12pt;height:18pt'>")

      assert {:ok, %{children: [control]}} = Style.compute_detailed(dom)
      assert control.style.width == 12.0
      assert control.style.height == 18.0
    end
  end

  test "canonical state ignores checked and readonly but raw values remain selector-visible" do
    html =
      "<style>input[checked][multiple]:not(:checked):read-write{color:blue}input:required{background:red}</style><input type=text name=plain checked multiple src='/never.png' value=Value><input type=checkbox name=toggle readonly required>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    nodes = HtmlFormValidator.prepare_initial_nodes(dom.children)
    [_style, plain, toggle] = nodes
    refute plain._initial_form_state.checked
    assert Map.has_key?(plain.attributes, "checked")
    assert Map.has_key?(plain.attributes, "multiple")
    refute toggle._initial_form_state.readonly
    assert toggle._initial_form_state.required
    assert toggle._initial_form_state.constraint_validation.will_validate
    assert toggle._initial_form_state.constraint_validation.checks.value_missing
    assert {:ok, tree} = Style.compute_detailed(dom)
    assert hd(tree.children).style.color == {0, 0, 1}
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, fields} = Forms.fields(pdf)
    assert Enum.all?(fields, &(not &1.read_only))
  end

  test "effective applicability ignores qualified control attributes and foreign lookalikes" do
    node = %{
      type: :element,
      tag: "input",
      attributes: %{
        "type" => "image",
        "src" => "javascript:ignored",
        "value" => "Visible",
        "name" => "wrong",
        "readonly" => "",
        "required" => "",
        "checked" => ""
      },
      attribute_namespaces: %{
        "type" => "urn:other",
        "name" => "urn:other",
        "readonly" => "urn:other",
        "required" => "urn:other"
      },
      children: []
    }

    assert HtmlFormValidator.initial_control_attributes(node) == %{"value" => "Visible"}
    assert {:ok, tree} = Style.compute_detailed(%{type: :document, children: [node]})
    [input] = tree.children
    assert input.attributes == node.attributes
    assert input.style._form_control.attributes == %{"value" => "Visible"}
    assert input.style._form_control.initial_state.constraint_validation.will_validate

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<svg multiple readonly checked width=20 height=10><rect width=20 height=10 fill=red></rect></svg>"
             )

    assert {:ok, _} = Style.compute_detailed(dom)
  end

  test "special input applicability preserves only its actual state attributes" do
    for type <- ~w(color file image),
        attrs = %{
          "type" => type,
          "readonly" => "",
          "checked" => "",
          "size" => "999",
          "placeholder" => "Unused",
          "multiple" => "",
          "required" => "",
          "accept" => "image/png",
          "src" => "/never.png",
          "alt" => "Alternative",
          "width" => "12",
          "height" => "8",
          "alpha" => "",
          "colorspace" => "display-p3"
        } do
      node = %{type: :element, tag: "input", attributes: attrs, children: []}
      active = HtmlFormValidator.initial_control_attributes(node)
      assert active["type"] == type
      for key <- ~w(readonly checked size placeholder), do: refute(Map.has_key?(active, key))
      assert Map.has_key?(active, "required") == (type == "file")
      assert Map.has_key?(active, "multiple") == (type == "file")
      assert Map.has_key?(active, "accept") == (type == "file")
      assert Map.has_key?(active, "src") == (type == "image")
      assert Map.has_key?(active, "alpha") == (type == "color")
      assert node.attributes == attrs
    end
  end

  test "active image URLs, unsupported handlers and Range tick artwork still diagnose" do
    for html <- [
          "<input type=image src='javascript:active'>",
          "<input type=text onclick=active>",
          "<input type=range list=ticks>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, forms: :static)
      assert diagnostic.stage == :html
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.source != nil
      assert diagnostic.line == 1
      assert diagnostic.column >= 1
      assert diagnostic.message != ""
    end

    assert {:error, {:unsupported_form, diagnostic}} =
             HtmlToPdf.render("<input type=range readonly>")

    assert diagnostic.stage == :forms and diagnostic.message =~ "forms: :static"
    assert {:ok, _} = HtmlParser.parse_detailed("<button type=unknown>Default button</button>")
    assert HtmlValidator.valid_form_attribute?("button", "type", "SUBMIT ")
  end
end
