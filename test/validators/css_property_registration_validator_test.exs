defmodule NativeElixirPdfUtilities.Validators.CssPropertyRegistrationValidatorTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Validators.CssPropertyRegistrationValidator, as: Registration
  alias NativeElixirPdfUtilities.TestSupport.CssStyles

  test "invalid descriptor syntax and non-independent initial values ignore the complete registration" do
    for {name, descriptors} <- [
          {"value", "syntax:'<length>';inherits:false;initial-value:1pt"},
          {"--value", "syntax:<length>;inherits:false;initial-value:1pt"},
          {"--value", "syntax:'<unknown>';inherits:false;initial-value:1pt"},
          {"--value", "syntax:'<transform-list>+';inherits:false;initial-value:translateX(1pt)"},
          {"--value", "syntax:'initial';inherits:false;initial-value:initial"},
          {"--value", "syntax:'1bad';inherits:false;initial-value:1bad"},
          {"--value", "syntax:'<length>';inherits:maybe;initial-value:1pt"},
          {"--value", "syntax:'<length>';inherits:true false;initial-value:1pt"},
          {"--value", "syntax:'<length>';inherits:false"},
          {"--value", "syntax:'<length>';inherits:false;initial-value:2em"},
          {"--value", "syntax:'<color>';inherits:false;initial-value:currentColor"}
        ] do
      assert {:ok, %{}} = Registration.prepare([%{name: name, declarations: descriptors}])
    end

    assert {:ok, %{"--value" => %{initial: "1.3333333333333333px"}}} =
             Registration.prepare([
               %{
                 name: "--value",
                 declarations:
                   "unknown:ignored;syntax:'<length>';inherits:false;initial-value:1pt"
               }
             ])
  end

  test "universal syntax retains an authored token sequence without typed conversion" do
    context = %{font_size: 12.0, color: {0, 0, 0}}

    assert {:ok, "{ color: red; }"} =
             Registration.compute(%{syntax: :any}, "{ color: red; }", context)
  end

  test "registered scalar image and transform types reject mismatches and preserve token values" do
    context = %{font_size: 12.0, _root_font_size: 12.0, _root_line_height: 14.4, color: {0, 0, 1}}

    for {type, source, expected} <- [
          {"length", "0", "0px"},
          {"color", "transparent", "rgba(0,0,0,0)"},
          {"color", "currentColor", "rgba(0,0,255,1)"},
          {"url", "url('image.png')", "url('image.png')"},
          {"image", "url(image.png)", "url(image.png)"},
          {"transform-function", "translateX(3pt)", "translateX(4.0px)"},
          {"transform-list", "translateX(3pt) rotate(1rad)", "translateX(4.0px) rotate(1rad)"},
          {"transform-list", "translateX(3pt)/*length*/rotate(1rad)",
           "translateX(4.0px)/*length*/rotate(1rad)"},
          {"length-percentage", "2pt", "2.6666666666666665px"},
          {"length-percentage", "25%", "25.0%"},
          {"length-percentage", "0", "0px"},
          {"length-percentage", "calc(2pt + 25%)", "calc(2.6666666666666665px + 25%)"}
        ] do
      registration = %{syntax: [{:type, type, nil}]}
      assert {:ok, ^expected} = Registration.compute(registration, source, context)
    end

    for {type, source} <- [
          {"number", "1pt"},
          {"integer", "1.5"},
          {"custom-ident", "initial"},
          {"custom-ident", "two tokens"},
          {"image", "red"},
          {"transform-function", "translateX(1pt) rotate(0rad)"},
          {"transform-function", "unknown(1pt)"},
          {"length-percentage", "red"}
        ] do
      assert :error = Registration.compute(%{syntax: [{:type, type, nil}]}, source, context)
    end

    assert :error = Registration.compute(%{syntax: [{:literal, "auto", nil}]}, "none", context)

    assert {:error, {:not_implemented, diagnostic}} =
             Registration.prepare([
               %{
                 name: "--image",
                 declarations:
                   "syntax:'<image>';inherits:false;initial-value:linear-gradient(red,blue)"
               }
             ])

    assert diagnostic.stage == :css and diagnostic.source == "linear-gradient(red,blue)"

    assert {:error, {:not_implemented, diagnostic}} =
             Registration.compute(%{syntax: [{:type, "length", nil}]}, "2ch", context)

    assert diagnostic.message =~ "selected font metrics"
  end

  test "viewport initial values wait for the actual viewport and source contexts cannot invent one" do
    css = "@property --size{syntax:'<length>';inherits:false;initial-value:10vw}"
    nodes = CssStyles.by_id("<p id='a' style='width:var(--size)'>Text</p>", [css])
    assert_in_delta nodes["a"].width, nodes["a"]._position_viewport.width * 0.075, 0.000001

    assert {:ok, registrations} =
             Registration.prepare([
               %{
                 name: "--size",
                 declarations: "syntax:'<length>';inherits:false;initial-value:10vw"
               }
             ])

    assert registrations["--size"].initial_tokens == "10vw"

    assert {:error, {:not_implemented, diagnostic}} =
             Registration.baseline(%{
               _property_registrations: registrations,
               _custom_properties: %{}
             })

    assert diagnostic.message =~ "original viewport"
  end

  test "registered CSS-wide values respect inheritance and literal alternatives do not create color cycles" do
    nodes =
      CssStyles.by_id(
        """
        <div style='--length:9pt;--tone:blue'>
          <p id='inherit' style='--length:inherit;--tone:inherit;width:var(--length);color:var(--tone)'>Text</p>
          <p id='unset' style='--length:unset;--tone:initial;width:var(--length);color:var(--tone)'>Text</p>
          <p id='literal' style='--literal:currentColor;color:var(--literal)'>Text</p>
        </div>
        """,
        [
          """
          @property --length{syntax:'<length>';inherits:false;initial-value:3pt}
          @property --tone{syntax:'<color>';inherits:true;initial-value:red}
          @property --literal{syntax:'currentColor | <color>';inherits:false;initial-value:blue}
          """
        ]
      )

    assert nodes["inherit"].width == 9.0 and nodes["inherit"].color == {0, 0, 1}
    assert nodes["unset"].width == 3.0 and nodes["unset"].color == {1, 0, 0}
    assert nodes["literal"]._custom_properties["--literal"] == "currentColor"
    assert nodes["literal"].color == {0, 0, 0}
  end

  test "identifier types preserve currentColor as tokens rather than adding a color dependency" do
    assert {:ok, %{"--list" => %{initial: "currentColor currentColor"}}} =
             Registration.prepare([
               %{
                 name: "--list",
                 declarations:
                   "syntax:'<custom-ident>+';inherits:false;initial-value:currentColor currentColor"
               }
             ])

    nodes =
      CssStyles.by_id("<p id='a' style='--ident:currentColor;color:var(--ident)'>Text</p>", [
        "@property --ident{syntax:'<custom-ident> | <color>';inherits:false;initial-value:blue}"
      ])

    assert nodes["a"]._custom_properties["--ident"] == "currentColor"
    assert nodes["a"].color == {0, 0, 0}
  end
end
