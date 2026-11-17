defmodule NativeElixirPdfUtilities.HtmlToPdf.CssIndividualTransformTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.CssTransformValidator

  setup do
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    {:ok,
     style: %{
       font_face: font,
       font_size: 12.0,
       line_height: 15.0,
       _root_font_face: font,
       _root_font_size: 12.0,
       _root_line_height: 15.0
     }}
  end

  test "individual translation, rotation and scale precede the transform list", %{style: style} do
    assert {:ok, style} = CssTransformValidator.individual(style, "scale", "2 3")
    assert {:ok, style} = CssTransformValidator.individual(style, "rotate", "90deg")
    assert {:ok, style} = CssTransformValidator.individual(style, "translate", "10px 20px")
    assert {:ok, style} = CssTransformValidator.apply(style, "transform", "translateX(4px)")
    assert {:ok, style} = CssTransformValidator.apply(style, "transform-origin", "0 0")

    assert {:ok, {a, b, c, d, e, f}} =
             CssTransformValidator.resolve(style, %{x: 0, top: 0, width: 100, height: 60})

    assert_in_delta a, 0, 0.00001
    assert_in_delta b, -2, 0.00001
    assert_in_delta c, 3, 0.00001
    assert_in_delta d, 0, 0.00001
    assert_in_delta e, 7.5, 0.00001
    assert_in_delta f, -21, 0.00001
    assert CssTransformValidator.valid_style?(style)
  end

  test "transform boxes determine percentages and origins from physical content geometry", %{
    style: style
  } do
    style =
      Map.merge(style, %{
        border_widths: %{top: 2, right: 2, bottom: 2, left: 2},
        padding: %{top: 3, right: 3, bottom: 3, left: 3}
      })

    assert {:ok, style} = CssTransformValidator.individual(style, "transform-box", "content-box")
    assert {:ok, style} = CssTransformValidator.individual(style, "translate", "100% 100%")
    assert {:ok, style} = CssTransformValidator.apply(style, "transform-origin", "0 0")

    assert {:ok, {_, _, _, _, 90.0, -50.0}} =
             CssTransformValidator.resolve(style, %{x: 0, top: 100, width: 100, height: 60})

    for {property, value} <- [
          {"translate", "1"},
          {"rotate", "1px"},
          {"scale", "1 2 3 4"},
          {"transform-box", "padding-box"}
        ] do
      assert :error = CssTransformValidator.individual(style, property, value)
    end
  end

  test "individual transforms are noninherited while explicit inheritance retains declaring font metrics" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style='font-size:10pt;translate:1em 0'><p>Plain</p><p style='font-size:30pt;translate:inherit'>Inherited</p></div>"
             )

    assert {:ok, %{children: [parent]}} =
             Style.compute_detailed(dom, system_font_discovery: false)

    [plain, inherited] = parent.children
    assert Map.get(plain.style, :css_translate, :none) == :none

    assert {:ok, {_, _, _, _, x, _}} =
             CssTransformValidator.resolve(inherited.style, %{
               x: 0,
               top: 0,
               width: 100,
               height: 60
             })

    assert_in_delta x, 10, 0.00001
  end
end
