Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("font_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.FontControlAdmissionTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, FontFallback, HtmlParser, Style, CssParser}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.Selection
  alias NativeElixirPdfUtilities.TestSupport.{Paths, FontFixture}

  alias NativeElixirPdfUtilities.Validators.{
    CssFontRuleValidator,
    CssFontFaceValidator,
    CssValueValidator,
    CssCascadeValidator,
    FontRenderingValidator,
    WriterValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "font-face auto instances, metric descriptors and malformed controls use one grammar" do
    for declaration <- [
          "font-named-instance:auto",
          "size-adjust:125%;ascent-override:80%;descent-override:20%;line-gap-override:0%"
        ] do
      assert {:ok, %{font_faces: [_]}} =
               CssParser.prepare_stylesheet(
                 "@font-face{font-family:F;src:url(f.ttf);#{declaration}}"
               )
    end

    for declaration <- ["font-named-instance:Bad", "font-feature-settings:bad", "size-adjust:-1%"] do
      assert {:error, _} =
               CssParser.prepare_stylesheet(
                 "@font-face{font-family:F;src:url(f.ttf);#{declaration}}"
               )
    end

    assert {:error, :invalid_document} =
             CssValueValidator.validate_resolved(
               CssValueValidator.policy_style(),
               "font-kerning",
               "invalid"
             )

    assert {:ok, %{reference_policy: :unresolved_syntax}} =
             CssFontFaceValidator.validate_source_candidate("")

    data = File.read!(Paths.fixture("fonts/selected-control-axes.ttf"))

    assert {:ok, [font]} =
             Font.load_configured_faces([%{family: "Axes", data: data, css_face: true}])

    assert {:ok, selected} =
             FontRenderingValidator.prepare(font, %{
               font_controls: %{:"variant-caps" => ["small-caps"]}
             })

    assert {:ok, selected} = FontRenderingValidator.prepare_grapheme(selected, "H")
    assert Font.line_size(Map.put(selected, :metric_scale, 2), 20) == 40
    italic = %{weight: 400, style: :normal, variation_italic?: true}
    slant = %{weight: 400, style: :normal, variation_oblique: {-20, 0}}

    assert Selection.rank(italic, 400, :italic) <
             Selection.rank(%{italic | variation_italic?: false}, 400, :italic)

    assert Selection.rank(slant, 400, :italic, %{oblique: -10}) <
             Selection.rank(%{weight: 400, style: :normal}, 400, :italic, %{oblique: -10})

    assert {:ok, %{cascade: %{font_definitions: definitions}}} =
             CssParser.prepare_source_stylesheet(
               "@media print{@font-feature-values F{@styleset{Pretty:1}}}",
               nil,
               %{type: "print", width: 800, height: 600, resolution: 96}
             )

    assert definitions.features["f"]["styleset"]["Pretty"] == [1]

    assert {:error, {:invalid_pdf_input, _}} =
             WriterValidator.prepare_color_boxes([%{type: :image}], 100)
  end

  test "invalid definitions recover locally and revert-layer preserves declarations from another tree scope" do
    definitions =
      CssFontRuleValidator.prepare([
        %{
          kind: "font-palette-values",
          name: "--Bad",
          declarations:
            "broken; font-family:F; base-palette:99999999999999999999999999999999999999999999;"
        }
      ])

    assert definitions == %{features: %{}, palettes: %{}}

    planned =
      CssCascadeValidator.plan(
        [
          %{declaration: {"font-family", "Outer"}, definition_scope: :outer},
          %{declaration: {"font-family", "Inner"}, definition_scope: :inner},
          %{declaration: {"font-family", "revert-layer"}, definition_scope: :inner}
        ],
        %{}
      )

    assert hd(planned).cascade_targets == ["font-family"]
    assert Enum.all?(tl(planned), &(&1.cascade_targets == []))
  end

  test "fallback candidates and active feature execution return structured public failures" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<p>H</p>")
    assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
    [paragraph] = tree.children
    [text] = paragraph.children
    font = text.style.font_face
    broken = %{font | data: FontFixture.rebuild(font.data, %{"GSUB" => <<>>})}

    style =
      %{text.style | font_face: broken}
      |> Map.put(:font_controls, %{:"variant-caps" => ["petite-caps"]})

    tree = %{tree | children: [%{paragraph | children: [%{text | style: style}]}]}
    assert {:error, {:invalid_font, %{message: message}}} = FontFallback.resolve(tree)
    assert message =~ "feature"
    data = File.read!(Paths.fixture("fonts/selected-colr.ttf"))

    assert {:error, {:unsupported_font, diagnostic}} =
             HtmlToPdf.render("<p style='font-family:Color;font-variant-emoji:text'>0</p>",
               fonts: [%{family: "Color", data: data}],
               system_font_discovery: false
             )

    assert diagnostic.message =~ "text emoji presentation"
  end

  test "fractional strike advances share CSS rounding and malformed color sources fail during measurement" do
    data = File.read!(Paths.fixture("fonts/selected-cbdt.ttf"))
    {_, tables} = FontFixture.tables(data)
    # 21 ppem yields fractional 10 * 1000 / 21 design-unit advances.
    cblc = FontFixture.replace(tables["CBLC"], 52, <<21, 21>>)
    data = FontFixture.rebuild(data, %{"CBLC" => cblc})
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Bitmap", data: data}])
    assert is_number(Font.css_text_width("H", font, 15))
    broken = %{font | data: FontFixture.rebuild(data, %{"CBLC" => <<>>})}
    assert {:error, {:invalid_font, %{operation: :text_width}}} = Font.text_width("H", broken, 15)
  end
end
