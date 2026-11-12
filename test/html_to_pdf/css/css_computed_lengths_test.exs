defmodule NativeElixirPdfUtilities.HtmlToPdf.CssComputedLengthsTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.TestSupport.CssStyles
  alias NativeElixirPdfUtilities.TestSupport.Paths
  alias NativeElixirPdfUtilities.Validators.{CssLengthValidator, CssMathValidator, HtmlValidator}

  # CSS Values 4, 12 March 2024, 6.1.1 and 6.1.2.
  test "the finite font unit matrix uses selected and root faces at their own scales" do
    styles =
      CssStyles.by_id(
        "<html style='font-family:DejaVu Sans;font-size:12pt;line-height:15pt'><body><div id='units' style='font-family:DejaVu Sans Mono;font-size:20pt;line-height:30pt'></div></body></html>"
      )

    style = styles["units"]

    assert {:ok, selected_bounds} = Font.glyph_bounds(style.font_face)
    assert {:ok, root_bounds} = Font.glyph_bounds(style._root_font_face)

    glyph_height = fn font, bounds, codepoint, size ->
      elem(elem(bounds, Map.fetch!(font.cmap, codepoint)), 3) * size / font.units_per_em
    end

    for {unit, expected} <- [
          {"em", 20},
          {"rem", 12},
          {"ex", glyph_height.(style.font_face, selected_bounds, ?x, 20)},
          {"rex", glyph_height.(style._root_font_face, root_bounds, ?x, 12)},
          {"ch", Font.text_width("0", style.font_face, 20)},
          {"rch", Font.text_width("0", style._root_font_face, 12)},
          {"cap", glyph_height.(style.font_face, selected_bounds, ?H, 20)},
          {"rcap", glyph_height.(style._root_font_face, root_bounds, ?H, 12)},
          {"ic", 20},
          {"ric", 12},
          {"lh", 30},
          {"rlh", 15}
        ] do
      assert {:ok, expression} = CssLengthValidator.prepare("calc(2#{unit})", :length)
      assert {:ok, actual} = CssLengthValidator.resolve(expression, 0, style)
      assert_in_delta actual, expected * 2, 1.0e-10, unit
    end

    # This genuine sparse font has none of the metric glyphs. CSS defines the
    # fallbacks as 0.5em for ex/ch, 1em for ic, and font ascent for cap.
    assert {:ok, registry} =
             Font.load_registry(
               fonts: [
                 %{
                   family: "Sparse",
                   path:
                     Paths.fixture("html_to_pdf/browser_parity/assets/math-glyph-reference.ttf")
                 }
               ],
               system_font_discovery: false
             )

    assert {:ok, _, sparse} = Font.resolve("Sparse", 400, :normal, registry)

    assert {:ok, metrics} =
             NativeElixirPdfUtilities.Validators.SourceSizeValidator.prepare_font_metrics(
               sparse,
               20,
               30
             )

    assert_in_delta metrics.ex, 20 / 0.75 / 2, 1.0e-10
    assert metrics.ch == metrics.ex
    assert_in_delta metrics.ic, 20 / 0.75, 1.0e-10
    assert_in_delta metrics.cap, sparse.ascent * 20 / 0.75 / sparse.units_per_em, 1.0e-10
    assert metrics.lh == 40

    assert {:ok, 20.0} =
             NativeElixirPdfUtilities.Validators.MathLayoutValidator.parse_font_size("2ex", %{
               font_face: sparse,
               font_size: 20,
               line_height: 30,
               _root_font_size: 12
             })
  end

  test "embedded documents retain their own viewport unit bases" do
    html =
      "<body style='margin:0'><iframe style='display:block;width:120pt;height:40pt;border:0' srcdoc=\"<body style='margin:0;overflow:hidden'><div style='margin-left:10vw'>ChildA</div></body>\"></iframe><iframe style='display:block;width:180pt;height:40pt;border:0' srcdoc=\"<body style='margin:0;overflow:hidden'><div style='margin-left:10dvw'>ChildB</div></body>\"></iframe></body>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               page_size: {300, 200},
               margin: 0,
               system_font_discovery: false
             )

    assert {:ok, %{pages: [page]}} = NativeElixirPdfUtilities.Text.extract_spans(pdf)
    [first, second] = Enum.filter(page.spans, &String.starts_with?(&1.text, "C"))
    assert_in_delta first.x, 12, 0.02
    assert_in_delta second.x, 18, 0.02
  end

  test "font-size uses inherited metrics and text lengths use the selected computed font" do
    styles =
      CssStyles.by_id(
        "<html style='font-size:12pt;line-height:15pt'><body><div style='font-size:20pt;line-height:30pt'><span id='text' style='font-family:DejaVu Sans Mono;font-size:calc(1em + 2pt);line-height:calc(1lh + 1em);letter-spacing:calc(1ch - 1pt)'>A</span><span id='inherited' style='font-size:1lh'>B</span><span id='percent' style='font-size:calc(50% + 2pt);line-height:calc(150% + 1pt)'>C</span></div></body></html>"
      )

    assert styles["text"].font_size == 22
    assert_in_delta styles["text"].line_height, 52, 1.0e-10

    assert_in_delta styles["text"].letter_spacing,
                    Font.text_width("0", styles["text"].font_face, 22) - 1,
                    1.0e-10

    assert styles["inherited"].font_size == 30
    assert styles["percent"].font_size == 12
    assert styles["percent"].line_height == 19
  end

  test "bare selected and root font units share the calculated dimension environment" do
    styles =
      CssStyles.by_id(
        "<html style='font-size:12pt;line-height:15pt'><body><div id='units' style='font-size:20pt;line-height:30pt;width:2ch;height:2rlh;padding:1cap;border:1ex solid red;margin:1ic'></div></body></html>"
      )

    style = styles["units"]

    assert_in_delta Layout.resolve_content_size(style, :width, 100, nil),
                    Font.text_width("00", style.font_face, 20),
                    1.0e-10

    assert Layout.resolve_content_size(style, :height, nil, nil) == 30
    assert style.margin.left == 20
    assert style.padding.left > 10
    assert style.border_widths.left >= 9
  end

  test "edge calculations retain real containing width through nested blocks and border box" do
    html =
      "<body style='margin:0'><div style='width:100pt'><div style='box-sizing:border-box;width:80pt;padding:calc(10% + 1pt);margin-left:calc(10% - 2pt);border:calc(1pt + 1pt) solid red;background:blue;height:40pt'></div></div></body>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 200}, margin: 0)

    assert Enum.any?(layout.boxes, fn box ->
             Map.get(box, :fill_color) == {0, 0, 1} and abs(box.x - 8) < 0.001 and
               abs(box.width - 80) < 0.001
           end)
  end

  test "length-only consumers reject percentages and clamp negative calculated lengths" do
    assert :error = CssLengthValidator.prepare("calc(1% + 2pt)", :length)
    assert :error = CssLengthValidator.prepare("-1ch", :padding)
    assert {:ok, expression} = CssLengthValidator.prepare("calc(-1pt)", :padding)
    style = CssStyles.by_id("<p id='style'>Text</p>")["style"]
    assert {:ok, +0.0} = CssLengthValidator.resolve(expression, 100, style)
    assert {:ok, expression} = CssLengthValidator.prepare("calc(10% + 2pt)", :margin)
    assert {:ok, frozen} = CssLengthValidator.resolve(expression, nil, style)
    assert CssLengthValidator.valid_edge?(frozen, :margin)
    assert {:ok, ^frozen} = CssLengthValidator.resolve(frozen, nil, style)
    assert {:ok, value} = CssLengthValidator.resolve(expression, 100, style)
    assert_in_delta value, 12.0, 1.0e-10
    refute CssLengthValidator.valid_edge?(expression, :padding)
    refute CssLengthValidator.valid_edge?(:invalid, :padding)
  end

  test "inherited calculations retain declaring font units and use the child's percentage space" do
    styles =
      CssStyles.by_id("""
      <html style="font-size:12pt;line-height:15pt"><body>
      <div id="parent" style="font-size:20pt;line-height:30pt;width:calc(50% + 1em);padding:calc(10% + 1em);left:calc(50% + 1em);height:calc-size(auto, size + 1em)">
      <div id="child" style="font-size:10pt;width:inherit;padding:inherit;left:inherit;height:inherit"></div>
      </div></body></html>
      """)

    child = styles["child"]
    assert Layout.resolve_content_size(child, :width, 100, nil) == 70
    assert {:ok, edges} = CssLengthValidator.resolve_edges(child, 100)
    assert edges.padding.left == 30

    assert {:ok, 70.0} =
             NativeElixirPdfUtilities.Validators.CssPositionValidator.resolve(
               child.offsets.left,
               100,
               child
             )

    assert {:ok, 100.0} =
             NativeElixirPdfUtilities.Validators.CssIntrinsicSizingValidator.resolve(
               child.height,
               nil,
               %{auto: 80},
               child
             )
  end

  test "root spacing uses its own computed root face and line height" do
    styles =
      CssStyles.by_id("""
      <html style="font-family:DejaVu Sans Mono;font-size:20pt;line-height:30pt;padding:1rlh;letter-spacing:calc(1rch)"><body style="padding:inherit"><p id="root" style="padding:inherit"></p></body></html>
      """)

    root = styles["root"]
    assert root.padding.left == 30
    assert_in_delta root.letter_spacing, Font.text_width("0", root.font_face, 20), 1.0e-10
  end

  test "all viewport families use the same print extent and logical axis" do
    for prefix <- ["", "s", "l", "d"] do
      for {unit, expected} <- [
            {"vw", 2},
            {"vh", 1},
            {"vi", 2},
            {"vb", 1},
            {"vmin", 1},
            {"vmax", 2}
          ] do
        assert {:ok, value, %{length: 1}} =
                 CssMathValidator.evaluate("1#{prefix}#{unit}", %{width: 200, height: 100})

        assert value == expected
      end

      assert {:ok, 1.0, %{length: 1}} =
               CssMathValidator.evaluate("1#{prefix}vi", %{
                 width: 200,
                 height: 100,
                 writing_mode: :vertical_rl
               })

      assert {:ok, 2.0, %{length: 1}} =
               CssMathValidator.evaluate("1#{prefix}vb", %{
                 width: 200,
                 height: 100,
                 writing_mode: :vertical_lr
               })
    end

    for unit <- ["vw", "vh", "vmin", "vmax"] do
      assert :error = CssMathValidator.evaluate("1#{unit}", %{width: nil, height: nil})
    end
  end

  test "unavailable geometry and nonfinite arithmetic return actionable consumer diagnostics" do
    for declaration <- [
          "width:1cqw",
          "margin:1cqw",
          "left:calc(1cqw)",
          "order:calc(1cqw / 1px)",
          "counter-reset:item calc(1cqw / 1px)",
          "line-height:calc(1cqw / 1px)"
        ] do
      assert {:error, {:not_implemented, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<div style='#{declaration}'>Text</div>",
                 system_font_discovery: false
               )

      assert diagnostic.stage == :style
      assert is_atom(diagnostic.module)
      assert is_binary(diagnostic.source)
      assert diagnostic.message =~ "measured"
    end

    for declaration <- [
          "width:calc(infinity * 1pt)",
          "margin:calc(infinity * 1pt)",
          "padding:calc(infinity * 1pt)",
          "border:calc(infinity * 1pt) solid red",
          "left:calc(infinity * 1pt)",
          "letter-spacing:calc(infinity * 1pt)",
          "line-height:calc(infinity * 1pt)"
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<div style='position:relative;#{declaration}'>Text</div>",
                 system_font_discovery: false
               )

      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.message =~ "finite"
    end
  end

  test "transform nonfinite values return their owning layout diagnostic" do
    assert {:error, {:invalid_layout, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<div style='transform:translateX(calc(infinity * 1pt))'>Text</div>",
               system_font_discovery: false
             )

    assert diagnostic.stage == :layout
    assert diagnostic.operation == :resolve_css_transform
    assert diagnostic.source =~ "infinity"
    assert diagnostic.message =~ "finite"
  end

  test "line-height scalar products and root cycles use the proper selected and parent metrics" do
    styles =
      CssStyles.by_id("""
      <html style="font-size:12pt;line-height:calc(1rlh + 1pt)"><body>
        <div style="font-size:20pt;line-height:30pt"><p id="scalar" style="font-size:10pt;line-height:calc(1em / 5pt);letter-spacing:1lh">Text</p></div>
      </body></html>
      """)

    assert_in_delta styles["scalar"].line_height, 20, 1.0e-10
    assert_in_delta styles["scalar"].letter_spacing, 20, 1.0e-10
  end

  test "prepared edge flags and dictionaries are checked before layout" do
    alias NativeElixirPdfUtilities.Validators.CssSizingValidator
    assert {:ok, margin} = CssLengthValidator.prepare("calc(10% + 2pt)", :margin)
    assert CssLengthValidator.valid_edge?(margin, :margin)
    assert {:ok, border} = CssLengthValidator.prepare("calc(2pt)", :length)
    assert CssLengthValidator.valid_edge?(border, :border_widths)
    edges = %{top: margin, right: margin, bottom: margin, left: margin}

    assert :ok =
             CssLengthValidator.validate_style(%{margin: edges, _calculated_edges: true}, "div")

    assert {:error, {:invalid_layout, _}} =
             CssLengthValidator.validate_style(
               %{margin: :invalid, _calculated_edges: true},
               "div"
             )

    assert {:error, {:invalid_layout, _}} =
             CssLengthValidator.validate_style(%{margin: edges}, "div")

    style = CssStyles.by_id("<p id='style' style='padding:calc(10% + 2pt)'>Text</p>")["style"]
    root = CssSizingValidator.prepare_root(style)
    assert root._calculated_edges
    assert :ok = CssSizingValidator.validate_root(root)

    assert {:error, {:invalid_layout, _}} =
             CssSizingValidator.validate_root(%{_calculated_edges: false})

    assert {:ok, intrinsic} = HtmlValidator.parse_css_size("min-content")
    assert :ok = CssSizingValidator.validate_root(%{width: intrinsic})
    refute CssSizingValidator.valid_calculation?(:invalid)

    assert {:ok, nan} =
             CssMathValidator.prepare_length("calc(NaN * 1pt)", %{"pt" => 1}, :css_pixel)

    assert {:ok, +0.0} = CssMathValidator.resolve_length(nan, 0, :property)
    assert CssMathValidator.valid_prepared_length?({:css_calculation, {"1pt", %{"pt" => 1}}})
  end

  test "deferred percentages report overflow at root, normal and positioned containing boxes" do
    for html <- [
          "<html style='padding:calc(infinity * 1pt + 1%)'><body>Text</body></html>",
          "<div style='padding:calc(infinity * 1pt + 1%)'>Text</div>",
          "<div style='position:absolute;padding:calc(infinity * 1pt + 1%)'>Text</div>",
          "<div style='width:calc-size(auto, infinity * 1pt)'>Text</div>",
          "<div style='background-size:1cqw'>Text</div>"
        ] do
      assert {:error, {reason, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

      assert reason in [:resource_limit_exceeded, :not_implemented]
      assert is_binary(diagnostic.message) and is_atom(diagnostic.stage)
    end

    assert {:error, {:resource_limit_exceeded, _}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<div style='background-image: url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC);background-position:calc(infinity * 1pt + 1%) 0;width:100pt;height:20pt'>Text</div>",
               system_font_discovery: false
             )
  end

  test "source substitutions inspect generated content and reject intrinsic background sizes" do
    alias NativeElixirPdfUtilities.Validators.CssValueValidator

    for value <- ["attr(data-text raw-string)", "env(unknown, \"Text\")"] do
      assert {:ok, %{kind: :substitution_tokens}} =
               CssValueValidator.prepare_unapplied_declaration(
                 %{declaration: {"content", value}},
                 HtmlValidator.new_css_budget()
               )
    end

    assert {:error, {:invalid_css, _}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<div style='background-size:min-content'>Text</div>"
             )

    assert HtmlValidator.normalize_css_unit("bad\\") == "bad\\"

    css = "content:attr()"

    assert {:ok, [record]} =
             NativeElixirPdfUtilities.HtmlToPdf.CssParser.prepare_source_declarations(
               css,
               NativeElixirPdfUtilities.Validators.HtmlCssSourceValidator.local(css)
             )

    assert {:error, {:invalid_css, _}} =
             CssValueValidator.prepare_unapplied_declaration(
               record,
               HtmlValidator.new_css_budget()
             )
  end

  test "allocated flex and grid content widths provide descendant percentage bases" do
    for display <- ["display:flex", "display:grid;grid-template-columns:100pt 100pt"] do
      html =
        "<body style='margin:0'><div style='#{display};width:200pt'><div style='flex:1'><div style='padding:calc(10% + 1pt)'>A</div></div><div style='flex:1'>B</div></div></body>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html,
                 page_size: {300, 200},
                 margin: 0,
                 system_font_discovery: false
               )

      assert {:ok, %{pages: [page]}} = NativeElixirPdfUtilities.Text.extract_spans(pdf)
      assert_in_delta Enum.find(page.spans, &(&1.text == "A")).x, 11, 0.01
      assert_in_delta Enum.find(page.spans, &(&1.text == "B")).x, 100, 0.01
    end
  end

  test "numeric tokens retain per-token ceilings in positions and box lengths" do
    alias NativeElixirPdfUtilities.Limits
    alias NativeElixirPdfUtilities.Validators.CssPositionValidator
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    Limits.install(Map.put(original, :max_numeric_token_bytes, 3))
    assert {:error, {:resource_limit_exceeded, _}} = CssPositionValidator.prepare_math("123pt")

    assert {:error, {:resource_limit_exceeded, _}} =
             HtmlValidator.with_render_budget(fn ->
               CssPositionValidator.valid_prepared?({:css_position_math, "123pt"})
             end)

    assert {:error, {:resource_limit_exceeded, _}} = CssLengthValidator.prepare("123ch", :padding)
  end
end
