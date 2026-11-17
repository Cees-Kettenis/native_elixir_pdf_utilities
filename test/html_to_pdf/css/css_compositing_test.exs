defmodule NativeElixirPdfUtilities.HtmlToPdf.CssCompositingTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.CssCompositingValidator

  test "opacity clamps numbers and percentages and admits scalar calculations" do
    for {source, expected} <- [
          {"-.2", 0.0},
          {"150%", 1.0},
          {"25%", 0.25},
          {"calc(1 / 2)", 0.5},
          {"calc(20% + .3)", 0.5}
        ] do
      assert {:ok, %{opacity: ^expected}} = CssCompositingValidator.apply(%{}, "opacity", source)
    end

    for source <- ["auto", "none", "1px", "50% 20%"] do
      assert :error = CssCompositingValidator.apply(%{}, "opacity", source)
    end
  end

  test "opacity and blending do not inherit implicitly but CSS-wide values use the parent" do
    {:ok, dom} =
      HtmlParser.parse_detailed(
        "<div style='opacity:.4;mix-blend-mode:multiply'><p>A</p><p style='opacity:inherit'>B</p><p style='opacity:unset'>C</p></div>"
      )

    assert {:ok, %{children: [parent]}} =
             Style.compute_detailed(dom, system_font_discovery: false)

    assert parent.style.opacity == 0.4
    [a, b, c] = parent.children
    assert Map.get(a.style, :opacity, 1.0) == 1.0
    assert Map.get(a.style, :mix_blend_mode, :Normal) == :Normal
    assert b.style.opacity == 0.4
    assert c.style.opacity == 1.0
  end

  test "blend keywords preserve the fixed PDF vocabulary and reject unknown modes" do
    assert {:ok, %{mix_blend_mode: :ColorDodge}} =
             CssCompositingValidator.apply(%{}, "mix-blend-mode", "color-dodge")

    assert :error = CssCompositingValidator.apply(%{}, "mix-blend-mode", "unknown")
    refute CssCompositingValidator.valid_style?(%{opacity: :auto})
    refute CssCompositingValidator.valid_style?(%{mix_blend_mode: :Unknown})
  end

  test "group opacity retains nested artwork and searchable text in transparency forms" do
    html =
      "<div style='opacity:.5;background:red'><p>Outer</p><div style='opacity:.4;mix-blend-mode:multiply;background:blue'><p>Inner</p></div></div>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, compress_streams: false, system_font_discovery: false)

    assert pdf =~ "/S /Transparency"
    assert pdf =~ "/BM /Multiply"
    assert pdf =~ "/ca 0.5"
    assert pdf =~ "/ca 0.4"
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Outer"
    assert text =~ "Inner"
  end

  test "neutral isolated stacks keep native edge paint while descendant blends retain isolation" do
    html =
      "<div style='isolation:isolate;width:80pt;height:40pt;background:#eee'><div style='width:60pt;height:30pt;background:blue'>Neutral</div></div>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    refute pdf =~ "/S /Transparency"
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Neutral"

    assert {:ok, blended} =
             HtmlToPdf.render(
               String.replace(html, "background:blue", "background:blue;mix-blend-mode:multiply"),
               system_font_discovery: false
             )

    assert blended =~ "/S /Transparency"
    assert blended =~ "/I true"
    assert blended =~ "/BM /Multiply"

    assert CssCompositingValidator.paint_group?(%{isolation: :isolate}, [
             %{boxes: [%{css_compositing_scopes: [%{mix_blend_mode: :Multiply}]}]}
           ])

    refute CssCompositingValidator.paint_group?(%{isolation: :isolate}, [%{boxes: [%{}]}])
  end

  test "zero opacity preserves content while using an entirely transparent group" do
    assert {:ok, pdf} =
             HtmlToPdf.render("<div style='opacity:0'>Hidden ink</div>", compress_streams: false)

    assert pdf =~ "/ca 0 /CA 0"
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Hidden ink"
  end

  test "background blend lists preserve order, reject empty layers and stay noninherited" do
    assert {:ok, %{background_blend_modes: [:Multiply, :Screen]}} =
             CssCompositingValidator.apply(%{}, "background-blend-mode", "multiply, screen")

    for source <- ["", "multiply,", ",screen", "multiply,,screen", "unknown"] do
      assert :error = CssCompositingValidator.apply(%{}, "background-blend-mode", source)
    end

    refute CssCompositingValidator.valid_style?(%{background_blend_modes: []})
    refute CssCompositingValidator.valid_style?(%{background_blend_modes: [:Unknown]})

    assert {:error, {:invalid_layout, diagnostic}} =
             NativeElixirPdfUtilities.Validators.WriterValidator.validate_native_boxes([
               %{css_inner_opacity_key: :forged}
             ])

    assert diagnostic.source == "css_inner_opacity_key"
  end

  test "compositing scopes reject cycles and malformed paint data" do
    scope = %{id: [0, 1], opacity: 0.5, mix_blend_mode: :Normal}
    assert :ok = CssCompositingValidator.validate_scopes([scope])

    for scopes <- [nil, [scope, scope], [%{scope | opacity: 2}], [%{}]] do
      assert {:error, {:invalid_layout, diagnostic}} =
               CssCompositingValidator.validate_scopes(scopes)

      assert diagnostic.stage == :writer
      assert diagnostic.source == "CSS compositing"
    end
  end

  test "compositing respects stack applicability and ancestor background ownership" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Stacking
    alias NativeElixirPdfUtilities.Validators.CssStackingValidator
    assert CssStackingValidator.level(%{position: :static, z_index: 10, opacity: 0.5}) == 0
    assert CssStackingValidator.level(%{position: :relative, z_index: 10}) == 10
    assert CssStackingValidator.level(%{_stacking_item: true, z_index: -2}) == -2
    style = %{_stacking_order: [0, 1], opacity: 0.5, z_index: 10}
    ancestor = %{_stacking_background_order: [0]}
    assert [^ancestor, child] = Stacking.attach([ancestor, %{}], style)
    assert child._stacking_path == [{[0, 1], 0}]
    assert child.css_compositing_scopes == [%{id: [0, 1], opacity: 0.5, mix_blend_mode: :Normal}]
    assert Stacking.attach([child], style) == [child]
    refute CssCompositingValidator.valid_style?(%{_css_compositing_contexts: [%{}]})

    assert {:error, {:invalid_layout, diagnostic}} =
             NativeElixirPdfUtilities.Validators.WriterValidator.validate_native_boxes([
               %{css_compositing_paint: true}
             ])

    assert diagnostic.source == "css_compositing_paint"
  end

  test "compositing retains static controls and links while individual transforms support editable fields" do
    alias NativeElixirPdfUtilities.Forms

    html =
      "<style>html,body{margin:0;font-size:12pt}div{opacity:.5;transform:rotate(8deg);width:120pt;height:70pt;margin:20pt}input{width:60pt;height:18pt}</style><div><a href='https://example.com/opacity'>Group link</a><input name='faded' value='Initial'></div>"

    assert {:error, {:unsupported_form, diagnostic}} =
             HtmlToPdf.render(html, page_size: {240, 180}, margin: 0)

    assert diagnostic.stage == :forms and diagnostic.source == "opacity/mix-blend-mode"
    assert diagnostic.message =~ "forms: :static"

    assert {:ok, static} =
             HtmlToPdf.render(html, page_size: {240, 180}, margin: 0, forms: :static)

    assert static =~ "/Subtype /Link"
    assert {:ok, []} = Forms.fields(static)

    html =
      String.replace(
        html,
        "opacity:.5;transform:rotate(8deg)",
        "translate:3pt 2pt;rotate:8deg;scale:.9;transform:translateX(2pt)"
      )

    assert {:ok, pdf} =
             HtmlToPdf.render(html, page_size: {240, 180}, margin: 0, compress_streams: false)

    assert pdf =~ "/Subtype /Link"
    assert pdf =~ "/QuadPoints ["
    assert {:ok, [%{name: "faded", value: "Initial", widgets: [_]}]} = Forms.fields(pdf)
    assert {:ok, changed} = Forms.fill(pdf, %{"faded" => "Changed"})
    assert {:ok, [%{value: "Changed"}]} = Forms.fields(changed)
    assert {:ok, flat} = Forms.flatten(changed)
    assert {:ok, []} = Forms.fields(flat)
    assert {:ok, text} = Text.extract(flat)
    assert text =~ "Changed"
  end

  test "resource groups reject conflicting parent ancestry and state across boxes" do
    a = %{id: [0], opacity: 0.5, mix_blend_mode: :Normal}
    b = %{id: [0, 1], opacity: 0.5, mix_blend_mode: :Multiply}

    assert :ok =
             CssCompositingValidator.validate_ancestry([
               %{css_compositing_scopes: [a]},
               %{css_compositing_scopes: [a, b]},
               %{css_compositing_scopes: [a, b]}
             ])

    for scopes <- [[b, a], [%{a | opacity: 0.2}], [%{a | mix_blend_mode: :Screen}]] do
      assert {:error, {:invalid_layout, diagnostic}} =
               CssCompositingValidator.validate_ancestry([
                 %{css_compositing_scopes: [a, b]},
                 %{css_compositing_scopes: scopes}
               ])

      assert diagnostic.source == "CSS compositing ancestry"
    end
  end
end
