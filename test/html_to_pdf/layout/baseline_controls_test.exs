defmodule NativeElixirPdfUtilities.HtmlToPdf.BaselineControlsTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.CssBaselineValidator

  test "vertical-align resets its three longhands and rejects duplicate components" do
    assert {:ok, style} =
             CssBaselineValidator.prepare(
               %{dominant_baseline: :hanging},
               "vertical-align",
               "last alphabetic -25%"
             )

    assert style.baseline_source == :last
    assert style.alignment_baseline == :alphabetic
    assert style.dominant_baseline == :hanging

    assert {:ok, computed} =
             CssBaselineValidator.compute_style(
               Map.merge(style, %{font_size: 12, line_height: 20})
             )

    assert computed.baseline_shift == -5
    assert :ok = CssBaselineValidator.validate_style(computed, "span")

    assert {:ok, reset} = CssBaselineValidator.prepare(style, "vertical-align", "top")
    assert reset.baseline_source == :auto and reset.alignment_baseline == :baseline
    assert reset.baseline_shift == :top and reset.vertical_align == :top

    for value <- [
          "auto",
          "first last",
          "alphabetic hanging",
          "sub 2pt",
          "2pt 3pt",
          "first first",
          "",
          "baseline none"
        ] do
      assert :error = CssBaselineValidator.prepare(%{}, "vertical-align", value)
    end

    for {property, value} <- [
          {"baseline-source", "baseline"},
          {"alignment-baseline", "auto"},
          {"dominant-baseline", "baseline"},
          {"baseline-shift", "baseline"},
          {"baseline-shift", "auto"},
          {"baseline-shift", "1"},
          {"unknown", "first"}
        ] do
      assert :error = CssBaselineValidator.prepare(%{}, property, value)
    end
  end

  test "longhand winners and CSS-wide values retain the other shorthand components" do
    for {declarations, expected} <- [
          {"vertical-align:first hanging 2pt;baseline-shift:4pt", {:first, :hanging, 4}},
          {"baseline-shift:4pt!important;vertical-align:last alphabetic 2pt",
           {:last, :alphabetic, 4}},
          {"vertical-align:first hanging 2pt;alignment-baseline:initial", {:first, :baseline, 2}},
          {"vertical-align:first hanging 2pt;vertical-align:initial", {:auto, :baseline, 0}},
          {"baseline-source:last;vertical-align:super", {:auto, :baseline, :super}}
        ] do
      assert {:ok, dom} = HtmlParser.parse("<span style='#{declarations}'>A</span>")
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      node = Enum.find(styled.children, &(&1.tag == "span"))

      assert {node.style.baseline_source, node.style.alignment_baseline,
              node.style.baseline_shift} == expected
    end
  end

  test "percent shifts use the child's used line height and leave sibling baselines intact" do
    html =
      "<p style='margin:0;font:12pt/20pt sans-serif'>X<span style='baseline-shift:25%'>A</span><span style='vertical-align:-10%'>B</span>Y</p>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, margin: 0)
    [base, raised, lowered, after_text] = Enum.filter(layout.boxes, &(&1.type == :text))
    assert base.y == after_text.y
    assert_in_delta raised.y - base.y, 5, 0.00001
    assert_in_delta lowered.y - base.y, -2, 0.00001
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "XABY"} = Text.extract(pdf, layout: false)
  end

  test "first and last inline-block baselines select the corresponding line" do
    for source <- ["first", "last", "auto"] do
      html =
        "<p style='margin:0;font:12pt/20pt sans-serif'>X<span style='display:inline-block;baseline-source:#{source}'>A<br>B</span>Y</p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, margin: 0)
      boxes = Map.new(Enum.filter(layout.boxes, &(&1.type == :text)), &{&1.text, &1})
      assert boxes["X"].y == boxes["Y"].y
      selected = if source == "first", do: "A", else: "B"
      assert_in_delta boxes[selected].y, boxes["X"].y, 0.00001
      assert_in_delta boxes["A"].y - boxes["B"].y, 20, 0.00001
    end
  end

  test "prepared baseline failures use the shared actionable layout diagnostic" do
    for style <- [
          %{baseline_source: :unknown},
          %{alignment_baseline: :unknown},
          %{dominant_baseline: :baseline},
          %{baseline_shift: :auto},
          %{vertical_align: {:baseline_controls, :unknown, 0}},
          %{vertical_align: :unknown},
          %{baseline_shift: 1.0e20}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               CssBaselineValidator.validate_style(style, "span")

      assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
      assert diagnostic.operation == :layout and diagnostic.source == "span"
      assert diagnostic.message =~ "recompute styles"
    end
  end

  test "center shifts use the final line height for inline text and atomic boxes" do
    html =
      "<p style='margin:0;font:12pt/20pt sans-serif'>X<span style='font:6pt/8pt sans-serif;baseline-shift:center'>A</span><span style='display:inline-block;width:10pt;height:60pt;baseline-shift:center;background:blue'></span>Y</p>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, margin: 0)
    [base, centered, after_text] = Enum.filter(layout.boxes, &(&1.type == :text))
    assert base.y == after_text.y and base.line_box_height == 60
    parent = hd(styled.children).style
    child = Enum.find(hd(styled.children).children, &(Map.get(&1, :tag) == "span")).style
    top = base.y + NativeElixirPdfUtilities.Validators.HtmlValidator.text_baseline_depth(parent)

    assert_in_delta centered.y,
                    top - 30 + 4 -
                      NativeElixirPdfUtilities.Validators.HtmlValidator.text_baseline_depth(child),
                    0.00001

    [rectangle] = Enum.filter(layout.boxes, &(&1.type == :rect and &1.fill_color == {0, 0, 1}))
    assert_in_delta rectangle.y, top - 60, 0.00001
  end

  test "mathematical alignment uses the selected font's MATH axis" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Font
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, font} = Font.resolve("math", 400, :normal, registry)
    # The bundled STIX Two Math fixture has 1000 units/em and AxisHeight 258.
    parent = %{font_face: font, font_size: 20, line_height: 24}

    assert {:ok, child} =
             CssBaselineValidator.prepare(
               %{parent | font_size: 10},
               "vertical-align",
               "mathematical"
             )

    assert {:ok, resolved} = CssBaselineValidator.resolve(child, parent, "A")
    assert_in_delta resolved.vertical_align, 2.58, 0.000000001
  end

  test "centered inline images center their margin box within the final line" do
    png =
      "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADElEQVR4nGMwTpsJAAICATNWh+JUAAAAAElFTkSuQmCC"

    [top, centered] =
      for alignment <- ["top", "center"] do
        html =
          "<p style='margin:0;font:12pt/36pt sans-serif'>X<img src='data:image/png;base64,#{png}' style='width:10pt;height:12pt;margin-top:2pt;margin-bottom:4pt;baseline-shift:#{alignment}'></p>"

        assert {:ok, dom} = HtmlParser.parse(html)
        assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
        assert {:ok, layout} = Layout.layout(styled, margin: 0)
        [image] = Enum.filter(layout.boxes, &(&1.type == :image))
        assert image.width == 10 and image.height == 12
        image
      end

    # The 18pt margin box leaves 18pt in the 36pt line, split equally.
    assert_in_delta top.y - centered.y, 9, 1.0e-8
  end

  test "CSS-wide baseline values derive HTML script hints only when the longhands are used" do
    for {tag, shift} <- [{"sub", :sub}, {"sup", :super}] do
      for declarations <- ["vertical-align:revert", "baseline-shift:revert"] do
        assert {:ok, dom} = HtmlParser.parse("<#{tag} style='#{declarations}'>A</#{tag}>")
        assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
        style = hd(styled.children).style
        assert CssBaselineValidator.initialize(style).baseline_shift == shift
        assert style.vertical_align == shift
      end
    end
  end

  test "fonts without MATH use the minus ink center and then the half-em mathematical fallback" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Font
    alias NativeElixirPdfUtilities.TestSupport.Paths

    assert {:ok, [font]} =
             Font.load_configured_faces([
               %{
                 family: "Minus baseline",
                 path: Paths.project("priv/fonts/dejavu/DejaVuSansMono.ttf")
               }
             ])

    assert {:ok, _, directory} =
             NativeElixirPdfUtilities.Validators.FontValidator.prepare_table_directory(font.data)

    refute Map.has_key?(directory, "MATH")
    assert {:ok, bounds} = Font.glyph_bounds(font)
    {_left, bottom, _right, top} = elem(bounds, font.cmap[0x2212])

    for {face, expected} <- [
          {font, (bottom + top) / 2 / font.units_per_em * 10},
          {%{font | cmap: Map.delete(font.cmap, 0x2212)}, 5}
        ] do
      parent = %{font_face: face, font_size: 20, line_height: 24}

      child = %{
        font_face: face,
        font_size: 10,
        line_height: 12,
        vertical_align: {:baseline_controls, :mathematical, 0}
      }

      assert {:ok, resolved} = CssBaselineValidator.resolve(child, parent, "A")
      assert_in_delta resolved.vertical_align, expected, 1.0e-8
    end
  end

  test "missing BASE tables use font edges, x-height and the synthetic math axis" do
    font = %{
      type: :built_in,
      family: "Helvetica",
      pdf_name: "Helvetica",
      ascent: 800,
      descent: -200,
      units_per_em: 1000
    }

    parent = %{font_face: font, font_size: 20, line_height: 24}
    child = %{parent | font_size: 10, line_height: 12}

    for {metric, expected} <- [
          {:middle, 2.5},
          {:mathematical, 5.0},
          {:text_top, 6.75},
          {:text_bottom, -0.75},
          {:central, 3.0}
        ] do
      selected = Map.put(child, :vertical_align, {:baseline_controls, metric, 0})
      assert {:ok, resolved} = CssBaselineValidator.resolve(selected, parent)
      assert_in_delta resolved.vertical_align, expected, 1.0e-8
    end

    for {writing, orientation, expected} <- [
          {:horizontal_tb, :mixed, 0.0},
          {:sideways_rl, :mixed, 0.0},
          {:vertical_rl, :mixed, 3.0},
          {:vertical_lr, :sideways, 0.0}
        ] do
      parent = Map.merge(parent, %{writing_mode: writing, text_orientation: orientation})

      child =
        Map.merge(child, %{
          writing_mode: writing,
          text_orientation: orientation,
          vertical_align: {:baseline_controls, :baseline, 0}
        })

      assert {:ok, resolved} = CssBaselineValidator.resolve(child, parent, "A")
      assert_in_delta resolved.vertical_align, expected, 1.0e-8
    end

    for alignment <- [:middle, :text_top, :text_bottom, 2.0] do
      style = CssBaselineValidator.initialize(%{vertical_align: alignment})
      assert style.alignment_baseline == if(is_number(alignment), do: :baseline, else: alignment)
      assert style.baseline_shift == if(is_number(alignment), do: alignment, else: 0.0)
    end

    assert "alignment-baseline" in CssBaselineValidator.properties()
    assert {:ok, selected} = CssBaselineValidator.prepare(child, "alignment-baseline", "middle")
    assert selected.alignment_baseline == :middle
  end
end
