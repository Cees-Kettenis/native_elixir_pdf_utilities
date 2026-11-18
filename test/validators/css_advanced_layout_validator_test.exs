defmodule NativeElixirPdfUtilities.Validators.CssAdvancedLayoutValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.CssParser

  alias NativeElixirPdfUtilities.Validators.{
    CssAdvancedLayoutValidator,
    CssAnchorValidator,
    CssBorderShapeValidator,
    CssBoxPaintValidator,
    CssGapRuleValidator
  }

  test "adopted declarations expose their longhands and validate authored combinations" do
    assert "flow-from" in CssAdvancedLayoutValidator.properties()
    assert "position-try-order" in CssAnchorValidator.properties()
    assert "border-top-clip" in CssBorderShapeValidator.longhands()

    assert CssBorderShapeValidator.shorthands()["border-inline-clip"] ==
             ~w(border-inline-start-clip border-inline-end-clip)

    assert "corner-shape" in CssBorderShapeValidator.properties()
    assert "rule-width" in CssGapRuleValidator.properties()
    assert "row-rule-width" in CssGapRuleValidator.longhands()
    assert CssGapRuleValidator.shorthands()["rule-width"] == ~w(row-rule-width column-rule-width)

    for {property, source, key, value} <- [
          {"flow-into", "none", :flow_into, nil},
          {"flow-into", "article element", :flow_into, {"article", :element}},
          {"flow-into", "article content", :flow_into, {"article", :content}},
          {"flow-from", "none", :flow_from, nil},
          {"line-grid", "match-parent", :line_grid, :match_parent},
          {"line-grid", "create", :line_grid, :create},
          {"block-step-size", "none", :block_step_size, nil},
          {"fit-tolerance", "normal", :fit_tolerance, nil},
          {"fit-tolerance", "infinite", :fit_tolerance, :infinite},
          {"fit-tolerance", "10%", :fit_tolerance, {:percent, 0.1}},
          {"wrap-through", "none", :wrap_through, :none},
          {"block-step-round", "nearest", :block_step_round, :nearest}
        ] do
      assert {:ok, %{^key => ^value}} = CssAdvancedLayoutValidator.prepare(property, source, %{})
    end

    for {property, invalid} <- [
          {"flow-into", "a b c"},
          {"flow-from", "auto"},
          {"flow-from", "two names"},
          {"line-grid", "named-grid"},
          {"block-step-size", "-1pt"},
          {"wrap-flow", "both end"}
        ] do
      assert :error = CssAdvancedLayoutValidator.prepare(property, invalid, %{})
    end
  end

  test "anchor names, area alternatives and validity properties stay typed and case-sensitive" do
    for {property, source, expected} <- [
          {"anchor-name", "none", %{anchor_name: []}},
          {"anchor-name", "--One, --two", %{anchor_name: ["--One", "--two"]}},
          {"anchor-scope", "none", %{anchor_scope: []}},
          {"anchor-scope", "all", %{anchor_scope: :all}},
          {"anchor-scope", "--One", %{anchor_scope: ["--One"]}},
          {"position-anchor", "auto", %{position_anchor: :auto}},
          {"position-anchor", "none", %{position_anchor: :none}},
          {"position-area", "none", %{position_area: nil}},
          {"position-area", "span-all inline-end", %{position_area: ["span-all", "inline-end"]}},
          {"position-try-order", "most-inline-size", %{position_try_order: :inline_size}},
          {"position-try-fallbacks", "none", %{position_try_fallbacks: []}},
          {"position-try-fallbacks", "top, flip-inline flip-block",
           %{position_try_fallbacks: [{:area, ["top"]}, {:rule, nil, [:inline, :block]}]}},
          {"position-visibility", "anchors-valid no-overflow",
           %{position_visibility: ~w(anchors-valid no-overflow)}}
        ] do
      assert {:ok, ^expected} = CssAnchorValidator.property(property, source, %{})
    end

    for {property, value} <- [
          {"anchor-name", "--"},
          {"position-area", "left right bottom"},
          {"position-try", ""},
          {"position-try-fallbacks", "--required --extra"},
          {"position-visibility", "no-overflow no-overflow"}
        ] do
      assert :error = CssAnchorValidator.property(property, value, %{})
    end

    assert :ordinary = CssAnchorValidator.function("calc(1pt + 2pt)", :left, %{})

    assert {:ok, {:css_anchor, "anchor-size", nil, :property, :auto}} =
             CssAnchorValidator.function("anchor-size()", :width, %{})

    assert {:ok, {:css_anchor, "anchor", nil, {:percent, 0.25}, 4.0}} =
             CssAnchorValidator.function("anchor(25%, 4pt)", :left, %{})

    for text <- [
          "anchor(top, 2pt, 3pt)",
          "anchor(nowhere)",
          "anchor(--one --two left)",
          "anchor(left bottom)",
          "anchor-size(left)"
        ] do
      assert :error = CssAnchorValidator.function(text, :left, %{})
    end

    assert {:ok, sheet} =
             CssParser.prepare_source_stylesheet("""
             @position-try invalid {top:20pt}
             @position-try --reasonable {top:3pt;color:red;left:anchor(--ref right)}
             .tip {left:anchor(--ref top);width:anchor-size()}
             """)

    assert sheet.cascade.position_tries == %{
             "--reasonable" => [{"top", "3pt"}, {"left", "anchor(--ref right)"}]
           }
  end

  test "unapplied physical anchor values retain their unresolved geometry" do
    alias NativeElixirPdfUtilities.Validators.{CssValueValidator, HtmlValidator}
    budget = HtmlValidator.new_css_budget()
    declaration = %{declaration: {"left", "anchor(--pin right, 3pt)"}}

    assert {:ok,
            %{kind: :anchor_position, anchor: {:css_anchor, "anchor", "--pin", "right", 3.0}}} =
             CssValueValidator.prepare_unapplied_declaration(declaration, budget)

    assert CssAnchorValidator.try_declarations([{"left", "2pt", :important}, {"left", "3pt"}]) ==
             [{"left", "3pt"}]
  end

  test "border clip, corner and list-valued shadow declarations stay independent" do
    style = %{writing_mode: :vertical_rl, direction: :ltr}

    assert {:ok, %{border_shape: nil}} =
             CssBorderShapeValidator.prepare(style, "border-shape", "none")

    assert {:ok, %{border_shape: [%{reference: :border_box}, %{reference: :content_box}]}} =
             CssBorderShapeValidator.prepare(
               style,
               "border-shape",
               "circle(45%) border-box ellipse(20% 35%) content-box"
             )

    assert {:ok, %{border_shape: [%{reference: :half_border_box}]}} =
             CssBorderShapeValidator.prepare(style, "border-shape", "inset(6pt) half-border-box")

    assert {:ok, %{border_limit: nil}} =
             CssBorderShapeValidator.prepare(style, "border-limit", "all")

    assert {:ok, %{border_limit: {:top, {:percent, 0.5}}}} =
             CssBorderShapeValidator.prepare(style, "border-limit", "top 50%")

    assert {:ok, %{border_right_clip: nil, border_left_clip: nil}} =
             CssBorderShapeValidator.prepare(style, "border-block-clip", "none")

    assert {:ok, %{border_top_clip: [{:fraction, 1.0}]}} =
             CssBorderShapeValidator.prepare(style, "border-inline-start-clip", "1fr")

    assert {:ok, %{border_top_clip: [10.0], border_left_clip: [10.0]}} =
             CssBorderShapeValidator.prepare(style, "border-clip", "10pt")

    for {source, value} <- [
          {"superellipse(infinity)", :square},
          {"superellipse(-infinity)", :notch},
          {"superellipse(2)", 2.0},
          {"bevel", 0.0}
        ] do
      assert {:ok, %{corner_top_right_shape: ^value}} =
               CssBorderShapeValidator.prepare(style, "corner-start-start-shape", source)
    end

    assert {:ok, %{corner_top_left_shape: :square}} =
             CssBorderShapeValidator.prepare(style, "corner-top-left-shape", "square")

    for source <- ["square", "bevel square", "round square bevel"] do
      assert {:ok, %{corner_top_left_shape: _}} =
               CssBorderShapeValidator.prepare(style, "corner-shape", source)
    end

    for {property, value} <- [
          {"border-limit", "top"},
          {"border-shape", "border-box"},
          {"border-shape", "circle(20%) circle(10%) circle(2%)"},
          {"border-shape", "not-a-shape"},
          {"corner-shape", "superellipse(2pt)"},
          {"corner-shape", "round bevel square scoop notch"},
          {"corner-top-left-shape", "funny"},
          {"border-clip", ""},
          {"border-clip", "10pt no"}
        ] do
      assert :error = CssBorderShapeValidator.prepare(style, property, value)
    end

    for {property, value} <- [
          {"box-shadow-position", "top"},
          {"box-shadow-offset", "1pt 2pt 3pt"},
          {"box-shadow-blur", "-1pt"},
          {"box-shadow-spread", "1pt 3pt"}
        ] do
      assert :error = CssBoxPaintValidator.prepare(%{}, property, value)
    end

    assert [%{x: 4.0, y: +0.0, blur: +0.0, color: :current_color, inset: false}] =
             CssBoxPaintValidator.computed_shadows(%{box_shadow_offset: [{4.0, 0.0}]})
  end

  test "gap shorthands and visibility admit only the adopted values" do
    for {property, source, expected} <- [
          {"rule-style", "dashed", %{row_rule_style: :dashed, column_rule_style: :dashed}},
          {"rule-break", "intersection",
           %{row_rule_break: :intersection, column_rule_break: :intersection}},
          {"rule-visibility-items", "between",
           %{row_rule_visibility_items: :between, column_rule_visibility_items: :between}},
          {"rule-inset", "2pt 10%",
           %{
             row_rule_inset_start: 2.0,
             column_rule_inset_start: 2.0,
             row_rule_inset_end: {:percent, 0.1},
             column_rule_inset_end: {:percent, 0.1}
           }},
          {"rule-overlap", "column-over-row", %{rule_overlap: :column}},
          {"row-rule-inset-end", "2pt", %{row_rule_inset_end: 2.0}}
        ] do
      assert {:ok, ^expected} = CssGapRuleValidator.prepare(%{}, property, source)
    end

    for {property, value} <- [
          {"row-rule-inset", "1pt 2pt 3pt"},
          {"row-rule-inset", "auto"},
          {"row-rule-inset", "something"},
          {"column-rule-break", "both"}
        ] do
      assert :error = CssGapRuleValidator.prepare(%{}, property, value)
    end
  end
end
