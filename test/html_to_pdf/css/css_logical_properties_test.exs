defmodule NativeElixirPdfUtilities.HtmlToPdf.CssLogicalPropertiesTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.TestSupport.CssStyles

  alias NativeElixirPdfUtilities.Validators.{
    CssLogicalValidator,
    CssValueValidator,
    HtmlValidator
  }

  # CSS Logical 1, 4 December 2025, 4.1 and 4.2; Writing Modes 4, 30 July 2019, 6.
  test "logical sides and dimensions map for every selected writing mode and direction" do
    for {mode, inline_start, inline_end, block_start, block_end} <- [
          {:horizontal_tb, :left, :right, :top, :bottom},
          {:vertical_rl, :top, :bottom, :right, :left},
          {:vertical_lr, :top, :bottom, :left, :right},
          {:sideways_rl, :top, :bottom, :right, :left},
          {:sideways_lr, :bottom, :top, :left, :right}
        ],
        direction <- [:ltr, :rtl] do
      axes = CssLogicalValidator.axes(%{writing_mode: mode, direction: direction})
      assert axes.inline_start == if(direction == :rtl, do: inline_end, else: inline_start)
      assert axes.inline_end == if(direction == :rtl, do: inline_start, else: inline_end)
      assert axes.block_start == block_start and axes.block_end == block_end
      assert axes.inline_size == if(mode == :horizontal_tb, do: :width, else: :height)
    end
  end

  test "physical and logical declarations compete using final axes and normal cascade order" do
    styles =
      CssStyles.by_id("""
      <div id="late" style="margin-left:1pt;margin-inline-start:2pt;direction:rtl"></div>
      <div id="vertical" style="width:3pt;inline-size:40pt;block-size:50pt;padding-block:6pt 7pt;border-inline-start:2pt solid red;inset-inline:8pt 9pt;writing-mode:vertical-rl"></div>
      <div id="priority" style="padding-inline-start:3pt!important;padding-left:7pt"></div>
      <div id="physical" style="margin-inline:2pt 4pt;margin-right:8pt"></div>
      """)

    assert styles["late"].margin.left == 1
    assert styles["late"].margin.right == 2
    assert styles["vertical"].width == 50 and styles["vertical"].height == 40
    assert styles["vertical"].padding.right == 6 and styles["vertical"].padding.left == 7
    assert styles["vertical"].border_widths.top == 1.5
    assert styles["vertical"].border_colors.top == {1, 0, 0}
    assert styles["vertical"].offsets.top == 8 and styles["vertical"].offsets.bottom == 9
    assert styles["priority"].padding.left == 3
    assert styles["physical"].margin.left == 2 and styles["physical"].margin.right == 8
  end

  test "variables, layers and cached declarations retain final axis ownership" do
    css = """
    @layer low { .item { padding-inline-start:5pt } }
    @layer high { .item { --mode:vertical-lr;writing-mode:var(--mode);padding-top:7pt } }
    """

    styles =
      CssStyles.by_id(
        "<div class='item' id='one'></div><div class='item' id='two' style='direction:rtl'></div>",
        [css]
      )

    assert styles["one"].padding.top == 7
    assert styles["two"].padding.top == 7 and styles["two"].padding.bottom == 5
  end

  test "logical inheritance reads parent axes and preserves deferred computed font terms" do
    styles =
      CssStyles.by_id("""
      <div style="writing-mode:vertical-rl;font-size:20pt;inline-size:calc(50% + 1em);padding-inline-start:6pt;margin-block-end:auto;border-inline-start:2pt solid red">
        <div id="child" style="writing-mode:horizontal-tb;font-size:10pt;inline-size:inherit;padding-inline-start:inherit;margin-block-end:inherit;border-inline-start:inherit"></div>
      </div>
      """)

    child = styles["child"]
    assert child.padding.left == 6
    assert child.border_widths.left == 1.5 and child.border_colors.left == {1, 0, 0}
    assert child.margin_block_auto.bottom

    assert NativeElixirPdfUtilities.HtmlToPdf.Layout.resolve_content_size(child, :width, 100, nil) ==
             70
  end

  test "logical pairs retain shared balanced grammar and reject invalid component values" do
    style = CssValueValidator.policy_style()

    assert {:ok, computed} =
             CssValueValidator.validate_resolved(style, "margin-inline", "calc(1pt + 2pt) 4pt")

    assert match?({:css_length_math, _}, computed.margin.left) and computed.margin.right == 4
    assert :error = CssValueValidator.validate_resolved(style, "padding-block", "1pt 2pt 3pt")
    assert :error = CssValueValidator.validate_resolved(style, "padding-inline-start", "-1pt")
    assert {:ok, inset} = CssValueValidator.validate_resolved(style, "inset", "1pt 2pt 3pt 4pt")
    assert inset.offsets == %{top: 1.0, right: 2.0, bottom: 3.0, left: 4.0}

    assert {:ok, _} =
             CssValueValidator.prepare_unapplied_declaration(
               %{declaration: {"inline-size", "calc(1ch + 2pt)"}},
               HtmlValidator.new_css_budget()
             )
  end

  test "axis grammar honors escapes and inheritance retains transparent edges" do
    assert {:ok, {:writing_mode, :vertical_rl}} =
             CssLogicalValidator.parse_axis("writing-mode", "VERTICAL-RL")

    assert {:ok, {:direction, :rtl}} = CssLogicalValidator.parse_axis("direction", ~S(r\74 l))
    assert :error = CssLogicalValidator.parse_axis("writing-mode", "diagonal")

    styles =
      CssStyles.by_id(
        "<div style='writing-mode:vertical-rl;border-inline-start:1pt solid transparent'><div id='child' style='writing-mode:horizontal-tb;border-inline-start:inherit'></div></div>"
      )

    assert styles["child"].border_colors.left == nil
  end

  test "logical shorthand expansion shares the physical inset component grammar" do
    alias NativeElixirPdfUtilities.Validators.CssLogicalValidator
    assert "inset" in CssLogicalValidator.properties()

    for {property, value, expected} <- [
          {"margin-inline", "2pt", [{"margin-left", "2pt"}, {"margin-right", "2pt"}]},
          {"inset", "1pt 2pt",
           [{"top", "1pt"}, {"right", "2pt"}, {"bottom", "1pt"}, {"left", "2pt"}]},
          {"inset", "1pt 2pt 3pt",
           [{"top", "1pt"}, {"right", "2pt"}, {"bottom", "3pt"}, {"left", "2pt"}]}
        ] do
      assert {:ok, ^expected} = CssLogicalValidator.expand(property, value, %{})
    end
  end

  test "source preparation rejects invalid logical components and malformed axis declarations" do
    for {property, value} <- [{"padding-inline", "-1pt"}, {"margin-block", "garbage"}] do
      assert {:error, _} =
               CssValueValidator.prepare_unapplied_declaration(
                 %{declaration: {property, value}, source: %{text: value, line: 1, column: 1}},
                 HtmlValidator.new_css_budget()
               )
    end

    assert {:error, _} =
             CssValueValidator.validate_cascade(
               CssValueValidator.policy_style(),
               [nil, {"direction", "rtl"}],
               HtmlValidator.new_css_budget()
             )
  end

  test "numeric logical inheritance clears only the replaced automatic margin edge" do
    styles =
      CssStyles.by_id(
        "<div style='margin-inline-start:10pt'><div id=a style='margin-left:auto;margin-right:auto;margin-inline-start:inherit'></div></div>"
      )

    assert styles["a"].margin.left == 10
    assert styles["a"].margin_inline_auto == %{left: false, right: true}
  end
end
