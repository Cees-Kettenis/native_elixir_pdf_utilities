defmodule NativeElixirPdfUtilities.HtmlToPdf.CssIntrinsicDimensionsTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.TestSupport.CssStyles
  alias NativeElixirPdfUtilities.Validators.{CssIntrinsicSizingValidator, HtmlValidator}

  # CSS Sizing 4 ED 2da1cb17a43306d3ef80d09d328883c71cdb54b2, section 7.
  # Its static percentage basis differs from interpolation preparation.
  test "intrinsic grammar retains its basis and rejects dimensional mixing" do
    for source <- [
          "min-content",
          "max-content",
          "fit-content",
          "fit-content(50%)",
          "fit-content(20pt)",
          "calc-size(max-content, size * .5)",
          "calc-size(calc-size(min-content, size + 1pt), size + 2pt)",
          "calc-size(50%, size + 1pt)",
          "calc-size(any, 20pt)"
        ] do
      assert {:ok, value} = HtmlValidator.parse_css_size(source, :width), source
      assert CssIntrinsicSizingValidator.valid?(value), source
      assert :ok = HtmlValidator.validate_prepared_css_size(value, :width)
    end

    for source <- [
          "calc-size(any, size)",
          "calc-size(min-content, size + 2)",
          "calc-size(auto, 1deg)",
          "calc-size(max-content)",
          "fit-content(auto)",
          "calc(min-content + 1pt)",
          "calc-size(unknown, size)",
          "fit-content(2pt"
        ] do
      assert :error = HtmlValidator.parse_css_size(source, :width), source
    end

    refute CssIntrinsicSizingValidator.valid?(:invalid)
  end

  test "definite and indefinite percentages have distinct calc-size roles" do
    style = CssStyles.by_id("<p id='context'>Context</p>")["context"]
    sizes = %{min_content: 20, max_content: 100, auto: 80}
    assert {:ok, value} = HtmlValidator.parse_css_size("calc-size(min-content, size + 10%)")
    assert {:ok, indefinite} = CssIntrinsicSizingValidator.resolve(value, nil, sizes, style)
    assert indefinite == 20
    assert {:ok, definite} = CssIntrinsicSizingValidator.resolve(value, 200, sizes, style)
    assert_in_delta definite, 40, 1.0e-10
    assert {:ok, value} = HtmlValidator.parse_css_size("calc-size(50%, size + 2pt)")
    assert {:ok, actual} = CssIntrinsicSizingValidator.resolve(value, nil, sizes, style)
    assert actual == 82
    assert {:ok, value} = HtmlValidator.parse_css_size("fit-content(50pt)")
    assert {:ok, 50.0} = CssIntrinsicSizingValidator.resolve(value, 200, sizes, style)
  end

  test "block intrinsic sizes use measured text rather than the containing width" do
    html =
      "<body style='margin:0;font-family:DejaVu Sans Mono;font-size:12pt;line-height:15pt'><div style='width:max-content;background:red'>AA BBBB</div><div style='width:min-content;background:blue'>AA BBBB</div><div style='width:calc-size(max-content, size / 2 + 2pt);background:green'>AA BBBB</div></body>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 200}, margin: 0)
    [body] = styled.children
    font = body.style.font_face

    for {color, expected} <- [
          {{1, 0, 0}, Font.text_width("AA BBBB", font, 12)},
          {{0, 0, 1}, Font.text_width("BBBB", font, 12)},
          {{0, 0.5019607843, 0}, Font.text_width("AA BBBB", font, 12) / 2 + 2}
        ] do
      box = Enum.find(layout.boxes, &(Map.get(&1, :fill_color) == color))
      assert box
      assert_in_delta box.width, expected, 0.02
    end
  end

  test "nested calculations clamp only their final result and accept related fit-content bases" do
    style = CssStyles.by_id("<p id='context'>Context</p>")["context"]
    sizes = %{min_content: 20, max_content: 100, auto: 80}

    for {source, expected} <- [
          {"calc-size(calc-size(auto, size - 100pt), size + 200pt)", 180},
          {"calc-size(-10pt, size + 20pt)", 10},
          {"calc-size(fit-content(50pt), size + 2pt)", 52},
          {"calc-size(any, -20pt)", 0}
        ] do
      assert {:ok, value} = HtmlValidator.parse_css_size(source)
      assert {:ok, actual} = CssIntrinsicSizingValidator.resolve(value, 200, sizes, style)
      assert_in_delta actual, expected, 1.0e-10
    end

    assert {:ok, value} = HtmlValidator.parse_css_size("fit-content(1rem)")
    assert CssIntrinsicSizingValidator.valid?(value)
    assert {:ok, actual} = CssIntrinsicSizingValidator.resolve(value, nil, sizes, style)
    assert actual == 20
  end

  test "block content heights, border-box widths and replaced ratios retain intrinsic meaning" do
    image =
      "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"

    html =
      "<body style='margin:0;font-family:DejaVu Sans Mono;font-size:12pt;line-height:15pt'><div style='width:100pt;height:calc-size(auto,size + 10pt);background:red'>AA</div><div style='box-sizing:border-box;width:max-content;padding:3pt;background:blue'>AA BBBB</div><img src='#{image}' style='display:block;width:min-content;height:30pt'><img src='#{image}' style='display:block;width:calc-size(max-content,size / 2)'><img src='#{image}' style='display:block;height:max-content'></body>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 200}, margin: 0)

    assert_in_delta Enum.find(layout.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0})).height,
                    25,
                    0.001

    [body] = styled.children
    expected = Font.text_width("AA BBBB", body.style.font_face, 12) + 6

    assert_in_delta Enum.find(layout.boxes, &(Map.get(&1, :fill_color) == {0, 0, 1})).width,
                    expected,
                    0.02

    [fixed_height, scaled, natural_height] = Enum.filter(layout.boxes, &(&1.type == :image))
    assert_in_delta fixed_height.width, 60, 0.001
    assert_in_delta fixed_height.height, 30, 0.001
    assert_in_delta scaled.width, 6, 0.001
    assert_in_delta scaled.height, 3, 0.001
    assert natural_height.height == 6
    assert natural_height.width == 12
  end

  test "fit-content limits and computed intrinsic metadata preserve font units and reject forged dictionaries" do
    style = CssStyles.by_id("<p id='context' style='font-size:20pt'>Context</p>")["context"]
    sizes = %{min_content: 10, max_content: 100, auto: 80}

    for {source, available, expected} <- [
          {"fit-content", 50, 50},
          {"fit-content(50%)", 100, 50},
          {"fit-content(50%)", nil, 100},
          {"fit-content(2em)", 200, 40},
          {"calc-size(1em, size + 1em)", 200, 40}
        ] do
      assert {:ok, expression} = HtmlValidator.parse_css_size(source)
      assert {:ok, frozen} = CssIntrinsicSizingValidator.compute(expression, style)
      assert CssIntrinsicSizingValidator.valid?(frozen)

      assert {:ok, ^frozen} =
               CssIntrinsicSizingValidator.compute(frozen, %{style | font_size: 10})

      assert {:ok, actual} =
               CssIntrinsicSizingValidator.resolve(frozen, available, sizes, %{
                 style
                 | font_size: 10
               })

      assert actual == expected
    end

    assert {:ok, expression} = HtmlValidator.parse_css_size("calc-size(auto, size + 2pt)")
    assert {:ok, nil} = CssIntrinsicSizingValidator.resolve(expression, nil, %{}, style)

    refute CssIntrinsicSizingValidator.valid?(
             {:css_intrinsic_size, {:computed, expression, %{"invented" => 2}}}
           )

    refute CssIntrinsicSizingValidator.valid?({:css_intrinsic_size, {:computed, expression, %{}}})
  end

  test "prepared percentage fit-content limits and intrinsic minimum constraints use their owning axes" do
    value = {:css_intrinsic_size, {:fit_content, {:percent, 0.5}}}
    assert CssIntrinsicSizingValidator.valid?(value)
    sizes = %{min_content: 10, max_content: 100, auto: 80}
    assert {:ok, 50.0} = CssIntrinsicSizingValidator.resolve(value, 100, sizes, %{})
    assert {:ok, 100} = CssIntrinsicSizingValidator.resolve(value, nil, sizes, %{})

    style =
      CssStyles.by_id(
        "<p id='size' style='width:20pt;min-width:calc-size(min-content,size + 2pt)'>Text</p>"
      )["size"]

    assert Layout.resolve_content_size(style, :width, 100, nil) == 20
  end
end
