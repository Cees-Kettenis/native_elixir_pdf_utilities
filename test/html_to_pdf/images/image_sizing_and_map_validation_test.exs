defmodule NativeElixirPdfUtilities.HtmlToPdf.ImageSizingAndMapValidationTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    Font,
    HtmlParser,
    Style,
    ResourceContext,
    ImageMapGeometry
  }

  alias NativeElixirPdfUtilities.Validators.{
    CssPositionValidator,
    CssSizingValidator,
    ResponsiveImageValidator,
    ImageMapValidator,
    HtmlValidator
  }

  @image "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"
  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, face} = Font.resolve("DejaVu Sans", 400, :normal, registry)
    %{style: %{font_face: face, font_size: 12.0, line_height: 15.0}}
  end

  test "position calculations retain typed zero, NaN, end offsets and invalid public diagnostics",
       %{style: style} do
    assert {:ok, {:css_position_math, "calc(0)"}} = CssPositionValidator.prepare_math("calc(0)")
    assert CssPositionValidator.valid_prepared?({:end, {:css_position_math, "calc(25%)"}})

    assert {:ok, 15.0} =
             CssPositionValidator.resolve({:end, {:css_position_math, "calc(25%)"}}, 20.0, style)

    for source <- ["calc(0)", "calc(NaN * 1px)"] do
      assert {:ok, zero} = CssPositionValidator.resolve({:css_position_math, source}, 20, style)
      assert zero == 0
    end

    for source <- ["calc(1)", "calc(1px + 1)", <<255>>] do
      refute CssPositionValidator.valid_prepared?({:css_position_math, source})

      assert {:error, {:invalid_layout, d}} =
               CssPositionValidator.resolve({:css_position_math, source}, 20, style)

      assert d.stage == :layout and d.source == inspect({:css_position_math, source})
    end

    refute CssPositionValidator.valid_prepared?({:css_position_math, <<255>>})

    assert {:error, {:invalid_layout, _}} =
             CssPositionValidator.resolve({:css_position_math, "calc(1)"}, 20, style)

    overflow = "calc(pow(1e9,32) * pow(1e9,32) * 1px)"
    assert :error = CssPositionValidator.prepare_math(overflow)

    assert {:error, {:invalid_layout, d}} =
             CssPositionValidator.resolve({:css_position_math, overflow}, 20, style)

    assert d.source == inspect({:css_position_math, overflow})
    assert CssPositionValidator.valid_prepared?(12)
    assert {:error, {:invalid_layout, d}} = CssPositionValidator.resolve(:invalid, 20, style)
    assert d.source == ":invalid"
    refute CssPositionValidator.valid_prepared?({:unknown, 1})

    assert {:error, {_reason, d}} =
             CssPositionValidator.math_environment(%{style | font_face: %{}}, 20)

    assert d.stage == :font and is_binary(d.message)
  end

  test "computed sizing metadata validates calculation context and nested minimum constraints",
       %{style: style} do
    assert :ok =
             CssSizingValidator.validate_root(%{
               width: {:min, [24.0, {:percent, 0.5}]},
               calculation_context: style
             })

    assert {:error, {:invalid_layout, _}} =
             CssSizingValidator.validate_root(%{calculation_context: %{}})

    assert {:error, {:invalid_layout, _}} = CssSizingValidator.validate_root(:invalid)

    assert {:error, {:invalid_layout, _}} =
             CssSizingValidator.validate_root(%{border_colors: %{top: :invalid}})

    refute CssSizingValidator.valid_calculation?({:css_size_math, "calc(1pt)"})
  end

  test "lazy image requests require an owner and measurement rejects forged or invalid geometry" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<img loading=lazy sizes=auto src='#{@image}'>")
    [node] = dom.children

    assert {:error, {:invalid_document, d}} =
             ResponsiveImageValidator.prepare_deferred(node, [], [])

    assert d.stage == :style and d.source == "img"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _owner ->
      assert {:ok, request, _} = ResponsiveImageValidator.prepare_deferred(node, [], [])
      assert {:ok, other, _} = ResponsiveImageValidator.prepare_deferred(node, [], [])
      assert request != other and ResponsiveImageValidator.valid_request?(other)

      box = %{
        type: :responsive_image,
        responsive_style: %{_responsive_image: request},
        x: 0,
        y: 1,
        width: 12,
        height: 6
      }

      assert {:ok, [%{boxes: [%{type: :flow_marker, height: 6, width: width}]}]} =
               ResponsiveImageValidator.measurement_boxes([%{boxes: [box]}])

      assert width == 0

      assert {:error, {:invalid_layout, d}} =
               ResponsiveImageValidator.measurement_boxes([%{boxes: [%{box | width: -1}]}])

      assert d.source == "img"
    end)

    assert ResponsiveImageValidator.source_size(" , ", %{
             width: 320.0,
             height: 240.0,
             density: 1.0,
             type: :print
           }) == 320.0
  end

  test "authored empty areas, nested maps, circles and clipped quadrilaterals retain ordered hit testing" do
    html =
      "<img src='#{@image}' usemap='#m' style='display:block;width:60pt;height:30pt'><map name=m><template><map name=inert><area shape=default href='https://example.com/inert'></map></template><span>text<area shape=circle coords='10,10,0'><area shape=poly coords='1,2'><area coords=''><area shape=circle coords='20,20,10' href='https://example.com/circle'></span></map>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    image = Enum.find(styled.children, &(&1.tag == "img"))
    model = image.style._image_map
    assert ImageMapValidator.valid_model?(model)
    assert :none = ImageMapValidator.hit_test(model, 1, 1, 80, 40)

    assert {:ok, {:uri, "https://example.com/circle"}} =
             ImageMapValidator.hit_test(model, 20, 20, 80, 40)

    viewport = %{x: 0, y: 0, width: 60, height: 30}
    [region] = ImageMapGeometry.regions(model, viewport, viewport)
    assert region.link_url == "https://example.com/circle"
    assert ImageMapValidator.valid_link_region?(region)
    assert :none = ImageMapValidator.lookup([%{size: {100, 100}, boxes: [region]}], 1, 80, 80)
    assert {:error, {:invalid_layout, d}} = ImageMapValidator.lookup([], 0, 0, 0)
    assert d.source == "map"

    refute ImageMapValidator.valid_model?(%{
             model
             | areas: [%{hd(model.areas) | shape: {:poly, [{1, 2}, {3, 4}, :invalid]}}]
           })

    refute ImageMapValidator.valid_model?(%{
             model
             | areas: [%{hd(model.areas) | shape: :invalid}]
           })

    refute ImageMapValidator.valid_model?(%{model | areas: [:invalid]})
    refute ImageMapValidator.valid_model?(:invalid)

    refute ImageMapValidator.valid_link_region?(%{
             region
             | link_quads: [[:invalid, {1, 1}, {2, 2}, {3, 3}]]
           })

    refute ImageMapValidator.valid_link_region?(%{region | x: :invalid})
    clipped = Map.put(region, :clip, %{x: 0, y: 0, width: 15, height: 15})

    assert {:ok, {:uri, "https://example.com/circle"}} =
             ImageMapValidator.lookup([%{size: {100, 100}, boxes: [clipped]}], 1, 14, 14)

    assert :none = ImageMapValidator.lookup([%{size: {100, 100}, boxes: [clipped]}], 1, 16, 16)
    refute ImageMapValidator.valid_link_region?(:invalid)
    clip = {0, 0, 20, 20}
    assert ImageMapGeometry.clip_quads([[], [{1, 1}], [{1, 1}, {2, 2}]], clip) == []
    assert [[_, _, _, _]] = ImageMapGeometry.clip_quads([[{1, 1}, {10, 1}, {1, 10}]], clip)

    assert length(ImageMapGeometry.clip_quads([[{-10, 10}, {10, -10}, {30, 10}, {10, 30}]], clip)) >
             1
  end
end
