defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgCascadeContractTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgViewport}
  alias NativeElixirPdfUtilities.Validators.{SvgStyleValidator, SvgViewportValidator}

  test "CSS wide keywords distinguish inherited paints from local opacity and geometry" do
    html =
      """
      <style>svg{--paint:blue} #one{fill:var(--paint);color:red}
      #two{fill:inherit;stroke:unset;opacity:inherit;width:inherit}
      #three{fill:initial;opacity:unset;width:unset}</style>
      <svg fill=green stroke=red opacity=.4 width=100 height=50>
      <rect id=one/><rect id=two/><rect id=three/>
      </svg>
      """
      |> String.replace("/>", " />")

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    [one, two, three] = Enum.filter(svg.children, &(&1.type == :element))
    assert one.style.svg_fill == {:color, {0, 0, 1}}
    assert two.style.svg_fill == {:color, {0, 0.5019607843, 0}}
    assert two.style.svg_stroke == {:color, {1, 0, 0}}
    assert two.style.svg_opacity == 0.4
    assert two.style.svg_width == {:user, 100.0}
    assert three.style.svg_fill == {:color, {0, 0, 0}}
    assert three.style.svg_opacity == 1.0
    assert three.style.svg_width == :auto
  end

  test "nested HTML uses the same CSS grammar while SVG text retains selected fonts and authored case" do
    html = """
    <svg font-size=20 style='text-transform:uppercase'><text>a<tspan font-size=12>b</tspan>c</text>
    <foreignObject width=40 height=20><div style='font-size:10px;text-transform:uppercase;width:20px'>mixed case</div></foreignObject>
    </svg>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    [text, foreign] = Enum.filter(svg.children, &(&1.type == :element))
    assert text.style.font_size == 15.0
    assert text.style.font_face.type == :embedded
    [a, span, c] = text.children
    assert {a.text, c.text} == {"a", "c"}
    assert span.style.font_size == 9.0
    [div] = foreign.children
    assert div.namespace == :html
    assert div.style.width == 15.0
    assert div.style.font_size == 7.5
    assert hd(div.children).text == "MIXED CASE"
  end

  test "hidden SVG descendants remain prepared for later validated reference expansion" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<svg style='display:none'><defs><rect id=a width=10 height=10 /></defs><g><use href='#a' /></g></svg>"
             )

    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    assert svg.style.display == :none
    assert length(svg.children) == 2
    assert hd(hd(svg.children).children).attributes["id"] == "a"
  end

  test "presentation properties preserve foreign attribute identity and reject unsupported CSS explicitly" do
    node = %{
      tag: "rect",
      attributes: %{"fill" => "red", "font-size" => "2em"},
      attribute_namespaces: %{"fill" => %{namespace: "urn:metadata"}}
    }

    assert {:ok, [{"font-size", "2em"}]} = SvgStyleValidator.presentation_declarations(node)
    assert {:ok, dom} = HtmlParser.parse_detailed("<svg style='filter:blur(2px)' />")
    assert {:error, {:invalid_css, d}} = Style.prepare_svg_detailed(dom)
    assert is_binary(d.message)

    for {property, value, expected} <- [
          {"clip-rule", "evenodd", :evenodd},
          {"visibility", "collapse", :collapse},
          {"stroke-opacity", "200%", 1.0},
          {"fill-opacity", "-2", 0.0},
          {"rx", "auto", :auto}
        ] do
      assert SvgStyleValidator.parse(property, value) == {:ok, expected}
    end

    for {property, value} <- [
          {"clip-rule", "bad"},
          {"x", :bad},
          {"r", "-1"},
          {"width", "1wrong"},
          {"unknown", "1"}
        ] do
      assert :error = SvgStyleValidator.parse(property, value)
    end

    assert :error = SvgStyleValidator.length(:bad)

    assert {:ok, dom} =
             HtmlParser.parse_detailed("<svg><rect style='fill:blue!important;fill:red' /></svg>")

    assert {:ok, %{children: [%{children: [rect]}]}} = Style.prepare_svg_detailed(dom)
    assert rect.style.svg_fill == {:color, {0, 0, 1}}
  end

  test "typed viewport input and computed transforms are bounded by the canonical geometry validator" do
    assert {:error, {:invalid_document, _}} =
             SvgViewport.measure(%{}, %{svg_width: {:points, -1.0}}, %{width: 1.0, height: nil})

    assert {:error, {:invalid_document, _}} =
             SvgViewport.measure(%{}, %{svg_width: :invalid}, %{width: 1.0, height: nil})

    assert {:error, {:invalid_document, _}} = SvgViewportValidator.view_box(:bad)
    assert {:error, {:invalid_document, _}} = SvgViewportValidator.aspect_ratio(:bad)

    assert {:error, {:invalid_document, _}} =
             SvgViewport.measure(%{}, %{font_size: "bad"}, %{width: 1.0, height: nil})

    assert {:error, {:invalid_document, _}} =
             SvgViewportValidator.measured_geometry(%{
               width: 1.0,
               height: 1.0,
               x: 0,
               y: 0,
               ctm: {1.0e20, 0, 0, 1, 0, 0}
             })

    assert {:error, {:invalid_document, _}} =
             SvgViewport.measure(
               %{
                 "width" => "1e8",
                 "height" => "1",
                 "viewBox" => "0 0 1e-10 1",
                 "preserveAspectRatio" => "none"
               },
               %{},
               %{width: 100.0, height: nil}
             )

    for overflow <- [:hidden, :auto, :scroll, :visible] do
      assert {:ok, m} =
               SvgViewport.measure(%{"width" => "1", "height" => "2"}, %{overflow: overflow}, %{
                 width: 100.0,
                 height: nil
               })

      assert is_nil(m.clip) == (overflow == :visible)
    end
  end

  test "prepared body identity survives the host cascade and geometry passes the rendering validator" do
    assert {:ok, %{children: [body]} = dom} =
             HtmlParser.parse_detailed("<body><svg hidden popover width=10 height=10 /></body>")

    assert {:ok, %{children: [prepared]}} =
             Style.prepare_svg_detailed(dom)

    assert prepared.source == body.source
    assert prepared.source.text == "<body>"
    assert hd(prepared.children).style.display == :inline

    assert :ok =
             NativeElixirPdfUtilities.Validators.ForeignContentValidator.validate_render_support(
               hd(body.children),
               true
             )
  end

  test "absolute SVG units and CSS auto override retain their validated meanings" do
    for {unit, points} <- [{"px", 0.75}, {"pt", 1.0}, {"mm", 72.0 / 25.4}, {"cm", 72.0 / 2.54}] do
      assert {:ok, {:points, actual}} = SvgStyleValidator.length("1" <> unit)
      assert_in_delta actual, points, 1.0e-12
    end

    assert :error = SvgStyleValidator.length("1e9in")
    assert {:ok, :nonzero} = SvgStyleValidator.parse("fill-rule", "nonzero")

    assert {:ok, measured} =
             SvgViewport.measure(%{"width" => "10", "height" => "1"}, %{svg_width: :auto}, %{
               width: 100.0,
               height: nil
             })

    assert measured.width == 225.0

    assert {:error, {:invalid_document, diagnostic}} =
             SvgViewport.measure(%{"width" => "1e9", "height" => "1e-308"}, %{}, %{
               width: 100.0,
               height: nil
             })

    assert diagnostic.message =~ "arithmetic"
  end

  test "nested auto axes use the parent viewport independently of viewBox aspect ratio" do
    for {attrs, expected} <- [
          {%{"height" => "20", "viewBox" => "0 0 1 1"}, {200.0, 20.0}},
          {%{"width" => "20", "viewBox" => "0 0 1 1"}, {20.0, 100.0}}
        ] do
      assert {:ok, m} =
               SvgViewport.measure(attrs, %{}, %{width: 200.0, height: 100.0, nested: true})

      assert {m.width, m.height} == expected
      assert m.units == :user
      assert m.baseline_from_bottom == 0.0
    end

    assert {:error, {:invalid_document, _}} =
             SvgViewport.measure(%{}, %{}, %{width: 200.0, height: nil, nested: true})

    assert {:ok, m} =
             SvgViewport.measure(%{"width" => "50%", "viewBox" => "0 0 2 1"}, %{}, %{
               width: 100.0,
               height: nil
             })

    assert m.width == 50.0
    assert m.intrinsic_widths == %{min: 225.0, max: 225.0}
    assert m.units == :points
  end
end
