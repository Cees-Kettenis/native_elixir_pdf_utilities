defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPreparationTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgViewport, Layout}
  alias NativeElixirPdfUtilities.Validators.{SvgStyleValidator, SvgViewportValidator}
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "preparation reuses host cascade, specificity, source order and currentColor inheritance" do
    html =
      "<style>div{color:blue;fill:red} svg rect{fill:yellow} .paint{fill:green}\n#ink{fill:currentColor} #ink{color:red} .paint{stroke:blue!important}</style>\n<div><svg width=120 height=40 fill=purple><g fill=currentColor color=green>\n<rect id=ink class=paint fill=black style='fill:currentColor;stroke:red' />\n<circle fill=none /><rect class=paint />\n</g></svg></div>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, %{type: :svg_prepared_document, children: [host]}} =
             Style.prepare_svg_detailed(dom)

    [svg] = host.children
    assert svg.namespace == :svg
    assert svg.attributes["width"] == "120"
    assert svg.style.svg_fill == {:color, {0.5019607843137255, 0, 0.5019607843137255}}
    [group] = Enum.filter(svg.children, &(&1.type == :element))
    [ink, circle, rect] = Enum.filter(group.children, &(&1.type == :element))
    assert group.style.svg_fill == :current_color
    assert ink.style.svg_fill == :current_color
    assert ink.style.color == {1, 0, 0}
    assert ink.style.svg_stroke == {:color, {0, 0, 1}}
    assert rect.style.svg_fill == {:color, {0, 0.5019607843, 0}}
    assert circle.style.svg_fill == :none

    assert {:error, :invalid_layout} =
             Layout.layout(%{type: :svg_prepared_document, children: []})

    assert {:ok, _pdf} = HtmlToPdf.render(html)
  end

  test "prepared SVG retains source identities, namespace attributes and authored text boundaries" do
    assert {:ok, %{children: [svg]} = dom} =
             HtmlParser.parse_detailed(
               "<svg><text style='text-transform:uppercase'>a<tspan>b</tspan>c</text></svg>"
             )

    forged =
      Map.merge(svg, %{
        node_id: "s",
        document_id: "d",
        tree_scope_id: "t",
        parent_id: nil,
        source: "authored"
      })

    assert {:error,
            {:invalid_document, %{module: NativeElixirPdfUtilities.Validators.HtmlTreeValidator}}} =
             Style.prepare_svg_detailed(%{type: :document, children: [forged]})

    assert {:ok, %{children: [prepared]}} = Style.prepare_svg_detailed(dom)

    assert Map.take(prepared, ~w(node_id document_id tree_scope_id parent_id source)a) ==
             Map.take(svg, ~w(node_id document_id tree_scope_id parent_id source)a)

    [text] = prepared.children
    assert text.namespace == :svg
    assert Enum.map(text.children, &Map.get(&1, :text)) == ["a", nil, "c"]
    assert hd(Enum.at(text.children, 1).children).text == "b"
    assert text.attribute_namespaces == %{}
  end

  test "visibility is inherited while opacity is local, and geometry attributes apply only to eligible elements" do
    html =
      "<svg visibility=hidden opacity='50%' width=100 height=50><g width=10 opacity=2><rect visibility=visible x=-2 width=20 height=0 /></g></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    [group] = svg.children
    [rect] = group.children
    assert svg.style.svg_opacity == 0.5
    assert group.style.svg_opacity == 1.0
    assert group.style.svg_visibility == :hidden
    assert group.style.svg_width == :auto
    assert rect.style.svg_visibility == :visible
    assert rect.style.svg_x == {:user, -2.0}
    assert rect.style.svg_height == {:user, +0.0}
    assert {:ok, {:user, 3.0}} = SvgStyleValidator.parse("stroke-width", "3")
  end

  test "style failure diagnostics remain validator owned" do
    for declaration <- [
          "fill:url()",
          "stroke-width:-1",
          "opacity:bad",
          "fill-rule:wrong",
          "visibility:maybe"
        ] do
      assert {:ok, dom} = HtmlParser.parse_detailed("<svg style='#{declaration}' />")
      assert {:error, {:invalid_css, diagnostic}} = Style.prepare_svg_detailed(dom)
      assert diagnostic.stage == :css
      assert diagnostic.module == SvgStyleValidator
      assert diagnostic.operation == :prepare
      assert diagnostic.source != ""
    end

    assert {:ok, dom} = HtmlParser.parse_detailed("<svg fill='red!important' />")
    assert {:error, {:invalid_css, %{message: message}}} = Style.prepare_svg_detailed(dom)
    assert message =~ "presentation attributes"

    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          namespace: :svg,
          tag: "svg",
          attributes: %{"onload" => "x"},
          children: []
        }
      ]
    }

    assert {:error, {:unsupported_html, _}} = Style.prepare_svg_detailed(dom)

    assert {:ok, %{type: :svg_prepared_document, children: [%{tag: "math"}]}} =
             Style.prepare_svg_detailed(%{
               type: :document,
               children: [
                 %{type: :element, namespace: :mathml, tag: "math", attributes: %{}, children: []}
               ]
             })

    assert {:error, {:invalid_document, _}} = Style.prepare_svg_detailed(:bad)
  end

  test "outer geometry separates natural dimensions, percentages, default and zero sizes" do
    host = %{width: 300.0, height: nil}

    for {attributes, expected} <- [
          {%{}, {225.0, 112.5}},
          {%{"width" => "120", "height" => "60"}, {90.0, 45.0}},
          {%{"width" => "2in", "height" => "1in"}, {144.0, 72.0}},
          {%{"width" => "50%", "height" => "50%", "viewBox" => "0 0 100 50"}, {150.0, 75.0}},
          {%{"width" => "80", "viewBox" => "0 0 20 10"}, {60.0, 30.0}},
          {%{"height" => "40", "viewBox" => "0 0 20 10"}, {60.0, 30.0}},
          {%{"width" => "0", "height" => "20"}, {+0.0, 15.0}},
          {%{"viewBox" => "0 0 10 20"}, {56.25, 112.5}},
          {%{"height" => "40"}, {225.0, 30.0}},
          {%{"width" => "80"}, {60.0, 112.5}}
        ] do
      assert {:ok, measured} = SvgViewport.measure(attributes, %{}, host)
      assert {measured.width, measured.height} == expected
      assert measured.baseline == measured.height
      assert measured.layout_extent == {+0.0, +0.0, measured.width, measured.height}
      assert measured.paint? == (measured.width > 0 and measured.height > 0)
    end

    assert {:ok, percentage} =
             SvgViewport.measure(%{"width" => "50%", "height" => "50%"}, %{}, %{
               width: 300.0,
               height: 100.0
             })

    assert percentage.intrinsic_width == nil
    assert percentage.intrinsic_height == nil
    assert {percentage.width, percentage.height} == {150.0, 50.0}

    assert {:ok, natural} =
             SvgViewport.measure(
               %{"width" => "40", "height" => "20", "viewBox" => "0 0 1 1"},
               %{},
               host
             )

    assert natural.intrinsic_ratio == 2.0
  end

  test "viewBox maps meet, slice and none with every alignment and viewport clipping" do
    for {x, xf} <- [{"Min", +0.0}, {"Mid", 0.5}, {"Max", 1.0}],
        {y, yf} <- [{"Min", +0.0}, {"Mid", 0.5}, {"Max", 1.0}],
        mode <- ["meet", "slice"] do
      attrs = %{
        "width" => "200",
        "height" => "100",
        "viewBox" => "10 20 100 100",
        "preserveAspectRatio" => "x#{x}Y#{y} #{mode}"
      }

      assert {:ok, m} = SvgViewport.measure(attrs, %{}, %{width: 300.0, height: 200.0})

      scale =
        if mode == "meet" do
          0.75
        else
          1.5
        end

      assert m.ctm ==
               {scale, +0.0, +0.0, scale, -10 * scale + (150 - 100 * scale) * xf,
                -20 * scale + (75 - 100 * scale) * yf}
    end

    assert {:ok, none} =
             SvgViewport.measure(
               %{
                 "width" => "200",
                 "height" => "100",
                 "viewBox" => "10,20,100,100",
                 "preserveAspectRatio" => "none slice"
               },
               %{overflow: :visible},
               %{width: 300.0, height: nil}
             )

    assert none.ctm == {1.5, +0.0, +0.0, 0.75, -15.0, -15.0}
    assert none.clip == nil

    assert {:ok, zero} =
             SvgViewport.measure(
               %{"width" => "20", "height" => "10", "viewBox" => "0 0 0 10"},
               %{},
               %{width: 30.0, height: nil}
             )

    refute zero.paint?
  end

  test "nested percentages use nearest SVG user viewport and font relative lengths use selected host metrics" do
    assert {:ok, m} =
             SvgViewport.measure(
               %{"x" => "10%", "y" => "-10", "width" => "50%", "height" => "50%"},
               %{},
               %{width: 200.0, height: 100.0, nested: true}
             )

    assert m.layout_extent == {+0.0, +0.0, 100.0, 50.0}
    assert m.ctm == {1.0, +0.0, +0.0, 1.0, 20.0, -10.0}
    assert m.clip == {20.0, -10.0, 100.0, 50.0}

    assert {:ok, auto} =
             SvgViewport.measure(%{}, %{}, %{width: 200.0, height: 100.0, nested: true})

    assert auto.user_viewport == {200.0, 100.0}

    for nested <- [false, true] do
      assert {:ok, font} =
               SvgViewport.measure(
                 %{"width" => "2em", "height" => "1rem"},
                 %{font_size: 15.0, _root_font_size: 12.0},
                 %{width: 200.0, height: 100.0, nested: nested}
               )

      assert {font.width, font.height} ==
               (if nested do
                  {40.0, 16.0}
                else
                  {30.0, 12.0}
                end)

      assert {:ok, physical} =
               SvgViewport.measure(%{"width" => "1pc", "height" => "1q"}, %{}, %{
                 width: 200.0,
                 height: 100.0,
                 nested: nested
               })

      assert physical.width ==
               (if nested do
                  16.0
                else
                  12.0
                end)
    end

    assert {:ok, css} =
             SvgViewport.measure(
               %{"width" => "100"},
               %{svg_width: {:points, 30.0}, svg_height: {:percent, 0.5}},
               %{width: 100.0, height: 100.0}
             )

    assert {css.width, css.height} == {30.0, 50.0}
  end

  test "zero inherited font metrics preserve fixed outer and nested SVG viewports" do
    for style <- [
          %{font_size: 0, _root_font_size: 12},
          %{font_size: 12, _root_font_size: 0},
          %{font_size: 0, _root_font_size: 0}
        ],
        nested <- [false, true] do
      assert {:ok, measured} =
               SvgViewport.measure(
                 %{"width" => "40", "height" => "20", "viewBox" => "0 0 40 20"},
                 style,
                 %{width: 200, height: 100, nested: nested}
               )

      assert {measured.width, measured.height} ==
               if(nested, do: {40, 20}, else: {30, 15})

      assert measured.paint?
      assert measured.user_viewport == {40, 20}
    end
  end

  test "zero em and rem viewport dimensions retain ordinary zero-area behavior" do
    for {style, expected} <- [
          {%{font_size: 0, _root_font_size: 12}, {0, 12}},
          {%{font_size: 12, _root_font_size: 0}, {24, 0}},
          {%{font_size: 0, _root_font_size: 0}, {0, 0}}
        ],
        nested <- [false, true] do
      assert {:ok, measured} =
               SvgViewport.measure(
                 %{"x" => "1em", "y" => "1rem", "width" => "2em", "height" => "1rem"},
                 style,
                 %{width: 200, height: 100, nested: nested}
               )

      {width, height} = expected
      scale = if nested, do: 1 / 0.75, else: 1
      assert_in_delta measured.width, width * scale, 0.0001
      assert_in_delta measured.height, height * scale, 0.0001
      refute measured.paint?
      assert measured.layout_extent == {0, 0, measured.width, measured.height}
    end
  end

  test "public rendering accepts fixed SVG shapes and positive text overrides under zero fonts" do
    for html <- [
          "<div style='font-size:0'><svg width='40' height='20'><rect width='40' height='20' fill='red'/></svg></div>",
          "<html style='font-size:0'><body><svg width='40' height='20'><rect width='40' height='20' fill='red'/></svg></body></html>",
          "<div style='font-size:0'><svg width='120' height='24'><text x='0' y='18' font-size='16'>Visible</text></svg></div>"
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)
      assert length(context.pages) == 1

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)
    end
  end

  @tag :browser_parity
  test "zero-font SVG shapes text overrides and relative dimensions match Chromium" do
    fixture = Paths.fixture("html_to_pdf/browser_parity/inline_svg_zero_font.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/inline_svg_zero_font",
        regions: [
          {"zero root font", {9, 9, 270, 42}},
          {"zero inherited font", {9, 51, 270, 42}},
          {"zero relative geometry", {9, 135, 270, 42}}
        ],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  test "invalid viewports and grammar return actionable diagnostics without allocating" do
    for attrs <- [
          %{"viewBox" => "0 0 -1 2"},
          %{"viewBox" => "0,,0,1,2"},
          %{"viewBox" => ",0 0 1 2"},
          %{"viewBox" => "0 0 1 2 trailing"},
          %{"viewBox" => "0 0 1e308 2"},
          %{"preserveAspectRatio" => "bad"},
          %{"width" => "-1"},
          %{"height" => "1vh"},
          %{"width" => "1e308in"}
        ] do
      assert {:error, {:invalid_document, d}} =
               SvgViewport.measure(attrs, %{}, %{width: 100.0, height: nil})

      assert d.stage == :layout
      assert d.source == "svg viewport"
      assert d.operation == :measure
    end

    for host <- [
          :bad,
          %{width: -1, height: nil},
          %{width: 1, height: -1},
          %{width: 1, height: nil, nested: :bad}
        ] do
      assert {:error, {:invalid_document, _}} = SvgViewport.measure(%{}, %{}, host)
    end

    for style <- [
          %{font_size: -1},
          %{_root_font_size: -1},
          %{font_size: :bad},
          %{_root_font_size: :bad}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               SvgViewport.measure(%{}, style, %{width: 1, height: nil})

      assert diagnostic.stage == :layout
      assert diagnostic.source == "svg viewport"
      assert diagnostic.module == SvgViewportValidator
      assert diagnostic.operation == :measure
      assert diagnostic.reason == :invalid_document
      assert diagnostic.message =~ "nonnegative font sizes"
    end

    assert {:error, {:invalid_document, _}} =
             SvgViewport.measure(%{}, %{overflow: :bad}, %{width: 1, height: nil})

    assert {:ok, {+0.0, +0.0, 1.0, 2.0}} = SvgViewportValidator.view_box("0. 0. 1. 2.")
    assert {:ok, {0.5, 0.5, :meet}} = SvgViewportValidator.aspect_ratio("defer xMidYMid")
  end
end
