defmodule NativeElixirPdfUtilities.HtmlToPdf.IntegrationWave8Test do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  test "one document cascade retains fieldset defaults, gauges, SVG paint and MathML roots" do
    html = """
    <style>
    fieldset{--size:12pt;font-size:var(--size)}
    meter{appearance:none;accent-color:red;vertical-align:1pt}
    rect{fill:red}math{math-style:normal}
    </style>
    <fieldset><legend>Legend</legend><p>Content</p></fieldset>
    <meter value=.5></meter><svg width=24 height=12><rect width=24 height=12 /></svg>
    <math><msqrt><mn>9</mn></msqrt></math>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [fieldset, meter, svg, math]}} = Style.compute_detailed(dom)
    assert fieldset.style.font_size == 12
    assert fieldset.style.padding.left == 9
    assert fieldset.style.padding.top > 4
    assert meter.style.appearance == :none and meter.style.accent_color == {1, 0, 0}
    assert meter.style.vertical_align == 1.0
    assert svg.style.svg_scene.source =~ "rgb(255,0,0)"
    assert math.style.math_style == :normal

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    for word <- ["Legend", "Content", "9"], do: assert(text =~ word)
    assert pdf =~ "/Subtype /Form"
    assert {:ok, []} = Forms.fields(pdf)
  end

  test "source contexts keep links and SVG policy while MathML font work remains explicit" do
    html = """
    <a id=target href='#target'>Link</a><img src='missing-source-image.png'>
    <iframe src='missing-source-frame.html'></iframe>
    <svg width=24 height=12><rect width=24 height=12 fill=red /></svg>
    <math><msqrt><mn>9</mn></msqrt></math>
    """

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    token = make_ref()

    assert {:ok, result} =
             Style.with_source_style_contexts(
               dom,
               [],
               NativeElixirPdfUtilities.Validators.HtmlValidator.new_css_budget(),
               fn node, pseudo, context ->
                 send(self(), {token, Map.get(node, :tag), pseudo, context})
                 :ok
               end
             )

    assert result.source_obligations.resource_references == :unresolved
    assert result.source_obligations.math_font_contexts == :unresolved
    assert_received {^token, "a", nil, %{status: :resolved, style: anchor}}
    assert anchor.font_face == nil
    refute Map.has_key?(anchor, :link_destination)
    assert_received {^token, "iframe", nil, %{status: :resolved, style: frame}}
    refute Map.has_key?(frame, :embedded_document)
    assert_received {^token, "rect", nil, %{status: :resolved, style: rect}}
    assert rect.svg_fill == {:color, {1.0, 0.0, 0.0}}
    assert_received {^token, "math", nil, %{status: :unresolved, reason: :math_font_context}}
    assert_received {^token, "mn", nil, %{status: :unresolved, reason: :math_font_context}}
  end

  test "configured invalid UTF-8 CSS returns the shared source error before media cache scanning" do
    assert {:error, {:invalid_css, diagnostic}} =
             HtmlToPdf.render("<p>Visible</p>", stylesheets: [{:css, <<255>>}])

    assert diagnostic.stage == :css and diagnostic.reason == :invalid_css
    assert diagnostic.message =~ "UTF-8"
  end
end
