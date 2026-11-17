defmodule NativeElixirPdfUtilities.HtmlToPdf.BoxPaintTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, Style}
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  alias NativeElixirPdfUtilities.Validators.{
    CssBorderImageValidator,
    CssBoxPaintValidator
  }

  test "nine border image patches keep corner colors, independent edges, center fill and outsets" do
    png = PngFixture.build(12, 12, 2, 8, 0, fn x, y -> [x * 20, y * 20, 0] end)
    uri = "data:image/png;base64," <> Base.encode64(png)

    html =
      "<div style='width:60pt;height:30pt;border:3pt solid black;border-image:url(#{uri}) 4 fill / 3pt 6pt / 2pt stretch'>Nine</div>"

    {_, layout} = layout(html)
    patches = Enum.filter(layout.boxes, &(Map.get(&1, :css_paint_role) == :border_image))
    assert length(patches) == 9
    assert Enum.map(patches, & &1.clip.width) |> Enum.uniq() |> Enum.sort() == [6.0, 58.0]
    assert Enum.map(patches, & &1.clip.height) |> Enum.uniq() |> Enum.sort() == [3.0, 34.0]
    assert Enum.min(Enum.map(patches, & &1.clip.x)) == -2

    assert Enum.uniq(Enum.map(patches, & &1.image.data)) == [
             for(y <- 0..11, x <- 0..11, into: <<>>, do: <<x * 20, y * 20, 0>>)
           ]

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Nine"
  end

  test "border images handle repeat round space auto widths overlapping slices and a resetting border shorthand" do
    for repetition <- ["repeat", "round space", "space repeat"] do
      html =
        "<div style='width:70pt;height:35pt;border:4pt solid black;border-image:linear-gradient(to right,red,blue) 20% 30% / auto 2 / 1 #{repetition}'></div>"

      {_, layout} = layout(html)
      assert Enum.count(layout.boxes, &(Map.get(&1, :css_paint_role) == :border_image)) >= 8
      assert {:ok, _} = HtmlToPdf.render(html, system_font_discovery: false)
    end

    {styled, _} =
      layout(
        "<div style='border-image:image(red) 1 / 8pt;border:1pt solid blue;width:20pt;height:20pt'></div>"
      )

    assert Map.get(hd(styled.children).style, :border_image_source) == nil

    {_, layout} =
      layout(
        "<div style='width:10pt;height:10pt;border:8pt solid black;border-image:image(red) 100% / 20 round'></div>"
      )

    assert Enum.all?(
             Enum.filter(layout.boxes, &(Map.get(&1, :css_paint_role) == :border_image)),
             &(&1.clip.width > 0)
           )
  end

  test "outlines leave source layout dimensions unchanged and paint shadows in back-to-front order" do
    html =
      "<div style='width:60pt;height:30pt;background:white;border:2pt solid black;border-radius:8pt / 4pt;outline:3pt dashed blue;outline-offset:2pt;box-shadow:3pt 4pt 2pt 1pt red,inset 2pt 3pt blue'>Paint</div>"

    {_, layout} = layout(html)
    assert Enum.count(layout.boxes, &(Map.get(&1, :css_paint_role) == :box_shadow)) == 2
    [outline] = Enum.filter(layout.boxes, &(Map.get(&1, :css_paint_role) == :css_outline))
    assert outline.width == 73 and outline.height == 43
    assert outline.border_radii.top_left == {13.0, 9.0}
    assert {:ok, _} = Pagination.paginate(layout)
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Paint"

    for source <- [
          "outline:auto",
          "outline:0 solid red",
          "outline:solid transparent",
          "outline:double;outline-offset:-50pt",
          "box-shadow:none",
          "box-shadow:0 0 transparent",
          "box-shadow:0 0 0 -50pt red",
          "box-shadow:0 0 0 5pt blue inset"
        ] do
      assert {:ok, _} =
               HtmlToPdf.render("<div style='width:20pt;height:15pt;#{source}'>P</div>",
                 system_font_discovery: false
               )
    end
  end

  test "text fill and stroke use independent colors and alpha while preserving logical extraction" do
    html =
      "<p style='color:red;-webkit-text-fill-color:rgba(0,255,0,.4);-webkit-text-stroke:1px rgba(0,0,255,.7)'>Stroke text</p>"

    {_, layout} = layout(html)
    text_boxes = Enum.filter(layout.boxes, &(&1.type == :text))

    assert Enum.all?(
             text_boxes,
             &(&1.color == {0.0, 1.0, 0.0, 0.4} and &1.text_stroke_color == {0.0, 0.0, 1.0, 0.7} and
                 &1.text_stroke_width == 0.75)
           )

    assert {:ok, pdf} =
             HtmlToPdf.render(html, system_font_discovery: false, compress_streams: false)

    assert pdf =~ "2 Tr"
    assert pdf =~ "0 0 1 RG"
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Stroke text"

    for declaration <- [
          "-webkit-text-fill-color:transparent;-webkit-text-stroke:thin",
          "-webkit-text-stroke:blue",
          "-webkit-text-stroke-width:0",
          "-webkit-text-fill-color:currentcolor;-webkit-text-stroke:1pt currentcolor"
        ] do
      assert {:ok, _} =
               HtmlToPdf.render("<p style='#{declaration}'>Text</p>",
                 system_font_discovery: false
               )
    end
  end

  test "invalid paint sources and forged prepared structures return diagnostics" do
    style = %{color: {0, 0, 0}, font_size: 12.0}

    for {property, source} <- [
          {"outline", "hidden"},
          {"outline", "red blue"},
          {"outline-width", "-1pt"},
          {"outline-style", "hidden"},
          {"box-shadow", "inset inset 1px 2px"},
          {"box-shadow", "1px"},
          {"box-shadow", "0 0 -1px"},
          {"box-shadow", "0 0 red blue"},
          {"-webkit-text-stroke", "red blue"},
          {"-webkit-text-stroke", "1px 2px 3px"}
        ] do
      refute match?({:ok, _}, CssBoxPaintValidator.prepare(style, property, source)),
             "#{property}:#{source}"
    end

    for {property, source} <- [
          {"border-image", ""},
          {"border-image", "red / 3 / 4 / 5"},
          {"border-image", "url(a) url(b)"},
          {"border-image-slice", "fill fill 1"},
          {"border-image-width", "-1"},
          {"border-image-outset", "10%"},
          {"border-image-repeat", "repeat repeat repeat"}
        ] do
      refute match?({:ok, _}, CssBorderImageValidator.prepare(style, property, source)),
             "#{property}:#{source}"
    end

    for fields <- [
          %{box_shadow: [%{x: 0}]},
          %{outline_offset: "bad"},
          %{text_stroke_width: -1},
          %{text_fill_color: :bad}
        ] do
      assert {:error, {:invalid_layout, %{stage: :layout, source: "div", message: message}}} =
               CssBoxPaintValidator.validate_style(fields, "div")

      assert message =~ "box paint"
    end

    for fields <- [
          %{border_image_width: %{top: :bad}},
          %{border_image_slice: %{edges: %{}, fill: false}},
          %{border_image_repeat: {:bad, :repeat}},
          %{border_image_source: :bad}
        ] do
      assert {:error, {:invalid_layout, %{stage: :layout, source: "div"}}} =
               CssBorderImageValidator.validate_style(fields, "div")
    end

    refute CssBoxPaintValidator.valid_text_box?(%{
             text_stroke_width: -1,
             text_stroke_color: {0, 0, 0}
           })

    refute CssBoxPaintValidator.valid_text_box?(%{text_stroke_color: {0, 0, 0}})
    assert :error == CssBoxPaintValidator.prepare(style, "other", "none")
  end

  test "owner property registries remain admitted by the shared cascade" do
    alias NativeElixirPdfUtilities.Validators.{
      CssBackgroundValidator,
      CssBorderRadiusValidator,
      CssCascadeValidator
    }

    for owner <- [
          CssBoxPaintValidator,
          CssBorderImageValidator,
          CssBackgroundValidator,
          CssBorderRadiusValidator
        ],
        property <- owner.properties(),
        do: assert(CssCascadeValidator.supported_property?(property), property)

    for {property, value} <- [
          {"outline-color", "RED"},
          {"outline-style", "SOLID"},
          {"box-shadow", "0 0 BLUE INSET"},
          {"-webkit-text-stroke-color", "BLUE"}
        ] do
      assert {:ok, _} = CssBoxPaintValidator.prepare(%{color: {0, 0, 0}}, property, value)
    end

    assert {:ok, _} = CssBorderImageValidator.prepare(%{}, "border-image-repeat", "ROUND SPACE")
    assert {:ok, _} = CssBorderImageValidator.prepare(%{}, "border-image", "20 / / 2 repeat")
    assert {:ok, _} = CssBorderImageValidator.prepare(%{}, "border-image", "url(a) repeat")

    for value <- ["1 2 3", "1 2 3 4"] do
      assert {:ok, _} = CssBorderImageValidator.prepare(%{}, "border-image-slice", value)
    end

    assert :error = CssBorderImageValidator.prepare(%{}, "other", "none")

    assert {:error, _} =
             NativeElixirPdfUtilities.Validators.CssBorderRadiusValidator.validate_style(
               %{border_radii: :bad},
               "div"
             )
  end

  test "blur spread and synthetic oblique text retain paint and extraction without extra fonts" do
    alias NativeElixirPdfUtilities.TestSupport.Paths

    html =
      "<div style='width:30pt;height:20pt;border-radius:1pt / 2pt;box-shadow:0 0 1pt 6pt rgba(0,0,0,.4)'><span style='font-family:PaintSynthetic;font-style:oblique 11deg;-webkit-text-fill-color:transparent;-webkit-text-stroke:thin black'>Oblique</span><span style='font-family:PaintSynthetic;font-style:oblique 11deg'>Shear</span></div>"

    opts = [
      system_font_discovery: false,
      fonts: [
        %{
          family: "PaintSynthetic",
          path: Paths.project("priv/fonts/dejavu/DejaVuSans.ttf"),
          weight: 400,
          style: :normal
        }
      ]
    ]

    assert {:ok, pdf} = HtmlToPdf.render(html, opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Oblique"
    assert text =~ "Shear"
  end

  test "overlapping slices retain all four full corner regions while discarding empty edges" do
    png = PngFixture.build(4, 4, 2, 8, 0, fn x, y -> [x * 80, y * 80, 0] end)
    uri = "data:image/png;base64," <> Base.encode64(png)

    {_, layout} =
      layout("<div style='width:40pt;height:20pt;border-image:url(#{uri}) 75% / 50%'></div>")

    patches = Enum.filter(layout.boxes, &(Map.get(&1, :css_paint_role) == :border_image))
    assert length(patches) == 4
    top_right = Enum.find(patches, &(&1.clip.x == 20 and &1.clip.y == 140))
    assert_in_delta top_right.x, 20 - 20 / 3, 0.000001
    assert_in_delta top_right.width, 80 / 3, 0.000001
  end

  test "inline decorations and image borders share continuous strips without requiring a background image" do
    png = PngFixture.build(4, 4, 2, 8, 0, fn x, y -> [x * 60, y * 60, 0] end)

    for css <- [
          "outline:1pt solid black;box-shadow:1pt 1pt 0 black",
          "border:2pt solid transparent;border-image:url(tile) 1 / auto / 0 repeat;border-radius:6pt / 2pt",
          "border:2pt solid transparent;border-image:url(tile) 0 1 1 0 fill / 0 3pt 3pt 0 repeat",
          "border:2pt solid transparent;border-image:url(tile) 0 fill / 2pt repeat"
        ] do
      html =
        "<p style='width:70pt;margin:0'><span style='#{css}'>Paint continues over several lines</span></p>"

      {_, layout} = layout(html, assets: %{"tile" => {:bytes, png}})

      assert Enum.any?(
               layout.boxes,
               &(Map.get(&1, :css_paint_role) in [:css_outline, :border_image])
             )

      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 assets: %{"tile" => {:bytes, png}},
                 system_font_discovery: false
               )

      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Paint"
    end

    assert {:ok, _} =
             HtmlToPdf.render(
               "<p style='-webkit-text-stroke:1px transparent'>Transparent stroke</p>",
               system_font_discovery: false
             )

    assert :error = CssBorderImageValidator.prepare(%{}, "border-image", "20 / bad")

    assert {:error, {:invalid_layout, _}} =
             CssBorderImageValidator.validate_style(%{border_image_repeat: :bad}, "div")

    assert {:error, {:invalid_layout, _}} =
             NativeElixirPdfUtilities.Validators.CssBorderRadiusValidator.validate_style(
               %{
                 border_radii: %{
                   top_left: :bad,
                   top_right: {0, 0},
                   bottom_left: {0, 0},
                   bottom_right: {0, 0}
                 }
               },
               "div"
             )
  end

  test "unapplied corner declarations retain parsed physical pairs without an element default" do
    alias NativeElixirPdfUtilities.HtmlToPdf.CssParser

    alias NativeElixirPdfUtilities.Validators.{
      HtmlCssSourceValidator,
      HtmlValidator,
      CssValueValidator
    }

    source = "border-radius:3pt 4pt / 1pt 2pt"

    assert {:ok, [record]} =
             CssParser.prepare_source_declarations(source, HtmlCssSourceValidator.local(source))

    assert {:ok, %{kind: :border_radii, properties: %{border_radii: %{top_left: {3.0, 1.0}}}}} =
             CssValueValidator.prepare_unapplied_declaration(
               record,
               HtmlValidator.new_css_budget()
             )
  end

  test "legacy scalar corner metadata is sliced at page boundaries" do
    {:ok, dom} =
      HtmlParser.parse(
        "<div style='background:red;border:2pt solid black;border-radius:6pt'><p style='height:100pt;margin:0'>First</p><p style='height:100pt;margin:0'>Second</p><p style='height:100pt;margin:0'>Third</p></div>"
      )

    {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    [node] = styled.children
    styled = %{styled | children: [%{node | style: Map.delete(node.style, :border_radii)}]}
    assert {:ok, plan} = Layout.layout(styled, page_size: {200, 150}, margin: 0)
    assert {:ok, pages} = Pagination.paginate(plan)
    assert length(pages) >= 2

    assert Enum.any?(pages, fn page -> Enum.any?(page.boxes, &Map.has_key?(&1, :border_radii)) end)
  end

  test "overflowing used background positions return actionable diagnostics for either axis" do
    for position <- ["calc(4000000000%) 0", "0 calc(4000000000%)"] do
      html =
        "<div style='width:200pt;height:200pt;background-image:image(red);background-size:10pt 10pt;background-repeat:no-repeat;background-position:#{position}'></div>"

      assert {:error, {:invalid_document, diagnostic}} =
               HtmlToPdf.render(html, system_font_discovery: false)

      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.CssMathValidator
      assert diagnostic.source == "calc(4000000000%)"
      assert diagnostic.message =~ "CSS calculation"
    end
  end

  defp layout(html, opts \\ []) do
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute(dom, Keyword.put(opts, :system_font_discovery, false))
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 150}, margin: 0)
    {styled, layout}
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.BoxPaintGlobalStateTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  alias NativeElixirPdfUtilities.Validators.{
    WriterValidator
  }

  test "border repetition and shadow rasters consume existing shared resource budgets" do
    previous = Limits.effective()

    try do
      Limits.install(Map.put(previous, :max_raster_pixels, 1))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render(
                 "<div style='width:30pt;height:20pt;box-shadow:2pt 2pt 4pt red'>P</div>",
                 system_font_discovery: false
               )

      assert diagnostic.message =~ "max_raster_pixels"
    after
      Limits.install(previous)
    end

    assert {:error, {_, %{stage: :pdf}}} =
             WriterValidator.validate_native_boxes([
               %{
                 type: :text,
                 x: 0,
                 y: 0,
                 font: "Helvetica",
                 font_size: 12,
                 text: "bad",
                 color: {0, 0, 0},
                 text_stroke_width: -1,
                 text_stroke_color: {0, 0, 0}
               }
             ])
  end

  test "generated gradient and repetition failures retain actionable shared budget diagnostics" do
    previous = Limits.effective()

    for {property, value} <- [{:max_raster_pixels, 1}, {:max_raster_work, 1}] do
      assert {:ok, dom} =
               HtmlParser.parse(
                 "<div style='width:20pt;height:20pt;background-image:linear-gradient(red,blue)'></div>"
               )

      assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)

      try do
        Limits.install(Map.put(previous, property, value))
        assert {:error, {:resource_limit_exceeded, diagnostic}} = Layout.layout(styled)
        assert diagnostic.source == "CSS gradient"
      after
        Limits.install(previous)
      end
    end

    assert {:ok, dom} =
             HtmlParser.parse(
               "<div style='width:20pt;height:20pt;background:image(red) 0 0/1pt 1pt repeat'></div>"
             )

    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)

    try do
      Limits.install(Map.put(previous, :max_entries, 50))
      assert {:error, {:resource_limit_exceeded, diagnostic}} = Layout.layout(styled)
      assert diagnostic.message =~ "tile count"
    after
      Limits.install(previous)
    end
  end

  test "calculated paint geometry rejects excessive used values through the shared diagnostics contract" do
    for html <- [
          "<div style='width:200pt;height:20pt;background:red;border-radius:calc(4000000000%) / 2pt'></div>",
          "<img src=tile style='width:200pt;height:20pt;border-radius:calc(4000000000%) / 2pt'>",
          "<div style='width:200pt;height:20pt;border:1pt solid transparent;border-image:url(tile) 1 / calc(4000000000%)'></div>"
        ] do
      png = PngFixture.build(4, 4, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

      assert {:error, {_, %{stage: stage, message: message}}} =
               HtmlToPdf.render(html,
                 assets: %{"tile" => {:bytes, png}},
                 system_font_discovery: false
               )

      assert stage in [:style, :layout]
      assert is_binary(message)
    end

    {:ok, dom} =
      HtmlParser.parse(
        "<div style='width:20pt;height:20pt;border:2pt solid transparent;border-image:linear-gradient(red,blue) 20 / 2pt'></div>"
      )

    {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    previous = Limits.effective()

    try do
      Limits.install(Map.put(previous, :max_raster_pixels, 1))

      assert {:error, {:resource_limit_exceeded, %{source: "CSS gradient"}}} =
               Layout.layout(styled)
    after
      Limits.install(previous)
    end

    {:ok, dom} =
      HtmlParser.parse(
        "<div style='width:20pt;height:20pt;background-image:image(red);background-size:.001pt 1pt'></div>"
      )

    {:ok, styled} = Style.compute(dom, system_font_discovery: false)

    try do
      Limits.install(Map.put(previous, :max_entries, 50))
      assert {:error, {:resource_limit_exceeded, %{message: message}}} = Layout.layout(styled)
      assert message =~ "tile count"
    after
      Limits.install(previous)
    end
  end
end
