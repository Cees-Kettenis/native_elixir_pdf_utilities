defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextProfileRegressionTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgBackend, SvgScene}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgTextValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "nested text roots and artwork inside text return their owning structure diagnostics" do
    for {content, reason, message} <- [
          {"<text><text>Nested</text></text>", :unsupported_html, "supported tspan"},
          {"<text><g>Artwork</g></text>", :unsupported_html, "supported tspan"},
          {"<text><title><text>Nested</text></title>Visible</text>", :invalid_document,
           "nested text roots"}
        ],
        backend <- [&SvgBackend.vectorize/3, &SvgBackend.rasterize/3] do
      assert {:error, {^reason, diagnostic}} =
               backend.(
                 "<svg width='200' height='50'>#{content}</svg>",
                 [],
                 %{
                   mode: :inline,
                   fonts: [%{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")}]
                 }
               )

      assert diagnostic.reason == reason
      assert diagnostic.stage == :layout and diagnostic.operation == :prepare
      assert diagnostic.module == SvgTextValidator and diagnostic.source == "text"
      assert diagnostic.message =~ message
    end
  end

  test "original text whitespace and tspan transforms cannot silently change logical geometry" do
    for {content, message, source} <- [
          {"<text xml:space='preserve'>  Original  </text>", "preserved whitespace", "text"},
          {"<text>A<tspan transform='translate(5 0)'>B</tspan></text>", "tspan transforms",
           "tspan"}
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render("<svg width=200 height=50>#{content}</svg>")

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout and diagnostic.operation == :prepare
      assert diagnostic.module == SvgTextValidator and diagnostic.source == source
      assert diagnostic.message =~ message
    end
  end

  test "raw text CSS requires preparation while prepared source paints and extracts" do
    html = "<svg width=200 height=50><text y=25 style='font-size:20px'>Prepared Ω</text></svg>"
    assert {:ok, %{children: [svg]}} = HtmlParser.parse_detailed(html)

    assert {:error, {:unsupported_html, diagnostic}} = SvgTextValidator.validate_tree(svg)
    assert diagnostic.stage == :layout and diagnostic.reason == :unsupported_html
    assert diagnostic.module == SvgTextValidator and diagnostic.source == "text"
    assert diagnostic.message =~ "host cascade preparation"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "Prepared Ω"} = Text.extract(pdf)
    assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
    assert Enum.all?(page.spans, &(&1.paints_text? and &1.font_size == 20))
  end

  test "prepared stroke gradient and transformed text require coherent visible logical painting" do
    for html <- [
          "<svg width=200 height=50><text y=25 stroke=red>Stroke</text></svg>",
          "<svg width=200 height=50><defs><linearGradient id=g><stop offset=0 /><stop offset=1 /></linearGradient></defs><text y=25 fill='url(#g)'>Gradient</text></svg>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :layout and diagnostic.reason == :unsupported_html
      assert diagnostic.module == SvgTextValidator and diagnostic.source == "text"
      assert diagnostic.message =~ "solid RGB fill without stroke"
    end

    for style <- ["text-transform:uppercase", "white-space:pre", "text-decoration:underline"] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(
                 "<svg width=200 height=50><text y=25 style='#{style}'>Original</text></svg>"
               )

      assert diagnostic.stage == :layout and diagnostic.reason == :unsupported_html
      assert diagnostic.module == SvgTextValidator and diagnostic.source == "text"
      assert diagnostic.message =~ "matching visible and logical text"
    end
  end

  test "supplied prepared scenes reject missing glyphs and restricted or metrics-only selected faces" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<svg width=200 height=50><text y=25>Original</text></svg>"
             )

    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    [text] = svg.children
    face = text.style.font_face

    missing = %{text | children: [%{type: :text, text: "\u{10FFFF}"}]}

    assert {:error, {:unsupported_html, diagnostic}} =
             SvgScene.prepare(%{svg | children: [missing]}, HtmlValidator.new_image_budget())

    assert diagnostic.module == SvgTextValidator and diagnostic.source == "text"
    assert diagnostic.stage == :layout and diagnostic.reason == :unsupported_html
    assert diagnostic.message =~ "does not cover its original text"

    restricted = %{text | style: %{text.style | font_face: %{face | embedding_flags: 0x0002}}}

    assert {:error, {:invalid_document, diagnostic}} =
             SvgScene.prepare(%{svg | children: [restricted]}, HtmlValidator.new_image_budget())

    assert diagnostic.stage == :font and diagnostic.reason == :invalid_document
    assert diagnostic.source == face.family
    assert diagnostic.message =~ "does not permit outline embedding"

    metrics = %{
      text
      | style: %{
          text.style
          | font_face: %{type: :built_in, family: "Helvetica", pdf_name: "Helvetica"}
        }
    }

    assert {:error, {:unsupported_html, diagnostic}} =
             SvgScene.prepare(%{svg | children: [metrics]}, HtmlValidator.new_image_budget())

    assert diagnostic.module == SvgTextValidator and diagnostic.source == "text"
    assert diagnostic.stage == :layout and diagnostic.reason == :unsupported_html
    assert diagnostic.message =~ "selected byte-backed TrueType face"
  end

  test "prepared scene font count stays bounded and independent preparation recovers", %{
    limits: limits
  } do
    html =
      "<svg width=200 height=50><text y=25>Normal<tspan font-weight=700>Bold</tspan></text></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    assert {:ok, original_scene} = SvgScene.prepare(svg, HtmlValidator.new_image_budget())
    assert length(original_scene.selected_fonts) == 2
    Limits.install(Map.put(limits, :max_font_count, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgScene.prepare(svg, HtmlValidator.new_image_budget())

    assert diagnostic.stage == :limits and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.source == "SVG text" and diagnostic.message =~ "max_font_count"
    Limits.install(limits)
    assert {:ok, scene} = SvgScene.prepare(svg, HtmlValidator.new_image_budget())
    assert length(scene.selected_fonts) == 2
    assert scene.logical_text == ["NormalBold"]
  end

  test "descriptive metadata within visible text never enters logical extraction" do
    html =
      "<svg width=200 height=50><text y=25><title>Title only</title><desc>Description only</desc>Visible Ω<tspan>!</tspan></text></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert svg.style.svg_scene.logical_text == ["Visible Ω!"]
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "Visible Ω!"} = Text.extract(pdf)
    assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
    assert Enum.map_join(page.spans, & &1.text) == "Visible Ω!"
    assert Enum.all?(page.spans, & &1.paints_text?)
  end
end
