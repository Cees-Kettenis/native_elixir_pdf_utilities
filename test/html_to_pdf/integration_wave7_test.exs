defmodule NativeElixirPdfUtilities.HtmlToPdf.IntegrationWave7Test do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Pdf.Reader

  test "inline SVG styles share the authored document cascade and preferred stylesheet set" do
    source = """
    <style title='Preferred'>p{color:green}</style>
    <p>Before</p><svg width=24 height=12>
      <style>rect{fill:red}p{color:blue}</style><rect width=24 height=12 />
    </svg><svg width=24 height=12><rect width=24 height=12 /></svg><p>After</p>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, %{children: [before, first, second, after_node]}} = Style.compute_detailed(dom)
    assert before.style.color == {0, 0, 1}
    assert after_node.style.color == before.style.color
    assert first.style.svg_scene.source =~ "rgb(255,0,0)"
    assert second.style.svg_scene.source =~ "rgb(255,0,0)"
    assert {:ok, pdf} = HtmlToPdf.render(source)
    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
    assert {:ok, document} = Reader.read(pdf)

    rgb_images =
      for {_ref,
           %{value: %{"Subtype" => {:name, "Image"}, "ColorSpace" => {:name, "DeviceRGB"}}} =
             image} <- document.objects do
        # Inspect this bounded generated plane without changing reader safety limits.
        pixels = :zlib.uncompress(image.stream)
        assert byte_size(pixels) == image.value["Width"] * image.value["Height"] * 3
        assert pixels == :binary.copy(<<255, 0, 0>>, image.value["Width"] * image.value["Height"])
        image
      end

    assert length(rgb_images) == 1
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Before" and text =~ "After"
  end

  test "native MathML child artwork keeps scoped fragment positions through glyph coalescing" do
    source = """
    <p><a href='#target'>Before</a></p>
    <math><mtext><span id=target style='background:yellow'>Painted</span><a href='#target'>Jump</a></mtext></math>
    <p>After</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(source)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Painted" and text =~ "Jump" and text =~ "Before" and text =~ "After"
    assert {:ok, []} = Forms.fields(pdf)
    assert {:ok, document} = Reader.read(pdf)

    destinations =
      for {_ref, %{value: %{"Subtype" => {:name, "Link"}, "Dest" => dest}}} <- document.objects,
          do: dest

    assert [first, second] = destinations
    assert first == second
    assert [{:ref, page}, {:name, "XYZ"}, x, y, nil] = first
    assert page in Enum.map(document.pages, & &1.ref)
    assert x >= 0 and y >= 0
  end

  test "literal stylesheet children and configured declaration diagnostics retain their source" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{type: :processing_instruction, target: "note", data: "ignored"},
            %{type: :text, text: "p{color:red}"}
          ]
        },
        %{type: :element, tag: "p", attributes: %{}, children: [%{type: :text, text: "Visible"}]}
      ]
    }

    assert {:error, {:invalid_document, %{stage: :style, source: "style", message: message}}} =
             Style.compute_detailed(dom)

    assert message =~ "literal text-only"

    assert {:error, {:invalid_css, diagnostic}} =
             HtmlToPdf.render("<p>Visible</p>", stylesheets: [{:css, "p{width:bananas}"}])

    assert diagnostic.stage == :css
    assert diagnostic.source =~ "width:bananas"
    assert diagnostic.message =~ "width"
  end

  test "invalid prepared font configuration cannot authorize style computation" do
    assert {:error, {:invalid_document, diagnostic}} =
             NativeElixirPdfUtilities.Validators.HtmlValidator.prepare_style_input(
               %{type: :document, children: []},
               [],
               {:error, :invalid_fonts}
             )

    assert diagnostic.stage == :style and diagnostic.reason == :invalid_document
    assert diagnostic.message =~ "fonts"
  end
end
