defmodule NativeElixirPdfUtilities.HtmlToPdf.EmbeddedDocumentRenderingTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedResource, HtmlParser, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, ObjectValidator}

  setup do
    root = Path.join(System.tmp_dir!(), "embedded-document-#{System.unique_integer([:positive])}")
    File.mkdir_p!(root)
    on_exit(fn -> File.rm_rf!(root) end)
    %{root: root}
  end

  test "local object transport preserves static range clipping and suppresses fallback", %{
    root: root
  } do
    File.write!(Path.join(root, "child.html"), """
    <style>body{margin:0;overflow:hidden}p{margin:0}</style>
    <p>Child range</p><input type=range min=0 max=10 value=5 style='width:160px'>
    """)

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<object data='child.html' type='text/html' style='width:90px;height:60px'>Unused fallback</object><p>After object</p>",
               base_url: root,
               forms: :static,
               default_font: "Helvetica",
               system_font_discovery: false,
               compress_streams: false,
               page_size: {240, 180}
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ ~r/Child\s+range/
    assert text =~ "After object"
    refute text =~ "Unused fallback"
    assert pdf =~ "W n"
    refute pdf =~ "/AcroForm"
  end

  test "default object-loading options preserve local authorization and reject inline descriptors",
       %{root: root} do
    html = "<p>Default object</p>"
    path = Path.join(root, "default.html")
    File.write!(path, html)

    assert :ok =
             ResourceContext.run(
               [authorization_root: root, base_url: root],
               HtmlValidator.new_image_budget(),
               fn parent ->
                 assert {:ok, default} = EmbeddedResource.load_object_html("default.html", parent)

                 assert {:ok, explicit} =
                          EmbeddedResource.load_object_html("default.html", parent, [])

                 assert default.html == explicit.html and default.html == html
                 assert default.source == {:file, path}
                 assert default.context.authorization_root == parent.authorization_root
                 assert default.context.scope == parent.scope
                 refute default.context.document_id == parent.document_id
                 assert ResourceContext.current() == parent

                 assert {:error, {:invalid_document, diagnostic}} =
                          EmbeddedResource.load_object_html({:srcdoc, html}, parent)

                 assert diagnostic.stage == :resource
                 assert diagnostic.reason == :invalid_document
                 assert diagnostic.operation == :load_embedded_resource
                 assert diagnostic.module == ObjectValidator
                 assert diagnostic.message =~ "selected local file reference"
                 :ok
               end
             )

    assert ResourceContext.current() == nil
  end

  test "source contexts visit object fallback ranges without reading selected child documents" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed("""
             <object data='never-read.html' style='width:100px'>
             <input type=range style='accent-color:teal'>
             </object>
             <template><input type=range style='accent-color:orange'></template>
             """)

    assert {:ok, result} =
             Style.with_source_style_contexts(
               dom,
               [],
               HtmlValidator.new_css_budget(),
               fn _node, _pseudo, _context -> :ok end
             )

    obligations = result.source_obligations.declaration_contexts
    assert length(obligations) == 3

    assert Enum.count(obligations, &(&1.status == :checked_in_original_context)) == 2

    assert Enum.any?(obligations, fn obligation ->
             obligation.status == :unresolved and
               obligation.reason == :template_insertion_context
           end)

    assert result.source_obligations.resource_references == :unresolved
    assert :resources in result.runtime_obligations
  end

  test "selected object children retain table presentation and converted gradient pixels", %{
    root: root
  } do
    File.write!(Path.join(root, "combined.html"), """
    <style>
      body{margin:0}svg{width:120px;height:40px}
      td{vertical-align:top}caption{caption-side:bottom}
    </style>
    <svg viewBox='0 0 120 40'>
      <defs>
        <linearGradient id='shade'>
          <stop offset='0' stop-color='red'/><stop offset='1' stop-color='blue'/>
        </linearGradient>
        <rect id='tile' width='30' height='20' fill='url(#shade)'/>
      </defs>
      <use href='#tile' x='5' y='5'/><use href='#tile' x='45' y='5'/>
    </svg>
    <table cellspacing=3 cellpadding=2 bgcolor='#eeeeee'>
      <caption>Child caption</caption><tr><td>Child grid</td></tr>
    </table>
    """)

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<object data='combined.html' type='text/html' style='width:150px;height:120px'>Unused child</object><p>Parent tail</p>",
               base_url: root,
               forms: :static,
               default_font: "Helvetica",
               system_font_discovery: false,
               compress_streams: false,
               page_size: {240, 220}
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ ~r/Child\s+grid/
    assert text =~ ~r/Child\s+caption/
    assert text =~ ~r/Parent\s+tail/
    assert text =~ ~r/Child\s+grid.*Child\s+caption.*Parent\s+tail/s

    for label <- [~r/Child\s+grid/, ~r/Child\s+caption/, ~r/Parent\s+tail/],
        do: assert(length(Regex.scan(label, text)) == 1)

    refute text =~ "Unused child"
    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
    assert {:ok, document} = Reader.read(pdf)

    rgb_images =
      for {_ref,
           %{value: %{"Subtype" => {:name, "Image"}, "ColorSpace" => {:name, "DeviceRGB"}}} =
             image} <- document.objects do
        # Inspect this bounded generated plane without changing reader safety limits.
        pixels = :zlib.uncompress(image.stream)
        assert byte_size(pixels) == image.value["Width"] * image.value["Height"] * 3
        {image.value["Width"], image.value["Height"], pixels}
      end

    assert [{width, height, pixels}] = rgb_images
    assert rem(width, 120) == 0 and rem(height, 40) == 0
    scale = div(width, 120)
    assert height == 40 * scale
    row_offset = 10 * scale * width * 3
    first_tile = binary_part(pixels, row_offset + 5 * scale * 3, 30 * scale * 3)
    second_tile = binary_part(pixels, row_offset + 45 * scale * 3, 30 * scale * 3)
    assert first_tile == second_tile
    gradient = for <<r, g, b <- first_tile>>, do: {r, g, b}
    assert Enum.any?(gradient, fn {r, g, b} -> r > 200 and g == 0 and b < 55 end)
    assert Enum.any?(gradient, fn {r, g, b} -> b > 200 and g == 0 and r < 55 end)
    assert Enum.any?(gradient, fn {r, g, b} -> r > 90 and b > 90 and g == 0 end)
    refute pdf =~ "/ShadingType 2"
    assert pdf =~ "W n"
    refute pdf =~ "/AcroForm"
  end

  test "selected documents transport temporal logical text through a bordered table", %{
    root: root
  } do
    File.write!(Path.join(root, "temporal-grid.html"), """
    <style>
      body{margin:0;padding-bottom:12pt}
      table{width:170px}td{vertical-align:top}
      p{margin:0;orphans:3;widows:3}
    </style>
    <table border=1 rules=all>
      <tr><td><input type=date readonly value=2026-10-13></td></tr>
      <tr><td><p>Selected temporal grid</p></td></tr>
    </table>
    """)

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<object data='temporal-grid.html' type='text/html' style='width:190px;height:100px'>Fallback temporal grid</object><p>Outer tail</p>",
               base_url: root,
               forms: :static,
               default_font: "Helvetica",
               system_font_discovery: false,
               compress_streams: false,
               page_size: {240, 180}
             )

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "2026-10-13"
    assert text =~ "Selected temporal grid"
    assert text =~ "Outer tail"
    refute text =~ "Fallback temporal grid"
    refute text =~ "10/13/2026"
    assert pdf =~ "W n"
    refute pdf =~ "/AcroForm"
  end

  test "frameset siblings initialize independent child controls and collapsed table surfaces", %{
    root: root
  } do
    File.write!(Path.join(root, "frame-grid.html"), """
    <style>body{margin:0}td{vertical-align:top}</style>
    <table border=1 rules=all><tr><td>
      <input type=date readonly value=2026-10-13>
      <p>Framed grid</p><a href='#child'>Child link</a>
    </td></tr></table>
    """)

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<frameset cols='50%,50%' border=0><frame src='frame-grid.html'><frame src='frame-grid.html'></frameset>",
               base_url: root,
               forms: :static,
               default_font: "Helvetica",
               system_font_discovery: false,
               compress_streams: false,
               page_size: {360, 180},
               margin: 0
             )

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert length(Regex.scan(~r/2026-10-13/, text)) == 2
    assert length(Regex.scan(~r/Framed grid/, text)) == 2
    assert length(Regex.scan(~r/Child link/, text)) == 2
    assert pdf =~ "W n"
    refute pdf =~ "/AcroForm"
  end

  test "selected child SVG uses source-font pixels while parent text remains extractable", %{
    root: root
  } do
    File.write!(Path.join(root, "svg-text.html"), """
    <style>body{margin:0}svg{width:180px;height:40px}</style>
    <svg viewBox='0 0 180 40'>
      <text x='4' y='24' font-family='DejaVu Sans' font-size='16'>Σ Child SVG</text>
    </svg>
    """)

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<object data='svg-text.html' type='text/html' style='width:190px;height:50px'>Unused glyph fallback</object><p>Parent text</p>",
               base_url: root,
               forms: :static,
               default_font: "Helvetica",
               system_font_discovery: false,
               compress_streams: false,
               page_size: {240, 180}
             )

    assert {:ok, text} = Text.extract(pdf, layout: false)
    refute text =~ "Σ Child SVG"
    assert text =~ "Parent text"
    refute text =~ "Unused glyph fallback"
    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
    assert {:ok, document} = Reader.read(pdf)

    masks =
      for {_,
           %{value: %{"Subtype" => {:name, "Image"}, "ColorSpace" => {:name, "DeviceGray"}}} =
             image} <- document.objects do
        # This bounded generated alpha plane proves source glyphs became pixels.
        pixels = :zlib.uncompress(image.stream)
        assert byte_size(pixels) == image.value["Width"] * image.value["Height"]
        :binary.bin_to_list(pixels)
      end

    assert [mask] = masks
    assert 0 in mask and Enum.any?(mask, &(&1 > 0))

    # Embedding must preserve the exact source-font glyph plane produced for
    # this same authored child when rendered on its own.
    assert {:ok, child_pdf} =
             HtmlToPdf.render(File.read!(Path.join(root, "svg-text.html")),
               forms: :static,
               default_font: "Helvetica",
               system_font_discovery: false,
               compress_streams: false,
               page_size: {240, 180}
             )

    assert {:ok, child_document} = Reader.read(child_pdf)

    child_masks =
      for {_,
           %{value: %{"Subtype" => {:name, "Image"}, "ColorSpace" => {:name, "DeviceGray"}}} =
             image} <- child_document.objects,
          do: :binary.bin_to_list(:zlib.uncompress(image.stream))

    assert child_masks == masks

    assert {:error, {:no_extractable_text, %{stage: :text_extraction}}} =
             Text.extract(child_pdf, layout: false)

    refute pdf =~ "/ActualText"
    assert pdf =~ "W n"
  end
end
