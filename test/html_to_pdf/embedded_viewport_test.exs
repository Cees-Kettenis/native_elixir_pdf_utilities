defmodule NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewportTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewport
  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
  alias NativeElixirPdfUtilities.Validators.EmbeddedViewportValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.ResourceValidator

  Code.require_file("../support/png_fixture.ex", __DIR__)
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  setup do
    root =
      Path.join(System.tmp_dir!(), "native-pdf-viewport-#{System.unique_integer([:positive])}")

    File.mkdir_p!(Path.join(root, "children"))

    File.write!(
      Path.join(root, "children/child.html"),
      "<body style='margin:0;overflow:hidden'><p>Local child</p></body>"
    )

    File.write!(
      Path.join(root, "children/image.png"),
      PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)
    )

    limits = Limits.effective()

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(root)
    end)

    %{root: root}
  end

  test "preparation isolates child styles and restores the parent document context" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert ResourceContext.current() == parent

      assert {:ok, surface} =
               EmbeddedViewport.prepare_html(
                 {:srcdoc, "<style>p{color:red}</style><p>Child</p>"},
                 parent
               )

      assert surface.context.depth == 1
      assert surface.context.document_id != parent.document_id
      assert surface.context.scope == parent.scope
      assert ResourceContext.current() == parent
      [body] = surface.tree.children
      [paragraph] = body.children
      assert paragraph.style.color == {1.0, 0.0, 0.0}
      assert {:ok, group} = EmbeddedViewport.layout(surface, 150.0, 60.0)
      assert group.type == :drawing_group
      assert group.width == 150.0
      assert group.height == 60.0
      assert group.clip == %{x: 0.0, y: 0.0, width: 150.0, height: 60.0}
      assert Enum.any?(group.boxes, &(&1.type == :text and &1.text == "Child"))
      assert ResourceContext.current() == parent
    end)

    assert ResourceContext.current() == nil
  end

  test "srcdoc wins even when empty and iframe blank initialization never selects a file" do
    assert ResourceValidator.iframe_source(%{"srcdoc" => "", "src" => "https://denied"}) ==
             {:srcdoc, ""}

    for attributes <- [%{}, %{"src" => ""}, %{"src" => "  about:blank \n"}] do
      assert ResourceValidator.iframe_source(attributes) == {:srcdoc, ""}
    end

    assert ResourceValidator.iframe_source(%{"src" => "  child.html  "}) == "child.html"

    assert ResourceValidator.renderer_context_options(base_url: "https://example.com") ==
             [authorization_root: nil, base_url: "https://example.com"]

    assert ResourceValidator.renderer_context_options(base_url: "/tmp") ==
             [authorization_root: "/tmp", base_url: "/tmp"]
  end

  test "plain text preserves literal markup and rejects invalid UTF-8" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, surface} = EmbeddedViewport.prepare_text("<tag> & \"literal\"\nNext", parent)
      assert {:ok, group} = EmbeddedViewport.layout(surface, 150.0, 60.0)
      text = group.boxes |> Enum.filter(&(&1.type == :text)) |> Enum.map_join(& &1.text)
      assert text =~ "<tag>"
      assert text =~ "Next"

      assert {:error, {:invalid_encoding, %{stage: :html, message: message}}} =
               EmbeddedViewport.prepare_text(<<255>>, parent)

      assert message != ""
    end)
  end

  test "the public renderer composes initial iframe content and suppresses raw fallback text" do
    html = """
    <style>body{margin:0}p{margin:0;font-size:10pt}</style>
    <p>Parent before</p><iframe style='border:0;width:90pt;height:60pt' srcdoc='&lt;p&gt;Child&lt;/p&gt;'>
    <img src="https://never-read.test">Fallback</iframe><p>Parent after</p>
    """

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {240, 180}, margin: 0)

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Parent before" and text =~ "Child" and text =~ "Parent after"
    refute text =~ "Fallback"
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert length(document.pages) == 1
  end

  test "local siblings render independently while active cycles and missing files diagnose", %{
    root: root
  } do
    html =
      "<iframe src='children/child.html'></iframe><iframe src='children/child.html'></iframe>"

    assert {:ok, pdf} = HtmlToPdf.render(html, base_url: root)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert length(Regex.scan(~r/Local child/, text)) == 2
    File.write!(Path.join(root, "children/child.html"), "<iframe src='child.html'></iframe>")
    assert {:error, {:resource_cycle, diagnostic}} = HtmlToPdf.render(html, base_url: root)
    assert diagnostic.stage == :resource and diagnostic.operation == :load_embedded_resource
    assert diagnostic.message =~ "active ancestor"

    assert {:error, {:resource_not_found, diagnostic}} =
             HtmlToPdf.render("<iframe src='missing.html'></iframe>", base_url: root)

    assert diagnostic.source == "missing.html" and diagnostic.message != ""
  end

  test "denied document and dependency mappings never invoke the resolver", %{root: root} do
    owner = self()

    resolver = fn request ->
      send(owner, {:resolved, request})
      {:ok, "<p>Bypass</p>"}
    end

    for source <- ["https://denied.test/child.html", "//denied.test/child.html", "../escape.html"] do
      assert {:error, {:resource_denied, diagnostic}} =
               HtmlToPdf.render("<iframe src='#{source}'></iframe>",
                 base_url: root,
                 assets: %{source => {:bytes, "<p>Bypass</p>"}},
                 asset_resolver: resolver
               )

      assert diagnostic.stage == :resource and diagnostic.source == source
      refute_received {:resolved, _}
    end

    for child <- [
          "<img src='https://denied.test/image.png'>",
          "<div style=\"background-image:url(https://denied.test/image.png)\">Denied</div>",
          "<style>@font-face{font-family:Child;src:url(https://denied.test/font.ttf)}</style><p>Denied</p>"
        ] do
      assert {:error, {:resource_denied, diagnostic}} =
               HtmlToPdf.render(iframe(child),
                 base_url: root,
                 assets: %{
                   "https://denied.test/image.png" =>
                     {:bytes, PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)}
                 },
                 asset_resolver: resolver
               )

      assert diagnostic.message != ""
      refute_received {:resolved, _}
    end
  end

  test "child fonts, CSS and images load through the authorized local APIs", %{root: root} do
    File.cp!("priv/fonts/dejavu/DejaVuSansMono.ttf", Path.join(root, "children/chosen.ttf"))

    File.write!(Path.join(root, "children/child.html"), """
    <head><style>@font-face{font-family:Child;src:url(chosen.ttf)}
    body{margin:0;font-family:Child;background-image:url(image.png);background-repeat:no-repeat}
    p{margin:0}</style></head><body><p>Selected local face</p><img src='image.png'></body>
    """)

    assert {:ok, pdf} =
             HtmlToPdf.render("<iframe src='children/child.html'></iframe>", base_url: root)

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Selected local face"
    assert pdf =~ "+Child-"
    assert pdf =~ "/Subtype /Image"

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, child} =
               EmbeddedViewport.prepare_html({:srcdoc, "<p>Configured CSS</p>"}, parent,
                 stylesheets: [{:file, "provided.css"}],
                 assets: %{"provided.css" => {:bytes, "p{color:red}"}}
               )

      assert hd(hd(child.tree.children).children).style.color == {1.0, 0.0, 0.0}

      assert {:ok, _} =
               EmbeddedViewport.prepare_html({:srcdoc, "<p>Explicit font</p>"}, parent,
                 fonts: [%{family: "Configured", path: "children/chosen.ttf"}]
               )

      assert {:error, {:resource_denied, %{stage: :resource}}} =
               EmbeddedViewport.prepare_html({:srcdoc, "<p>Denied font</p>"}, parent,
                 fonts: [%{family: "Configured", path: "../outside.ttf"}]
               )

      assert {:error, {:resource_denied, %{stage: :resource}}} =
               EmbeddedViewport.prepare_html({:srcdoc, "<p>Denied CSS</p>"}, parent,
                 stylesheets: [{:file, "https://denied.test/style.css"}]
               )

      assert {:ok, child} = EmbeddedViewport.prepare_image("children/image.png", parent)
      assert {:ok, group} = EmbeddedViewport.layout(child, 40, 20)
      assert Enum.any?(group.boxes, &(&1.type == :image))

      assert {:error, {:resource_denied, _}} =
               EmbeddedViewport.prepare_image("https://denied.test/image.png", parent)

      assert {:ok, remote} = ResourceContext.rebase(parent, "https://denied.test/")

      assert {:error, {:resource_denied, diagnostic}} =
               EmbeddedViewport.prepare_image("children/image.png", remote)

      assert diagnostic.message != ""
    end)
  end

  test "parent flow and translated nested viewport links remain bounded to one child viewport" do
    child =
      "<body style='margin:0;overflow:hidden'><a href='https://example.com/child' style='font-size:10pt;line-height:12pt'>Child link</a>" <>
        "<div style='height:120pt'>Overflow</div><a href='https://example.com/hidden'>Hidden link</a></body>"

    nested =
      "<body style='margin:0;overflow:hidden'>" <>
        iframe(
          child,
          "style='border:0;display:block;width:80pt;height:25pt;position:relative;left:5pt;top:3pt'"
        ) <> "</body>"

    html =
      "<style>body{margin:0}p{margin:0;font-size:10pt;line-height:12pt}</style>" <>
        "<p style='height:75pt'>First page</p>" <>
        iframe(nested, "style='border:0;display:block;width:100pt;height:40pt'") <>
        "<p>After child</p>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, page_size: {200, 100}, margin: 0, compress_streams: false)

    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert length(document.pages) == 2

    links =
      for {_, %{value: %{"Subtype" => {:name, "Link"}} = annotation}} <- document.objects,
          do: annotation

    assert length(links) == 1
    [link] = links
    assert link["A"]["URI"] == {:string, "https://example.com/child"}
    [left, bottom, right, top] = link["Rect"]
    assert left >= 5 and right <= 85 and bottom >= 57 and top <= 97
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "First page" and text =~ "After child"
  end

  test "child image links retain their border box and intersect the actual translated viewport" do
    image = PngFixture.build(4, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    child =
      "<body style='margin:0;overflow:hidden;font-size:0;line-height:0'><a href='https://example.com/border' style='display:block'><img src='data:image/png;base64,#{Base.encode64(image)}' style='display:block;width:60pt;height:20pt;object-fit:contain'></a></body>"

    html =
      "<body style='margin:0'>" <>
        iframe(
          child,
          "style='display:block;border:0;position:relative;left:7pt;top:3pt;width:50pt;height:10pt'"
        ) <> "</body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 100}, margin: 0)
    assert pdf =~ "/Rect [7 87 57 97]"
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    links =
      for {_, %{value: %{"Subtype" => {:name, "Link"}} = annotation}} <- document.objects,
          do: annotation

    assert [%{"Rect" => [7, 87, 57, 97]}] = links
  end

  test "static child controls paint completely while interactive widgets diagnose" do
    child =
      "<body style='margin:0;overflow:hidden'><input name=shared value='Child value'><textarea>Child notes</textarea></body>"

    assert {:ok, pdf} = HtmlToPdf.render(iframe(child), forms: :static)
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Child value" and text =~ "Child notes"
    assert {:error, {:unsupported_form, diagnostic}} = HtmlToPdf.render(iframe(child))
    assert diagnostic.stage == :layout and diagnostic.operation == :layout_embedded_viewport
    assert diagnostic.message =~ "transformed widget geometry"
    assert diagnostic.message =~ "forms: :static"
  end

  test "sibling documents charge aggregate source, image, CSS and child budgets", %{root: root} do
    html =
      "<iframe src='children/child.html'></iframe><iframe src='children/child.html'></iframe>"

    Limits.install(Map.put(Limits.defaults(), :max_embedded_documents, 1))

    assert {:error, {:resource_limit_exceeded, %{stage: :resource, message: message}}} =
             HtmlToPdf.render(html, base_url: root)

    assert message =~ "max_embedded_documents"

    Limits.install(
      Map.put(Limits.defaults(), :max_aggregate_html_source_bytes, byte_size(html) + 1)
    )

    assert {:error, {:resource_limit_exceeded, %{stage: :html, message: message}}} =
             HtmlToPdf.render(html, base_url: root)

    assert message =~ "max_aggregate_html_source_bytes"
    File.write!(Path.join(root, "children/child.html"), "<img src='image.png'>")

    for {limit, bound} <- [max_image_count: 1, max_aggregate_decoded_image_bytes: 5] do
      Limits.install(Map.put(Limits.defaults(), limit, bound))

      assert {:error, {:resource_limit_exceeded, %{message: message}}} =
               HtmlToPdf.render(html, base_url: root)

      assert message =~
               if(limit == :max_image_count,
                 do: "image count",
                 else: "aggregate decoded image bytes"
               )
    end

    css = "p{color:red}"
    File.write!(Path.join(root, "children/child.html"), "<style>#{css}</style><p>Styled</p>")
    Limits.install(Map.put(Limits.defaults(), :max_aggregate_css_source_bytes, byte_size(css)))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             HtmlToPdf.render(html, base_url: root)

    assert message =~ "max_aggregate_css_source_bytes"
  end

  defp iframe(child, attrs \\ "style='border:0;width:90pt;height:45pt'") do
    encoded =
      child
      |> String.replace("&", "&amp;")
      |> String.replace("<", "&lt;")
      |> String.replace("\"", "&quot;")

    "<iframe #{attrs} srcdoc=\"#{encoded}\"></iframe>"
  end

  test "required automatic iframe scrollbars diagnose until client-area geometry is supported" do
    for child <- [
          "<body style='margin:0'><div style='height:100pt'>Tall</div></body>",
          "<body style='margin:0'><div style='width:120pt'>Wide</div></body>"
        ] do
      assert {:error, {:not_implemented, diagnostic}} = HtmlToPdf.render(iframe(child))
      assert diagnostic.stage == :layout and diagnostic.source == "iframe"
      assert diagnostic.message =~ "automatic scrollbars"
      assert diagnostic.message =~ "reduced client area"
    end

    for child <- [
          "<html style='overflow:hidden'><body style='margin:0'><div style='height:100pt'>Tall</div></body></html>",
          "<body style='margin:0;overflow:hidden'><div style='width:120pt'>Wide</div></body>"
        ] do
      assert {:ok, _} = HtmlToPdf.render(iframe(child))
    end

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, surface} = EmbeddedViewport.prepare_iframe({:srcdoc, ""}, parent)
      assert surface.host == :iframe
      assert {:ok, _} = EmbeddedViewport.layout(surface, 0, 0)
    end)
  end

  test "shared caches preserve separate root selector and document language scopes" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, dom} =
               HtmlParser.parse_document_detailed("""
               <html><head><meta http-equiv='content-language' content='en'><style>
               html:first-child{color:blue}p:lang(en){color:blue}</style></head><body><p>Parent</p></body></html>
               """)

      assert {:ok, tree} = Style.compute_detailed(dom, [], parent.image_budget)
      assert hd(hd(tree.children).children).style.color == {0.0, 0.0, 1.0}

      assert {:ok, child} =
               EmbeddedViewport.prepare_html(
                 {:srcdoc,
                  """
                  <html><head><meta http-equiv='content-language' content='fr'><style>
                  html:first-child{color:red}p:lang(fr){color:red}</style></head><body><p>Child</p></body></html>
                  """},
                 parent
               )

      assert hd(hd(child.tree.children).children).style.color == {1.0, 0.0, 0.0}

      assert {:ok, child} =
               EmbeddedViewport.prepare_html(
                 {:srcdoc,
                  """
                  <html><head><style>html:first-child{color:green}</style></head><body><p>Sibling</p></body></html>
                  """},
                 parent
               )

      assert hd(hd(child.tree.children).children).style.color == {0, 0.5019607843, 0}
    end)
  end

  test "iframe raw text uses the shared validator and replaced auto sizing" do
    alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
    assert HtmlValidator.html_text_mode("iframe") == :raw_text
    assert HtmlValidator.html_text_mode({:foreign, :svg, "iframe", %{}}) == :data

    assert HtmlValidator.html_text_end_match("literal</iframe-suffix></IFRAME>", "iframe") ==
             {23, 8}

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<iframe>literal<img src=denied.png></iframe-suffix></IFRAME>"
             )

    [frame] = dom.children
    assert frame.children == [%{type: :text, text: "literal<img src=denied.png></iframe-suffix>"}]

    assert {:ok, %{children: [%{text: "<p>literal &amp;</p>"}]}} =
             HtmlParser.parse_fragment_detailed("<p>literal &amp;</p>", "iframe")

    assert {:error, {:unsupported_html, %{source: "</iframe bad>"}}} =
             HtmlParser.parse_detailed("<iframe></iframe bad>")

    assert EmbeddedViewportValidator.prepare_iframe_style(%{
             display: :inline_flex,
             width: :auto,
             height: nil
           }) ==
             %{display: :inline_block, width: 225.0, height: 112.5}

    assert EmbeddedViewportValidator.prepare_iframe_style(%{display: :grid, width: 0, height: 0}) ==
             %{display: :block, width: 0, height: 0}

    assert EmbeddedViewportValidator.prepare_iframe_style(%{display: :inline}) ==
             %{display: :inline_block, width: 225.0, height: 112.5}
  end

  test "viewport failures retain scope cleanup and child execution policy" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:error, {:invalid_options, _}} =
               EmbeddedViewportValidator.prepare_options(:bad, parent)

      assert {:error, {:invalid_options, _}} =
               EmbeddedViewportValidator.prepare_options([private_bypass: true], parent)

      assert {:ok, child} = EmbeddedViewport.prepare_html({:srcdoc, "<p>Valid</p>"}, parent)

      assert {:error, {:invalid_layout, diagnostic}} =
               EmbeddedViewport.layout(
                 %{child | tree: %{type: :document, children: [%{}]}},
                 10,
                 10
               )

      assert diagnostic.stage == :layout and diagnostic.operation == :layout_embedded_viewport
      assert ResourceContext.current() == parent

      assert {:error, {:invalid_options, diagnostic}} =
               EmbeddedViewportValidator.prepare_paint(
                 %{page_size: {100, 100}, boxes: []},
                 %{child | forms: :invalid},
                 100,
                 100
               )

      assert diagnostic.message != ""

      assert {:error, {:unsupported_html, %{line: 1, source: source}}} =
               EmbeddedViewport.prepare_html({:srcdoc, "<script>alert(1)</script>"}, parent)

      assert source =~ "script"
      assert ResourceContext.current() == parent

      assert {:ok, child} =
               EmbeddedViewport.prepare_html(
                 {:srcdoc, "<p style='break-before:page'>Viewport page break</p>"},
                 parent
               )

      assert {:ok, group} = EmbeddedViewport.layout(child, 60, 20)
      refute Enum.any?(group.boxes, &(&1.type == :page_break))

      assert {:ok, _} =
               NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.render([
                 %{size: {100, 100}, boxes: [group]}
               ])
    end)
  end

  test "invalid or expired contexts and viewport dimensions produce actionable diagnostics" do
    assert {:error, {:invalid_options, %{stage: :resource, message: message}}} =
             ResourceContext.with_document(nil, fn -> flunk("must not execute") end)

    assert message != ""

    assert {:error, {:invalid_layout, %{stage: :layout, operation: :layout_embedded_viewport}}} =
             EmbeddedViewport.layout(%{}, -1, 10)

    assert {:error, {:invalid_options, %{stage: :resource}}} =
             EmbeddedViewportValidator.prepare_options([], nil)

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:error, {:invalid_options, %{message: message}}} =
               EmbeddedViewportValidator.prepare_options([fonts: [nil]], parent)

      assert message =~ "font"
      assert {:ok, blank} = EmbeddedViewport.prepare_html({:srcdoc, ""}, parent)
      assert {:ok, group} = EmbeddedViewport.layout(blank, 0, 0)
      assert group.clip.width == 0
    end)
  end

  test "child document URLs, authored bases and inherited srcdoc keep the immutable root", %{
    root: root
  } do
    File.mkdir_p!(Path.join(root, "children/nested"))
    File.cp!(Path.join(root, "children/image.png"), Path.join(root, "children/nested/image.png"))

    File.write!(Path.join(root, "children/base.html"), """
    <head><base href='nested/'><base href='https://ignored.test/'></head>
    <body style='margin:0;overflow:hidden'><img src='image.png'><a href='next.html'>Local link</a></body>
    """)

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, child} = EmbeddedViewport.prepare_html("children/base.html", parent)
      assert child.context.authorization_root == root
      assert URI.parse(child.context.resolution_base).scheme == "file"

      assert URI.parse(child.context.resolution_base).path ==
               Path.join(root, "children/nested") <> "/"

      [body] = child.tree.children
      assert Enum.any?(body.children, &(Map.get(&1, :tag) == "img"))

      assert {:ok, inherited} =
               EmbeddedViewport.prepare_html({:srcdoc, "<img src='image.png'>"}, child.context)

      assert inherited.context.authorization_root == root
      assert inherited.context.resolution_base == child.context.resolution_base
      assert {:ok, group} = EmbeddedViewport.layout(inherited, 90, 45)
      assert Enum.any?(group.boxes, &(&1.type == :image))

      assert {:ok, rebased} =
               ResourceContext.rebase(
                 parent,
                 {:document_url, "file://" <> Path.join(root, "children/base.html")}
               )

      assert rebased.resolution_base =~ "/children/base.html"

      assert {:ok, sibling} =
               EmbeddedViewport.prepare_html({:srcdoc, "<img src='image.png'>"}, rebased)

      assert {:ok, _} = EmbeddedViewport.layout(sibling, 90, 45)

      assert {:error, {:resource_denied, diagnostic}} =
               EmbeddedViewport.prepare_html(
                 {:srcdoc, "<base href='https://remote.test/'><img src='image.png'>"},
                 parent,
                 asset_resolver: fn _ -> flunk("denied child base must precede resolver") end
               )

      assert diagnostic.source == "image.png"
      assert diagnostic.message != ""

      assert {:ok, destinations} =
               EmbeddedViewport.prepare_html(
                 {:srcdoc, "<a href='#target'>Target</a><p id='target'>Here</p>"},
                 parent
               )

      assert {:error, {:unsupported_html, diagnostic}} =
               EmbeddedViewport.layout(destinations, 150, 60)

      assert diagnostic.stage == :layout and diagnostic.source == "link_scope"
      assert diagnostic.message =~ "final composition geometry"
      assert ResourceContext.current() == parent
    end)
  end

  test "hidden iframes do not select sources and bounded fragments retain supplied font bytes", %{
    root: root
  } do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<iframe style='display:none' src='https://denied.test/never.html'></iframe><p>Visible parent</p>"
             )

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Visible parent"
    font = File.read!("priv/fonts/dejavu/DejaVuSansMono.ttf")
    File.write!(Path.join(root, "children/chosen.ttf"), font)

    File.write!(
      Path.join(root, "children/provided.css"),
      "@font-face{font-family:Configured;src:url(chosen.ttf)}p{font-family:Configured}"
    )

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, child} =
               EmbeddedViewport.prepare_html("children/child.html", parent,
                 stylesheets: [{:file, "provided.css"}]
               )

      assert {:ok, group} = EmbeddedViewport.layout(child, 150, 60)
      assert Enum.any?(group.boxes, &(Map.get(Map.get(&1, :font_face, %{}), :data) == font))

      assert {:ok, child} =
               EmbeddedViewport.prepare_html({:srcdoc, "<p>Supplied bytes</p>"}, parent,
                 fonts: [%{family: "Supplied", data: font}],
                 default_font: "Supplied"
               )

      # Reusable styled fragment surfaces need no HTML body wrapper.
      [body] = child.tree.children
      fragment = %{child | tree: %{child.tree | children: body.children}}
      assert {:ok, group} = EmbeddedViewport.layout(fragment, 150, 60)
      assert Enum.any?(group.boxes, &(Map.get(&1, :text) == "Supplied bytes"))
    end)
  end
end
