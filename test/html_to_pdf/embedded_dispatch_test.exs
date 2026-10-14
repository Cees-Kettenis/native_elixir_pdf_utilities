defmodule NativeElixirPdfUtilities.HtmlToPdf.EmbeddedDispatchTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedResource, EmbeddedViewport, ResourceContext}
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, ObjectValidator}

  Code.require_file("../support/png_fixture.ex", __DIR__)
  alias NativeElixirPdfUtilities.TestSupport.{JpegFixture, PngFixture}

  setup do
    root = Path.join(System.tmp_dir!(), "native-dispatch-#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(root, "child"))
    png = PngFixture.build(40, 20, 2, 8, 0, fn _, _ -> [10, 140, 220] end)
    File.write!(Path.join(root, "child/image.png"), png)
    File.write!(Path.join(root, "child/image.jpg"), JpegFixture.baseline(32, 16))

    File.write!(Path.join(root, "child/image.svg"), """
    <svg xmlns="http://www.w3.org/2000/svg" width="40" height="20" viewBox="0 0 40 20">
    <rect width="40" height="20" fill="blue"/></svg>
    """)

    File.write!(Path.join(root, "child/literal.txt"), "\n<tag> & literal\nSecond\tline")

    File.write!(Path.join(root, "child/document.html"), """
    <body style='margin:0;overflow:hidden'><p style='margin:0'>Embedded HTML</p>
    <img src='image.png'></body>
    """)

    limits = Limits.effective()

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(root)
    end)

    %{root: root, png: png}
  end

  test "selection distinguishes object data and embed src, absent and blank initialization", %{
    root: root
  } do
    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, :fallback} = ObjectValidator.select(%{}, parent)
      assert {:ok, :empty} = ObjectValidator.select(%{}, parent, :embed)

      for source <- ["", "  about:blank\n"] do
        assert {:ok, selected} = ObjectValidator.select(%{"src" => source}, parent, :embed)
        assert selected == if(source == "", do: :blank, else: {:blank, "about:blank"})

        assert {:ok, {:viewport, surface}} =
                 EmbeddedViewport.prepare_selected(:blank, parent, [], :embed)

        assert surface.host == :embed
        assert surface.context.document_id != parent.document_id
        assert {:ok, group} = EmbeddedViewport.layout(surface, 90, 45)
        assert group.clip == %{x: 0.0, y: 0.0, width: 90, height: 45}
      end

      assert {:ok, {:document, "child/literal.txt", "text/plain"}} =
               ObjectValidator.select(
                 %{"data" => "child/literal.txt", "src" => "other.html"},
                 parent
               )

      assert {:ok, {:document, "child/document.html", "text/html"}} =
               ObjectValidator.select(
                 %{"src" => "child/document.html", "data" => "other.txt"},
                 parent,
                 :embed
               )

      assert {:error, {:not_implemented, diagnostic}} =
               ObjectValidator.select(
                 %{"src" => "plugin.bin", "type" => "application/x-native-test"},
                 parent,
                 :embed
               )

      assert diagnostic.source == "plugin.bin" and diagnostic.message =~ "plugin"
    end)
  end

  test "literal text preserves initial lines and markup in its file document scope", %{root: root} do
    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, {:viewport, surface}} =
               EmbeddedViewport.prepare_selected(
                 {:document, "child/literal.txt", "text/plain"},
                 parent,
                 [],
                 :object
               )

      [%{tag: "html", children: children}] = surface.prepared_dom.children
      body = Enum.find(children, &(&1.tag == "body"))
      [pre] = body.children

      assert pre.children |> Enum.map_join(&Map.get(&1, :text, "")) ==
               "\n<tag> & literal\nSecond\tline"

      assert surface.document_url =~ "/child/literal.txt"
      assert surface.context.authorization_root == parent.authorization_root
      assert surface.context.document_id != parent.document_id
      assert surface.context.depth == parent.depth + 1
      assert {:ok, group} = EmbeddedViewport.layout(surface, 150, 72)
      text = group.boxes |> Enum.filter(&(&1.type == :text)) |> Enum.map_join(& &1.text)
      assert text =~ "<tag> & literal"
      assert text =~ "Second"
      assert ResourceContext.current() == parent
    end)

    for tag <- ["object", "embed"] do
      attribute = if tag == "object", do: "data", else: "src"
      closing = if tag == "object", do: "</object>", else: ""

      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<#{tag} #{attribute}='child/literal.txt' width='240' height='100'>#{closing}",
                 base_url: root
               )

      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
      assert text =~ "<tag> & literal" and text =~ "Second"
    end
  end

  test "PNG, JPEG and restricted static SVG decode as replacements with intrinsic sizes", %{
    root: root
  } do
    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      for {source, mime, width, height} <- [
            {"child/image.png", "image/png", 40, 20},
            {"child/image.jpg", "image/jpeg", 32, 16},
            {"child/image.svg", "image/svg+xml", 40, 20}
          ] do
        assert {:ok, {:image, style}} =
                 EmbeddedViewport.prepare_selected({:image, source, mime}, parent, [], :object)

        assert style.image.width_px == width and style.image.height_px == height

        if mime == "image/svg+xml" do
          assert {:ok, request} =
                   NativeElixirPdfUtilities.Validators.SvgDocumentImageValidator.request(
                     style.image
                   )

          assert request.authored_source == File.read!(Path.join(root, source))

          assert {:ok, image} =
                   NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentImage.compose(
                     style.image,
                     60,
                     30
                   )

          assert image.format == :png
          assert {image.width_px, image.height_px} == {160, 80}
          assert image.data == :binary.copy(<<0, 0, 255>>, image.width_px * image.height_px)

          assert {:ok, pdf} =
                   NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.render(
                     [
                       %{
                         size: {60, 30},
                         boxes: [%{type: :image, image: image, x: 0, y: 0, width: 60, height: 30}]
                       }
                     ],
                     compress_streams: false
                   )

          assert pdf =~ "/Subtype /Image"
          assert pdf =~ "60 0 0 30 0 0 cm"
        else
          assert style.image.format == if(mime == "image/png", do: :png, else: :jpeg)
        end

        assert ResourceContext.current() == parent
      end
    end)

    for {tag, attribute} <- [{"object", "data"}, {"embed", "src"}],
        source <- ["child/image.png", "child/image.jpg", "child/image.svg"] do
      closing = if tag == "object", do: "Fallback</object>", else: ""

      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<p>Before image</p><#{tag} #{attribute}='#{source}' style='width:60pt;height:30pt;object-fit:contain'>#{closing}",
                 base_url: root
               )

      assert pdf =~ "/Subtype /Image"

      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
      assert text =~ "Before image"
      refute text =~ "Fallback"
    end
  end

  test "selected source bytes cannot be replaced by mappings or resolver callbacks", %{
    root: root,
    png: png
  } do
    owner = self()

    resolver = fn request ->
      send(owner, {:resolved, request})
      {:ok, "<p>Bypass</p>"}
    end

    for {tag, attribute} <- [{"object", "data"}, {"embed", "src"}] do
      closing = if tag == "object", do: "Fallback</object>", else: ""

      for source <- ["child/literal.txt", "child/document.html", "child/image.png"] do
        assert {:ok, pdf} =
                 HtmlToPdf.render(
                   "<p>Before selected source</p><#{tag} #{attribute}='#{source}'>#{closing}",
                   base_url: root,
                   assets: %{
                     source => {:bytes, "<p>Bypass</p>"},
                     Path.join(root, source) => {:bytes, png <> "replacement"}
                   },
                   asset_resolver: resolver
                 )

        assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
        refute text =~ "Bypass"
        refute text =~ "Fallback"
        refute_received {:resolved, _}
      end
    end
  end

  test "selected text and image denial, missing, MIME and decoding failures remain diagnostics",
       %{root: root} do
    for {tag, attribute} <- [{"object", "data"}, {"embed", "src"}],
        {source, mime} <- [{"child/literal.txt", "text/plain"}, {"child/image.png", "image/png"}] do
      closing = if tag == "object", do: "Fallback</object>", else: ""

      for denied <- ["https://denied.test/#{source}", "../#{source}"] do
        assert {:error, {:resource_denied, diagnostic}} =
                 HtmlToPdf.render("<#{tag} #{attribute}='#{denied}' type='#{mime}'>#{closing}",
                   base_url: root
                 )

        assert diagnostic.stage == :resource and diagnostic.source == denied
        assert diagnostic.operation == :load_embedded_resource and diagnostic.message != ""
      end

      assert {:error, {:resource_not_found, diagnostic}} =
               HtmlToPdf.render("<#{tag} #{attribute}='missing' type='#{mime}'>#{closing}",
                 base_url: root
               )

      assert diagnostic.message != "" and diagnostic.source == "missing"
    end

    File.write!(Path.join(root, "child/literal.txt"), <<255>>)

    assert {:error, {:invalid_encoding, diagnostic}} =
             HtmlToPdf.render("<object data='child/literal.txt'></object>", base_url: root)

    assert diagnostic.stage == :html and diagnostic.message != ""
    File.write!(Path.join(root, "child/image.png"), "%PDF-1.7\n")

    assert {:error, {:invalid_mime_type, diagnostic}} =
             HtmlToPdf.render("<embed src='child/image.png'>", base_url: root)

    assert diagnostic.stage == :resource and diagnostic.message =~ "MIME"
    File.write!(Path.join(root, "child/image.png"), <<137, "PNG", 13, 10, 26, 10, 0>>)

    assert {:error, {_reason, diagnostic}} =
             HtmlToPdf.render("<object data='child/image.png'></object>", base_url: root)

    assert diagnostic.message != ""
  end

  test "generated still-image markup keeps the existing restricted SVG image API", %{root: root} do
    File.write!(Path.join(root, "child/image.svg"), """
    <svg xmlns="http://www.w3.org/2000/svg" width="40" height="20"><foreignObject width="40" height="20">
    <div xmlns="http://www.w3.org/1999/xhtml">Active HTML</div></foreignObject></svg>
    """)

    for html <- ["<object data='child/image.svg'></object>", "<embed src='child/image.svg'>"] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, base_url: root)
      assert diagnostic.message =~ "foreignObject"
    end

    File.write!(
      Path.join(root, "child/image.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='20'><text y='15'>Required text</text></svg>"
    )

    assert {:error, {:invalid_font, diagnostic}} =
             HtmlToPdf.render("<embed src='child/image.svg'>", base_url: root)

    assert diagnostic.stage == :font and diagnostic.source == "child/image.svg"
    assert diagnostic.message =~ "authorized independent font bytes"

    File.write!(
      Path.join(root, "child/image.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='20'><image href='https://denied.test/image.png'/></svg>"
    )

    assert {:error, {_reason, diagnostic}} =
             HtmlToPdf.render("<object data='child/image.svg'></object>", base_url: root)

    assert diagnostic.message =~ "authorized local path"
  end

  test "source bytes are charged once and documents share depth and count budgets", %{
    root: root,
    png: png
  } do
    Limits.install(Map.put(Limits.defaults(), :max_aggregate_image_source_bytes, byte_size(png)))

    assert {:ok, _pdf} =
             HtmlToPdf.render("<object data='child/image.png'></object>", base_url: root)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<object data='child/image.png'></object><embed src='child/image.png'>",
               base_url: root
             )

    assert diagnostic.message =~ "image"
    Limits.install(Map.put(Limits.defaults(), :max_embedded_document_depth, 1))
    File.write!(Path.join(root, "child/document.html"), "<embed src='literal.txt'>")

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<embed src='child/document.html'>", base_url: root)

    assert diagnostic.message =~ "max_embedded_document_depth"
    Limits.install(Map.put(Limits.defaults(), :max_aggregate_svg_surface_pixels, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<embed src='child/image.svg'>", base_url: root)

    assert diagnostic.source == "child/image.svg"
    assert diagnostic.operation == :rasterize_document
    assert diagnostic.message =~ "max_aggregate_svg_surface_pixels"

    File.write!(Path.join(root, "child/raster.svg"), """
    <svg xmlns="http://www.w3.org/2000/svg" width="40" height="20">
    <a href="#target"><rect id="target" width="40" height="20" fill="blue"/></a></svg>
    """)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<embed src='child/raster.svg'>", base_url: root)

    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.source == "child/raster.svg"
    assert diagnostic.operation == :rasterize_document
    assert diagnostic.message =~ "max_aggregate_svg_surface_pixels"
  end

  test "invalid dispatch plans and MIME inputs fail before reads", %{root: root} do
    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      for plan <- [
            :fallback,
            {:image, "child/image.png", "text/html"},
            {:document, "child/literal.txt", "image/png"}
          ] do
        assert {:error, {:invalid_document, diagnostic}} =
                 EmbeddedViewport.prepare_selected(plan, parent, [], :object)

        assert diagnostic.message != ""
      end

      assert {:error, {:invalid_document, _}} =
               EmbeddedViewport.prepare_selected(
                 {:document, "child/literal.txt", "text/plain"},
                 parent,
                 [],
                 :iframe
               )

      assert {:error, {:invalid_document, _}} =
               EmbeddedResource.load_file("missing", "application/octet-stream", parent)

      assert {:error, {:invalid_mime_type, _}} =
               EmbeddedResource.load_file("missing", nil, parent)
    end)
  end

  test "selected SVG documents preserve aspect metadata for their actual viewport" do
    alias NativeElixirPdfUtilities.Validators.{SvgDocumentViewportValidator, SvgValidator}

    for value <- ["xMinYMax meet", "xMaxYMin slice", "none"] do
      source =
        "<svg xmlns='http://www.w3.org/2000/svg' width='120' height='40' viewBox='0 0 100 100' preserveAspectRatio='#{value}'><rect width='100' height='100'/></svg>"

      assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :image})
      assert {:ok, plan} = SvgDocumentViewportValidator.prepare(summary)
      assert plan.width == 90.0 and plan.height == 30.0
      assert plan.attributes["preserveAspectRatio"] == value

      assert {:ok, raster} =
               SvgDocumentViewportValidator.prepare_raster(
                 plan,
                 60,
                 45,
                 HtmlValidator.new_image_budget()
               )

      assert raster.width == 160 and raster.height == 120
    end

    assert {:ok, summary} =
             SvgValidator.validate_processing(
               "<svg viewBox='0 0 10 10' preserveAspectRatio='invalid'/>",
               %{mode: :image}
             )

    assert {:error, {:invalid_document, diagnostic}} =
             SvgDocumentViewportValidator.prepare(summary)

    assert diagnostic.message =~ "preserveAspectRatio"
  end

  test "embed never exposes fallback children and hidden hosts never select sources" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<embed><p>After absent embed</p><embed src='about:blank' width='40' height='20'><object style='display:none' data='https://denied.test/file.png'></object><embed hidden src='https://denied.test/file.txt'>"
             )

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "After absent embed"

    assert {:error, {:unsupported_html, _}} =
             HtmlToPdf.render("<embed src='about:blank'></embed>")
  end
end
