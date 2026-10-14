defmodule NativeElixirPdfUtilities.HtmlToPdf.InitialChildMimeTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    EmbeddedViewport,
    HtmlParser,
    Layout,
    ResourceContext,
    Style
  }

  alias NativeElixirPdfUtilities.Validators.{
    EmbeddedViewportValidator,
    HtmlValidator,
    ResourceValidator
  }

  Code.require_file("../support/png_fixture.ex", __DIR__)
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  setup do
    root =
      Path.join(System.tmp_dir!(), "native-initial-mime-#{System.unique_integer([:positive])}")

    File.mkdir_p!(root)
    text = "\n<b>Literal & markup</b>\nSecond line"
    png = PngFixture.build(40, 20, 2, 8, 0, fn _, _ -> [255, 0, 0] end)
    File.write!(Path.join(root, "literal.txt"), text)
    File.write!(Path.join(root, "unknown.document"), text)
    File.write!(Path.join(root, "image.png"), png)

    File.write!(
      Path.join(root, "image.jpg"),
      NativeElixirPdfUtilities.TestSupport.JpegFixture.baseline(32, 16)
    )

    limits = Limits.effective()

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(root)
    end)

    %{root: root, text: text, png: png}
  end

  test "literal file documents retain source bytes as text in distinct issued scopes", %{
    root: root,
    text: text
  } do
    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      for source <- ["literal.txt", "unknown.document"] do
        assert {:ok, prepared} = EmbeddedViewport.prepare_iframe_document(source, parent)
        [%{tag: "html", children: children}] = prepared.prepared_dom.children
        [pre] = Enum.find(children, &(&1.tag == "body")).children
        assert pre.tag == "pre"
        assert Enum.map_join(pre.children, &Map.get(&1, :text, "")) == text
        assert prepared.source == {:file, Path.join(root, source)}
        assert prepared.context.document_id != parent.document_id
        assert prepared.context.tree_scope_id != parent.tree_scope_id
        assert prepared.context.authorization_root == parent.authorization_root
        assert prepared.context.image_budget == parent.image_budget
        assert :ok = ResourceValidator.validate_context(prepared.context)
        assert {:ok, group} = EmbeddedViewport.layout(prepared, 150, 75)
        rendered = group.boxes |> Enum.filter(&(&1.type == :text)) |> Enum.map_join(& &1.text)
        assert rendered =~ "<b>Literal & markup</b>"
        assert ResourceContext.current() == parent
      end
    end)
  end

  test "a fitting literal document uses its full initial width before wrapping", %{root: root} do
    File.write!(Path.join(root, "fitting.txt"), "<b>Literal & markup</b>\nSecond line\n")

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} = EmbeddedViewport.prepare_iframe_document("fitting.txt", parent)
      assert prepared.initial_document_kind == :text
      geometry = EmbeddedViewportValidator.initial_viewport(prepared, 90, 60)
      refute geometry.vertical
      assert geometry.client_width == 90
      assert {:ok, group} = EmbeddedViewport.layout(prepared, 90, 60)
      assert group.client_rect == %{x: 0.0, y: 0.0, width: 90.0, height: 60.0}
      assert Enum.count(group.boxes, &(&1.type == :text)) == 3

      assert Enum.map_join(Enum.filter(group.boxes, &(&1.type == :text)), & &1.text) =~
               "Second line"
    end)
  end

  test "PNG and JPEG initial documents keep natural block geometry and exact authorized bytes", %{
    root: root,
    png: png
  } do
    owner = self()

    resolver = fn request ->
      send(owner, {:resolved, request})
      {:ok, png}
    end

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      for {source, width, height} <- [{"image.png", 30.0, 15.0}, {"image.jpg", 24.0, 12.0}] do
        previous_count = :atomics.get(parent.image_budget, 1)
        previous_bytes = :atomics.get(parent.image_budget, 2)
        path = Path.join(root, source)

        assert {:ok, prepared} =
                 EmbeddedViewport.prepare_iframe_document(source, parent,
                   assets: %{path => {:bytes, "replacement must not enter decoder"}},
                   asset_resolver: resolver
                 )

        assert {:bytes, bytes} = prepared.options[:assets][path]
        assert bytes == File.read!(path)
        assert {:ok, surface} = EmbeddedViewport.style_viewport(prepared, 90, 60)
        assert :atomics.get(parent.image_budget, 1) == previous_count + 1
        [body] = surface.tree.children
        [image] = body.children
        assert image.style.display == :image and image.style.image_display == :block
        assert body.style.margin == %{top: 0.0, bottom: 0.0, left: 0.0, right: 0.0}
        assert {:ok, group} = EmbeddedViewport.layout(surface, 90, 60)
        box = Enum.find(group.boxes, &(&1.type == :image))
        assert {box.x, box.width, box.height} == {0.0, width, height}
        # The deliberately pre-styled surface then reflows at the initial auto
        # gutter and at its final gutter-free width. Each real decode is charged.
        assert :atomics.get(parent.image_budget, 1) == previous_count + 3
        assert :atomics.get(parent.image_budget, 2) == previous_bytes + 3 * byte_size(bytes)
        assert prepared.context.authorization_root == parent.authorization_root
        assert ResourceContext.current() == parent
      end
    end)

    refute_received {:resolved, _}
  end

  test "text and image sources share frame and iframe initialization", %{root: root} do
    for html <- [
          "<iframe src='literal.txt'></iframe>",
          "<frameset border='0' cols='*'><frame src='literal.txt'></frameset>"
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html, base_url: root, page_size: {180, 120}, margin: 0)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "<b>Literal & markup</b>"
    end
  end

  test "authorization and selected content failures cannot use mappings, resolver or fallback", %{
    root: root,
    png: png
  } do
    owner = self()

    resolver = fn request ->
      send(owner, {:resolved, request})
      {:ok, png}
    end

    for source <- ["https://denied.test/image.png", "../outside.txt"] do
      assert {:error, {:resource_denied, diagnostic}} =
               HtmlToPdf.render("<iframe src='#{source}'></iframe>",
                 base_url: root,
                 assets: %{source => {:bytes, png}},
                 asset_resolver: resolver
               )

      assert diagnostic.source == source and diagnostic.operation == :load_embedded_resource
    end

    refute_received {:resolved, _}
    File.write!(Path.join(root, "invalid.txt"), <<255>>)
    File.write!(Path.join(root, "mismatch.txt"), png)
    File.write!(Path.join(root, "broken.png"), "not a PNG")

    for {source, reason} <- [
          {"invalid.txt", :invalid_encoding},
          {"mismatch.txt", :invalid_mime_type},
          {"broken.png", :invalid_document}
        ] do
      assert {:error, {^reason, diagnostic}} =
               HtmlToPdf.render("<iframe src='#{source}'>Unused fallback</iframe>",
                 base_url: root
               )

      assert diagnostic.message != ""
    end
  end

  test "srcdoc source precedence bypasses MIME dispatch without reading the local src", %{
    root: root
  } do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<iframe src='missing.png' srcdoc='&lt;p&gt;Inline selected&lt;/p&gt;'></iframe>",
               base_url: root
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Inline selected"
  end

  test "shared source and pixel ceilings diagnose before decoder allocation and retain current context",
       %{root: root, png: png} do
    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      limits = Limits.effective()
      Limits.install(Map.put(limits, :max_image_source_bytes, byte_size(png) - 1))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               EmbeddedViewport.prepare_iframe_document("image.png", parent)

      assert diagnostic.operation == :load_embedded_resource
      assert ResourceContext.current() == parent
      Limits.install(Map.put(limits, :max_decoded_image_bytes, 1))
      assert {:ok, prepared} = EmbeddedViewport.prepare_iframe_document("image.png", parent)

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               EmbeddedViewport.style_viewport(prepared, 90, 60)

      assert diagnostic.message =~ "decoded image"
      assert ResourceContext.current() == parent
      Limits.install(limits)
      :continued
    end)
    |> then(&assert(&1 == :continued))
  end

  test "malformed MIME dispatch input and unsupported file types use shared diagnostics" do
    for {bytes, path} <- [{:invalid, "a.txt"}, {"bytes", 12}] do
      assert {:error, {:invalid_document, diagnostic}} =
               EmbeddedViewportValidator.prepare_initial_file(bytes, path)

      assert diagnostic.stage == :resource and diagnostic.operation == :load_embedded_resource
    end

    assert {:error, {:not_implemented, diagnostic}} =
             EmbeddedViewportValidator.prepare_initial_file("", "document.pdf")

    assert diagnostic.source == "document.pdf"

    assert {:error, {:invalid_mime_type, diagnostic}} =
             EmbeddedViewportValidator.prepare_initial_file("%PDF-1.7", "a.html")

    assert diagnostic.message =~ "conflicts"
  end

  test "unpainted normal-flow bottom margins contribute to viewport scrolling extent" do
    source = "<body style='margin:0'><div style='height:40px;margin-bottom:20px'></div></body>"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, dom} = HtmlParser.parse_document_detailed(source)
      assert {:ok, tree} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout_viewport(tree, 90, 30)
      assert [%{type: :flow_marker, role: :explicit_block_space, height: 30.0}] = layout.boxes

      assert {:ok, blank_pdf} =
               NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.render([
                 %{boxes: layout.boxes, size: {90, 30}}
               ])

      assert {:error, {:no_extractable_text, _}} = Text.extract(blank_pdf)

      assert {:ok, blank_form} =
               NativeElixirPdfUtilities.Validators.PdfFormValidator.prepare_source(blank_pdf)

      assert String.trim(blank_form.stream) == ""
      assert layout.content_height == 45.0
      assert {:ok, prepared} = EmbeddedViewport.prepare_iframe_document({:srcdoc, source}, parent)
      assert {:ok, group} = EmbeddedViewport.layout(prepared, 90, 30)
      [content, _ui] = group.boxes
      assert content.client_rect.width == 78.75
      assert ResourceContext.current() == parent
    end)
  end

  test "initial auto gutter reflows away for fitting documents and respects disabled scrolling" do
    for host <- [:iframe, :frame, :object, :embed] do
      initial = EmbeddedViewportValidator.initial_viewport(%{host: host}, 90, 60)
      assert initial.vertical and initial.client_width == 78.75
    end

    for host <- [:iframe, :frame] do
      initial =
        EmbeddedViewportValidator.initial_viewport(%{host: host, frame_scrollbars: :none}, 90, 60)

      refute initial.vertical
      assert initial.client_width == 90
    end

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      for source <- [
            "<body style='margin:0'><div style='height:1px'></div></body>",
            "<body style='margin:0;overflow:hidden'><div style='height:200px'></div></body>"
          ] do
        assert {:ok, prepared} =
                 EmbeddedViewport.prepare_iframe_document({:srcdoc, source}, parent)

        assert {:ok, group} = EmbeddedViewport.layout(prepared, 90, 60)
        assert group.client_rect == %{x: 0.0, y: 0.0, width: 90.0, height: 60.0}
      end
    end)
  end
end
