defmodule NativeElixirPdfUtilities.HtmlToPdf.FramesetTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{FramesetValidator, HtmlValidator}
  Code.require_file("../support/png_fixture.ex", __DIR__)

  setup do
    root = Path.join(System.tmp_dir!(), "native-frameset-#{System.unique_integer([:positive])}")
    File.mkdir_p!(root)

    File.write!(
      Path.join(root, "child.html"),
      "<body style='margin:0;overflow:hidden'><p style='margin:0'>Frame child</p><div style='width:300pt;height:300pt;background:red'></div></body>"
    )

    File.write!(Path.join(root, "margins.html"), "<p style='margin:0'>Frame margins</p>")
    limits = Limits.effective()

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(root)
    end)

    %{root: root}
  end

  test "fixed, percentage, relative and oversized tracks match legacy priority and integer remainders" do
    assert {:ok, plan} =
             FramesetValidator.prepare(%{"border" => "0", "rows" => "80,*", "cols" => "25%,*"})

    assert {:ok, %{columns: [75.0, 225.0], rows: [60.0, 139.5]}} =
             FramesetValidator.prepare_layout(plan, 300, 200)

    assert {:ok, plan} = FramesetValidator.prepare(%{"frameborder" => "0", "cols" => "80,25%,2*"})

    assert {:ok, %{columns: [60.0, 75.0, 165.0]}} =
             FramesetValidator.prepare_layout(plan, 300, 90)

    assert {:ok, plan} = FramesetValidator.prepare(%{"border" => "0", "cols" => "500,100%"})
    assert {:ok, %{columns: [300.0, +0.0]}} = FramesetValidator.prepare_layout(plan, 300, 90)
    assert {:ok, plan} = FramesetValidator.prepare(%{"border" => "0", "cols" => "100%,100%,*"})

    assert {:ok, %{columns: [150.0, 150.0, +0.0]}} =
             FramesetValidator.prepare_layout(plan, 300, 90)

    assert {:ok, tracks} = FramesetValidator.tracks("0*,*,2*,")
    assert tracks == [relative: 1.0, relative: 1.0, relative: 2.0]
    assert {:ok, tracks} = FramesetValidator.tracks("garbage,,25.5%")
    assert tracks == [absolute: 0.0, relative: 1.0, percentage: 25.5]
    assert {:ok, plan} = FramesetValidator.prepare(%{"border" => "0", "cols" => "1,1"})
    assert {:ok, %{columns: [75.0, 75.0]}} = FramesetValidator.prepare_layout(plan, 150, 90)
    assert {:ok, %{columns: [+0.0, +0.0]}} = FramesetValidator.prepare_layout(plan, 0, 0)
  end

  test "dimension tokens trim only ASCII whitespace and charge their original byte length" do
    assert {:ok, tracks} = FramesetValidator.tracks(" \t80\r, \n25%\f, 2* , \t ")
    assert tracks == [absolute: 80.0, percentage: 25.0, relative: 2.0, relative: 1.0]

    assert {:ok, plan} =
             FramesetValidator.prepare(%{"border" => "0", "cols" => "80, 25%, 2*"})

    assert {:ok, %{columns: [60.0, 75.0, 165.0]}} =
             FramesetValidator.prepare_layout(plan, 300, 90)

    assert {:ok, [absolute: +0.0, absolute: +0.0]} =
             FramesetValidator.tracks("\u00A025%,\u200380")

    Limits.install(Map.put(Limits.effective(), :max_html_numeric_token_bytes, 3))
    assert {:error, {:resource_limit_exceeded, diagnostic}} = FramesetValidator.tracks(" 80 ")
    assert diagnostic.stage == :limits
    assert diagnostic.message =~ "max_html_numeric_token_bytes"
  end

  test "undersubscribed unequal tracks refill proportionally with integer residues" do
    for cols <- ["40,80", "10%,20%", "1.9,2.9", "0.5*,1.9*"] do
      assert {:ok, plan} = FramesetValidator.prepare(%{"border" => "0", "cols" => cols})
      assert {:ok, %{columns: columns}} = FramesetValidator.prepare_layout(plan, 300, 90)
      expected = if cols == "0.5*,1.9*", do: [150.0, 150.0], else: [99.75, 200.25]
      assert columns == expected
    end

    assert {:ok, plan} = FramesetValidator.prepare(%{"border" => "0", "cols" => "0,0,0"})

    assert {:ok, %{columns: [99.75, 99.75, 100.5]}} =
             FramesetValidator.prepare_layout(plan, 300, 90)
  end

  test "track allocation work joins the shared cell budget before allocation" do
    assert {:ok, plan} = FramesetValidator.prepare(%{"border" => "0", "cols" => "*"})
    Limits.install(Map.put(Limits.effective(), :max_html_tree_work, 48))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               FramesetValidator.prepare_layout(plan, 300, 90)
             end)

    assert diagnostic.stage == :layout
    assert diagnostic.message =~ "max_html_tree_work"

    Limits.install(Map.put(Limits.effective(), :max_html_tree_work, 49))

    assert {:ok, %{columns: [300.0], rows: [90.0]}} =
             HtmlValidator.with_render_budget(fn ->
               FramesetValidator.prepare_layout(plan, 300, 90)
             end)
  end

  test "display none legacy frames initialize and extra cells validate at zero area", %{
    root: root
  } do
    html =
      "<frameset border='0' style='display:none'><frame src='child.html' style='display:none'><frame src='extra.html' style='display:none'></frameset>"

    File.write!(Path.join(root, "extra.html"), "<body style='margin:0'>Extra child<input></body>")

    assert {:error, {:unsupported_form, diagnostic}} = HtmlToPdf.render(html, base_url: root)
    assert diagnostic.message =~ "form"
    assert {:ok, pdf} = HtmlToPdf.render(html, base_url: root, forms: :static)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Frame child"
    refute text =~ "Extra child"
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)

    for source <- ["https://denied.test/child.html", "malformed.html"] do
      File.write!(Path.join(root, "malformed.html"), <<255>>)
      selected = String.replace(html, "extra.html", source)

      assert {:error, {_reason, failure}} =
               HtmlToPdf.render(selected, base_url: root, forms: :static)

      assert failure.message != ""
    end

    Limits.install(Map.put(Limits.effective(), :max_embedded_documents, 1))

    assert {:error, {:resource_limit_exceeded, budget}} =
             HtmlToPdf.render(html, base_url: root, forms: :static)

    assert budget.message =~ "max_embedded_documents"
  end

  test "full-document frameset is the body alternative and noframes remains literal inactive source" do
    source =
      "<html><head><title>Frames</title></head><frameset cols='*' border='0'><frame><noframes><img src='https://denied.test/image'><style>@font-face{font-family:X;src:url(https://denied.test/font)}</style>Fallback</noframes></frameset></html>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(source)
    [html] = dom.children
    [head, frameset] = html.children
    assert head.tag == "head" and frameset.tag == "frameset"
    [frame, fallback] = frameset.children
    assert frame.children == []
    assert fallback.tag == "noframes"
    assert [literal] = fallback.children
    assert literal.type == :text and literal.text =~ "https://denied.test/image"
    assert {:ok, pdf} = HtmlToPdf.render(source, page_size: {150, 90}, margin: 0)
    refute pdf =~ "Fallback"

    for invalid <- [
          "<body><frameset border='0'><frame></frameset></body>",
          "<frameset border='0'><p>Invalid grid child</p></frameset>",
          "<frame src='child.html'>"
        ] do
      assert {:error, {_reason, diagnostic}} = HtmlParser.parse_document_detailed(invalid)
      assert diagnostic.stage == :html
    end
  end

  test "nested native grids compose actual clipped cell viewports and preserve selected document scope",
       %{root: root} do
    html =
      "<frameset cols='80,25%,2*' border='0'><frame src='child.html'><frameset rows='40,2*,*'><frame src='child.html'><frame src='child.html'><frame src='about:blank#empty'></frameset><frame src='child.html'></frameset>"

    ResourceContext.run(
      [authorization_root: root],
      HtmlValidator.new_image_budget(),
      fn context ->
        assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
        assert {:ok, tree} = Style.compute_detailed(dom, [base_url: root], context.image_budget)
        [frameset] = tree.children
        [left, nested, right] = frameset.children
        assert left.style.embedded_document.host == :frame
        assert left.style.embedded_document.context.authorization_root == root
        assert left.style.embedded_document.context.image_budget == context.image_budget

        assert left.style.embedded_document.context.document_id !=
                 right.style.embedded_document.context.document_id

        assert nested.style._frameset.columns == [relative: 1.0]
        assert {:ok, layout} = Layout.layout(tree, page_size: {300, 200}, margin: 0)
        [grid] = layout.boxes
        assert grid.clip == %{x: 0.0, y: 0.0, width: 300, height: 200}
        [left_box, nested_box, right_box] = grid.boxes
        assert left_box.width == 60 and nested_box.width == 75 and right_box.width == 165
        assert nested_box.clip.width == 75 and length(nested_box.boxes) == 3
        assert Enum.at(nested_box.boxes, 0).height == 30
        assert ResourceContext.current() == context
      end
    )
  end

  test "selected frames deny remote and malformed documents without noframes fallback or resolver access",
       %{root: root} do
    owner = self()

    resolver = fn request ->
      send(owner, {:resolved, request})
      {:ok, "<p>Bypass</p>"}
    end

    assert {:error, {:resource_denied, diagnostic}} =
             HtmlToPdf.render(
               "<frameset border='0'><frame src='https://denied.test/page'><noframes>Fallback</noframes></frameset>",
               base_url: root,
               asset_resolver: resolver
             )

    assert diagnostic.source == "https://denied.test/page"
    File.write!(Path.join(root, "bad.html"), <<255>>)

    assert {:error, {:invalid_encoding, diagnostic}} =
             HtmlToPdf.render(
               "<frameset border='0'><frame src='bad.html'><noframes>Fallback</noframes></frameset>",
               base_url: root,
               asset_resolver: resolver
             )

    assert diagnostic.source == "bad.html"
    refute_receive {:resolved, _}
  end

  test "default/custom borders, author geometry and visible noframes produce explicit diagnostics",
       %{root: root} do
    for html <- [
          "<frameset><frame></frameset>",
          "<frameset border='4'><frame></frameset>",
          "<frameset border='0' style='height:50pt'><frame></frameset>",
          "<frameset border='0'><frame scrolling='no'></frameset>",
          "<frameset border='0'><noframes style='display:block'>Fallback</noframes></frameset>"
        ] do
      assert {:error, {:not_implemented, diagnostic}} = HtmlToPdf.render(html, base_url: root)
      assert diagnostic.message != "" and diagnostic.source == "frameset"
    end
  end

  test "zero border width preserves the separate inherited enable flag", %{root: root} do
    assert {:ok, parent} = FramesetValidator.prepare(%{"border" => "0"})
    assert parent.border_enabled == true and parent.border_width == 0
    assert {:ok, nested} = FramesetValidator.prepare(%{"frameborder" => "1"}, parent)
    assert nested.border_enabled == true and nested.border_width == 0
    assert {:ok, frame} = FramesetValidator.prepare_frame(%{"frameborder" => "1"}, parent)
    assert frame.border_enabled == true

    assert {:error, {:not_implemented, diagnostic}} =
             FramesetValidator.prepare(%{"border" => "10"}, parent)

    assert diagnostic.stage == :layout and diagnostic.source == "frameset"
    assert diagnostic.message =~ "beveled borders"

    assert {:ok, disabled} = FramesetValidator.prepare(%{"frameborder" => "no"})
    assert {:ok, nested} = FramesetValidator.prepare(%{"border" => "10"}, disabled)
    assert nested.border_enabled == false and nested.border_width == 0

    assert {:error, {:not_implemented, _}} =
             FramesetValidator.prepare(%{"border" => "10", "frameborder" => "yes"}, disabled)

    assert {:ok, frame} = FramesetValidator.prepare_frame(%{"frameborder" => "yes"}, parent)
    assert frame.border_enabled == false

    assert {:error, {:invalid_document, _}} =
             FramesetValidator.prepare(%{}, %{border_width: "bad"})

    assert {:error, {:invalid_document, _}} = FramesetValidator.prepare(%{"frameborder" => false})

    assert {:error, {:not_implemented, public}} =
             HtmlToPdf.render(
               "<frameset border='0'><frame src='child.html'><frameset border='10'><frame src='child.html'></frameset></frameset>",
               base_url: root
             )

    assert public.message =~ "beveled borders"
  end

  test "frame margin defaults retain author CSS precedence", %{root: root} do
    ResourceContext.run(
      [authorization_root: root],
      HtmlValidator.new_image_budget(),
      fn context ->
        assert {:ok, dom} =
                 HtmlParser.parse_document_detailed(
                   "<frameset border='0'><frame src='margins.html' marginwidth='12' marginheight='4'></frameset>"
                 )

        assert {:ok, tree} = Style.compute_detailed(dom, [base_url: root], context.image_budget)
        [frameset] = tree.children
        [frame] = frameset.children

        assert {:ok, child} =
                 NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewport.style_viewport(
                   frame.style.embedded_document,
                   150,
                   90
                 )

        [body] = child.tree.children
        assert body.style.margin == %{left: 9.0, right: 9.0, top: 3.0, bottom: 3.0}

        File.write!(
          Path.join(root, "override.html"),
          "<body style='margin:0'><p>Author override</p></body>"
        )

        assert {:ok, dom} =
                 HtmlParser.parse_document_detailed(
                   "<frameset border='0'><frame src='override.html' marginwidth='12' marginheight='4'></frameset>"
                 )

        assert {:ok, tree} = Style.compute_detailed(dom, [base_url: root], context.image_budget)
        [frameset] = tree.children
        [frame] = frameset.children

        assert {:ok, child} =
                 NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewport.style_viewport(
                   frame.style.embedded_document,
                   150,
                   90
                 )

        [body] = child.tree.children
        assert body.style.margin == %{left: 0.0, right: 0.0, top: 0.0, bottom: 0.0}
      end
    )
  end

  test "track and cell allocations use central limits and frame documents share existing ceilings",
       %{root: root} do
    limits = Limits.effective()
    Limits.install(Map.put(limits, :max_frameset_tracks, 1))
    assert {:error, {:resource_limit_exceeded, diagnostic}} = FramesetValidator.tracks("*,*")
    assert diagnostic.message =~ "max_frameset_tracks"
    Limits.install(Map.put(limits, :max_frameset_cells, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FramesetValidator.prepare(%{"border" => "0", "rows" => "*,*", "cols" => "*,*"})

    assert diagnostic.message =~ "max_frameset_cells"
    Limits.install(Map.put(limits, :max_embedded_documents, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<frameset border='0' cols='*,*'><frame src='child.html'><frame src='child.html'></frameset>",
               base_url: root
             )

    assert diagnostic.message =~ "max_embedded_documents"
  end

  test "malformed and exhausted native grid requests return shared diagnostics before painting" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Frameset

    for node <- [
          nil,
          %{},
          %{type: :element, tag: "frameset", style: %{_frameset: nil}, children: []}
        ] do
      assert {:error, {_reason, diagnostic}} = Frameset.layout(node, 150, 90)
      assert diagnostic.stage == :layout and diagnostic.reason != nil and diagnostic.message != ""
    end

    assert {:error, {:invalid_document, _}} = FramesetValidator.tracks(<<255>>)

    assert {:error, {:invalid_document, _}} =
             FramesetValidator.prepare_body_margins(%{horizontal: -1, vertical: nil})

    assert {:error, {:not_implemented, _}} =
             FramesetValidator.validate_style(%{display: :block, padding: nil})

    assert {:ok, plan} = FramesetValidator.prepare(%{"border" => "0"})

    node = %{
      type: :element,
      tag: "frameset",
      style: %{_frameset: plan, display: :block},
      children: [nil]
    }

    assert {:error, {:invalid_document, _}} = Frameset.layout(node, 150, 90)
    limits = Limits.effective()
    Limits.install(Map.put(limits, :max_html_tree_work, 0))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FramesetValidator.prepare_layout(plan, 150, 90)

    assert diagnostic.message =~ "max_html_tree_work"
  end

  test "zero-area cells still select and validate content before omitting paint destinations", %{
    root: root
  } do
    File.write!(
      Path.join(root, "linked.html"),
      "<p id='target'><a href='#target'>Clipped link</a></p>"
    )

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<frameset border='0' cols='500,100%'><frame src='child.html'><frame src='linked.html'></frameset>",
               base_url: root,
               page_size: {300, 200},
               margin: 0
             )

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    refute text =~ "Clipped link"

    assert {:error, {:resource_denied, _}} =
             HtmlToPdf.render(
               "<frameset border='0' cols='500,100%'><frame src='child.html'><frame src='https://denied.test/zero.html'></frameset>",
               base_url: root
             )

    File.write!(Path.join(root, "invalid-zero.html"), <<255>>)

    assert {:error, {:invalid_encoding, _}} =
             HtmlToPdf.render(
               "<frameset border='0' cols='500,100%'><frame src='child.html'><frame src='invalid-zero.html'></frameset>",
               base_url: root
             )
  end

  test "initial frame names remain metadata while target navigation is explicitly diagnosed", %{
    root: root
  } do
    ResourceContext.run(
      [authorization_root: root],
      HtmlValidator.new_image_budget(),
      fn context ->
        assert {:ok, dom} =
                 HtmlParser.parse_document_detailed(
                   "<frameset border='0'><frame src='child.html' name='named-child'></frameset>"
                 )

        assert {:ok, tree} = Style.compute_detailed(dom, [base_url: root], context.image_budget)
        [frameset] = tree.children
        [frame] = frameset.children
        assert frame.style.embedded_document.frame_name == "named-child"
      end
    )

    for target <- ["named-child", "_parent", "_top"] do
      File.write!(
        Path.join(root, "navigation.html"),
        "<a href='child.html' target='#{target}'>Navigation</a>"
      )

      assert {:error, {:not_implemented, diagnostic}} =
               HtmlToPdf.render("<frameset border='0'><frame src='navigation.html'></frameset>",
                 base_url: root
               )

      assert diagnostic.message =~ "target navigation"
    end

    assert {:error, {:invalid_document, _}} = FramesetValidator.validate_document_navigation(nil)
  end

  test "zero-width and zero-height frames retain field and shared allocation failures", %{
    root: root
  } do
    File.write!(
      Path.join(root, "zero-fields.html"),
      "<input name='shared' value='Clipped field'>"
    )

    File.write!(
      Path.join(root, "zero-links.html"),
      "<p id='target'><a href='#target'>Zero cell link</a></p>"
    )

    File.write!(Path.join(root, "zero-image.html"), "<img src='small.png'>")

    File.write!(
      Path.join(root, "small.png"),
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(1, 1, 2, 8, 0, fn _, _ ->
        [20, 150, 80]
      end)
    )

    limits = Limits.effective()

    for axis <- ["rows", "cols"] do
      html =
        "<frameset border='0' #{axis}='500,100%'><frame src='child.html'><frame src='zero-links.html'></frameset>"

      assert {:ok, pdf} = HtmlToPdf.render(html, base_url: root, page_size: {300, 200}, margin: 0)
      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
      refute text =~ "Zero cell link"
      html = String.replace(html, "zero-links.html", "zero-fields.html")

      assert {:error, {:unsupported_form, diagnostic}} =
               HtmlToPdf.render(html, base_url: root, page_size: {300, 200}, margin: 0)

      assert diagnostic.operation == :layout_embedded_viewport

      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 base_url: root,
                 page_size: {300, 200},
                 margin: 0,
                 forms: :static
               )

      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
      Limits.install(Map.put(limits, :max_embedded_documents, 1))

      assert {:error, {:resource_limit_exceeded, _}} =
               HtmlToPdf.render(html, base_url: root, forms: :static)

      Limits.install(Map.put(limits, :max_aggregate_decoded_image_bytes, 0))
      html = String.replace(html, "zero-fields.html", "zero-image.html")

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render(html, base_url: root, page_size: {300, 200}, margin: 0)

      assert diagnostic.message =~ "image"
      Limits.install(limits)
    end
  end
end
