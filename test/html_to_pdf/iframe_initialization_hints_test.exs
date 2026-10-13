defmodule NativeElixirPdfUtilities.HtmlToPdf.IframeInitializationHintsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedViewport, HtmlParser, ResourceContext, Style}

  alias NativeElixirPdfUtilities.Validators.{
    EmbeddedViewportValidator,
    FramesetValidator,
    HtmlValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "frames and iframes share legacy margins, numeric recovery and scrollbar keywords" do
    for {attributes, margins, scrollbars} <- [
          {%{}, %{horizontal: nil, vertical: nil}, :auto},
          {%{"marginwidth" => "12", "marginheight" => "4", "scrolling" => "NO"},
           %{horizontal: 9.0, vertical: 3.0}, :none},
          {%{"marginwidth" => "  +12suffix", "marginheight" => "4.5", "scrolling" => "off"},
           %{horizontal: 9.0, vertical: 3.0}, :none},
          {%{"marginwidth" => "-2", "marginheight" => "-3", "scrolling" => "noscroll"},
           %{horizontal: nil, vertical: nil}, :none},
          {%{"marginwidth" => "invalid", "marginheight" => "2147483648", "scrolling" => " no "},
           %{horizontal: 0.0, vertical: 0.0}, :auto}
        ] do
      for host <- [:iframe, :frame] do
        assert {:ok, %{margins: ^margins, scrollbars: ^scrollbars}} =
                 EmbeddedViewportValidator.prepare_initialization(attributes, host)
      end

      assert {:ok, frame} = FramesetValidator.prepare_frame(attributes, nil)
      assert frame.margins == margins and frame.scrollbars == scrollbars
    end
  end

  test "iframe border hints use the browser int32 conversion and retain CSS precedence" do
    for value <- ["0", "0suffix", "bad", "", "2147483648"] do
      assert {:ok, %{border_widths: edges}} =
               HtmlValidator.iframe_hints(%{"frameborder" => value})

      assert edges == %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    end

    for value <- ["1", "-1", "2"] do
      assert {:ok, hints} = HtmlValidator.iframe_hints(%{"frameborder" => value})
      refute Map.has_key?(hints, :border_widths)
      assert hints._iframe_border and hints.border_color == {238 / 255, 238 / 255, 238 / 255}
    end

    assert HtmlValidator.parse_html_legacy_int32("-2147483648") == {:ok, -2_147_483_648}
    assert HtmlValidator.parse_html_legacy_int32("2147483647") == {:ok, 2_147_483_647}
    assert HtmlValidator.parse_html_legacy_int32("-2147483649") == :error

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      source =
        "<iframe frameborder='0' marginwidth='12' marginheight='4' style='border:3px solid blue' srcdoc='&lt;body style=&quot;margin:11px&quot;&gt;&lt;div&gt;Child&lt;/div&gt;&lt;/body&gt;'></iframe>"

      assert {:ok, dom} = HtmlParser.parse_document_detailed(source)
      assert {:ok, tree} = Style.compute_detailed(dom, [], parent.image_budget)
      [body] = tree.children
      [iframe] = body.children
      assert iframe.style.border_widths == %{top: 2.25, right: 2.25, bottom: 2.25, left: 2.25}

      assert iframe.style.border_styles == %{
               top: :solid,
               right: :solid,
               bottom: :solid,
               left: :solid
             }

      assert {:ok, styled} =
               EmbeddedViewport.style_viewport(iframe.style.embedded_document, 90, 60)

      [child_body] = styled.tree.children
      assert child_body.style.margin == %{top: 8.25, right: 8.25, bottom: 8.25, left: 8.25}
      assert styled.context.image_budget == parent.image_budget
      assert styled.context.document_id != parent.document_id
      assert ResourceContext.current() == parent
    end)
  end

  test "iframe descriptors carry real margins and client scrollbar geometry without source replacement" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      for {hint, client} <- [
            {"no", {90.0, 60.0}},
            {"OFF", {90.0, 60.0}},
            {"yes", {78.75, 48.75}},
            {" no ", {78.75, 48.75}}
          ] do
        source =
          "<iframe width='120' height='80' marginwidth='12' marginheight='4' scrolling='#{hint}' srcdoc='&lt;div style=&quot;width:800px;height:800px;background:red&quot;&gt;&lt;/div&gt;'></iframe>"

        assert {:ok, dom} = HtmlParser.parse_document_detailed(source)
        assert {:ok, tree} = Style.compute_detailed(dom, [], parent.image_budget)
        [body] = tree.children
        [iframe] = body.children
        surface = iframe.style.embedded_document
        assert surface.host == :iframe
        assert surface.frame_margins == %{horizontal: 9.0, vertical: 3.0}

        assert {:ok, styled} =
                 EmbeddedViewport.style_viewport(surface, elem(client, 0), elem(client, 1))

        [child_body] = styled.tree.children
        assert child_body.style.margin == %{top: 3.0, right: 9.0, bottom: 3.0, left: 9.0}
        assert {:ok, group} = EmbeddedViewport.layout(surface, 90, 60)

        rect =
          case group do
            %{client_rect: rect} -> rect
            %{boxes: [%{client_rect: rect} | _]} -> rect
          end

        assert {rect.width, rect.height} == client
        assert surface.context.authorization_root == parent.authorization_root
        assert ResourceContext.current() == parent
      end
    end)
  end

  test "malformed direct initialization records and numeric resource limits return shared diagnostics" do
    for {attributes, host} <- [{nil, :iframe}, {%{}, :object}, {%{"scrolling" => 12}, :iframe}] do
      assert {:error, {:invalid_document, diagnostic}} =
               EmbeddedViewportValidator.prepare_initialization(attributes, host)

      assert diagnostic.stage == :layout and diagnostic.operation == :initialize_child_viewport
      assert diagnostic.message != "" and diagnostic.module == EmbeddedViewportValidator
    end

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlValidator.iframe_hints(%{"frameborder" => 12})

    assert diagnostic.source == "iframe" and diagnostic.operation == :prepare_hints
    assert {:error, {:invalid_document, _}} = HtmlValidator.iframe_hints(nil)

    Limits.install(Map.put(Limits.effective(), :max_html_numeric_token_bytes, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<iframe frameborder='00' src='https://denied.test/never'></iframe>"
             )

    assert diagnostic.source == "00" and diagnostic.operation == :render

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             EmbeddedViewportValidator.prepare_initialization(%{"marginwidth" => "12"}, :iframe)

    assert diagnostic.source == "12" and diagnostic.message =~ "max_html_numeric_token_bytes"
  end

  test "invalid prepared scrollbar policy remains diagnostic for iframe and frame hosts" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} =
               EmbeddedViewport.prepare_iframe_document({:blank, "about:blank"}, parent)

      for host <- [:iframe, :frame] do
        assert {:error, {:invalid_layout, diagnostic}} =
                 EmbeddedViewportValidator.prepare_layout(
                   Map.merge(prepared, %{host: host, frame_scrollbars: :invalid}),
                   90,
                   60
                 )

        assert diagnostic.operation == :layout_embedded_viewport
        assert diagnostic.message =~ "validated initial scrollbar"
      end

      assert ResourceContext.current() == parent
    end)
  end

  test "outer initialization margins do not leak into a nested selected object document" do
    root = Path.join(System.tmp_dir!(), "iframe-hints-#{System.unique_integer([:positive])}")
    File.mkdir_p!(root)
    on_exit(fn -> File.rm_rf!(root) end)

    File.write!(
      Path.join(root, "outer.html"),
      "<object data='leaf.html' type='text/html'></object>"
    )

    File.write!(
      Path.join(root, "leaf.html"),
      "<div style='width:10px;height:10px;background:green'></div>"
    )

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, dom} =
               HtmlParser.parse_document_detailed(
                 "<iframe marginwidth='12' marginheight='4' src='outer.html'></iframe>"
               )

      assert {:ok, tree} = Style.compute_detailed(dom, [base_url: root], parent.image_budget)
      [body] = tree.children
      [iframe] = body.children

      assert {:ok, outer} =
               EmbeddedViewport.style_viewport(iframe.style.embedded_document, 90, 60)

      [outer_body] = outer.tree.children
      assert outer_body.style.margin == %{top: 3.0, right: 9.0, bottom: 3.0, left: 9.0}
      [object] = outer_body.children

      assert {:ok, inner} =
               EmbeddedViewport.style_viewport(object.style.embedded_document, 45, 30)

      [inner_body] = inner.tree.children
      assert inner_body.style.margin == %{top: 6.0, right: 6.0, bottom: 6.0, left: 6.0}

      assert inner.context.authorization_root == root and
               inner.context.image_budget == parent.image_budget

      assert inner.context.depth == outer.context.depth + 1
      assert inner.context.document_id != outer.context.document_id
      assert inner.context.tree_scope_id != outer.context.tree_scope_id
      assert ResourceContext.current() == parent
    end)
  end
end
