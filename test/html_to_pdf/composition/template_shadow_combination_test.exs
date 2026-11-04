defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateShadowCombinationTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlComposition, HtmlParser, Style}
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare, PngFixture}
  alias NativeElixirPdfUtilities.Validators.{HtmlTreeValidator, HtmlValidator}

  @opts [system_font_discovery: false]

  test "unmatched processing instructions in shadow contents stay invisible" do
    html =
      "<div><template shadowrootmode=open><?marker name=label><p>Visible content</p><?ordinary data?></template></div>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, "Visible content"} = Text.extract(pdf)
  end

  test "shadow markers retain authored identity and validated projection ancestry" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div><template shadowrootmode=open><?marker name=label><p>Visible</p></template></div>"
             )

    [host] = dom.children
    [template] = host.children
    [original, _paragraph] = template.content.children
    assert {:ok, projection} = HtmlComposition.compose(dom)
    [composed_host] = projection.composed.children
    [marker, _paragraph] = composed_host.children
    assert projection.logical == dom

    assert Map.take(marker, [:node_id, :document_id, :tree_scope_id, :parent_id, :source]) ==
             Map.take(original, [:node_id, :document_id, :tree_scope_id, :parent_id, :source])

    assert marker.shadow_scope_id == template.content.node_id
    assert marker.shadow_host_id == host.node_id
    assert marker.composed_parent_id == host.node_id
    assert {:ok, ^marker} = HtmlValidator.validate_html_processing_instruction(marker)
    assert :ok = HtmlTreeValidator.validate_node_metadata(marker)
  end

  test "valid shadow mode wins over for regardless of source attribute order or matching target" do
    for attributes <- [
          "for=A shadowrootmode=open",
          "shadowrootmode=open for=A",
          "for=A shadowrootmode=CLOSED"
        ] do
      html =
        "<div><?marker name='A'><template #{attributes}><p>Shadow content</p></template>Light content</div>"

      assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
      assert {:ok, "Shadow content"} = Text.extract(pdf)

      assert {:ok, unmatched} =
               HtmlToPdf.render(
                 "<div><template #{attributes}>Shadow without target</template>Light</div>",
                 @opts
               )

      assert {:ok, "Shadow without target"} = Text.extract(unmatched)
    end
  end

  test "a shadow-precedence template does not consume the earlier replacement marker" do
    html =
      "<div><?marker name='A'><template for=A shadowrootmode=open><slot></slot></template><template for=A><span>Inserted once</span></template></div>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, "Inserted once"} = Text.extract(pdf)
  end

  test "invalid shadow mode and auxiliary shadow flags retain ordinary replacement semantics" do
    for attributes <- [
          "for=A shadowrootmode=invalid",
          "for=A shadowrootdelegatesfocus shadowrootclonable shadowrootserializable",
          "shadowrootmode='' for=A"
        ] do
      html =
        "<div><?marker name='A'><template #{attributes}><p>Patched content</p></template>After</div>"

      assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Patched content" and text =~ "After"
    end
  end

  test "invalid hosts and subsequent shadow templates stay inert rather than consuming markers" do
    for html <- [
          "<button><?marker name='A'><template for=A shadowrootmode=open><span>Inert</span></template>Button</button>",
          "<div><?marker name='A'><template shadowrootmode=open><slot></slot></template><template for=A shadowrootmode=closed><span>Inert</span></template>Assigned</div>"
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
      assert {:ok, text} = Text.extract(pdf)
      assert text in ["Button", "Assigned"]
      refute text =~ "Inert"
    end
  end

  test "replacement inside a shadow root stays in its marker scope" do
    html =
      "<div><?marker name='A'><template shadowrootmode=open><?marker name='A'><template for=A><p>Shadow inserted</p></template><slot></slot></template><template for=A><span>Light inserted</span></template></div>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Shadow inserted" and text =~ "Light inserted"
    assert length(Regex.scan(~r/inserted/, text)) == 2
  end

  test "nested shadow markers preserve slot fallback and assigned light contents" do
    html =
      "<div><template shadowrootmode=open><?marker name=outer><section><template shadowrootmode=closed><?marker name=inner><slot>Inner fallback</slot></template></section><slot name=title>Outer fallback</slot></template><span slot=title>Assigned title</span><b>Unassigned</b></div>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Inner fallback" and text =~ "Assigned title"
    refute text =~ "Outer fallback" or text =~ "Unassigned"
  end

  test "shadow combinations work inside an explicitly supplied child document" do
    child =
      "<div><template shadowrootmode=open for=A><?marker name='A'><p>Child visible</p></template></div>"

    assert {:ok, pdf} = HtmlToPdf.render("<iframe srcdoc=\"#{child}\"></iframe>", @opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Child visible"
  end

  test "removed original shadow contents still require authorized resources" do
    html =
      "<div><template shadowrootmode=open><?start name='A'><img src=original.png><?end?><template for=A><p>Replacement</p></template></template></div>"

    assert {:error, {_reason, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.stage == :asset and diagnostic.source == "original.png"
    assert diagnostic.operation == :render and diagnostic.message != ""

    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    assert {:ok, pdf} =
             HtmlToPdf.render(
               html,
               Keyword.put(@opts, :assets, %{"original.png" => {:bytes, png}})
             )

    assert {:ok, "Replacement"} = Text.extract(pdf)
  end

  test "unused combined shadow templates still validate their original executable source" do
    html =
      "<div><template shadowrootmode=open>Visible</template><template for=A shadowrootmode=closed><script>run()</script></template></div>"

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.stage == :html and diagnostic.operation == :render
    assert diagnostic.source =~ "script" and diagnostic.message != ""
  end

  test "malformed projection metadata is rejected by canonical node validation" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div><template shadowrootmode=open><?marker name='A'></template></div>"
             )

    assert {:ok, projection} = HtmlComposition.compose(dom)
    [host] = projection.composed.children
    [marker] = host.children

    for bad <- [
          Map.put(marker, :composed_parent_id, "forged"),
          Map.put(marker, :shadow_scope_id, "forged"),
          Map.put(marker, :shadow_host_id, "forged"),
          Map.put(marker, :shadow_scope_id, make_ref()),
          Map.delete(marker, :shadow_host_id),
          Map.put(marker, :attributes, %{"onclick" => "run()"})
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               HtmlValidator.validate_html_processing_instruction(bad)

      assert diagnostic.stage == :style and diagnostic.source == "processing_instruction"
      assert diagnostic.message != ""
    end
  end

  test "public authored DOM transport cannot supply projection ancestry on markers" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<?marker name='A'><p>Visible</p>")
    [marker, paragraph] = dom.children

    for metadata <- [
          %{composed_parent_id: nil},
          %{shadow_scope_id: marker.tree_scope_id, shadow_host_id: make_ref()}
        ] do
      forged = %{dom | children: [Map.merge(marker, metadata), paragraph]}
      assert {:error, {:invalid_document, diagnostic}} = Style.compute_detailed(forged, @opts)
      assert diagnostic.stage == :style
      assert diagnostic.message =~ "composition ancestry"
    end
  end

  for scenario <- ~w(markers precedence nested_slots replacement) do
    @scenario scenario
    @tag :browser_parity
    test "template shadow #{@scenario} matches Chromium" do
      stats =
        PdfVisualCompare.assert_browser_match!(
          Paths.fixture("html_to_pdf/browser_parity/template_shadow_#{@scenario}.html"),
          artifact_dir: "tmp/browser_parity/template_shadow_#{@scenario}",
          render_opts: @opts,
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 1
      assert stats.scripting == :disabled
    end
  end
end
