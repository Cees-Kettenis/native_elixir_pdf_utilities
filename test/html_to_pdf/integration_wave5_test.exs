defmodule NativeElixirPdfUtilities.HtmlToPdf.IntegrationWave5Test do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Font, PdfWriter}
  alias NativeElixirPdfUtilities.{Text, Forms, Limits}
  alias NativeElixirPdfUtilities.Validators.WriterValidator

  test "validated math processing instructions preserve original token boundaries and ActualText" do
    html =
      "<math><mi>f<?marker data=Invisible?>i</mi><mspace><?marker?></mspace><mi> x </mi></math>"

    reference = String.replace(html, ~r/<\?.*?>/s, "<!--boundary-->")
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [atom]}} = Style.compute_detailed(dom)
    assert [f, i, x] = atom.style._math_box.glyphs
    assert f.glyph_id == 3331 and i.glyph_id == 3334
    assert x.glyph_id == x.font_face.cmap[?x]
    assert atom.style._math_box.actual_text == "fix"
    assert {:ok, pdf} = HtmlToPdf.render(html, compress_streams: false)
    assert {:ok, ^pdf} = HtmlToPdf.render(reference, compress_streams: false)
    assert {:ok, "fix"} = Text.extract(pdf, layout: false)
    refute pdf =~ "Invisible"

    legacy_dom = NativeElixirPdfUtilities.HtmlTreeHelpers.tree_content(dom)

    bad =
      put_in(legacy_dom.children, [
        %{
          hd(legacy_dom.children)
          | children: [%{type: :processing_instruction, target: "xml", data: ""}]
        }
      ])

    assert {:error, {:invalid_document, diagnostic}} = Style.compute_detailed(bad)
    assert diagnostic.stage == :style and diagnostic.reason == :invalid_document
    assert diagnostic.message =~ "processing instruction"
  end

  test "selected math glyph groups survive nested translated clipped child viewports and pagination" do
    leaf =
      "<body style='margin:0;overflow:hidden'><math style='font-size:18pt'><mi>f<?marker?>i</mi><mn>42</mn></math><p>Leaf caption</p></body>"

    child =
      "<body style='margin:0;overflow:hidden'><p>Child caption</p><iframe style='border:0;width:110pt;height:60pt' srcdoc='#{escape(leaf)}'></iframe></body>"

    html =
      "<style>@page{size:240pt 180pt;margin:12pt}body{margin:0}p{margin:0}</style><p style='height:140pt'>First page</p><iframe style='border:0;width:140pt;height:90pt' srcdoc='#{escape(child)}'></iframe><p>After child</p>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               forms: :static,
               system_font_discovery: false,
               compress_streams: false
             )

    assert {:ok, 2} = NativeElixirPdfUtilities.Info.page_count(pdf)
    assert {:ok, text} = Text.extract(pdf, layout: false)

    for caption <- ["First page", "Child caption", "Leaf caption", "fi42", "After child"],
        do: assert(text =~ caption)

    assert {:ok, []} = Forms.fields(pdf)
    assert length(Regex.scan(~r{/ActualText}, pdf)) == 1
    assert length(Regex.scan(~r{ re W n}, pdf)) >= 2
    assert pdf =~ "STIXTwoMath"
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    assert {:ok, second} =
             NativeElixirPdfUtilities.Pdf.Reader.dictionary(
               document,
               {:ref, Enum.at(document.pages, 1).ref}
             )

    assert {:ok, content} =
             NativeElixirPdfUtilities.Pdf.Reader.decoded_stream(document, second["Contents"])

    assert content =~ "/ActualText" and content =~ " cm "
  end

  test "bounded child retains static gauge shading and image border-box links through its local base" do
    root = Path.expand("test/fixtures/html_to_pdf/browser_parity/assets")

    child =
      "<base href='./assets/'><body style='margin:0;overflow:hidden'><meter value='.6' style='appearance:none'></meter><a href='https://example.test/photo'><img src='iframe-bars.png' style='width:60pt;height:40pt;object-fit:contain'></a><math><mn>12</mn></math></body>"

    html =
      "<iframe style='display:block;border:0;width:100pt;height:70pt' srcdoc='#{escape(child)}'></iframe>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               base_url: Path.dirname(root),
               forms: :static,
               system_font_discovery: false,
               compress_streams: false,
               asset_resolver: fn request ->
                 flunk("child resource escaped local loading: #{inspect(request)}")
               end
             )

    assert pdf =~ "/Shading << /Gauge" and pdf =~ "/FunctionType 3"
    assert pdf =~ "/ActualText"
    assert {:ok, []} = Forms.fields(pdf)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    links =
      for {_ref, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects, do: link

    assert [
             %{
               "Rect" => [left, bottom, right, top],
               "A" => %{"URI" => {:string, "https://example.test/photo"}}
             }
           ] = links

    assert_in_delta right - left, 60, 0.001
    assert_in_delta top - bottom, 40, 0.001
  end

  test "fieldset disabledness preserves legend and root text values beside static clipped child controls" do
    root =
      "<fieldset disabled><legend><textarea name=legend rows=2 cols=8>Legend\nvalue</textarea></legend><textarea name=locked rows=2 cols=8>Locked\nvalue</textarea><meter value='.6' style='appearance:none'></meter></fieldset><textarea name=plain rows=2 cols=8>Plain\nvalue</textarea>"

    assert {:ok, pdf} = HtmlToPdf.render(root, forms: :interactive, compress_streams: false)
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

    assert {:ok, %{fields: fields}} =
             NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    assert Enum.map(fields, &{&1.name, &1.value, &1.flags}) == [
             {"legend", "Legend\nvalue", 4096},
             {"locked", "Locked\nvalue", 4097},
             {"plain", "Plain\nvalue", 4096}
           ]

    assert pdf =~ "/Shading << /Gauge"

    child =
      "<body style='margin:0;overflow:hidden'><fieldset disabled><legend>Child legend</legend><progress value='.5' style='appearance:none;width:35pt;height:10pt;display:block'></progress><textarea rows=2 cols=8 style='height:20pt;overflow:hidden'>Child line 1\nChild line 2\nChild line 3</textarea></fieldset></body>"

    assert {:ok, child_pdf} =
             HtmlToPdf.render(
               "<iframe style='border:0;width:100pt;height:70pt' srcdoc='#{escape(child)}'></iframe>",
               forms: :static,
               compress_streams: false
             )

    assert {:ok, []} = Forms.fields(child_pdf)
    assert {:ok, child_text} = Text.extract(child_pdf, layout: false)
    assert child_text =~ "Child legend"
    assert length(Regex.scan(~r{ re W n}, child_pdf)) >= 2
  end

  test "nested glyph transport rejects private fields malformed GIDs transformed bounds and box budgets" do
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, font} = Font.resolve("math", 400, :normal, registry)

    text = %{
      type: :text,
      text: "f",
      glyph_ids: [3331],
      x: 1,
      y: 1,
      width: 12,
      height: 14,
      font: Font.pdf_name(font),
      font_face: font,
      font_size: 12,
      color: {0, 0, 0}
    }

    glyphs = %{type: :glyph_group, actual_text: "f", boxes: [text]}

    inner = %{
      type: :drawing_group,
      width: 30,
      height: 30,
      transform: {1, 0, 0, 1, 5, 5},
      clip: %{x: 0, y: 0, width: 30, height: 30},
      boxes: [glyphs]
    }

    outer = %{inner | width: 50, height: 50, boxes: [inner]}
    page = %{size: {100, 100}, boxes: [outer]}
    assert {:ok, pdf} = PdfWriter.render([page])
    assert {:ok, "f"} = Text.extract(pdf, layout: false)

    for invalid <- [
          Map.put(glyphs, :drawing_page_height, 100),
          %{glyphs | boxes: [Map.put(text, :drawing_annotation_rect, {0, 0, 1, 1})]},
          %{glyphs | boxes: [%{text | glyph_ids: [65_536]}]},
          %{glyphs | boxes: [%{text | x: Limits.get(:max_pdf_numeric_magnitude)}]},
          %{glyphs | actual_text: <<255>>},
          %{glyphs | boxes: [%{text | type: :rect}]}
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               WriterValidator.prepare(
                 [%{page | boxes: [%{outer | boxes: [%{inner | boxes: [invalid]}]}]}],
                 []
               )

      assert diagnostic.stage == :pdf and diagnostic.operation == :write_pdf
      assert diagnostic.reason == :invalid_pdf_input and diagnostic.message != ""
    end

    previous = Limits.effective()

    try do
      Limits.install(Map.put(previous, :max_layout_boxes, 6))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               WriterValidator.prepare([page], [])

      assert diagnostic.reason == :resource_limit_exceeded and
               diagnostic.message =~ "max_layout_boxes"
    after
      Limits.install(previous)
    end
  end

  test "MathML child painting succeeds while unsupported SVG scenes diagnose and child destinations serialize" do
    child = "<math><mtext><span style='background:red'>Painted</span></mtext></math>"

    assert {:ok, painted} =
             HtmlToPdf.render(
               "<iframe srcdoc='#{escape("<body style=\"overflow:hidden\">" <> child <> "</body>")}'></iframe>",
               forms: :static
             )

    assert {:ok, text} = Text.extract(painted, layout: false)
    assert text =~ "Painted"

    for content <- ["<svg><foreignObject><p>SVG child</p></foreignObject></svg>"] do
      html =
        "<iframe srcdoc='#{escape("<body style=\"overflow:hidden\">" <> content <> "</body>")}'></iframe>"

      assert {:error, {reason, diagnostic}} = HtmlToPdf.render(html, forms: :static)
      assert reason == :unsupported_html
      assert diagnostic.reason == reason and diagnostic.message =~ "complete scene"
      assert diagnostic.stage == :layout and diagnostic.operation == :prepare
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgSceneValidator
      assert diagnostic.source == "foreignObject"
    end

    child =
      "<body style='overflow:hidden'><a href='#target'>Jump</a><p id=target>Target</p></body>"

    assert {:ok, pdf} =
             HtmlToPdf.render("<iframe srcdoc='#{escape(child)}'></iframe>", forms: :static)

    assert pdf =~ "/Dest ["
  end

  test "authorized exact font and PNG bytes pass the closed SVG backend into a local child image" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{AssetLoader, ResourceContext, SvgBackend}
    alias NativeElixirPdfUtilities.Validators.HtmlValidator
    root = Path.expand(".")

    ResourceContext.run(
      [authorization_root: root, base_url: root],
      HtmlValidator.new_image_budget(),
      fn parent ->
        assert {:ok, font} =
                 AssetLoader.resolve_local("priv/fonts/dejavu/DejaVuSans.ttf", :font, parent, [])

        assert {:ok, png} =
                 AssetLoader.resolve_local(
                   "test/fixtures/html_to_pdf/browser_parity/assets/iframe-bars.png",
                   :image,
                   parent,
                   []
                 )

        source =
          "<svg xmlns='http://www.w3.org/2000/svg' width='80' height='30'><image href='svg-resource:bars' width='30' height='30'/><text x='32' y='20' font-family='DejaVu Sans' font-size='12'>SVG</text></svg>"

        assert {:ok, artwork} =
                 SvgBackend.rasterize(
                   source,
                   [],
                   %{
                     mode: :reference,
                     fonts: [%{data: font}],
                     resources: %{"svg-resource:bars" => %{data: png, media_type: "image/png"}}
                   },
                   parent.image_budget
                 )

        child =
          "<body style='margin:0;overflow:hidden'><img src='prepared.png' style='width:80pt;height:30pt'><math><mn>7</mn></math></body>"

        assert {:ok, pdf} =
                 HtmlToPdf.render(
                   "<iframe style='border:0;width:120pt;height:50pt' srcdoc='#{escape(child)}'></iframe>",
                   base_url: root,
                   forms: :static,
                   system_font_discovery: false,
                   assets: %{"prepared.png" => {:bytes, artwork}}
                 )

        assert {:ok, "7"} = Text.extract(pdf, layout: false)
        assert {:ok, []} = Forms.fields(pdf)

        assert {:error, {:resource_denied, diagnostic}} =
                 AssetLoader.resolve_local("/etc/passwd", :font, parent, [])

        assert diagnostic.stage == :resource and diagnostic.reason == :resource_denied
      end
    )
  end

  test "root destinations coexist with PI math tokens child URI artwork and clickable image borders" do
    child =
      "<body style='overflow:hidden;margin:0'><a href='https://example.test/child'>Child URI</a><math><mi>f<?marker?>i</mi></math><meter style='appearance:none' value='.6'></meter></body>"

    png =
      File.read!(Path.expand("test/fixtures/html_to_pdf/browser_parity/assets/iframe-bars.png"))

    html =
      "<style>@page{size:240pt 180pt;margin:12pt}body{margin:0}p{margin:0}</style><p><a href='#target'>Root jump</a><a href='#image'><img src=tile style='width:30pt;height:20pt;object-fit:contain'></a></p><p style='height:140pt'>Before target</p><p id=target>Root target <math><mi>f<?marker?>i</mi></math></p><iframe style='border:0;width:130pt;height:60pt' srcdoc='#{escape(child)}'></iframe><img id=image src=tile style='width:20pt;height:10pt'>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               forms: :static,
               assets: %{"tile" => {:bytes, png}},
               compress_streams: false
             )

    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    links =
      for {_ref, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects, do: link

    assert length(links) == 3

    assert [%{"A" => %{"URI" => {:string, "https://example.test/child"}}}] =
             Enum.filter(links, &Map.has_key?(&1, "A"))

    destinations = Enum.filter(links, &Map.has_key?(&1, "Dest"))
    assert length(destinations) == 2

    for %{"Dest" => [{:ref, ref}, {:name, "XYZ"}, x, y, nil]} <- destinations do
      assert ref in Enum.map(Enum.drop(document.pages, 1), & &1.ref)
      assert x >= 0 and x <= 240 and y >= 0 and y <= 180
    end

    assert Enum.any?(destinations, fn %{"Rect" => [left, bottom, right, top]} ->
             right - left == 30 and top - bottom == 20
           end)

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Root target" and text =~ "Child URI" and text =~ "fi"
    assert pdf =~ "/Shading << /Gauge"
    assert {:ok, []} = Forms.fields(pdf)

    assert {:ok, atom} =
             HtmlToPdf.render(
               "<a href='#equation'>Equation</a><math id=equation><mi>x</mi></math>"
             )

    assert atom =~ "/Dest ["

    assert {:error, {:unsupported_html, %{stage: :layout}}} =
             HtmlToPdf.render("<a href='#token'>Token</a><math><mi id=token>x</mi></math>")

    assert {:ok, child_targets} =
             HtmlToPdf.render(
               "<p id=parent>Parent</p><iframe srcdoc='#{escape("<a href=#parent>Child cannot select parent</a><p id=parent>Child target</p>")}'></iframe>"
             )

    assert child_targets =~ "/Dest ["
  end

  test "shared ID authority preserves logical table PI children and first scoped header selection" do
    alias NativeElixirPdfUtilities.Validators.{
      HtmlValidator,
      HtmlMetadataValidator,
      HtmlTableValidator
    }

    html =
      "<div id=blocked hidden>Blocker</div><template id=template><span id=h>Inert header</span></template><table><tr><?marker?><th id=h>Header</th><th id=f>Later foreign duplicate</th><td id=value headers='h h blocked f template empty'>Data</td><th id=empty><?marker?></th></tr></table>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    foreign = %{
      type: :element,
      namespace: :svg,
      tag: "g",
      attributes: %{"id" => "f"},
      children: []
    }

    legacy_dom = NativeElixirPdfUtilities.HtmlTreeHelpers.tree_content(dom)
    nodes = HtmlValidator.assign_render_metadata([foreign | legacy_dom.children])
    assert {:ok, prepared} = HtmlTableValidator.prepare_document(nodes)
    table = List.last(prepared)
    cells = Map.new(table._table_model.cells, &{&1.id, &1})
    ids = HtmlMetadataValidator.first_tree_ids(nodes)
    assert ids[{nil, nil, "f"}].namespace == :svg
    assert cells["value"].headers == [cells["h"].source]
    assert cells["empty"].empty
    assert cells["h"].source == [3, 0, 1]
    assert {:ok, _} = HtmlToPdf.render(html)

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<table><tr><?marker?><th id=h>Header</th><td headers=h>Data</td></tr></table>"
             )

    table = hd(dom.children)

    scoped = fn node, document, tree ->
      Map.merge(node, %{document_id: document, tree_scope_id: tree})
    end

    siblings =
      for {document, tree} <- [{make_ref(), make_ref()}, {make_ref(), make_ref()}] do
        row = hd(table.children)
        children = Enum.map(row.children, &scoped.(&1, document, tree))

        scoped.(
          %{table | children: [scoped.(%{row | children: children}, document, tree)]},
          document,
          tree
        )
      end

    assert {:ok, prepared} =
             HtmlTableValidator.prepare_document(HtmlValidator.assign_render_metadata(siblings))

    for table <- prepared do
      [header, value] = table._table_model.cells
      assert value.headers == [header.source]
      assert header.identity.document_id == table.document_id

      assert Map.has_key?(
               HtmlMetadataValidator.first_tree_ids(
                 prepared,
                 {table.document_id, table.tree_scope_id}
               ),
               {table.document_id, table.tree_scope_id, "h"}
             )
    end
  end

  test "canonical group destinations serialize and public URL contexts cannot bypass option validation" do
    text = %{
      type: :text,
      text: "Jump",
      x: 1,
      y: 1,
      width: 12,
      height: 14,
      font: "Helvetica",
      font_size: 12,
      color: {0, 0, 0},
      link_destination: {:xyz, 1, 1, 1}
    }

    group = %{
      type: :drawing_group,
      width: 30,
      height: 30,
      transform: {1, 0, 0, 1, 0, 0},
      boxes: [text]
    }

    for box <- [
          group,
          %{group | boxes: [%{type: :glyph_group, actual_text: "Jump", boxes: [text]}]}
        ] do
      assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [box]}])
      assert pdf =~ "/Dest ["
    end

    assert {:error, {:unsupported_html, diagnostic}} =
             PdfWriter.render([
               %{
                 size: {100, 100},
                 boxes: [Map.put(%{group | boxes: []}, :link_destination, {:xyz, 1, 1, 1})]
               }
             ])

    assert diagnostic.stage == :pdf and diagnostic.operation == :write_pdf
    assert diagnostic.message =~ "composition geometry"

    urls = %{
      fallback_base: "file:///tmp/child.html",
      resolution_base: "file:///tmp/child.html",
      authored_base?: false
    }

    assert {:error, {:invalid_options, diagnostic}} =
             HtmlToPdf.render("<p>Public</p>", __document_url_context__: urls)

    assert diagnostic.stage == :options and diagnostic.reason == :invalid_options
    assert diagnostic.message =~ "__document_url_context__"
    assert {:ok, public_dom} = HtmlParser.parse_detailed("<p>Public</p>")

    assert {:error, {:invalid_options, %{source: "__document_url_context__"}}} =
             Style.compute_detailed(public_dom, __document_url_context__: %{})

    assert {:error, {:invalid_options, %{source: "__document_url_context__"}}} =
             Style.compute_detailed(public_dom, __document_url_context__: urls)

    alias NativeElixirPdfUtilities.HtmlToPdf.{ResourceContext, EmbeddedViewport}
    alias NativeElixirPdfUtilities.Validators.{HtmlValidator, HtmlUrlValidator}

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, child} = EmbeddedViewport.prepare_html({:srcdoc, "<p>Child</p>"}, parent)

      ResourceContext.with_document(child.context, fn ->
        urls = %{
          fallback_base: "relative",
          resolution_base: child.context.resolution_base,
          authored_base?: false
        }

        assert {:error, {:invalid_document, detail}} =
                 HtmlUrlValidator.prepare_style_output_options(__document_url_context__: urls)

        assert detail.source == "relative" or detail.source == "__document_url_context__"
      end)
    end)

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div contenteditable><math contenteditable=false><mi>x</mi></math></div>"
             )

    assert {:ok, _} = Style.compute_detailed(dom)

    assert {:ok, _} =
             HtmlToPdf.render(
               "<div contenteditable><math contenteditable=true><mi>x</mi></math></div>"
             )
  end

  test "ordinary operators retain PI token text inside linked child artwork" do
    child =
      "<body style='overflow:hidden;margin:0'><a href='https://example.test/operator'>Child operator</a><math><mi>f<?marker?>i</mi><mo>+</mo><mn>1</mn><mo>=</mo><mn>2</mn></math><meter style='appearance:none' value='.6'></meter></body>"

    html =
      "<math><mn>3</mn><mo>+</mo><mn>4</mn></math><iframe style='border:0;width:180pt;height:70pt' srcdoc='#{escape(child)}'></iframe>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               forms: :static,
               system_font_discovery: false,
               compress_streams: false
             )

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, ~r/\s/u, "") =~ "3+4Childoperatorfi+1=2"
    refute text =~ "marker"
    assert length(Regex.scan(~r{/ActualText}, pdf)) == 2
    assert pdf =~ "/Shading << /Gauge"
    assert {:ok, []} = Forms.fields(pdf)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    assert [%{"A" => %{"URI" => {:string, "https://example.test/operator"}}}] =
             for(
               {_ref, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects,
               do: link
             )
  end

  defp escape(html) do
    html
    |> String.replace("&", "&amp;")
    |> String.replace("<", "&lt;")
    |> String.replace(">", "&gt;")
    |> String.replace("'", "&#39;")
  end
end
