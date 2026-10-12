defmodule NativeElixirPdfUtilities.Validators.HtmlLinkValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout, Pagination, PdfWriter}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, HtmlLinkValidator}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.HtmlToPdf

  test "selection retains first IDs, ID before name, raw before decoded, and separate document identities" do
    html = """
    <a name=legacy>Legacy</a><a name=priority>Earlier name</a>
    <p id=priority>Later ID</p><p id=dup hidden>Hidden first</p><p id=dup>Visible duplicate</p>
    <p id='a%20b'>Raw target</p><p id='a b'>Decoded target</p><p id=top>Authored top</p>
    <template><p id=unused>Unused target</p></template>
    <a href='#legacy'>Legacy link</a><a href='#priority'>Priority link</a>
    <a href='#dup'>Duplicate link</a><a href='#a%20b'>Raw link</a><a href='#a%20b%20c'>Missing link</a>
    <a href='#'>Beginning link</a><a href='#TOP'>Top link</a><a href='#top'>Authored top link</a>
    <a href='https://example.test/#remote'>External link</a>
    """

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    children = HtmlValidator.assign_render_metadata(dom.children)
    url = %{fallback_base: nil, resolution_base: nil, authored_base?: false}
    assert {:ok, context} = HtmlLinkValidator.prepare(children, url)
    assert {:ok, sibling} = HtmlLinkValidator.prepare(children, url)
    refute context.document_id == sibling.document_id
    refute context.tree_scope_id == sibling.tree_scope_id
    refute Map.has_key?(context.ids, "unused")
    assert context.links["#legacy"] == {:target, context.names["legacy"]}
    assert context.links["#priority"] == {:target, context.ids["priority"]}
    assert context.links["#dup"] == {:target, context.ids["dup"]}
    assert context.links["#a%20b"] == {:target, context.ids["a%20b"]}
    assert context.links["#a%20b%20c"] == nil
    assert context.links["#top"] == {:target, context.ids["top"]}
    assert context.links["#TOP"] == {:top, {context.document_id, context.tree_scope_id}}
    assert context.links["#"] == {:top, {context.document_id, context.tree_scope_id}}
    assert context.links["https://example.test/#remote"] == {:uri, "https://example.test/#remote"}

    assert HtmlLinkValidator.select("#a%20b", "a b", %{
             context
             | ids: Map.delete(context.ids, "a%20b")
           }) == {:target, context.ids["a b"]}

    assert {:error, {:invalid_document, %{stage: :style, source: "relative.html"}}} =
             HtmlLinkValidator.prepare(
               HtmlValidator.assign_render_metadata([
                 %{
                   type: :element,
                   tag: "a",
                   attributes: %{"href" => "relative.html"},
                   children: []
                 }
               ]),
               url
             )
  end

  test "shared first-tree-ID authority preserves complete nodes and light-tree boundaries" do
    alias NativeElixirPdfUtilities.Validators.HtmlMetadataValidator
    source = %{text: "<td id=x>", line: 7, column: 3}

    td = %{
      type: :element,
      tag: "td",
      attributes: %{"id" => "x", "hidden" => ""},
      children: [],
      _document_order: [0, 0],
      _selector_id: make_ref(),
      node_id: make_ref(),
      document_id: make_ref(),
      tree_scope_id: make_ref(),
      parent_id: make_ref(),
      source: source
    }

    th = %{td | tag: "th", attributes: %{"id" => "x"}, source: %{source | text: "<th id=x>"}}
    foreign = %{td | tag: "g", attributes: %{"id" => "foreign"}} |> Map.put(:namespace, :svg)

    namespaced =
      foreign
      |> Map.put(:attributes, %{"id" => "x"})
      |> Map.put(:attribute_namespaces, %{"id" => "urn:other"})

    template =
      %{td | tag: "template", attributes: %{"id" => "template"}}
      |> Map.put(:content, %{
        type: :document_fragment,
        children: [%{td | attributes: %{"id" => "fragment"}}]
      })

    tree = [
      %{type: :text, text: "Text"},
      namespaced,
      td,
      th,
      foreign,
      template,
      %{td | attributes: %{"id" => ""}}
    ]

    index = HtmlMetadataValidator.first_tree_ids(tree)
    {document, scope} = {td.document_id, td.tree_scope_id}
    assert index[{document, scope, "x"}] == td
    assert index[{document, scope, "foreign"}] == foreign
    assert index[{document, scope, "template"}] == template

    refute Map.has_key?(index, {document, scope, "fragment"}) or
             Map.has_key?(index, {document, scope, ""})

    assert index[{document, scope, "x"}].source == source
    other_scope = make_ref()
    shadow = %{td | tree_scope_id: other_scope, attributes: %{"id" => "shadow"}}

    refute Map.has_key?(
             HtmlMetadataValidator.first_tree_ids([td, shadow], {document, scope}),
             {document, scope, "shadow"}
           )

    assert HtmlMetadataValidator.first_tree_ids([shadow], {document, other_scope})[
             {document, other_scope, "shadow"}
           ] == shadow

    legacy = Map.drop(td, [:document_id, :tree_scope_id])
    assert HtmlMetadataValidator.first_tree_ids([legacy])[{nil, nil, "x"}] == legacy

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<template id=first></template><p id=first>Later visible duplicate</p><a href='#first'>Jump</a>"
             )

    refute pdf =~ "/Subtype /Link"
  end

  test "document-root targets use the actual printed viewport and beginning uses the page origin" do
    html =
      "<html id=document><head><style>html,body{margin:0;padding:0;font-size:10pt;line-height:12pt}</style></head><body><a href='#document'>Document</a><a href='#'>Beginning</a></body></html>"

    opts = [page_size: {200, 100}, margin: 10, system_font_discovery: false]
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, opts)
    assert {:ok, layout} = Layout.layout(styled, opts)
    assert {:ok, [page]} = Pagination.paginate(layout, opts)

    assert Enum.find(page.boxes, &(Map.get(&1, :text) == "Document")).link_destination ==
             {:xyz, 1, 10.0, 90.0}

    assert Enum.find(page.boxes, &(Map.get(&1, :text) == "Beginning")).link_destination ==
             {:xyz, 1, 0, 100.0}

    assert {:ok, pdf} = PdfWriter.render([page], opts)
    assert pdf =~ "/XYZ 10 90 null]" and pdf =~ "/XYZ 0 100 null]"
    [{document, tree, _node} = identity] = Enum.map(styled.link_document_targets, & &1.identity)
    scope = {document, tree}

    assert :ok =
             HtmlLinkValidator.validate_document_targets(
               [%{identity: identity, source: "document", x: 10, y: 90}],
               scope,
               {200, 100}
             )

    for invalid <- [
          %{identity: identity, source: "document"},
          %{identity: identity, source: "document", x: 201, y: 90},
          %{identity: identity, source: "document", x: "bad", y: 90},
          %{identity: identity, source: "document", x: 10, y: -1}
        ] do
      assert {:error, {:invalid_layout, %{source: "link_document_targets"}}} =
               HtmlLinkValidator.validate_document_targets([invalid], scope, {200, 100})
    end

    assert {:error, {:invalid_layout, _}} =
             HtmlLinkValidator.validate_document_targets([%{}], scope, {200, 100})
  end

  test "issued C5 identities remain immutable during link preparation" do
    alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext

    assert {:ok, dom} =
             HtmlParser.parse_document_detailed("<p id=x>Target</p><a href='#x'>Jump</a>")

    children = HtmlValidator.assign_render_metadata(dom.children)
    url = %{fallback_base: nil, resolution_base: nil, authored_base?: false}
    budget = HtmlValidator.new_image_budget()

    assert {:issued_link_resource, expired} =
             ResourceContext.run(
               [authorization_root: Path.expand("../fixtures/html_to_pdf", __DIR__)],
               budget,
               fn resource ->
                 assert {:ok, context} = HtmlLinkValidator.prepare(children, url, resource)
                 assert context.document_id == resource.document_id
                 assert context.tree_scope_id == resource.tree_scope_id
                 alias NativeElixirPdfUtilities.Validators.ResourceValidator

                 assert {:ok, _source, first_child} =
                          ResourceValidator.authorize_document(
                            "browser_parity/html_base_relative_links.html",
                            resource
                          )

                 assert {:ok, _source, second_child} =
                          ResourceValidator.authorize_document(
                            "browser_parity/html_base_relative_links.html",
                            resource
                          )

                 assert {:ok, first_links} = HtmlLinkValidator.prepare(children, url, first_child)

                 assert {:ok, second_links} =
                          HtmlLinkValidator.prepare(children, url, second_child)

                 refute first_links.document_id == second_links.document_id
                 refute first_links.tree_scope_id == second_links.tree_scope_id
                 assert first_child.authorization_root == second_child.authorization_root
                 assert first_child.image_budget == second_child.image_budget

                 for forged <- [
                       %{resource | document_id: make_ref()},
                       %{resource | tree_scope_id: make_ref()},
                       %{resource | authorization_root: "/forged"},
                       %{resource | image_budget: HtmlValidator.new_image_budget()}
                     ] do
                   assert {:error, {:invalid_options, _}} =
                            HtmlLinkValidator.prepare(children, url, forged)
                 end

                 {:issued_link_resource, resource}
               end
             )

    assert {:error, {:invalid_options, _}} = HtmlLinkValidator.prepare(children, url, expired)
    assert {:error, {:invalid_options, _}} = HtmlLinkValidator.prepare(children, url, %{})
  end

  test "empty targets retain zero-height flow, auto margins and relative movement use actual placement" do
    html = """
    <style>html,body{margin:0;font-size:10pt;line-height:12pt}p{margin:0}</style>
    <p><a href='#empty'>Empty</a> <a href='#center'>Center</a> <a href='#shifted'>Shifted</a></p>
    <p><span id=empty></span></p><p id=center style='width:40pt;margin-left:auto;margin-right:auto'>Centered</p>
    <div id=shifted style='position:relative;left:5pt;top:3pt'><p>Moved target</p></div>
    """

    opts = [page_size: {200, 100}, margin: 10, system_font_discovery: false]
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, opts)
    assert {:ok, layout} = Layout.layout(styled, opts)
    assert {:ok, [page]} = Pagination.paginate(layout, opts)
    boxes = Enum.filter(page.boxes, &Map.has_key?(&1, :link_destination))
    assert Enum.find(boxes, &(&1.text == "Empty")).link_destination == {:xyz, 1, 10.0, 78.0}
    assert Enum.find(boxes, &(&1.text == "Center")).link_destination == {:xyz, 1, 80.0, 78.0}
    assert Enum.find(boxes, &(&1.text == "Shifted")).link_destination == {:xyz, 1, 15.0, 63.0}

    assert Enum.find(page.boxes, &(Map.get(&1, :text) == "Centered")).y >
             Enum.find(page.boxes, &(Map.get(&1, :text) == "Moved target")).y
  end

  test "table, grid, flex and anonymous inline targets retain their source geometry" do
    cases = [
      {"caption",
       "<table><caption id=caption>Caption target</caption><tbody><tr><td>Cell</td></tr></tbody></table>"},
      {"cell", "<table><tbody><tr><td id=cell>Cell target</td></tr></tbody></table>"},
      {"row", "<table><tbody><tr id=row><td>Row target</td></tr></tbody></table>"},
      {"section",
       "<table><tbody id=section><tr><td>Section target</td></tr><tr><td>Second row</td></tr></tbody></table>"},
      {"flex", "<div style='display:flex'><div id=flex>Flex target</div></div>"},
      {"grid", "<div style='display:grid'><div id=grid>Grid target</div></div>"},
      {"wrapper", "<a id=wrapper><div>Wrapped block target</div><div>Second block</div></a>"},
      {"empty", "<div id=empty></div><p>After empty block</p>"}
    ]

    for {id, content} <- cases do
      html =
        "<style>html,body{margin:0;font-size:10pt;line-height:12pt}p{margin:0}table{border-spacing:0}td{padding:0}</style><p><a href='#" <>
          id <> "'>Jump</a></p>" <> content

      opts = [page_size: {200, 200}, margin: 10, system_font_discovery: false]
      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom, opts)
      assert {:ok, layout} = Layout.layout(styled, opts)
      assert {:ok, [page]} = Pagination.paginate(layout, opts)
      box = Enum.find(page.boxes, &(Map.get(&1, :text) == "Jump"))
      assert box.link_destination == {:xyz, 1, 10.0, 178.0}, id
      assert {:ok, pdf} = PdfWriter.render([page], opts)
      assert pdf =~ "/XYZ 10 178 null]", id
    end
  end

  test "visible structural targets without supported geometry diagnose instead of disappearing" do
    html =
      "<a href='#column'>Jump</a><table><colgroup><col id=column></colgroup><tbody><tr><td>Cell</td></tr></tbody></table>"

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.stage == :pagination and diagnostic.source == "column"

    assert diagnostic.message =~ "no final placement" and
             diagnostic.message =~ "geometry transport"
  end

  test "selected target geometry outside the printed page diagnoses unavailable transport" do
    html =
      "<a href='#outside'>Jump</a><p id=outside style='position:relative;left:500pt'>Outside</p>"

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(html, page_size: {200, 100}, margin: 10)

    assert diagnostic.stage == :pagination and diagnostic.operation == :paginate
    assert diagnostic.source == "outside"

    assert diagnostic.message =~ "no representable destination" and
             diagnostic.message =~ "final PDF page"
  end

  test "document/tree boundaries reject child geometry without binding parent targets" do
    parent = {make_ref(), make_ref()}
    child = {make_ref(), make_ref()}
    {document, tree} = parent
    identity = {document, tree, make_ref()}

    target = %{
      type: :flow_marker,
      x: 20,
      y: 40,
      width: 0,
      height: 0,
      html_targets: [%{identity: identity, dx: 0, dy: 0}]
    }

    link = %{type: :text, x: 0, y: 0, link_destination: {:target, identity}}
    assert :ok = HtmlLinkValidator.validate_boxes([target, link], parent)

    assert {:ok, [%{boxes: [resolved_target, resolved_link]}]} =
             HtmlLinkValidator.resolve_pages([%{size: {100, 100}, boxes: [target, link]}], parent)

    refute Map.has_key?(resolved_target, :html_targets)
    assert resolved_link.link_destination == {:xyz, 1, 20, 40}

    assert :ok =
             HtmlLinkValidator.validate_boxes(
               [%{link | link_destination: {:top, parent}}],
               parent
             )

    for boxes <- [[target, link], [%{link | link_destination: {:top, parent}}]] do
      assert {:error, {:unsupported_html, %{source: "link_scope", operation: :paginate}}} =
               HtmlLinkValidator.validate_boxes(boxes, child)
    end

    assert {:error, {:invalid_layout, _}} =
             HtmlLinkValidator.validate_boxes([%{html_targets: :invalid}], parent)

    assert :ok =
             HtmlLinkValidator.validate_expected_targets(
               [%{identity: identity, source: "target"}],
               parent
             )

    for invalid <- [
          nil,
          [%{}],
          [%{identity: identity, source: <<255>>}],
          [%{identity: identity, source: nil}],
          [%{identity: {:bad, tree, make_ref()}, source: "target"}]
        ] do
      assert {:error, {:invalid_layout, %{source: "link_expected_targets"}}} =
               HtmlLinkValidator.validate_expected_targets(invalid, parent)
    end

    {child_document, child_tree} = child

    child_link = %{
      link
      | link_destination: {:target, {child_document, child_tree, elem(identity, 2)}}
    }

    assert {:ok, [%{boxes: [_target, missing_link]}]} =
             HtmlLinkValidator.resolve_pages(
               [%{size: {100, 100}, boxes: [target, child_link]}],
               parent
             )

    refute Map.has_key?(missing_link, :link_destination)
  end

  test "missing and hidden fragments preserve painted captions without annotation or resource requests" do
    html = """
    <style>html,body{margin:0}p{margin:0}</style>
    <a href='#missing'>Missing link</a><a href='#hidden'>Hidden link</a><a href='#dup'>Duplicate link</a>
    <a inert href='#visible'>Inert link</a><p id=hidden hidden>Hidden target</p>
    <p id=dup style='display:none'>Hidden first duplicate</p><p id=dup>Visible duplicate</p><p id=visible>Visible target</p>
    <template><p id=missing>Unused template target</p><img src=denied></template>
    """

    owner = self()

    resolver = fn request ->
      send(owner, {:link_resource_request, request})
      :not_found
    end

    assert {:ok, pdf} = HtmlToPdf.render(html, asset_resolver: resolver)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

    for caption <- [
          "Missing link",
          "Hidden link",
          "Duplicate link",
          "Inert link",
          "Visible duplicate",
          "Visible target"
        ],
        do: assert(text =~ caption)

    refute text =~ "Hidden target" or text =~ "Hidden first" or text =~ "Unused template"
    refute pdf =~ "/Subtype /Link"
    refute_received {:link_resource_request, _}
  end

  test "nested wrapped, empty, atomic and image targets resolve from final pages and serialize actual page references" do
    image =
      "data:image/svg+xml;base64," <>
        Base.encode64(
          "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='10'><rect width='20' height='10' fill='red'/></svg>"
        )

    html = """
    <style>html,body{margin:0;padding:0;font-size:10pt;line-height:12pt}p{margin:0 0 6pt}</style>
    <p><a href='#nested'>Nested jump</a> <a href='#a%20b'>Decoded jump</a> <a href='#empty'>Empty jump</a></p>
    <p><a href='#atomic'>Atomic jump</a> <a href='#image'>Image jump</a> <a href='#'>Top jump</a></p>
    <div style='break-before:page'><div id=nested><p>Nested target</p><p><span id='a b'>Decoded target wraps with every word preserved across lines</span><span id=empty></span></p>
    <p><span id=atomic style='display:inline-block;padding:3pt;border:1pt solid black'>Atomic target</span></p>
    <p><img id=image src='#{image}' style='width:20pt;height:10pt'><a href='#image'><img src='#{image}' style='width:30pt;height:20pt;object-fit:contain;border:2pt solid black'></a></p></div></div>
    """

    opts = [page_size: {240, 200}, margin: 10, system_font_discovery: false]
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, opts)
    assert {:ok, layout} = Layout.layout(styled, opts)
    assert {:ok, pages} = Pagination.paginate(layout, opts)
    assert length(pages) == 2

    destinations =
      for page <- pages,
          box <- page.boxes,
          Map.has_key?(box, :link_destination),
          do: {Map.get(box, :text), box.type, box.link_destination}

    assert Enum.any?(destinations, &match?({"Nested jump", :text, {:xyz, 2, 10.0, 190.0}}, &1))

    for caption <- ["Decoded jump", "Empty jump", "Atomic jump", "Image jump"] do
      assert Enum.any?(destinations, fn {text, _type, destination} ->
               text == caption and elem(destination, 1) == 2
             end)
    end

    assert Enum.any?(destinations, &match?({"Top jump", :text, {:xyz, 1, 0, 200.0}}, &1))
    assert Enum.any?(destinations, &match?({nil, :image, {:xyz, 2, _, _}}, &1))
    assert {:ok, pdf} = PdfWriter.render(pages, opts)
    assert {:ok, context} = Reader.read_validated(pdf)
    second = Enum.at(context.pages, 1).ref

    annotations =
      for {_ref, %{value: %{"Subtype" => {:name, "Link"}} = value}} <- context.document.objects,
          do: value

    assert Enum.any?(
             annotations,
             &match?([{:ref, ^second}, {:name, "XYZ"}, 10, 190, nil], &1["Dest"])
           )

    {_caption, _type, {:xyz, 2, image_x, image_y}} =
      Enum.find(destinations, &match?({"Image jump", _, _}, &1))

    assert Enum.any?(
             annotations,
             &(&1["Dest"] == [{:ref, second}, {:name, "XYZ"}, image_x, image_y, nil])
           )

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

    for caption <- ["Nested target", "Decoded target", "wraps with every word", "Atomic target"],
        do: assert(text =~ caption)
  end

  test "prepared target descriptors and canonical destinations are validated without changing diagnostics" do
    identity = {make_ref(), make_ref(), make_ref()}
    assert HtmlLinkValidator.valid_identity?(identity)
    assert {:ok, top_pdf} = HtmlToPdf.render("<a href=''>Empty URL</a>")
    assert top_pdf =~ "/XYZ 0 841.89 null]"

    for box <- [
          nil,
          %{html_targets: [%{identity: identity, dx: 0, dy: 0}]},
          %{type: :rect, link_destination: {:target, identity}}
        ] do
      assert {:error, {:invalid_layout, _}} = HtmlLinkValidator.validate_boxes([box])
    end

    assert {:error, {:invalid_layout, %{stage: :pagination, operation: :paginate}}} =
             Pagination.paginate(%{
               type: :layout,
               page_size: {100, 100},
               margin: 0,
               boxes: [
                 %{type: :flow_marker, x: 0, y: 0, width: 0, height: 0, html_targets: :invalid}
               ]
             })

    refute HtmlLinkValidator.valid_identity?(:forged)
    refute HtmlLinkValidator.valid_identity?({:forged, make_ref(), make_ref()})
    assert HtmlLinkValidator.valid_prepared_destination?({:target, identity})
    refute HtmlLinkValidator.valid_prepared_destination?({:uri, "bad"})

    assert :ok =
             HtmlLinkValidator.validate_boxes([
               %{x: 0, y: 0, html_targets: [%{identity: identity, dx: 0, dy: 12}]}
             ])

    for invalid <- [
          nil,
          [%{}],
          [%{identity: identity, dx: "bad", dy: 12}],
          [%{identity: :bad, dx: 0, dy: 12}]
        ] do
      assert {:error,
              {:invalid_layout,
               %{
                 stage: :pagination,
                 operation: :paginate,
                 source: "html_targets/link_destination"
               }}} = HtmlLinkValidator.validate_boxes([%{x: 0, y: 0, html_targets: invalid}])
    end

    assert {:error, {:invalid_layout, _}} =
             HtmlLinkValidator.validate_boxes([%{link_destination: :forged}])

    box = %{
      type: :text,
      text: "Jump",
      x: 10,
      y: 10,
      font: "Helvetica",
      font_size: 10,
      color: {0, 0, 0},
      annotation_width: 30,
      line_height: 12,
      link_destination: {:xyz, 2, 20, 40}
    }

    pages = [%{size: {100, 100}, boxes: [box]}, %{size: {50, 50}, boxes: []}]
    assert {:ok, pdf} = PdfWriter.render(pages)
    assert pdf =~ "/XYZ 20 40 null]"
    without_line_height = Map.delete(box, :line_height)

    assert {:ok, _pdf} =
             PdfWriter.render([%{hd(pages) | boxes: [without_line_height]}, List.last(pages)])

    for invalid <- [
          Map.put(box, :line_height, "bad"),
          Map.put(box, :line_height, 0),
          box
          |> Map.delete(:link_destination)
          |> Map.put(:link_url, "https://example.test")
          |> Map.put(:line_height, "bad")
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([%{hd(pages) | boxes: [invalid]}, List.last(pages)])

      assert diagnostic.stage == :pdf and diagnostic.operation == :write_pdf
      assert diagnostic.message =~ "valid pages"
    end

    for invalid <- [
          {:xyz, 0, 0, 0},
          {:xyz, 3, 0, 0},
          {:xyz, 1.0, 0, 0},
          {:xyz, 2, 51, 40},
          {:xyz, 2, 20, -1},
          {:xyz, 2, "bad", 1},
          {:target, identity},
          {:top, {make_ref(), make_ref()}}
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([
                 %{hd(pages) | boxes: [%{box | link_destination: invalid}]},
                 List.last(pages)
               ])

      assert diagnostic.stage == :pdf and diagnostic.reason == :invalid_pdf_input
      assert diagnostic.operation == :write_pdf and diagnostic.source == "link_destination"
      assert diagnostic.message =~ "existing page"
    end

    assert {:error, {:invalid_pdf_input, _}} =
             PdfWriter.render([
               %{hd(pages) | boxes: [Map.put(box, :link_url, "https://example.test")]}
             ])

    assert {:error, {:invalid_pdf_input, _}} =
             HtmlLinkValidator.validate_destinations([
               %{size: {100, 100}, boxes: [%{type: :rect, link_destination: {:xyz, 1, 10, 10}}]}
             ])

    for {key, value} <- [
          __link_context__: %{},
          __document_url_context__: %{},
          html_target: identity
        ] do
      assert {:error, {:invalid_options, _}} = HtmlToPdf.render("<p>Text</p>", [{key, value}])

      assert {:error, {:invalid_options, %{operation: :write_pdf}}} =
               PdfWriter.render(pages, [{key, value}])
    end
  end
end
