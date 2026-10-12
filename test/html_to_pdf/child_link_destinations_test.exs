defmodule NativeElixirPdfUtilities.HtmlToPdf.ChildLinkDestinationsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    EmbeddedViewport,
    Pagination,
    PdfWriter,
    ResourceContext
  }

  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.{HtmlLinkValidator, HtmlValidator}

  @child "<html id=root><style>html,body{margin:0;overflow:hidden;font-size:10pt;line-height:12pt}p{margin:0}</style><body><p><a href='#shared'>Jump</a> <a href='#'>Top</a></p><p id=shared>Child target</p></body></html>"

  test "public rendering keeps same parent and repeated child IDs on their actual parent pages" do
    root =
      Path.join(System.tmp_dir!(), "native-child-links-#{System.unique_integer([:positive])}")

    File.mkdir_p!(root)
    File.write!(Path.join(root, "child.html"), @child)
    on_exit(fn -> File.rm_rf!(root) end)

    html = """
    <style>html,body{margin:0;font-size:10pt;line-height:12pt}p{margin:0}iframe{display:block;border:0;width:90pt;height:60pt}</style>
    <p id=shared>Parent target</p><p><a href='#shared'>Parent jump</a></p>
    <iframe src='child.html'></iframe><iframe style='break-before:page' src='child.html'></iframe>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, base_url: root, page_size: {220, 180}, margin: 10)
    assert {:ok, context} = Reader.read_validated(pdf)
    assert length(context.pages) == 2
    [first, second] = Enum.map(context.pages, & &1.ref)
    destinations = destinations(context)
    assert [{:ref, first}, {:name, "XYZ"}, 9.75, 170.25, nil] in destinations
    assert [{:ref, first}, {:name, "XYZ"}, 9.75, 134.25, nil] in destinations
    assert [{:ref, first}, {:name, "XYZ"}, 9.75, 146.25, nil] in destinations
    assert [{:ref, second}, {:name, "XYZ"}, 9.75, 158.25, nil] in destinations
    assert [{:ref, second}, {:name, "XYZ"}, 9.75, 170.25, nil] in destinations
    assert length(destinations) == 5
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert length(Regex.scan(~r/Child target/, text)) == 2
  end

  test "nested positive nonuniform group scales use rounded paint CTMs for destinations and regions" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, surface} = EmbeddedViewport.prepare_html({:srcdoc, @child}, parent)

      assert surface.tree.link_scope ==
               {surface.context.document_id, surface.context.tree_scope_id}

      assert {:ok, child} = EmbeddedViewport.layout(surface, 90, 60)
      child = %{child | transform: {1.250049, 0, 0, 0.750049, 17.000049, 25.000049}}

      outer = %{
        type: :drawing_group,
        width: 140,
        height: 100,
        transform: {2, 0, 0, 1.5, 10, 20},
        clip: %{x: 0, y: 0, width: 140, height: 100},
        boxes: [child]
      }

      layout = %{
        type: :layout,
        page_size: {320, 200},
        margin: 0,
        boxes: [Map.merge(outer, %{x: 10, y: 20})]
      }

      assert {:ok, [page]} = Pagination.paginate(layout)
      [outer] = page.boxes
      [child] = outer.boxes
      links = Enum.filter(child.boxes, &Map.has_key?(&1, :link_destination))

      assert Enum.find(links, &(&1.text == "Jump")).link_destination ==
               {:xyz, 1, 44.0, 111.5}

      assert Enum.find(links, &(&1.text == "Top")).link_destination ==
               {:xyz, 1, 44.0, 125.0}

      assert {:ok, pdf} = PdfWriter.render([page])
      assert {:ok, context} = Reader.read_validated(pdf)

      assert [{:ref, hd(context.pages).ref}, {:name, "XYZ"}, 44, 111.5, nil] in destinations(
               context
             )

      annotations =
        for {_ref, %{value: %{"Subtype" => {:name, "Link"}} = value}} <- context.document.objects,
            do: value

      assert Enum.all?(annotations, fn annotation ->
               [left, bottom, right, top] = annotation["Rect"]
               left >= 44 and bottom >= 57.5 and right <= 269 and top <= 125
             end)
    end)
  end

  test "child hidden and missing first targets omit links without binding matching parent IDs" do
    child =
      "<body style='margin:0;overflow:hidden'><p id=shared hidden>Hidden</p><p id=shared>Later duplicate</p><a href='#shared'>Hidden jump</a><a href='#missing'>Missing jump</a></body>"

    html = "<p id=shared>Parent target</p>" <> iframe(child)
    assert {:ok, pdf} = HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Later duplicate" and text =~ "Hidden jump" and text =~ "Missing jump"
    refute text =~ "HiddenLater"
  end

  test "visible clipped and zero-viewport child targets produce public actionable diagnostics" do
    for {child, attrs, source} <- [
          {"<body style='margin:0;overflow:hidden'><a href='#outside'>Jump</a><p id=outside style='position:relative;left:500pt'>Outside</p></body>",
           "", "outside"},
          {@child, "style='display:block;width:0;height:0;border:0'", "shared"}
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(iframe(child, attrs))
      assert diagnostic.stage == :pagination and diagnostic.operation == :paginate
      assert diagnostic.source in [source, "document beginning"]
      assert diagnostic.message =~ "no representable destination"
    end
  end

  test "empty target markers survive child placement and are removed before writer normalization" do
    child =
      "<style>html,body{margin:0;font-size:10pt;line-height:12pt}p{margin:0}</style><p><a href='#empty'>Empty</a></p><p><span id=empty></span></p><p>After</p>"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, surface} = EmbeddedViewport.prepare_html({:srcdoc, child}, parent)
      assert {:ok, group} = EmbeddedViewport.layout(surface, 90, 60)

      assert Enum.any?(
               group.boxes,
               &(Map.get(&1, :type) == :flow_marker and Map.get(&1, :html_targets, []) != [])
             )

      group = Map.merge(group, %{transform: {1, 0, 0, 1, 10, 20}, x: 10, y: 20})

      assert {:ok, [page]} =
               Pagination.paginate(%{
                 type: :layout,
                 page_size: {200, 120},
                 margin: 0,
                 boxes: [group]
               })

      [group] = page.boxes
      refute Enum.any?(group.boxes, &(&1.type == :flow_marker))

      assert Enum.find(group.boxes, &(Map.get(&1, :text) == "Empty")).link_destination ==
               {:xyz, 1, 10.0, 68.0}

      assert {:ok, _pdf} = PdfWriter.render([page])
    end)
  end

  test "scoped transport rejects absent root origins and invalid nested target scopes" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, surface} = EmbeddedViewport.prepare_html({:srcdoc, @child}, parent)
      assert {:ok, group} = EmbeddedViewport.layout(surface, 90, 60)

      assert {:error, {:invalid_layout, %{source: "link_document_origin"}}} =
               HtmlLinkValidator.validate_boxes([Map.delete(group, :link_document_origin)], nil)

      assert {:error, {:invalid_layout, %{source: "link_scope"}}} =
               HtmlLinkValidator.validate_boxes([Map.put(group, :link_scope, :forged)], nil)

      assert {:error, {:invalid_layout, %{source: "link_document_targets"}}} =
               HtmlLinkValidator.validate_boxes(
                 [Map.put(group, :link_document_origin, %{x: 91, y: 60})],
                 nil
               )

      assert {:error, {:invalid_layout, %{source: "link_document_origin"}}} =
               HtmlLinkValidator.validate_boxes([Map.put(group, :link_document_origin, nil)], nil)
    end)
  end

  test "child root IDs and legacy raw-decoded-name precedence survive nested glyph paint groups" do
    child =
      "<html id=root><style>html,body{margin:0;font-size:10pt;line-height:12pt}p{margin:0}</style><body><p><a href='#root'>Root</a></p><p><a href='#a%20b'>Raw</a></p><p><a href='#legacy'>Name</a></p><p id='a%20b'>Raw target</p><p id='a b'>Decoded target</p><a name=legacy>Named target</a></body></html>"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, surface} = EmbeddedViewport.prepare_html({:srcdoc, child}, parent)
      assert {:ok, group} = EmbeddedViewport.layout(surface, 180, 120)
      assert [%{source: "root"}] = group.link_document_targets
      group = Map.merge(group, %{x: 10, y: 20, transform: {1, 0, 0, 1, 10, 20}})
      # ActualText grouping retains existing selected text boxes and their target
      # identities. It does not change their document or coordinate system.
      {texts, others} = Enum.split_with(group.boxes, &(&1.type == :text))
      glyphs = %{type: :glyph_group, actual_text: Enum.map_join(texts, & &1.text), boxes: texts}
      group = %{group | boxes: others ++ [glyphs]}
      assert :ok = HtmlLinkValidator.validate_boxes([group], nil)

      assert {:ok, [page]} =
               Pagination.paginate(%{
                 type: :layout,
                 page_size: {220, 180},
                 margin: 0,
                 boxes: [group]
               })

      [group] = page.boxes
      glyphs = Enum.find(group.boxes, &(&1.type == :glyph_group))

      assert Enum.find(glyphs.boxes, &(&1.text == "Root")).link_destination ==
               {:xyz, 1, 10.0, 140.0}

      assert Enum.find(glyphs.boxes, &(&1.text == "Raw")).link_destination ==
               {:xyz, 1, 10.0, 104.0}

      assert Enum.find(glyphs.boxes, &(&1.text == "Name")).link_destination ==
               {:xyz, 1, 10.0, 80.0}

      assert {:ok, _pdf} = PdfWriter.render([page])

      assert {:error, {:invalid_pdf_input, %{message: message}}} =
               HtmlLinkValidator.validate_boxes([%{group | transform: {1, 1, 0, 1, 10, 20}}], nil)

      assert message =~ "rotations"
    end)
  end

  test "zero child viewports keep hidden and missing targets scoped without link output" do
    child =
      "<body style='margin:0;overflow:hidden'><p id=shared hidden>Hidden target</p><a href='#shared'>Hidden jump</a><a href='#missing'>Missing jump</a></body>"

    html =
      "<p id=shared>Parent target</p>" <>
        iframe(child, "style='display:block;width:0;height:0;border:0'")

    assert {:ok, pdf} = HtmlToPdf.render(html, compress_streams: false)
    assert pdf =~ "0 0 0 0 re W n"
    refute pdf =~ "/Subtype /Link" or pdf =~ "/Dest ["
  end

  defp iframe(child, attrs \\ "") do
    encoded =
      child
      |> String.replace("&", "&amp;")
      |> String.replace("<", "&lt;")
      |> String.replace("\"", "&quot;")

    "<iframe #{attrs} srcdoc=\"#{encoded}\"></iframe>"
  end

  defp destinations(context) do
    for {_ref, %{value: %{"Subtype" => {:name, "Link"}, "Dest" => destination}}} <-
          context.document.objects,
        do: destination
  end
end
