defmodule NativeElixirPdfUtilities.HtmlToPdf.CssTransformContractTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, Font, ResourceContext}
  alias NativeElixirPdfUtilities.Validators.{CssTransformValidator, HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, face} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    %{
      style: %{
        font_face: face,
        font_size: 12,
        line_height: 15,
        _root_font_face: face,
        _root_font_size: 12,
        _root_line_height: 15,
        _position_viewport: %{width: 200, height: 120}
      }
    }
  end

  test "independent axes and one-angle skew preserve the authored origin", %{style: style} do
    for {source, point} <- [
          {"translateX(20px)", {22.5, 45.0}},
          {"translateY(20px)", {7.5, 30.0}},
          {"scaleY(2)", {7.5, 30.0}},
          {"skew(45deg)", {22.5, 45.0}},
          {"skewY(45deg)", {7.5, 37.5}},
          {"skew(45deg,45deg)", {22.5, 37.5}}
        ] do
      assert {:ok, prepared} = CssTransformValidator.apply(style, "transform", source)
      assert {:ok, prepared} = CssTransformValidator.apply(prepared, "transform-origin", "0 0")

      assert {:ok, matrix} =
               CssTransformValidator.resolve(prepared, %{x: 0, top: 60, width: 60, height: 30})

      {x, y} = DrawingGeometry.transform_point(7.5, 45, matrix)
      assert_in_delta x, elem(point, 0), 0.000001
      assert_in_delta y, elem(point, 1), 0.000001
    end
  end

  test "CSS reset keywords discard inherited metric captures", %{style: style} do
    for property <- ["transform", "transform-origin"] do
      assert {:ok, prepared} =
               CssTransformValidator.apply(
                 style,
                 property,
                 if(property == "transform", do: "translate(1em)", else: "left top")
               )

      inherited = CssTransformValidator.metric_style(prepared, property)

      metric_key =
        if property == "transform",
          do: :css_transform_metric_style,
          else: :transform_origin_metric_style

      for keyword <- ["initial", "unset", "revert", "revert-layer"] do
        assert {:ok, reset} =
                 CssTransformValidator.apply(
                   Map.put(prepared, metric_key, inherited),
                   property,
                   keyword
                 )

        refute Map.has_key?(reset, metric_key)
        assert CssTransformValidator.valid_style?(reset)
        assert CssTransformValidator.inherited_metric_style(reset, property) == nil
      end
    end
  end

  test "malformed caller recipes and scope boundaries return actionable diagnostics", %{
    style: style
  } do
    for value <- [nil, 10, [], "translate(1px", "rotate(", "translate()"] do
      assert :error = CssTransformValidator.prepare(value)
    end

    for value <- [
          %{css_transform: nil},
          %{css_transform: [:invalid]},
          %{css_transform: [{"rotate", ["90deg"]}]},
          :invalid
        ] do
      refute CssTransformValidator.valid_style?(value)
    end

    for scopes <- [
          :invalid,
          [nil],
          [
            %{
              id: make_ref(),
              matrix: {1, 0, 0, 1, 0, 0},
              border_box: %{x: 0, top: 10, width: -1, height: 10}
            }
          ]
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               CssTransformValidator.validate_scopes(scopes)

      assert diagnostic.stage == :layout
      assert diagnostic.source == "css_transform_scopes"
      assert diagnostic.message =~ "CSS transform"
    end

    assert {:ok, prepared} = CssTransformValidator.apply(style, "transform", "translate(1px)")

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      boxes =
        CssTransformValidator.mark_border_box(prepared, [], %{x: 4, top: 30, width: 0, height: 0})

      assert [%{type: :flow_marker, x: 4, y: 30}] = boxes
      assert {:ok, [box]} = CssTransformValidator.attach_scope(prepared, boxes)
      assert :ok = CssTransformValidator.validate_scopes(box.css_transform_scopes)

      assert {:ok, [plain]} =
               CssTransformValidator.attach_scope(Map.put(style, :css_transform, :none), boxes)

      refute Map.has_key?(plain, :css_transform_scopes)
    end)
  end

  test "mixed scoped and unscoped real controls retain separate editable AP ownership" do
    source =
      "<style>body{margin:0}section{transform:translateX(10px);width:100px}input{width:60px;height:20px}</style><input name=plain value=Plain><section><input name=moved value=Moved></section>"

    assert {:ok, pdf} = HtmlToPdf.render(source, page_size: {200, 150}, margin: 0)
    assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert Enum.sort(Enum.map(fields, & &1.name)) == ["moved", "plain"]

    assert {:ok, filled} =
             NativeElixirPdfUtilities.Forms.fill(pdf, %{"plain" => "One", "moved" => "Two"})

    assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(filled)
    assert Enum.sort(Enum.map(fields, & &1.value)) == ["One", "Two"]
    assert {:ok, flattened} = NativeElixirPdfUtilities.Forms.flatten(filled)
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(flattened)
  end

  test "real nested control planes issue APs once and reject mixed ownership" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedViewport, Layout, PdfWriter}
    alias NativeElixirPdfUtilities.Validators.{EmbeddedFormValidator, HtmlFormValidator}

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, surface} =
               EmbeddedViewport.prepare_html(
                 {:srcdoc,
                  "<body style='margin:0'><input name=actual value=Source style='width:50px;height:20px'></body>"},
                 parent
               )

      ResourceContext.with_document(surface.context, fn ->
        assert {:ok, layout} = Layout.layout_viewport(surface.tree, 90, 60)

        assert {:ok, style} =
                 CssTransformValidator.apply(
                   hd(surface.tree.children).style,
                   "transform",
                   "translateX(6px)"
                 )

        assert {:ok, scope} =
                 CssTransformValidator.prepare_scope(style, %{
                   x: 0,
                   top: 60,
                   width: 90,
                   height: 60
                 })

        group = %{
          type: :drawing_group,
          width: 90,
          height: 60,
          transform: {1, 0, 0, 1, 0, 0},
          clip: %{x: 0, y: 0, width: 60, height: 60},
          boxes: layout.boxes,
          css_transform_scopes: [scope]
        }

        assert {:ok, pages} =
                 CssTransformValidator.materialize(
                   [%{size: {120, 90}, boxes: [group]}],
                   [],
                   surface.context
                 )

        assert {:ok, pdf} = PdfWriter.render(pages)

        assert {:ok, [%{name: "actual", value: "Source"}]} =
                 NativeElixirPdfUtilities.Forms.fields(pdf)

        assert {:ok, static_pages} =
                 CssTransformValidator.materialize(
                   [%{size: {120, 90}, boxes: [group]}],
                   [forms: :static],
                   surface.context
                 )

        assert {:ok, static_pdf} = PdfWriter.render(static_pages)
        assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(static_pdf)
        assert {:ok, static_text} = NativeElixirPdfUtilities.Text.extract(static_pdf)
        assert static_text =~ "Source"

        assert {:ok, [_page], fields} =
                 HtmlFormValidator.prepare_scoped(
                   [%{size: {90, 60}, boxes: layout.boxes}],
                   [],
                   surface.context
                 )

        assert {:ok, descriptor} = EmbeddedFormValidator.issue(surface.context, fields)

        assert {:error, {:invalid_layout, diagnostic}} =
                 CssTransformValidator.materialize(
                   [%{size: {120, 90}, boxes: [Map.put(group, :embedded_widgets, descriptor)]}],
                   [],
                   surface.context
                 )

        assert diagnostic.source == "embedded_widgets"
        assert diagnostic.message =~ "mix raw controls"
      end)
    end)
  end

  test "finite affine overflow and malformed retained scopes use canonical errors", %{
    style: style
  } do
    Limits.install(%{
      Limits.effective()
      | max_css_numeric_magnitude: 1.0e300,
        max_pdf_numeric_magnitude: 1.0e300
    })

    assert {:ok, prepared} =
             CssTransformValidator.apply(style, "transform", "scale(1e200) scale(1e200)")

    assert {:error, {:invalid_layout, diagnostic}} =
             CssTransformValidator.resolve(prepared, %{x: 0, top: 30, width: 20, height: 20})

    assert diagnostic.message =~ "finite affine"
    assert diagnostic.stage == :layout
    source = %{type: :rect, x: 0, y: 0, width: 20, height: 20, css_transform_scopes: [nil]}

    assert {:error, {:invalid_layout, diagnostic}} =
             CssTransformValidator.materialize([%{size: {90, 60}, boxes: [source]}], [])

    assert diagnostic.source == "css_transform_scopes"
  end

  test "actual decorated source scopes follow fragment border boxes across sheets" do
    text = Enum.map_join(1..8, fn index -> "<p>Source #{index}</p>" end)

    source =
      "<style>html,body{margin:0;font-size:10pt;line-height:15pt}p{margin:0}section{width:70pt;transform:rotate(3deg);background:#eee;padding:4pt}</style><section>#{text}</section>"

    assert {:ok, pdf} = HtmlToPdf.render(source, page_size: {120, 90}, margin: 10)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)
    assert length(document.document.pages) == 2
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert String.replace(text, ~r/\s+/, "") =~ "Source1"
    assert String.replace(text, ~r/\s+/, "") =~ "Source8"
  end

  test "root targets and nested clipped Math retain native source geometry through print slicing" do
    source =
      "<html id=root><style>html{transform:translateX(2pt)}html,body{margin:0}section{overflow:hidden;width:70pt;height:35pt}p{margin:0}</style><body><a href='#root'>Root</a><section><math><mi>x</mi><mo>+</mo><mi>y</mi></math></section></body></html>"

    assert {:ok, pdf} = HtmlToPdf.render(source, page_size: {120, 90}, margin: 0)
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)
    [page] = context.document.pages
    assert Map.has_key?(context.document.objects[page.ref].value, "Annots")
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert String.replace(text, ~r/\s+/, "") =~ "Rootx+y"
  end

  test "native selection groups retain actual measured text inside scoped overflow viewports", %{
    style: style
  } do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, PdfWriter, Style}

    assert true =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
               assert {:ok, dom} =
                        HtmlParser.parse_document_detailed(
                          "<style>html,body,p{margin:0}</style><p>Measured source</p>"
                        )

               assert {:ok, tree} = Style.compute_detailed(dom, [], context.image_budget)
               assert {:ok, layout} = Layout.layout_viewport(tree, 90, 60)
               text = Enum.filter(layout.boxes, &(&1.type == :text))
               assert text != []
               actual_text = Enum.map_join(text, & &1.text)
               selection = %{type: :glyph_group, actual_text: actual_text, boxes: text}

               assert {:ok, transformed} =
                        CssTransformValidator.apply(style, "transform", "translateX(3px)")

               assert {:ok, scope} =
                        CssTransformValidator.prepare_scope(transformed, %{
                          x: 0,
                          top: 60,
                          width: 90,
                          height: 60
                        })

               group = %{
                 type: :drawing_group,
                 role: :overflow_clip,
                 width: 90,
                 height: 60,
                 transform: {1, 0, 0, 1, 0, 0},
                 clip: %{x: 0, y: 0, width: 90, height: 60},
                 boxes: [selection],
                 css_transform_scopes: [scope]
               }

               assert {:ok, pages} =
                        CssTransformValidator.materialize(
                          [%{size: {120, 90}, boxes: [group]}],
                          [css_fragmentation_margins: %{left: 0, right: 0, top: 0, bottom: 0}],
                          context
                        )

               assert {:ok, pdf} = PdfWriter.render(pages)
               assert {:ok, extracted} = NativeElixirPdfUtilities.Text.extract(pdf)
               assert String.replace(extracted, ~r/\s+/, "") == "Measuredsource"
             end)
  end

  test "real transformed paint obeys the clip arrangement allocation ceiling" do
    Limits.install(%{Limits.effective() | max_clip_projection_work: 0})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<style>html,body{margin:0}section{height:30pt;width:40pt;background:red;transform:rotate(5deg)}</style><section></section>",
               page_size: {120, 90},
               margin: 0
             )

    assert diagnostic.message =~ "max_clip_projection_work"
    assert diagnostic.stage == :layout
  end

  test "scoped control preparation diagnoses actual invalid context and field capacity before issuance" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

    assert true =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
               assert {:ok, dom} =
                        HtmlParser.parse_document_detailed(
                          "<style>section{transform:translateX(3px)}</style><input name=plain><section><input name=moved></section>"
                        )

               assert {:ok, tree} = Style.compute_detailed(dom, [], context.image_budget)
               assert {:ok, layout} = Layout.layout_viewport(tree, 120, 90)
               pages = [%{size: {120, 90}, boxes: layout.boxes}]

               assert {:error, {:invalid_options, diagnostic}} =
                        CssTransformValidator.materialize(pages, [], :invalid)

               assert diagnostic.message =~ "scoped context"
               Limits.install(%{Limits.effective() | max_pdf_form_fields: 0})

               assert {:error, {:resource_limit_exceeded, diagnostic}} =
                        CssTransformValidator.materialize(pages, [], context)

               assert diagnostic.message =~ "max_pdf_form_fields"
             end)
  end

  test "native clipped source fragments preserve measured transform recipes with optional page margins" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} =
               HtmlParser.parse_document_detailed(
                 "<style>html,body{margin:0}section{height:100pt;transform:rotate(3deg)}p{margin:0}</style><section><p>First source</p><p>Second source</p></section>"
               )

      assert {:ok, tree} = Style.compute_detailed(dom, [], context.image_budget)
      assert {:ok, layout} = Layout.layout(tree, page_size: {120, 60}, margin: 0)
      first = DrawingGeometry.clip_boxes(layout.boxes, %{x: 0, y: 0, width: 120, height: 60})

      second =
        DrawingGeometry.clip_boxes(DrawingGeometry.translate_boxes(layout.boxes, 0, 60), %{
          x: 0,
          y: 0,
          width: 120,
          height: 60
        })

      source_pages = [%{size: {120, 60}, boxes: first}, %{size: {120, 60}, boxes: second}]

      for opts <- [[], [css_fragmentation_margins: %{top: 5, bottom: 5, left: 0, right: 0}]] do
        assert {:ok, pages} = CssTransformValidator.materialize(source_pages, opts, context)
        assert length(pages) == 2

        assert Enum.all?(pages, fn page ->
                 Enum.all?(page.boxes, &(Map.get(&1, :css_transform_scopes) == nil))
               end)
      end
    end)
  end

  test "painted empty source capacity obeys real page limits and rejects orphan descendant owners" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, Style}
    limits = Limits.effective()
    Limits.install(%{limits | max_rendered_pages: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<style>body{margin:0}section{min-height:250pt;background:#ddd}</style><section></section>",
               page_size: {100, 60},
               margin: 10
             )

    assert diagnostic.message =~ "max_rendered_pages"
    assert diagnostic.stage == :pagination
    Limits.install(limits)

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} =
               HtmlParser.parse_document_detailed(
                 "<style>body{margin:0}section{position:relative;height:100pt;background:#ddd}div{position:absolute;top:80pt;width:10pt;height:10pt;background:blue}</style><section><div></div></section>"
               )

      assert {:ok, tree} = Style.compute_detailed(dom, [], context.image_budget)
      assert {:ok, layout} = Layout.layout(tree, page_size: {100, 60}, margin: 0)

      malformed = %{
        layout
        | boxes:
            Enum.map(layout.boxes, fn box ->
              if Map.has_key?(box, :positioned_fragment_owner),
                do: %{box | positioned_fragment_owner: make_ref()},
                else: box
            end)
      }

      assert {:error, {:invalid_layout, diagnostic}} = Pagination.paginate(malformed)
      assert diagnostic.stage == :pagination
      assert diagnostic.source == "fieldset_paint_id"
    end)
  end

  test "positioned fields in fragmented empty parents keep one source AP and real widget slices" do
    source =
      "<style>html,body{margin:0;font-family:DejaVu Sans;font-size:10pt}section{position:relative;height:150pt;background:#ddd}input{position:absolute;left:10pt;top:35pt;width:60pt;height:95pt}</style><section><input name=actual value=Original></section>"

    assert {:ok, pdf} = HtmlToPdf.render(source, page_size: {120, 90}, margin: 0)

    assert {:ok, [%{name: "actual", value: "Original", widgets: widgets}]} =
             NativeElixirPdfUtilities.Forms.fields(pdf)

    assert Enum.map(widgets, & &1.page) == [1, 2]
    assert {:ok, filled} = NativeElixirPdfUtilities.Forms.fill(pdf, %{"actual" => "Changed"})

    assert {:ok, [%{value: "Changed", widgets: widgets}]} =
             NativeElixirPdfUtilities.Forms.fields(filled)

    assert length(widgets) == 2
    assert {:ok, flattened} = NativeElixirPdfUtilities.Forms.flatten(filled)
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(flattened)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(flattened)
    assert String.replace(text, ~r/\s+/, "") =~ "Changed"

    assert {:ok, static} =
             HtmlToPdf.render(source, page_size: {120, 90}, margin: 0, forms: :static)

    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(static)
  end
end
