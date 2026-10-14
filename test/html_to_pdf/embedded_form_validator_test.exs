defmodule NativeElixirPdfUtilities.HtmlToPdf.EmbeddedFormValidatorTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedViewport, Layout, ResourceContext}

  alias NativeElixirPdfUtilities.Validators.{
    EmbeddedFormValidator,
    HtmlFormValidator,
    HtmlValidator,
    WriterValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "actual child controls issue opaque scoped fields and retain real clipped appearance paint" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} =
               EmbeddedViewport.prepare_iframe_document(
                 {:srcdoc,
                  "<body style='margin:0'><input name='shared' value='Alpha' style='width:60px;height:24px'></body>"},
                 parent
               )

      assert {:ok, surface} = EmbeddedViewport.style_viewport(prepared, 90, 60)

      ResourceContext.with_document(surface.context, fn ->
        assert {:ok, layout} = Layout.layout_viewport(surface.tree, 90, 60)

        assert {:ok, [_page], [field]} =
                 HtmlFormValidator.prepare_scoped(
                   [%{size: {90, 60}, boxes: layout.boxes}],
                   [],
                   surface.context
                 )

        assert field.name == "shared" and field.value == "Alpha"
        assert {:ok, descriptor} = EmbeddedFormValidator.issue(surface.context, [field])
        assert {:ok, [^field]} = EmbeddedFormValidator.resolve(descriptor)
        assert {:ok, slice} = EmbeddedFormValidator.issue_slice(descriptor)
        assert slice.token != descriptor.token
        assert {:ok, [^field]} = EmbeddedFormValidator.resolve(slice)
        assert {:ok, source} = EmbeddedFormValidator.source_identity(descriptor)
        assert {:ok, ^source} = EmbeddedFormValidator.source_identity(slice)
        assert {:ok, next_slice} = EmbeddedFormValidator.issue_slice(slice)
        assert {:ok, ^source} = EmbeddedFormValidator.source_identity(next_slice)

        group = %{
          type: :drawing_group,
          width: 90,
          height: 60,
          transform: {1, 0, 0, 1, 0, 0},
          boxes: [],
          embedded_widgets: descriptor
        }

        assert {:error, {reason, diagnostic}} =
                 NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.render([
                   %{size: {90, 60}, boxes: [group, group]}
                 ])

        assert reason in [:invalid_form, :unsupported_form]
        assert diagnostic.stage == :forms
        assert diagnostic.message =~ "duplicate" or diagnostic.message =~ "repeat"

        [left, bottom, right, top] = field.rect

        assert {:ok, [projected]} =
                 EmbeddedFormValidator.compose(
                   descriptor,
                   {1, 1, 10, 20, {left + 15, bottom + 23, right + 6, top + 18}, 60}
                 )

        assert projected.rect == [left + 15, bottom + 23, right + 6, top + 18]
        assert projected.appearance.size == {right - left - 9, top - bottom - 5}
        [group] = projected.appearance.boxes
        assert group.transform == {1, 0, 0, 1, -5, -3}

        [source_plane] = group.boxes
        assert source_plane.width == 90 and source_plane.height == 60
        assert source_plane.transform == {1, 0, 0, 1, -left, -bottom}
        assert field.appearance.paint_frame == %{x: left, y: bottom, width: 90, height: 60}

        assert source_plane.boxes ==
                 Enum.map(field.appearance.boxes, &Map.drop(&1, [:form_control, :form_owner]))

        assert Enum.any?(source_plane.boxes, &(&1.type == :text and &1.text == "Alpha"))
        assert {:ok, _context} = WriterValidator.prepare_appearances([projected.appearance], 100)

        assert {:ok, []} =
                 EmbeddedFormValidator.compose(
                   descriptor,
                   {1, 1, 0, 0, {1000, 1000, 1100, 1100}, 60}
                 )

        assert ResourceContext.current() == surface.context
      end)

      assert ResourceContext.current() == parent
    end)
  end

  test "public axis projections validate geometry and preserve genuine empty clips" do
    assert true =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
               assert {:ok, surface} =
                        EmbeddedViewport.prepare_html(
                          {:srcdoc,
                           "<input name=actual value=Source style='width:40px;height:20px'>"},
                          parent
                        )

               assert {:ok, layout} = Layout.layout_viewport(surface.tree, 90, 60)

               assert {:ok, [_page], fields} =
                        HtmlFormValidator.prepare_scoped(
                          [%{size: {90, 60}, boxes: layout.boxes}],
                          [],
                          surface.context
                        )

               assert {:ok, descriptor} = EmbeddedFormValidator.issue(surface.context, fields)

               for operation <- [
                     &EmbeddedFormValidator.compose/2,
                     &EmbeddedFormValidator.validate_composition/2,
                     &EmbeddedFormValidator.projection_bounds/2
                   ],
                   composition <- [
                     :invalid,
                     {},
                     {1, 1, 0, 0, nil},
                     {0, 1, 0, 0, nil, 60},
                     {-1, 1, 0, 0, nil, 60},
                     {1, 1, :invalid, 0, nil, 60},
                     {1, 1, 0, 0, %{}, 60},
                     {1, 1, 0, 0, {0, 0, :invalid, 20}, 60},
                     {1, 1, 0, 0, nil, -1}
                   ] do
                 assert {:error, {:invalid_form, diagnostic}} =
                          operation.(descriptor, composition)

                 assert diagnostic.stage == :forms
                 assert diagnostic.operation == :compose_child_widgets
                 assert diagnostic.message =~ "axis placement"
               end

               assert :ok =
                        EmbeddedFormValidator.validate_composition(
                          descriptor,
                          {1, 1, 0, 0, {100, 100, 0, 0}, nil}
                        )

               assert {:ok, []} =
                        EmbeddedFormValidator.compose(
                          descriptor,
                          {1, 1, 0, 0, {100, 100, 0, 0}, nil}
                        )

               assert {:ok, [bounds]} =
                        EmbeddedFormValidator.projection_bounds(
                          descriptor,
                          {1, 1, 0, 0, {100, 100, 0, 0}, nil}
                        )

               assert elem(bounds, 0) >= elem(bounds, 2)
               Limits.install(%{Limits.effective() | max_pdf_numeric_magnitude: 100})

               for operation <- [
                     &EmbeddedFormValidator.compose/2,
                     &EmbeddedFormValidator.validate_composition/2,
                     &EmbeddedFormValidator.projection_bounds/2
                   ] do
                 assert {:error, {:invalid_pdf_input, diagnostic}} =
                          operation.(descriptor, {1, 1, 200, 0, nil, 60})

                 assert diagnostic.message =~ "max_pdf_numeric_magnitude"
               end

               Limits.install(%{Limits.defaults() | max_pdf_objects: 1})

               assert {:error, {:resource_limit_exceeded, diagnostic}} =
                        EmbeddedFormValidator.compose_affine(
                          descriptor,
                          {1, 0, 0, 1, 0, 0},
                          {0, 0, 90, 60},
                          nil
                        )

               assert diagnostic.message =~ "max_pdf_objects"

               Limits.install(%{Limits.defaults() | max_appearance_widgets: 0})

               assert {:error, {:resource_limit_exceeded, diagnostic}} =
                        EmbeddedFormValidator.compose_affine(
                          descriptor,
                          {1, 0, 0, 1, 0, 0},
                          {0, 0, 90, 60},
                          nil
                        )

               assert diagnostic.message =~ "max_appearance_widgets"
             end)
  end

  test "drawing slice validation retains live AP identities through nested groups and late field limits" do
    for invalid <- [nil, [], %{type: :drawing_group, boxes: []}] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               EmbeddedFormValidator.issue_drawing_slice(invalid)

      assert diagnostic.stage == :pdf
    end

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, surface} =
               EmbeddedViewport.prepare_html(
                 {:srcdoc, "<input name=actual value=Source>"},
                 context
               )

      assert {:ok, layout} = Layout.layout_viewport(surface.tree, 90, 60)

      assert {:ok, [_page], fields} =
               HtmlFormValidator.prepare_scoped(
                 [%{size: {90, 60}, boxes: layout.boxes}],
                 [],
                 surface.context
               )

      assert {:ok, descriptor} = EmbeddedFormValidator.issue(surface.context, fields)

      leaf = %{
        type: :drawing_group,
        width: 90,
        height: 60,
        transform: {1, 0, 0, 1, 0, 0},
        boxes: [],
        embedded_widgets: descriptor
      }

      group = %{
        type: :drawing_group,
        width: 90,
        height: 60,
        transform: {1, 0, 0, 1, 0, 0},
        boxes: [leaf]
      }

      assert {:ok, %{boxes: [%{embedded_widgets: slice}]}} =
               EmbeddedFormValidator.issue_drawing_slice(group)

      assert slice.token != descriptor.token
      assert {:ok, identity} = EmbeddedFormValidator.source_identity(descriptor)
      assert {:ok, ^identity} = EmbeddedFormValidator.source_identity(slice)
      assert {:ok, ^fields} = EmbeddedFormValidator.resolve(slice)
      Limits.install(%{Limits.effective() | max_pdf_form_fields: 0})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               EmbeddedFormValidator.issue_drawing_slice(group)

      assert diagnostic.message =~ "max_pdf_form_fields"
      assert diagnostic.stage == :forms
    end)
  end

  test "unknown, tampered, cross-document and expired descriptors cannot become fields" do
    descriptor =
      ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
        assert {:ok, first} =
                 EmbeddedViewport.prepare_iframe_document({:blank, "about:blank"}, parent)

        assert {:ok, second} =
                 EmbeddedViewport.prepare_iframe_document({:blank, "about:blank"}, parent)

        assert {:ok, descriptor} = EmbeddedFormValidator.issue(first.context, [])

        assert {:error, {:invalid_form, slice_diagnostic}} =
                 EmbeddedFormValidator.issue_slice(%{descriptor | token: make_ref()})

        assert slice_diagnostic.operation == :compose_child_widgets

        assert {:error, {:invalid_form, diagnostic}} =
                 EmbeddedFormValidator.resolve(%{descriptor | token: make_ref()})

        assert diagnostic.operation == :compose_child_widgets

        assert {:error, {:invalid_form, diagnostic}} =
                 EmbeddedFormValidator.resolve(%{descriptor | context: second.context})

        assert diagnostic.message =~ "different document"

        assert {:error, {:invalid_options, _}} =
                 EmbeddedFormValidator.resolve(%{
                   descriptor
                   | context: %{first.context | authorization_root: "/changed"}
                 })

        assert {:error, {:invalid_form, _}} = EmbeddedFormValidator.resolve(%{fields: []})
        descriptor
      end)

    assert {:error, {:invalid_options, diagnostic}} = EmbeddedFormValidator.resolve(descriptor)
    assert diagnostic.message =~ "immutable scope"
  end

  test "source widget charging is cumulative and does not abandon the caller callback" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} =
               EmbeddedViewport.prepare_iframe_document(
                 {:srcdoc, "<input name='budget' value='Real source'>"},
                 parent
               )

      assert {:ok, surface} = EmbeddedViewport.style_viewport(prepared, 90, 60)
      assert {:ok, layout} = Layout.layout_viewport(surface.tree, 90, 60)

      assert {:ok, [_page], fields} =
               HtmlFormValidator.prepare_scoped(
                 [%{size: {90, 60}, boxes: layout.boxes}],
                 [],
                 surface.context
               )

      assert [%{value: "Real source"}] = fields
      Limits.install(Map.put(Limits.effective(), :max_pdf_form_fields, 0))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               EmbeddedFormValidator.issue(surface.context, fields)

      assert diagnostic.operation == :compose_child_widgets
      assert diagnostic.message =~ "max_pdf_form_fields"
      assert ResourceContext.current() == parent
      :continued
    end)
    |> then(&assert(&1 == :continued))
  end

  test "geometry authority rejects malformed or excessive rect coordinates before appearance allocation" do
    for values <- [
          [],
          [0, 0, 1, 1],
          List.duplicate(:invalid, 8),
          [0, 0, Limits.get(:max_pdf_numeric_magnitude) + 1, 1, 0, 0, 1, 1]
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               WriterValidator.validate_pdf_geometry(values)

      assert diagnostic.stage == :pdf and diagnostic.message != ""
    end
  end
end
