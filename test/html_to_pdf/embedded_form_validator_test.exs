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

        assert group.boxes ==
                 Enum.map(field.appearance.boxes, &Map.drop(&1, [:form_control, :form_owner]))

        assert Enum.any?(group.boxes, &(&1.type == :text and &1.text == "Alpha"))
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

  test "unknown, tampered, cross-document and expired descriptors cannot become fields" do
    descriptor =
      ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
        assert {:ok, first} =
                 EmbeddedViewport.prepare_iframe_document({:blank, "about:blank"}, parent)

        assert {:ok, second} =
                 EmbeddedViewport.prepare_iframe_document({:blank, "about:blank"}, parent)

        assert {:ok, descriptor} = EmbeddedFormValidator.issue(first.context, [])

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
