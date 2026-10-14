defmodule NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewportLifecycleTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedViewport, ResourceContext}
  alias NativeElixirPdfUtilities.Validators.{EmbeddedViewportValidator, HtmlValidator}

  test "true empty initial documents retain their issued source and full client without parsed-body gutters" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      for source <- [{:blank, "about:blank"}, {:blank, "ABOUT:blank?original#scope"}] do
        assert {:ok, prepared} = EmbeddedViewport.prepare_iframe_document(source, parent)
        assert prepared.source == source
        assert prepared.context.document_id != parent.document_id
        assert prepared.context.depth == parent.depth + 1
        assert prepared.context.image_budget == parent.image_budget
        original = prepared.prepared_dom

        for {width, height} <- [{22.5, 7.5}, {22.5, 0.75}, {22.5, 0}, {0, 7.5}] do
          initial = EmbeddedViewportValidator.initial_viewport(prepared, width, height)

          assert Map.take(initial, [:horizontal, :vertical]) == %{
                   horizontal: false,
                   vertical: false
                 }

          assert {:ok, styled} = EmbeddedViewport.style_viewport(prepared, width, height)
          assert styled.prepared_dom == original and styled.source == source
          assert styled.context == prepared.context
          [body] = styled.tree.children
          assert body.style.margin == %{top: 6.0, right: 6.0, bottom: 6.0, left: 6.0}
          assert {:ok, group} = EmbeddedViewport.layout(styled, width, height)
          assert group.role == :iframe_viewport
          assert group.clip == %{x: 0.0, y: 0.0, width: width, height: height}
          assert group.client_rect == group.clip
          assert group.boxes == []
          assert ResourceContext.current() == parent
        end
      end

      assert {:ok, parsed} = EmbeddedViewport.prepare_iframe_document({:srcdoc, ""}, parent)
      assert {:ok, %{boxes: [client, ui]}} = EmbeddedViewport.layout(parsed, 22.5, 3.75)
      assert client.client_rect == %{x: 0.0, y: 0.0, width: 11.25, height: 3.75}
      assert Enum.any?(ui.boxes, &(&1.role == :scrollbar))
      assert parsed.source == {:srcdoc, ""}
    end)
  end

  test "automatic gutter removal retains the original SVG document, selected fonts and source context" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      source = """
      <style>body{margin:0}svg{width:180px;height:40px}</style>
      <svg viewBox='0 0 180 40'><text x='4' y='24' font-family='DejaVu Sans' font-size='16'>Σ Child SVG</text></svg>
      """

      assert {:ok, prepared} = EmbeddedViewport.prepare_iframe_document({:srcdoc, source}, parent)
      prepared = Map.put(prepared, :host, :object)
      assert {:ok, styled} = EmbeddedViewport.style_viewport(prepared, 131.25, 37.5)
      assert {:ok, group} = EmbeddedViewport.layout(styled, 142.5, 37.5)
      assert group.client_rect == %{x: 0.0, y: 0.0, width: 142.5, height: 37.5}
      assert Enum.any?(group.boxes, &(&1.type == :drawing_group))
      refute Enum.any?(group.boxes, &(Map.get(&1, :role) == :scrollbar))
      assert styled.source == {:srcdoc, source}
      assert styled.prepared_dom == prepared.prepared_dom
      assert styled.context == prepared.context
      assert ResourceContext.current() == parent
    end)
  end
end
