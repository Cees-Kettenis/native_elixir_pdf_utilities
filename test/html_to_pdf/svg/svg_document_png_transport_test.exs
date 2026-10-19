defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentPngTransportTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedViewport, ResourceContext, SvgDocumentImage}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  setup do
    root = Path.join(System.tmp_dir!(), "nepu-document-png-#{System.unique_integer([:positive])}")
    File.mkdir_p!(root)
    on_exit(fn -> File.rm_rf!(root) end)
    %{root: root}
  end

  test "authorized selected source remains immutable in its child scope and converts only to PNG",
       %{root: root} do
    source = "<svg width='100' height='50'><rect width='100' height='50' fill='red'/></svg>"
    path = Path.join(root, "child.svg")
    File.write!(path, source)

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, {:image, %{image: document}}} =
               EmbeddedViewport.prepare_selected(
                 {:image, "child.svg", "image/svg+xml"},
                 parent,
                 [],
                 :object
               )

      assert document.context.document_id != parent.document_id
      assert {document.width, document.height} == {75.0, 37.5}
      File.write!(path, String.replace(source, "red", "blue"))
      assert {:ok, image} = SvgDocumentImage.compose(document, 120, 90)
      assert image.format == :png
      assert {image.width_px, image.height_px} == {320, 240}
      assert binary_part(image.data, (60 * image.width_px + 140) * 3, 3) == <<255, 0, 0>>
      assert :binary.at(image.alpha_data, 180 * image.width_px + 250) == 0
      refute Map.has_key?(image, :logical_text)
      assert {:ok, ^image} = SvgDocumentImage.compose(document, 120, 90)

      assert {:error, {:invalid_document, diagnostic}} =
               SvgDocumentImage.compose(%{document | context: parent}, 120, 90)

      assert diagnostic.stage == :resource and diagnostic.operation == :rasterize_document
      assert ResourceContext.current() == parent
    end)
  end

  test "independent document fonts require explicit authorized bytes", %{root: root} do
    source =
      "<svg width='80' height='80'><rect width='80' height='80' fill='red'/><text x='12' y='52' font-family='DejaVu Sans' font-size='32' fill='white'>SG</text></svg>"

    File.write!(Path.join(root, "text.svg"), source)
    font = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:error, {:invalid_font, diagnostic}} =
               EmbeddedViewport.prepare_selected(
                 {:image, "text.svg", "image/svg+xml"},
                 parent,
                 [],
                 :object
               )

      assert diagnostic.stage == :font and
               diagnostic.message =~ "explicitly authorized independent"

      assert {:ok, {:image, %{image: document}}} =
               EmbeddedViewport.prepare_selected(
                 {:image, "text.svg", "image/svg+xml"},
                 parent,
                 [fonts: [%{family: "DejaVu Sans", data: font}]],
                 :object
               )

      assert {:ok, image} = SvgDocumentImage.compose(document, 60, 60)
      assert image.format == :png
      assert Enum.any?(for(<<r, g, b <- image.data>>, do: {r, g, b}), &(&1 == {255, 255, 255}))
    end)
  end

  test "the selected root keeps its font-relative viewport context", %{root: root} do
    File.write!(
      Path.join(root, "relative.svg"),
      "<svg style='font-size:20px;width:2em;height:1em'><rect width='100%' height='100%' fill='red'/></svg>"
    )

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, {:image, %{image: document}}} =
               EmbeddedViewport.prepare_selected(
                 {:image, "relative.svg", "image/svg+xml"},
                 parent,
                 [],
                 :object
               )

      assert {document.width, document.height} == {30.0, 15.0}
      assert {:ok, image} = SvgDocumentImage.compose(document, 60, 30)
      assert image.format == :png
    end)
  end
end
