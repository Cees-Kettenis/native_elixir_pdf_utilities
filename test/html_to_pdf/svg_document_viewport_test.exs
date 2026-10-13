defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentViewportTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedViewport, ResourceContext, SvgBackend}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgTransformValidator}

  setup do
    root =
      Path.join(System.tmp_dir!(), "native-svg-document-#{System.unique_integer([:positive])}")

    File.mkdir_p!(root)
    limits = Limits.effective()

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(root)
    end)

    %{root: root}
  end

  test "no viewBox preserves absolute user geometry beyond the original root canvas", %{
    root: root
  } do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='100' height='50'><rect width='200' height='100' fill='red'/><rect x='10' y='10' width='10' height='10' fill='lime'/></svg>"

    File.write!(Path.join(root, "child.svg"), source)
    budget = HtmlValidator.new_image_budget()

    ResourceContext.run([authorization_root: root], budget, fn parent ->
      assert {:ok, {:image, %{image: document}}} =
               EmbeddedViewport.prepare_selected(
                 {:image, "child.svg", "image/svg+xml"},
                 parent,
                 [],
                 :object
               )

      assert document.width == 75.0 and document.height == 37.5
      assert :atomics.get(budget, 2) == byte_size(source)
      assert :atomics.get(budget, 3) == 0
      assert document.context.document_id != parent.document_id
      assert document.context.authorization_root == parent.authorization_root
      assert document.context.image_budget == budget
      assert ResourceContext.current() == parent

      # Later file writes and caller-authored descriptor keys cannot substitute
      # the already authorized bytes held by the issued request.
      File.write!(Path.join(root, "child.svg"), String.replace(source, "red", "blue"))

      assert {:ok, image} =
               SvgBackend.paint_document(Map.put(document, :source, "<svg/>"), 120, 90)

      assert {image.width_px, image.height_px} == {320, 240}
      assert pixel(image, 280, 180) == <<255, 0, 0>>
      assert pixel(image, 25, 25) == <<0, 255, 0>>
      assert :binary.at(image.alpha_data, 220 * 320 + 280) == 0
      assert :atomics.get(budget, 3) == 320 * 240 * 4
      assert {:ok, ^image} = SvgBackend.paint_document(document, 120, 90)
      assert :atomics.get(budget, 2) == byte_size(source)
      assert :atomics.get(budget, 3) == 320 * 240 * 4
      assert ResourceContext.current() == parent

      assert {:error, {:invalid_document, diagnostic}} =
               SvgBackend.paint_document(%{document | context: parent}, 120, 90)

      assert diagnostic.message =~ "not issued" and diagnostic.operation == :rasterize_document
      assert ResourceContext.current() == parent

      assert {:error, {:invalid_document, metadata}} =
               SvgBackend.paint_document(%{document | width: 90}, 120, 90)

      assert metadata.message =~ "canonical intrinsic dimensions"

      assert {:error, {:invalid_document, budget_error}} =
               SvgBackend.prepare_document(source, HtmlValidator.new_image_budget())

      assert budget_error.message =~ "shared image budget"
    end)
  end

  test "differing root intrinsic and viewBox aspects use the actual child viewport", %{root: root} do
    for {aspect, left, top, outside} <- [
          {"xMidYMid meet", 40, 0, true},
          {"xMinYMax meet", 0, 0, false},
          {"none", 0, 0, false},
          {"xMaxYMin slice", 0, 0, false}
        ] do
      source =
        "<svg width='100' height='50' viewBox='0 0 100 100' preserveAspectRatio='#{aspect}'><rect width='100' height='100' fill='red'/></svg>"

      ResourceContext.run(
        [authorization_root: root],
        HtmlValidator.new_image_budget(),
        fn _parent ->
          assert {:ok, document} =
                   SvgBackend.prepare_document(source, ResourceContext.current().image_budget)

          assert document.width == 75.0 and document.height == 37.5
          assert {:ok, image} = SvgBackend.paint_document(document, 120, 90)
          assert pixel(image, left + 20, top + 20) == <<255, 0, 0>>
          assert is_map(image)
          if outside, do: assert(:binary.at(image.alpha_data, 20 * 320 + 10) == 0)
        end
      )
    end
  end

  test "unsized roots and authored transforms retain CSS-unit geometry", %{root: root} do
    ResourceContext.run(
      [authorization_root: root],
      HtmlValidator.new_image_budget(),
      fn context ->
        source =
          "<svg transform='translate(10 5)'><rect width='100' height='50' fill='red'/></svg>"

        assert {:ok, document} = SvgBackend.prepare_document(source, context.image_budget)
        assert document.width == 225.0 and document.height == 112.5
        assert {:ok, image} = SvgBackend.paint_document(document, 120, 90)
        assert pixel(image, 40, 25) == <<255, 0, 0>>
        assert :binary.at(image.alpha_data, 2 * 320 + 2) == 0
        assert :binary.at(image.alpha_data, 140 * 320 + 300) == 0
      end
    )
  end

  test "root XML attribute spelling and namespaces match the unchanged native source", %{
    root: root
  } do
    ResourceContext.run(
      [authorization_root: root],
      HtmlValidator.new_image_budget(),
      fn context ->
        source =
          "<svg xmlns:q='urn:ignored' WIDTH='100' q:width='90' height='50' x='40' y='20' viewbox='0 0 100 100'><rect width='200' height='100' fill='red'/></svg>"

        assert {:ok, document} = SvgBackend.prepare_document(source, context.image_budget)
        assert document.width == 225.0 and document.height == 37.5
        assert {:ok, image} = SvgBackend.paint_document(document, 120, 90)
        assert pixel(image, 280, 180) == <<255, 0, 0>>
      end
    )
  end

  test "unresolved percentages, root CSS sizing and selected-image omissions fail before raster allocation",
       %{root: root} do
    for {source, reason, message} <- [
          {"<svg width='100' height='50'><rect width='100%' height='100%'/></svg>",
           :not_implemented, "viewport-relative"},
          {"<svg width='50%' height='50' viewBox='0 0 10 10'/>", :not_implemented,
           "root percentage"},
          {"<svg width='100' height='50' style='width:200px' viewBox='0 0 10 10'/>",
           :not_implemented, "CSS viewport"},
          {"<svg width='100' height='50'><svg><rect width='10' height='10'/></svg></svg>",
           :not_implemented, "nested viewports"},
          {"<svg width='100' height='50'><foreignObject width='10' height='10'/></svg>",
           :unsupported_html, "foreignObject"},
          {"<svg width='100' height='50'><text>Missing font</text></svg>", :unsupported_html,
           "font"},
          {"<svg width='100' height='50'><image href='https://denied.test/a.png'/></svg>",
           :invalid_document, "not authorized"},
          {"<svg width='100' height='50'><use href='#loop' id='loop'/></svg>", :invalid_document,
           "cycle"},
          {"<svg width='100' height='50'><rect width='bad' height='10'/></svg>",
           :invalid_document, "geometry length"},
          {"<svg width='100' height='50' transform='translate(bad)'/>", :invalid_document,
           "transform"},
          {"<svg><rect></svg>", :invalid_document, "malformed"}
        ] do
      ResourceContext.run(
        [authorization_root: root],
        HtmlValidator.new_image_budget(),
        fn context ->
          assert {:error, {^reason, diagnostic}} =
                   SvgBackend.prepare_document(source, context.image_budget)

          assert diagnostic.reason == reason and is_atom(diagnostic.stage)
          assert diagnostic.message =~ message
          assert diagnostic.source != nil
          assert :atomics.get(context.image_budget, 3) == 0
        end
      )
    end
  end

  test "actual raster ceilings are shared across differently sized allocations", %{root: root} do
    source = "<svg width='100' height='50'><rect width='200' height='100' fill='red'/></svg>"
    Limits.install(Map.put(Limits.effective(), :max_aggregate_decoded_image_bytes, 320 * 240 * 4))

    ResourceContext.run(
      [authorization_root: root],
      HtmlValidator.new_image_budget(),
      fn context ->
        assert {:ok, document} = SvgBackend.prepare_document(source, context.image_budget)
        assert {:ok, _image} = SvgBackend.paint_document(document, 120, 90)

        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 SvgBackend.paint_document(document, 60, 45)

        assert diagnostic.message =~ "aggregate decoded image" and diagnostic.stage == :limits
        assert ResourceContext.current() == context
      end
    )

    Limits.install(Map.put(Limits.defaults(), :max_svg_raster_dimension, 100))

    ResourceContext.run(
      [authorization_root: root],
      HtmlValidator.new_image_budget(),
      fn context ->
        assert {:ok, document} = SvgBackend.prepare_document(source, context.image_budget)

        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 SvgBackend.paint_document(document, 120, 90)

        assert diagnostic.message =~ "per-axis" and :atomics.get(context.image_budget, 3) == 0
      end
    )
  end

  test "zero hosts retain preparation failures and paint no image or link rectangle", %{
    root: root
  } do
    File.write!(
      Path.join(root, "valid.svg"),
      "<svg width='100' height='50'><rect width='200' height='100'/></svg>"
    )

    File.write!(Path.join(root, "bad.svg"), "<svg><rect></svg>")

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<a href='https://example.test/'><object data='valid.svg' style='width:0;height:40pt'>Unused</object></a>",
               base_url: root
             )

    refute pdf =~ "/Subtype /Image"
    refute pdf =~ "/Subtype /Link"

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render(
               "<object data='bad.svg' style='width:0;height:40pt'>Unused</object>",
               base_url: root
             )

    assert diagnostic.message =~ "malformed" and diagnostic.source == "bad.svg"

    Limits.install(Map.put(Limits.effective(), :max_aggregate_image_source_bytes, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<object data='valid.svg' style='width:0;height:40pt'>Unused</object>",
               base_url: root
             )

    assert diagnostic.message =~ "read limit" and Path.basename(diagnostic.source) == "valid.svg"
  end

  test "matrix inverse rejects singular and unbounded numeric inputs" do
    matrix = {2.0, 0.0, 0.0, 4.0, 10.0, 20.0}
    assert {:ok, inverse} = SvgTransformValidator.inverse(matrix)
    assert SvgTransformValidator.multiply(matrix, inverse) == {1.0, 0.0, 0.0, 1.0, 0.0, 0.0}
    assert :error = SvgTransformValidator.inverse({0.0, 0.0, 0.0, 1.0, 0.0, 0.0})
    assert :error = SvgTransformValidator.inverse({1.0e-300, 0.0, 0.0, 1.0, 0.0, 0.0})
    assert :error = SvgTransformValidator.inverse({:invalid, 0, 0, 1, 0, 0})
  end

  test "actual canvas validation returns recoverable diagnostics before invalid allocations" do
    alias NativeElixirPdfUtilities.Validators.SvgValidator

    for {width, height} <- [{0, 1}, {1, -1}, {1.5, 1}, {:invalid, 1}] do
      assert {:error, {:invalid_document, diagnostic}} =
               SvgValidator.validate_raster_dimensions(width, height, nil)

      assert diagnostic.source == "SVG" and diagnostic.message =~ "positive integers"
    end

    assert {:error, {:invalid_document, diagnostic}} =
             SvgValidator.validate_raster_dimensions(1, 1, :invalid)

    assert diagnostic.message =~ "budget"
    Limits.install(Map.put(Limits.effective(), :max_aggregate_svg_raster_pixels, 3))

    assert :callback_continued =
             HtmlValidator.with_render_budget(fn ->
               assert {:error, {:resource_limit_exceeded, diagnostic}} =
                        SvgValidator.validate_raster_dimensions(2, 2, nil)

               assert diagnostic.operation == :rasterize and
                        diagnostic.message =~ "max_aggregate_svg_raster_pixels"

               :callback_continued
             end)
  end

  defp pixel(image, x, y) do
    binary_part(image.data, (y * image.width_px + x) * 3, 3)
  end
end
