defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentViewportTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedViewport, ResourceContext, SvgDocumentImage}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator}

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
               SvgDocumentImage.compose(Map.put(document, :source, "<svg/>"), 120, 90)

      assert {image.width_px, image.height_px} == {320, 240}
      assert pixel(image, 280, 180) == <<255, 0, 0>>
      assert pixel(image, 25, 25) == <<0, 255, 0>>
      assert :binary.at(image.alpha_data, 220 * 320 + 280) == 0
      assert :atomics.get(budget, 3) == 320 * 240 * 4
      assert {:ok, ^image} = SvgDocumentImage.compose(document, 120, 90)
      assert :atomics.get(budget, 2) == byte_size(source)
      assert :atomics.get(budget, 3) == 320 * 240 * 4
      assert ResourceContext.current() == parent

      assert {:error, {:invalid_document, diagnostic}} =
               SvgDocumentImage.compose(%{document | context: parent}, 120, 90)

      assert diagnostic.message =~ "issued child scope" and
               diagnostic.operation == :rasterize_document

      assert ResourceContext.current() == parent

      assert {:error, {:invalid_document, metadata}} =
               SvgDocumentImage.compose(%{document | width: 90}, 120, 90)

      assert metadata.message =~ "intrinsic dimensions"

      assert {:error, {:invalid_document, budget_error}} =
               prepare_document(source, HtmlValidator.new_image_budget())

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
                   prepare_document(source, ResourceContext.current().image_budget)

          assert document.width == 75.0 and document.height == 37.5
          assert {:ok, image} = SvgDocumentImage.compose(document, 120, 90)
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

        assert {:ok, document} = prepare_document(source, context.image_budget)
        assert document.width == 225.0 and document.height == 112.5
        assert {:ok, image} = SvgDocumentImage.compose(document, 120, 90)
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

        assert {:ok, document} = prepare_document(source, context.image_budget)
        assert document.width == 225.0 and document.height == 37.5
        assert {:ok, image} = SvgDocumentImage.compose(document, 120, 90)
        assert pixel(image, 280, 180) == <<255, 0, 0>>
      end
    )
  end

  test "exact selected root dimensions depend on document host before native parsing", %{
    root: root
  } do
    fixtures = Path.expand("../fixtures/html_to_pdf/browser_parity/assets", __DIR__)

    for {file, used_width, used_height} <- [
          {"svg-initial-absolute.svg", 100, 50},
          {"svg-initial-relative.svg", 80, 120},
          {"svg-initial-css.svg", 120, 60},
          {"svg-initial-percent-shape.svg", 100, 50}
        ] do
      source = File.read!(Path.join(fixtures, file))

      ResourceContext.run(
        [authorization_root: root],
        HtmlValidator.new_image_budget(),
        fn context ->
          assert {:ok, iframe} =
                   prepare_document(source, context.image_budget, file, :iframe)

          assert {:ok, object} =
                   prepare_document(source, context.image_budget, file, :object)

          assert {:ok, frame_image} = SvgDocumentImage.compose(iframe, 120, 90)
          assert {:ok, object_image} = SvgDocumentImage.compose(object, 120, 90)
          assert {frame_image.width_px, frame_image.height_px} == {320, 240}

          assert :binary.at(frame_image.alpha_data, (used_height * 2 - 1) * 320 + used_width * 2) ==
                   0

          if used_height < 120,
            do:
              assert(
                :binary.at(
                  frame_image.alpha_data,
                  used_height * 2 * 320 + max(used_width * 2 - 1, 0)
                ) == 0
              )

          if file == "svg-initial-percent-shape.svg" do
            assert pixel(frame_image, 150, 90) == <<216, 74, 58>>
            assert :binary.at(frame_image.alpha_data, 90 * 320 + 250) == 0
            assert pixel(object_image, 250, 200) == <<216, 74, 58>>
          end

          assert ResourceContext.current() == context
        end
      )
    end
  end

  test "completed original percentage and inline CSS sources use actual replaced canvas", %{
    root: root
  } do
    for source <- [
          "<svg width='100' height='50'><rect width='100%' height='100%'/></svg>",
          "<svg width='50%' height='50' viewBox='0 0 10 10'/>",
          "<svg width='100' height='50' style='width:200px' viewBox='0 0 10 10'/>"
        ] do
      ResourceContext.run(
        [authorization_root: root],
        HtmlValidator.new_image_budget(),
        fn context ->
          assert {:ok, document} = prepare_document(source, context.image_budget)
          assert {:ok, image} = SvgDocumentImage.compose(document, 120, 90)
          assert {image.width_px, image.height_px} == {320, 240}
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
        assert {:ok, document} = prepare_document(source, context.image_budget)
        assert {:ok, _image} = SvgDocumentImage.compose(document, 120, 90)

        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 SvgDocumentImage.compose(document, 60, 45)

        assert diagnostic.message =~ "aggregate decoded image" and diagnostic.stage == :limits
        assert ResourceContext.current() == context
      end
    )

    Limits.install(Map.put(Limits.defaults(), :max_svg_raster_dimension, 100))

    ResourceContext.run(
      [authorization_root: root],
      HtmlValidator.new_image_budget(),
      fn context ->
        assert {:ok, document} = prepare_document(source, context.image_budget)

        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 SvgDocumentImage.compose(document, 120, 90)

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

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<object data='bad.svg' style='width:0;height:40pt'>Unused</object>",
               base_url: root
             )

    assert diagnostic.stage == :html and diagnostic.message =~ "</svg>"

    Limits.install(Map.put(Limits.effective(), :max_aggregate_image_source_bytes, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<object data='valid.svg' style='width:0;height:40pt'>Unused</object>",
               base_url: root
             )

    assert diagnostic.message =~ "read limit" and Path.basename(diagnostic.source) == "valid.svg"
  end

  test "zero hosts and roots skip paint after validating the issued source", %{root: root} do
    Limits.install(Map.put(Limits.effective(), :max_aggregate_decoded_image_bytes, 1))

    for {source, host, width, height} <- [
          {"<svg width='100' height='50'><rect width='100%' height='100%'/></svg>", :object, 0,
           90},
          {"<svg width='100' height='50'><rect width='100%' height='100%'/></svg>", :iframe, 120,
           0},
          {"<svg width='0' height='50'><rect width='100' height='50'/></svg>", :iframe, 120, 90},
          {"<svg width='100' height='0'><rect width='100' height='50'/></svg>", :iframe, 120, 90},
          {"<svg width='100' height='50' viewBox='0 0 0 50'><rect width='100' height='50'/></svg>",
           :object, 120, 90}
        ] do
      ResourceContext.run(
        [authorization_root: root],
        HtmlValidator.new_image_budget(),
        fn context ->
          assert {:ok, document} =
                   prepare_document(source, context.image_budget, "original.svg", host)

          assert {:ok, :skipped} = SvgDocumentImage.compose(document, width, height)
          assert {:ok, :skipped} = SvgDocumentImage.compose(document, width, height)
          assert :atomics.get(context.image_budget, 3) == 0
          assert ResourceContext.current() == context

          assert {:error, {:invalid_document, diagnostic}} =
                   SvgDocumentImage.compose(
                     %{document | document_source: "substitute.svg"},
                     width,
                     height
                   )

          assert diagnostic.message =~ "issued child scope"

          for dimensions <- [{-1, height}, {:invalid, height}] do
            assert {:error, {:invalid_layout, diagnostic}} =
                     SvgDocumentImage.compose(document, elem(dimensions, 0), elem(dimensions, 1))

            assert diagnostic.operation == :layout_viewport
            assert diagnostic.stage == :layout
          end
        end
      )
    end
  end

  test "skipped iframe child has an empty clipped group without a raster", %{root: root} do
    File.write!(
      Path.join(root, "zero.svg"),
      "<svg width='0' height='50'><rect width='100' height='50'/></svg>"
    )

    ResourceContext.run(
      [authorization_root: root],
      HtmlValidator.new_image_budget(),
      fn context ->
        assert {:ok, surface} = EmbeddedViewport.prepare_iframe_document("zero.svg", context)
        assert {:ok, group} = EmbeddedViewport.layout(surface, 120, 90)
        assert group.role == :iframe_viewport
        assert group.boxes == []
        assert :atomics.get(context.image_budget, 2) > 0
        assert :atomics.get(context.image_budget, 3) == 0
        assert ResourceContext.current() == context
      end
    )
  end

  test "skipped roots retain source and reference failures", %{root: root} do
    for {source, message} <- [
          {"<svg width='0' height='50'><rect width='bad' height='50'/></svg>", "geometry length"},
          {"<svg width='0' height='50'><use id='cycle' href='#cycle'/></svg>", "cycle"},
          {"<svg width='0' height='50'><image href='https://denied.test/a.png'/></svg>",
           "not authorized"},
          {"<svg width='0' height='50'><rect></svg>", "malformed"}
        ] do
      ResourceContext.run(
        [authorization_root: root],
        HtmlValidator.new_image_budget(),
        fn context ->
          assert {:error, {reason, diagnostic}} =
                   prepare_document(
                     source,
                     context.image_budget,
                     "skipped.svg",
                     :iframe
                   )

          assert is_binary(diagnostic.source)
          assert reason in [:invalid_document, :invalid_css, :unsupported_html]
          assert diagnostic.reason == reason
          assert diagnostic.message != ""
          assert is_binary(message)
          assert :atomics.get(context.image_budget, 3) == 0
        end
      )
    end

    source = "<svg width='0' height='50'><rect width='100' height='50'/></svg>"

    Limits.install(
      Map.put(Limits.effective(), :max_aggregate_svg_source_bytes, byte_size(source) - 1)
    )

    ResourceContext.run(
      [authorization_root: root],
      HtmlValidator.new_image_budget(),
      fn context ->
        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 prepare_document(source, context.image_budget, "skipped.svg", :iframe)

        assert diagnostic.message =~ "max_aggregate_svg_source_bytes"
        assert diagnostic.source == "SVG"
        assert :atomics.get(context.image_budget, 3) == 0
      end
    )
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

  defp prepare_document(source, budget, origin \\ "SVG", host \\ :object, fonts \\ []) do
    SvgDocumentImage.prepare(source, budget, origin, host, fonts, [])
  end
end
