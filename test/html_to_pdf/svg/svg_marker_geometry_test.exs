alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgMarkerGeometryTest do
  Code.require_file(Paths.support("svg_public_workflow.ex"))
  Code.require_file(Paths.support("png_fixture.ex"))
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "marker shorthand keeps canonical longhand precedence and context paint" do
    assert {:ok, {:fragment, "m"}} =
             NativeElixirPdfUtilities.Validators.SvgStyleValidator.parse("marker", "url(#m)")

    assert {:ok, "context-stroke"} =
             NativeElixirPdfUtilities.Validators.SvgStyleValidator.parse("fill", "CONTEXT-STROKE")

    assert :error =
             NativeElixirPdfUtilities.Validators.SvgStyleValidator.parse(
               "stop-color",
               "context-stroke"
             )

    source =
      "<style>.marked{marker:url(#m);marker-end:none}</style><svg width='100' height='60'><defs><marker id='m' markerUnits='userSpaceOnUse' markerWidth='8' markerHeight='8' refX='4' refY='4'><circle cx='4' cy='4' r='3' fill='context-stroke'/></marker></defs><path class='marked' d='M10 30H40V45' fill='none' stroke='#174a86'/></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, tree} = Style.compute_detailed(dom)
    svg = Enum.find(tree.children, &(&1[:tag] == "svg"))
    assert svg.style.svg_scene.source =~ "marker-start=\"url(#m)\""
    assert svg.style.svg_scene.source =~ "marker-mid=\"url(#m)\""
    assert svg.style.svg_scene.source =~ "marker-end=\"none\""
    assert svg.style.svg_scene.source =~ "fill=\"context-stroke\""
    assert {:ok, png_pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(source)
    refute png_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               png_pdf,
               false
             )
  end

  alias NativeElixirPdfUtilities.HtmlToPdf.{PngDecoder, HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator}
  alias NativeElixirPdfUtilities.{Limits}

  test "marker viewport meet letterboxing retains artwork in inline and image contexts" do
    source =
      "<svg width='100' height='60'><defs><marker id='m' markerUnits='userSpaceOnUse' markerWidth='40' markerHeight='20' refX='10' refY='10' viewBox='0 0 20 20'><rect x='-8' y='4' width='36' height='12' fill='#174a86'/></marker></defs><path d='M10 20H50' stroke='black' fill='none' marker-end='url(#m)'/></svg>"

    for mode <- [:inline, :image] do
      assert {:ok, png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
                 source,
                 [width: 100, height: 60],
                 %{mode: mode}
               )

      assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)

      for x <- ~c"\"2A" do
        assert binary_part(image.data, (20 * image.width_px + x) * 3, 3) == <<23, 74, 134>>
        assert :binary.at(image.alpha_data, 20 * image.width_px + x) == 255
      end
    end
  end

  test "marker reference percentages resolve against the referencing viewport canvas" do
    # Chromium resolves percentage refX/refY against the viewport canvas that
    # references the marker and then reads the point in the marker viewBox
    # system (verified with /usr/bin/chromium print-to-pdf at 96 dpi): on a
    # 100x60 canvas the 50% reference lands on the path end, so the circle
    # stays at its authored (10, 10), while 200 and 300 canvases offset the
    # reference point far enough to push the artwork out of the canvas. resvg
    # resolves the same way; the deleted painter resolved the percentages
    # against the marker viewport instead, which painted the circle onto the
    # path end for every width.
    for {width, reference} <- [{100, {10, 10}}, {200, nil}, {300, nil}] do
      source =
        "<svg width='#{width}' height='60'><defs><marker id='m' markerUnits='userSpaceOnUse' markerWidth='20' markerHeight='20' refX='50%' refY='50%' viewBox='0 0 20 20'><circle cx='10' cy='10' r='4' fill='#174a86'/></marker></defs><path d='M10 30H50' stroke='black' fill='none' marker-end='url(#m)'/></svg>"

      assert {:ok, png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
                 source,
                 [width: width, height: 60],
                 %{mode: :inline}
               )

      assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)

      case reference do
        {x, y} ->
          assert binary_part(image.data, (y * image.width_px + x) * 3, 3) == <<23, 74, 134>>
          assert :binary.at(image.alpha_data, y * image.width_px + x) == 255

        nil ->
          refute Enum.any?(
                   for <<r, g, b <- image.data>> do
                     {r, g, b}
                   end,
                   &(&1 == {23, 74, 134})
                 )
      end
    end
  end

  test "marker children retain the enclosing SVG viewport for percentages" do
    source =
      "<svg width='200' height='60'><defs><marker id='m' markerUnits='userSpaceOnUse' markerWidth='20' markerHeight='20' refX='50%' refY='50%' viewBox='0 0 20 20'><circle cx='50%' cy='50%' r='25%' fill='#174a86'/></marker></defs><path d='M10 30H50' stroke='black' fill='none' marker-end='url(#m)'/></svg>"

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               source,
               [width: 200, height: 60],
               %{mode: :inline}
             )

    assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)

    refute Enum.any?(
             for <<r, g, b <- image.data>> do
               {r, g, b}
             end,
             &(&1 == {23, 74, 134})
           )
  end

  test "zero marker dimensions and zero-area viewBox retain empty paint" do
    for geometry <- ["markerWidth='0'", "markerHeight='0'", "viewBox='0 0 0 20'"] do
      source =
        "<svg width='100' height='60'><defs><marker id='m' #{geometry}><rect width='8' height='8' fill='red'/></marker></defs><path d='M10 20H50' stroke='black' marker-end='url(#m)'/></svg>"

      assert {:ok, png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
                 source,
                 [width: 100, height: 60],
                 %{mode: :inline}
               )

      assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)

      refute Enum.any?(
               for <<r, g, b <- image.data>> do
                 {r, g, b}
               end,
               &(&1 == {255, 0, 0})
             )
    end
  end

  test "automatic marker directions use nonzero curve tangents and closed-join bisectors" do
    # Degenerate curve endpoints use the first nonzero derivative. Both markers
    # at a closed join use the incoming/outgoing direction bisector.
    for {path, position, inside, outside} <- [
          {"M50 30C50 30 50 60 90 30", "start", {50, 36}, {56, 30}},
          {"M10 30C70 30 50 30 50 30", "end", {44, 30}, {56, 30}},
          {"M50 30C80 10 85 55 80 60Z", "end", {50, 24}, {44, 30}},
          {"M50 30C80 10 85 55 80 60Z", "start", {50, 24}, {50, 36}}
        ] do
      source =
        "<svg width='110' height='90'><defs><marker id='m' markerUnits='userSpaceOnUse' markerWidth='24' markerHeight='24' refX='12' refY='12' orient='auto'><path d='M12 8L23 12L12 16Z' fill='#174a86'/></marker></defs><path d='#{path}' fill='none' stroke='black' marker-#{position}='url(#m)'/></svg>"

      assert {:ok, png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
                 source,
                 [width: 110, height: 90],
                 %{mode: :inline}
               )

      assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)
      {x, y} = inside
      assert binary_part(image.data, (y * image.width_px + x) * 3, 3) == <<23, 74, 134>>
      {x, y} = outside
      refute binary_part(image.data, (y * image.width_px + x) * 3, 3) == <<23, 74, 134>>
    end
  end
end
