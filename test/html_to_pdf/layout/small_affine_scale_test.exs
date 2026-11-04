defmodule NativeElixirPdfUtilities.HtmlToPdf.SmallAffineScaleTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, PdfWriter}

  test "local and cumulative matrices preserve small linear coefficients" do
    for scale <- [0.00001, -0.00001, 0.00006, 1.0e-12] do
      group = %{width: 1_000_000, height: 10, transform: {scale, 0, 0, 1, 0, 0}}
      assert DrawingGeometry.local_group_transform(group, nil) == {scale, 0, 0, 1, 0, 0}
      composition = DrawingGeometry.compose_group(group, nil)
      {matrix, nil, 10} = DrawingGeometry.composition_parts(composition)
      {x, y} = DrawingGeometry.transform_point(1_000_000, 5, matrix)
      assert_in_delta x, 1_000_000 * scale, 1.0e-12
      assert y == 5
    end
  end

  test "PDF matrix serialization preserves small scales without exponent notation" do
    rectangle = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 1_000_000,
      height: 10,
      fill_color: {1, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0
    }

    for {scale, encoded} <- [
          {0.00001, "0.00001"},
          {0.00006, "0.00006"},
          {1.0e-12, "0.000000000001"}
        ] do
      group = %{
        type: :drawing_group,
        width: 1_000_000,
        height: 10,
        transform: {scale, 0, 0, 1, 0, 0},
        boxes: [rectangle]
      }

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {100, 100}, boxes: [group]}], compress_streams: false)

      assert pdf =~ "q #{encoded}"
    end
  end

  test "CSS retains small nonzero matrices in PDF while zero scale omits them" do
    for {scale, visible?} <- [{"0.00001", true}, {"0", false}] do
      html = """
      <style>html,body{margin:0}.frame{position:absolute;left:8px;top:8px;width:80px;height:50px}
      .paint{width:1000000px;height:10px;background:red;transform-origin:0 0;transform:scaleX(#{scale})}</style>
      <div class="frame"><div class="paint"></div></div>
      """

      assert {:ok, pdf} =
               HtmlToPdf.render(html, page_size: {100, 100}, margin: 0, compress_streams: false)

      assert Regex.match?(~r/0\.000010? 0 0 1/, pdf) == visible?
    end
  end
end
