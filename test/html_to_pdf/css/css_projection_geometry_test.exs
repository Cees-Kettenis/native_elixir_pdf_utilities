defmodule NativeElixirPdfUtilities.HtmlToPdf.CssProjectionGeometryTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.ProjectionGeometry, as: G
  alias NativeElixirPdfUtilities.Validators.CssProjectionValidator

  test "homogeneous analytic vectors retain column-major CSS matrix input and operation order" do
    matrix = G.operation("matrix3d", [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 10, 20, 30, 1])
    assert G.point(matrix, 1, 2, 3) == {11.0, 22.0, 33.0, 1.0}
    matrix = G.multiply(G.operation("perspective", [100]), G.operation("translate3d", [0, 0, 50]))
    assert G.point(matrix, 10, 20) == {10.0, 20.0, 50.0, 0.5}
    assert {u, v, depth} = G.source_point(matrix, G.inverse_plane(matrix), 20, 40)
    assert_in_delta u, 10, 0.000001
    assert_in_delta v, 20, 0.000001
    assert_in_delta depth, 100, 0.000001
    assert G.inverse_plane(G.operation("scale3d", [0, 1, 1])) == :empty
    assert G.backface?(G.operation("rotatey", [:math.pi()]))
    refute G.backface?(G.identity())
  end

  test "eye-plane and page clipping stay finite when a source crosses the projection singularity" do
    matrix =
      G.multiply(G.operation("perspective", [10]), G.operation("rotatey", [:math.pi() / 3]))

    polygon = G.polygon(matrix, %{x: -20.0, y: -20.0, width: 40.0, height: 40.0}, {100, 100})
    assert length(polygon) >= 3

    assert Enum.all?(polygon, fn {x, y} ->
             x >= -0.000001 and x <= 100.000001 and y >= -0.000001 and y <= 100.000001
           end)

    assert G.polygon(
             G.operation("translate3d", [0, 0, 20])
             |> then(&G.multiply(G.operation("perspective", [10]), &1)),
             %{x: 0, y: 0, width: 10, height: 10},
             {100, 100}
           ) == []

    assert {:error, {:invalid_layout, diagnostic}} =
             CssProjectionValidator.validate_matrix({1, 2})

    assert diagnostic.source == "CSS 3D transform"
  end
end
