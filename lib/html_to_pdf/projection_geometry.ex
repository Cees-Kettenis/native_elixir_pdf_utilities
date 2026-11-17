defmodule NativeElixirPdfUtilities.HtmlToPdf.ProjectionGeometry do
  @moduledoc "Homogeneous static CSS transforms and finite page projection."
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  @identity {1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0}

  @doc "Returns the row-major homogeneous identity matrix."
  @spec identity() :: tuple()
  def identity, do: @identity

  @doc "Multiplies two admitted row-major matrices, applying the right matrix first."
  @spec multiply(tuple(), tuple()) :: tuple()
  def multiply(left, right) do
    for row <- 0..3, col <- 0..3 do
      Enum.reduce(0..3, 0.0, fn k, sum ->
        sum + elem(left, row * 4 + k) * elem(right, k * 4 + col)
      end)
    end
    |> List.to_tuple()
  end

  @doc "Embeds an admitted native six-value affine matrix in homogeneous geometry."
  @spec affine(tuple()) :: tuple()
  def affine({a, b, c, d, e, f}),
    do: {a, c, 0.0, e, b, d, 0.0, f, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0}

  @doc "Builds a canonical CSS transform operation in CSS pixel coordinates."
  @spec operation(binary(), [number()]) :: tuple()
  def operation(name, values) do
    case {name, values} do
      {"matrix", [a, b, c, d, e, f]} ->
        affine({a, b, c, d, e, f})

      {"matrix3d", values} ->
        values = List.to_tuple(values)

        List.to_tuple(for row <- 0..3, col <- 0..3, do: elem(values, col * 4 + row))

      {"translate", [x]} ->
        operation("translate3d", [x, 0, 0])

      {"translate", [x, y]} ->
        operation("translate3d", [x, y, 0])

      {"translatex", [x]} ->
        operation("translate3d", [x, 0, 0])

      {"translatey", [y]} ->
        operation("translate3d", [0, y, 0])

      {"translatez", [z]} ->
        operation("translate3d", [0, 0, z])

      {"translate3d", [x, y, z]} ->
        {1.0, 0.0, 0.0, x, 0.0, 1.0, 0.0, y, 0.0, 0.0, 1.0, z, 0.0, 0.0, 0.0, 1.0}

      {"scale", [s]} ->
        operation("scale3d", [s, s, 1])

      {"scale", [x, y]} ->
        operation("scale3d", [x, y, 1])

      {"scalex", [x]} ->
        operation("scale3d", [x, 1, 1])

      {"scaley", [y]} ->
        operation("scale3d", [1, y, 1])

      {"scalez", [z]} ->
        operation("scale3d", [1, 1, z])

      {"scale3d", [x, y, z]} ->
        {x, 0.0, 0.0, 0.0, 0.0, y, 0.0, 0.0, 0.0, 0.0, z, 0.0, 0.0, 0.0, 0.0, 1.0}

      {name, [angle]} when name in ["rotate", "rotatez"] ->
        operation("rotate3d", [0, 0, 1, angle])

      {"rotatex", [angle]} ->
        operation("rotate3d", [1, 0, 0, angle])

      {"rotatey", [angle]} ->
        operation("rotate3d", [0, 1, 0, angle])

      {"rotate3d", [x, y, z, angle]} ->
        length = :math.sqrt(x * x + y * y + z * z)

        if length == 0 do
          @identity
        else
          x = x / length
          y = y / length
          z = z / length
          c = :math.cos(angle)
          s = :math.sin(angle)
          t = 1 - c

          {t * x * x + c, t * x * y - s * z, t * x * z + s * y, 0.0, t * x * y + s * z,
           t * y * y + c, t * y * z - s * x, 0.0, t * x * z - s * y, t * y * z + s * x,
           t * z * z + c, 0.0, 0.0, 0.0, 0.0, 1.0}
        end

      {"perspective", [distance]} ->
        put_elem(@identity, 14, -1 / distance)

      {"skew", [x]} ->
        operation("skew", [x, 0])

      {"skew", [x, y]} ->
        affine({1.0, :math.tan(y), :math.tan(x), 1.0, 0.0, 0.0})

      {"skewx", [x]} ->
        operation("skew", [x, 0])

      {"skewy", [y]} ->
        operation("skew", [0, y])
    end
  end

  @doc "Changes CSS pixel/downward-y coordinates into native PDF points/upward-y coordinates."
  @spec css_to_pdf(tuple()) :: tuple()
  def css_to_pdf(matrix),
    do:
      multiply(
        multiply(operation("scale3d", [0.75, -0.75, 0.75]), matrix),
        operation("scale3d", [1 / 0.75, -1 / 0.75, 1 / 0.75])
      )

  @doc "Applies an admitted matrix around a measured three-dimensional origin."
  @spec around(tuple(), number(), number(), number()) :: tuple()
  def around(matrix, x, y, z),
    do:
      multiply(
        multiply(operation("translate3d", [x, y, z]), matrix),
        operation("translate3d", [-x, -y, -z])
      )

  @doc "Transforms one admitted point, retaining the homogeneous denominator."
  @spec point(tuple(), number(), number()) :: tuple()
  @spec point(tuple(), number(), number(), number()) :: tuple()
  def point(matrix, x, y, z \\ 0.0),
    do:
      for(
        row <- 0..3,
        do:
          elem(matrix, row * 4) * x + elem(matrix, row * 4 + 1) * y +
            elem(matrix, row * 4 + 2) * z + elem(matrix, row * 4 + 3)
      )
      |> List.to_tuple()

  @doc "Composes a validated native group and its separate child camera/flattening boundary in one homogeneous context."
  @spec group_matrices(tuple(), map(), number()) :: {tuple(), tuple()}
  def group_matrices(parent, box, height) do
    local =
      box
      |> NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.local_group_transform(height)
      |> affine()

    projection =
      Map.get(box, :css_projection, %{matrix: @identity, perspective: @identity, preserve: false})

    own = multiply(multiply(parent, local), projection.matrix)
    flatten = if projection.preserve, do: @identity, else: put_elem(@identity, 10, 0.0)
    {own, multiply(multiply(own, flatten), projection.perspective)}
  end

  @doc "Returns the inverse planar homography or an empty projection for a singular plane."
  @spec inverse_plane(tuple()) :: tuple() | :empty
  def inverse_plane(matrix) do
    [a, b, c, d, e, f, g, h, i] = Enum.map([0, 1, 3, 4, 5, 7, 12, 13, 15], &elem(matrix, &1))
    determinant = a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)

    if determinant == 0 do
      :empty
    else
      {(e * i - f * h) / determinant, (c * h - b * i) / determinant,
       (b * f - c * e) / determinant, (f * g - d * i) / determinant,
       (a * i - c * g) / determinant, (c * d - a * f) / determinant,
       (d * h - e * g) / determinant, (b * g - a * h) / determinant,
       (a * e - b * d) / determinant}
    end
  end

  @doc "Maps a page point back to an admitted source plane, retaining the local projected depth."
  @spec source_point(tuple(), tuple(), number(), number()) :: tuple() | :hidden
  def source_point(matrix, inverse, x, y) do
    {a, b, c, d, e, f, g, h, i} = inverse
    w = g * x + h * y + i

    if w == 0 do
      :hidden
    else
      u = (a * x + b * y + c) / w
      v = (d * x + e * y + f) / w
      {_, _, z, w} = point(matrix, u, v)
      if w > 0, do: {u, v, z / w}, else: :hidden
    end
  end

  @doc "Clips an admitted plane to the positive eye half-space and finite physical page before perspective division."
  @spec polygon(tuple(), map(), tuple()) :: [tuple()]
  def polygon(matrix, bounds, viewport) do
    {left, bottom, right, top} =
      case viewport do
        {width, height} -> {0, 0, width, height}
        bounds -> bounds
      end

    project_polygon(
      matrix,
      [
        {bounds.x, bounds.y},
        {bounds.x + bounds.width, bounds.y},
        {bounds.x + bounds.width, bounds.y + bounds.height},
        {bounds.x, bounds.y + bounds.height}
      ],
      {left, bottom, right, top}
    )
  end

  @doc "Projects an admitted source polygon through the eye plane and finite viewport."
  @spec project_polygon(tuple(), [tuple()], tuple()) :: [tuple()]
  def project_polygon(matrix, polygon, {left, bottom, right, top}) do
    points = Enum.map(polygon, fn {x, y} -> point(matrix, x, y) end)

    points =
      Enum.reduce(
        [{0, 0, 0, 1}, {1, 0, 0, -left}, {-1, 0, 0, right}, {0, 1, 0, -bottom}, {0, -1, 0, top}],
        points,
        fn plane, points ->
          HtmlValidator.reserve_render_resource(:max_work, length(points) + 1, :layout)

          case points do
            [] ->
              []

            _ ->
              Enum.zip(points, tl(points) ++ [hd(points)])
              |> Enum.flat_map(fn {first, last} ->
                distance = fn point ->
                  Enum.reduce(0..3, 0.0, fn i, n -> n + elem(point, i) * elem(plane, i) end)
                end

                a = distance.(first)
                b = distance.(last)

                intersection = fn ->
                  fraction = a / (a - b)

                  for(i <- 0..3, do: elem(first, i) + (elem(last, i) - elem(first, i)) * fraction)
                  |> List.to_tuple()
                end

                cond do
                  a >= 0 and b >= 0 -> [last]
                  a >= 0 -> [intersection.()]
                  b >= 0 -> [intersection.(), last]
                  true -> []
                end
              end)
          end
        end
      )

    Enum.flat_map(points, fn {x, y, _z, w} -> if w > 0, do: [{x / w, y / w}], else: [] end)
    |> Enum.dedup()
  end

  @doc "Identifies a back-facing plane from the inverse accumulated CSS matrix."
  @spec backface?(tuple()) :: boolean()
  def backface?(matrix) do
    minor = fn excluded_row, excluded_col ->
      [a, b, c, d, e, f, g, h, i] =
        for row <- 0..3,
            col <- 0..3,
            row != excluded_row,
            col != excluded_col,
            do: elem(matrix, row * 4 + col)

      a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
    end

    determinant =
      Enum.reduce(0..3, 0.0, fn col, total ->
        total + elem(matrix, col) * minor.(0, col) * if(rem(col, 2) == 0, do: 1, else: -1)
      end)

    determinant != 0 and minor.(2, 2) / determinant < 0
  end

  @doc "Returns an equivalent native affine placement when the plane has a constant positive denominator."
  @spec planar_affine(tuple()) :: tuple() | nil
  def planar_affine(matrix) do
    w = elem(matrix, 15)

    if elem(matrix, 12) == 0 and elem(matrix, 13) == 0 and w > 0,
      do:
        {elem(matrix, 0) / w, elem(matrix, 4) / w, elem(matrix, 1) / w, elem(matrix, 5) / w,
         elem(matrix, 3) / w, elem(matrix, 7) / w}
  end
end
