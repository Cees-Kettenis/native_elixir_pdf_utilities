defmodule NativeElixirPdfUtilities.Validators.CssProjectionValidator do
  @moduledoc "Static 3D property semantics, finite homogeneous matrices and scene admission."
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.ProjectionGeometry

  alias NativeElixirPdfUtilities.Validators.{
    CssTransformValidator,
    CssLengthValidator,
    CssPositionValidator,
    CssValueValidator,
    CssClipValidator,
    CssMaskValidator,
    HtmlValidator
  }

  @functions ~w(matrix3d translate3d translatez scale3d scalez rotatex rotatey rotatez rotate3d perspective)

  @doc "Prepares perspective, origins, flattening and backface properties through canonical length/position grammar."
  @spec prepare(map(), binary(), binary()) :: {:ok, map()} | :error | {:error, term()}
  def prepare(style, property, source) do
    source = String.trim(source)

    case property do
      "perspective" ->
        if String.downcase(source, :ascii) == "none" do
          {:ok, Map.put(style, :css_perspective, :none)}
        else
          with {:ok, length} <- CssLengthValidator.prepare(source, :length),
               do: {:ok, Map.put(style, :css_perspective, length)}
        end

      "perspective-origin" ->
        with {:ok, position} <-
               CssValueValidator.parse_two_axis_position(
                 source,
                 Map.get(style, :font_size, 12),
                 false
               ),
             do: {:ok, Map.put(style, :css_perspective_origin, position)}

      "transform-style" ->
        case String.downcase(source, :ascii) do
          "flat" -> {:ok, Map.put(style, :css_transform_style, :flat)}
          "preserve-3d" -> {:ok, Map.put(style, :css_transform_style, :preserve_3d)}
          _ -> :error
        end

      "backface-visibility" ->
        case String.downcase(source, :ascii) do
          "visible" -> {:ok, Map.put(style, :css_backface_visibility, :visible)}
          "hidden" -> {:ok, Map.put(style, :css_backface_visibility, :hidden)}
          _ -> :error
        end
    end
  end

  @doc "Freezes declaring perspective lengths and origin font units before CSS inheritance."
  @spec compute_style(map()) :: {:ok, map()} | :error | {:error, term()}
  def compute_style(style) do
    case style do
      style
      when not is_map_key(style, :css_perspective) and
             not is_map_key(style, :css_perspective_origin) ->
        {:ok, style}

      _ ->
        result =
          case Map.get(style, :css_perspective, :none) do
            :none ->
              {:ok, style}

            value ->
              with {:ok, value} <- CssLengthValidator.resolve(value, nil, style),
                   do: {:ok, Map.put(style, :css_perspective, value)}
          end

        with {:ok, style} <- result do
          case Map.get(style, :css_perspective_origin) do
            nil ->
              {:ok, style}

            origin ->
              with {:ok, origin} <- CssPositionValidator.compute_position(origin, style),
                   do: {:ok, Map.put(style, :css_perspective_origin, origin)}
          end
        end
    end
  end

  @doc "Identifies a homogeneous operation, a perspective ancestor or a retained 3D scene boundary."
  @spec needs_group?(map()) :: boolean()
  def needs_group?(style) do
    case style do
      style
      when not is_map_key(style, :_css_3d_context) and
             not is_map_key(style, :css_transform_style) and
             not is_map_key(style, :css_perspective) and
             not is_map_key(style, :css_transform) and
             not is_map_key(style, :css_translate) and
             not is_map_key(style, :css_rotate) and
             not is_map_key(style, :css_scale) ->
        false

      _ ->
        Map.get(style, :_css_3d_context, false) or
          Map.get(style, :css_transform_style, :flat) == :preserve_3d or
          Map.get(style, :css_perspective, :none) != :none or
          Enum.any?([:css_transform, :css_translate, :css_rotate, :css_scale], fn key ->
            case Map.get(style, key, :none) do
              :none ->
                false

              operations when is_list(operations) ->
                Enum.any?(operations, fn entry ->
                  case entry do
                    {name, _} -> name in @functions
                    _ -> false
                  end
                end)

              _ ->
                false
            end
          end)
    end
  end

  @doc "Checks static 3D computed property facts at the styled-tree boundary."
  @spec valid_style?(map()) :: boolean()
  def valid_style?(style) do
    case style do
      style
      when is_map(style) and not is_map_key(style, :css_transform_style) and
             not is_map_key(style, :css_backface_visibility) and
             not is_map_key(style, :_css_3d_context) and
             not is_map_key(style, :css_perspective) and
             not is_map_key(style, :css_perspective_origin) ->
        true

      _ ->
        Map.get(style, :css_transform_style, :flat) in [:flat, :preserve_3d] and
          Map.get(style, :css_backface_visibility, :visible) in [:visible, :hidden] and
          is_boolean(Map.get(style, :_css_3d_context, false)) and
          (Map.get(style, :css_perspective, :none) == :none or
             (is_number(style.css_perspective) and style.css_perspective >= 0 and
                style.css_perspective <= Limits.get(:max_numeric_magnitude))) and
          case Map.get(style, :css_perspective_origin, {{:percent, 0.5}, {:percent, 0.5}}) do
            {x, y} ->
              CssPositionValidator.valid_prepared?(x) and CssPositionValidator.valid_prepared?(y)

            _ ->
              false
          end
    end
  end

  @doc "Determines the used preserve-3d value after CSS grouping properties force a flat result."
  @spec preserve?(map()) :: boolean()
  def preserve?(style) do
    Map.get(style, :css_transform_style, :flat) == :preserve_3d and
      Map.get(style, :opacity, 1.0) == 1 and Map.get(style, :mix_blend_mode, :Normal) == :Normal and
      Map.get(style, :isolation, :auto) != :isolate and Map.get(style, :css_filter, []) == [] and
      Map.get(style, :css_backdrop_filter, []) == [] and not CssClipValidator.active?(style) and
      not CssMaskValidator.active?(style) and :paint not in Map.get(style, :contain, []) and
      Enum.all?(
        [:overflow_x, :overflow_y],
        &(Map.get(style, &1, Map.get(style, :overflow, :visible)) in [:visible, :clip])
      )
  end

  @doc "Resolves canonical used operations and measured origins into a bounded PDF-space homogeneous matrix."
  @spec resolve(map(), map()) :: {:ok, tuple() | :empty} | {:error, {atom(), map()}}
  def resolve(style, bounds) do
    HtmlValidator.with_render_budget(fn ->
      reference =
        CssClipValidator.reference_box(
          bounds,
          style,
          case Map.get(style, :css_transform_box, :view_box) do
            :fill_box -> :content_box
            value -> value
          end
        )

      with {:ok, operations} <-
             CssTransformValidator.used_operations(
               style,
               reference.width,
               reference.height,
               reference
             ),
           {:ok, matrix} <-
             Enum.reduce_while(operations, {:ok, ProjectionGeometry.identity()}, fn {name, values},
                                                                                    {:ok, matrix} ->
               matrix =
                 ProjectionGeometry.multiply(matrix, ProjectionGeometry.operation(name, values))

               case validate_matrix(matrix) do
                 :ok -> {:cont, {:ok, matrix}}
                 error -> {:halt, error}
               end
             end),
           origin = Map.get(style, :transform_origin, {{:percent, 0.5}, {:percent, 0.5}}),
           metrics =
             Map.merge(style, CssTransformValidator.metric_style(style, "transform-origin")),
           {:ok, ox} <- CssPositionValidator.resolve(elem(origin, 0), reference.width, metrics),
           {:ok, oy} <- CssPositionValidator.resolve(elem(origin, 1), reference.height, metrics),
           {:ok, oz} <-
             if(tuple_size(origin) == 3,
               do: CssPositionValidator.resolve(elem(origin, 2), 0, metrics),
               else: {:ok, 0.0}
             ),
           matrix =
             matrix
             |> ProjectionGeometry.css_to_pdf()
             |> ProjectionGeometry.around(reference.x + ox, reference.top - oy, oz),
           :ok <- validate_matrix(matrix) do
        affine = ProjectionGeometry.planar_affine(matrix)

        if affine != nil and not Map.get(style, :_css_3d_context, false) and
             Map.get(style, :css_transform_style, :flat) != :preserve_3d and
             Map.get(style, :css_perspective, :none) == :none do
          {a, b, c, d, _, _} = affine

          hidden =
            Map.get(style, :css_backface_visibility, :visible) == :hidden and
              ProjectionGeometry.backface?(matrix)

          {:ok, if(a * d - b * c == 0 or hidden, do: :empty, else: affine)}
        else
          {:ok, matrix}
        end
      end
    end)
  rescue
    ArithmeticError ->
      invalid(
        "3D transform arithmetic exceeds finite projection geometry; reduce intermediate magnitudes or perspective extremes"
      )
  end

  @doc "Resolves the perspective camera applied to child planes around the actual border-box origin."
  @spec perspective(map(), map()) :: {:ok, tuple()} | {:error, {atom(), map()}}
  def perspective(style, bounds) do
    case Map.get(style, :css_perspective, :none) do
      :none ->
        {:ok, ProjectionGeometry.identity()}

      distance ->
        {x, y} = Map.get(style, :css_perspective_origin, {{:percent, 0.5}, {:percent, 0.5}})

        with {:ok, x} <- CssPositionValidator.resolve(x, bounds.width, style),
             {:ok, y} <- CssPositionValidator.resolve(y, bounds.height, style) do
          matrix =
            put_elem(ProjectionGeometry.identity(), 14, -1 / max(distance, 0.75))
            |> ProjectionGeometry.around(bounds.x + x, bounds.top - y, 0.0)

          {:ok, matrix}
        end
    end
  end

  @doc "Admits a homogeneous matrix through the existing shared numeric magnitude bound."
  @spec validate_matrix(term()) :: :ok | {:error, {atom(), map()}}
  def validate_matrix(matrix) do
    if is_tuple(matrix) and tuple_size(matrix) == 16 and
         Enum.all?(
           Tuple.to_list(matrix),
           &(is_number(&1) and abs(&1) <= Limits.get(:max_numeric_magnitude))
         ),
       do: :ok,
       else: invalid("3D matrices require sixteen finite values within max_numeric_magnitude")
  end

  @doc "Checks an optional measured projective group before source painting or link target traversal."
  @spec validate_descriptor(term()) :: :ok | {:error, {atom(), map()}}
  def validate_descriptor(descriptor) do
    case descriptor do
      nil ->
        :ok

      %{
        matrix: matrix,
        perspective: perspective,
        preserve: preserve,
        backface: backface,
        bounds: %{x: x, top: top, width: width, height: height}
      }
      when is_boolean(preserve) and backface in [:visible, :hidden] ->
        with :ok <- validate_matrix(matrix), :ok <- validate_matrix(perspective) do
          if Enum.all?(
               [x, top, width, height],
               &(is_number(&1) and abs(&1) <= Limits.get(:max_numeric_magnitude))
             ) and width >= 0 and height >= 0,
             do: :ok,
             else: invalid("3D paint groups require finite measured source border bounds")
        end

      _ ->
        invalid(
          "3D paint groups require a measured matrix, child perspective and explicit flattening/backface policy"
        )
    end
  end

  @doc "Identifies placed projective groups without changing ordinary affine pages."
  @spec scoped_boxes?([map()]) :: boolean()
  def scoped_boxes?(boxes),
    do:
      Enum.any?(boxes, fn box ->
        Map.has_key?(box, :css_projection) or
          (is_list(Map.get(box, :boxes)) and scoped_boxes?(box.boxes))
      end)

  @doc "Composes admitted group geometry and checks the accumulated matrices against the shared numeric budget."
  @spec compose_group(tuple(), map(), number()) :: {:ok, {tuple(), tuple()}} | {:error, term()}
  def compose_group(parent, box, height) do
    {own, child} = ProjectionGeometry.group_matrices(parent, box, height)
    with :ok <- validate_matrix(own), :ok <- validate_matrix(child), do: {:ok, {own, child}}
  rescue
    ArithmeticError ->
      invalid(
        "Accumulated 3D transforms exceed finite projection geometry; reduce nested scales or camera extremes"
      )
  end

  @doc "Admits a finite projective raster using the existing image, pixel and work budgets."
  @spec canvas(map(), non_neg_integer()) ::
          {:ok, {pos_integer(), pos_integer()}} | {:error, term()}
  def canvas(region, planes) do
    alias NativeElixirPdfUtilities.Validators.{WriterValidator, SvgValidator}

    with :ok <-
           WriterValidator.validate_pdf_geometry([
             region.x,
             region.y,
             region.width,
             region.height,
             0,
             0,
             0,
             0
           ]),
         dimensions =
           SvgValidator.default_viewport_dimensions(region.width / 0.75, region.height / 0.75),
         width = dimensions[:width],
         height = dimensions[:height],
         :ok <- SvgValidator.validate_raster_dimensions(width, height, nil),
         :ok <-
           HtmlValidator.reserve_render_resource(:max_decoded_bytes, width * height * 4, :layout),
         :ok <-
           HtmlValidator.reserve_render_resource(
             :max_raster_work,
             width * height * max(planes, 1) * 4,
             :layout
           ) do
      {:ok, {width, height}}
    end
  end

  @doc "Returns actionable projection failures without raising for ordinary CSS input."
  @spec invalid(binary()) :: {:error, {atom(), map()}}
  def invalid(message),
    do:
      Diagnostics.error(:layout, :invalid_layout, message,
        module: __MODULE__,
        operation: :resolve_css_projection,
        source: "CSS 3D transform"
      )
end
