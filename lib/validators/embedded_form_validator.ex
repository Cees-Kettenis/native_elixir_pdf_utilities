defmodule NativeElixirPdfUtilities.Validators.EmbeddedFormValidator do
  @moduledoc "Validator-owned issued child fields and their real clipped PDF appearance geometry."

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, RenderCache, ResourceContext}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, ResourceValidator, WriterValidator}

  @typedoc "An opaque field descriptor issued from canonical HtmlFormValidator.prepare fields within the active document scope."
  @type descriptor :: %{context: ResourceContext.t(), token: reference()}

  @doc "Issues internal canonical child fields. The field list comes exclusively from HtmlFormValidator.prepare/2, after its input, state and appearance policy succeeds."
  @spec issue(ResourceContext.t(), [map()]) ::
          {:ok, descriptor()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def issue(context, fields) do
    with :ok <- ResourceValidator.validate_context(context) do
      HtmlValidator.check_render_resource(:max_pdf_form_fields, length(fields), :forms)
      token = make_ref()

      RenderCache.fetch(context.scope, {__MODULE__, token}, fn ->
        {:ok, %{document_id: context.document_id, fields: fields}}
      end)

      {:ok, %{context: context, token: token}}
    end
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      {:error,
       {reason,
        Diagnostics.with_context(diagnostic,
          module: __MODULE__,
          operation: :compose_child_widgets
        )}}
  end

  @doc "Resolves only a validator-issued child field descriptor within its immutable live document scope."
  @spec resolve(term()) :: {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def resolve(descriptor) do
    case descriptor do
      %{context: context, token: token} when is_reference(token) ->
        with :ok <- ResourceValidator.validate_context(context),
             {:ok, record} <-
               RenderCache.fetch(context.scope, {__MODULE__, token}, fn ->
                 error("child field descriptor was not issued by the form validator")
               end) do
          if record.document_id == context.document_id,
            do: {:ok, record.fields},
            else: error("child field descriptor belongs to a different document")
        end

      _ ->
        error("child fields require a validator-issued descriptor")
    end
  end

  @doc "Issues another physical page slice of the same canonical source fields and appearance graph."
  @spec issue_slice(descriptor()) ::
          {:ok, descriptor()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def issue_slice(descriptor) do
    with {:ok, fields} <- resolve(descriptor),
         {:ok, source_token} <- source_identity(descriptor),
         {:ok, slice} <- issue(descriptor.context, fields) do
      RenderCache.fetch(descriptor.context.scope, {__MODULE__, :slice, slice.token}, fn ->
        {:ok, source_token}
      end)

      {:ok, slice}
    end
  end

  @doc "Issues distinct physical widget slices throughout one validated native source drawing, retaining each original field and AP graph."
  @spec issue_drawing_slice(term()) :: {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def issue_drawing_slice(box) do
    with :ok <- WriterValidator.validate_native_boxes([box]), do: slice_drawing(box)
  end

  defp slice_drawing(box) do
    with {:ok, box} <-
           (case Map.get(box, :embedded_widgets) do
              nil ->
                {:ok, box}

              descriptor ->
                with {:ok, slice} <- issue_slice(descriptor),
                     do: {:ok, Map.put(box, :embedded_widgets, slice)}
            end) do
      case box do
        %{type: :drawing_group, boxes: children} ->
          children
          |> Enum.reduce_while({:ok, []}, fn child, {:ok, sliced} ->
            case slice_drawing(child) do
              {:ok, child} -> {:cont, {:ok, [child | sliced]}}
              failure -> {:halt, failure}
            end
          end)
          |> case do
            {:ok, children} -> {:ok, %{box | boxes: Enum.reverse(children)}}
            failure -> failure
          end

        _ ->
          {:ok, box}
      end
    end
  end

  @doc "Returns the validator-issued source identity shared by genuine physical slices only."
  @spec source_identity(descriptor()) ::
          {:ok, reference()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def source_identity(descriptor) do
    with {:ok, _fields} <- resolve(descriptor) do
      RenderCache.fetch(descriptor.context.scope, {__MODULE__, :slice, descriptor.token}, fn ->
        {:ok, descriptor.token}
      end)
    end
  end

  @doc "Composes canonical issued field rectangles and real appearance paint into a validated axis-aligned clipped parent group."
  @spec compose(
          descriptor(),
          term()
        ) ::
          {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def compose(descriptor, composition) do
    with :ok <- validate_axis_composition(composition),
         {:ok, fields} <- resolve(descriptor) do
      {sx, sy, _tx, _ty, _clip, _height} = composition

      Enum.reduce_while(fields, {:ok, []}, fn field, {:ok, acc} ->
        with {:ok, {full_x, full_y, _right, _top}, {x, y, r, t}} <-
               projected_bounds(field.rect, composition) do
          if r > x and t > y do
            appearance =
              axis_appearance(field.appearance, sx, sy, full_x - x, full_y - y, r - x, t - y)

            projected = field |> Map.put(:rect, [x, y, r, t]) |> Map.put(:appearance, appearance)

            projected =
              NativeElixirPdfUtilities.Validators.FormAppearanceValidator.project_field(
                projected,
                {sx, 0, 0, sy, full_x - x, full_y - y},
                [[{0, 0}, {r - x, 0}, {r - x, t - y}, {0, t - y}]],
                {0, 0, r - x, t - y}
              )

            projected =
              case Map.get(field, :appearance_states) do
                nil ->
                  projected

                states ->
                  Map.put(
                    projected,
                    :appearance_states,
                    Enum.map(
                      states,
                      &axis_appearance(&1, sx, sy, full_x - x, full_y - y, r - x, t - y)
                    )
                  )
              end

            {:cont, {:ok, [projected | acc]}}
          else
            {:cont, {:ok, acc}}
          end
        else
          {:error, _} = failure -> {:halt, failure}
        end
      end)
      |> case do
        {:ok, fields} -> {:ok, Enum.reverse(fields)}
        failure -> failure
      end
    end
  end

  @doc "Checks issued field projection bounds before the writer allocates normalized group appearances."
  @spec validate_composition(descriptor(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_composition(descriptor, composition) do
    with :ok <- validate_axis_composition(composition),
         {:ok, fields} <- resolve(descriptor) do
      Enum.reduce_while(fields, :ok, fn field, :ok ->
        case projected_bounds(field.rect, composition) do
          {:ok, _full, _visible} -> {:cont, :ok}
          failure -> {:halt, failure}
        end
      end)
    end
  end

  @doc "Returns genuine issued widget rectangles under the same validated composition and clip as their appearances."
  @spec projection_bounds(descriptor(), term()) ::
          {:ok, [tuple()]} | {:error, Diagnostics.detailed_error()}
  def projection_bounds(descriptor, composition) do
    with :ok <- validate_axis_composition(composition),
         {:ok, fields} <- resolve(descriptor) do
      Enum.reduce_while(fields, {:ok, []}, fn field, {:ok, rectangles} ->
        case projected_bounds(field.rect, composition) do
          {:ok, _full, visible} -> {:cont, {:ok, [visible | rectangles]}}
          failure -> {:halt, failure}
        end
      end)
      |> case do
        {:ok, rectangles} -> {:ok, Enum.reverse(rectangles)}
        failure -> failure
      end
    end
  end

  @doc "Removes raw control transport from validated source artwork, preserving real appearance paint and omitting nonpainting hosts."
  @spec source_paint(term()) :: {:ok, [map()]} | {:error, Diagnostics.detailed_error()}
  def source_paint(boxes) do
    with :ok <- WriterValidator.validate_native_boxes(boxes),
         do: {:ok, Enum.flat_map(boxes, &appearance_paint/1)}
  end

  @doc "Projects canonical fields through a PDF Form's validated affine placement and actual visible clip geometry."
  @spec compose_affine(descriptor(), tuple(), tuple(), term()) ::
          {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def compose_affine(descriptor, matrix, viewport, clip_geometry) do
    alias NativeElixirPdfUtilities.HtmlToPdf.FormAppearanceComposition
    alias NativeElixirPdfUtilities.Validators.ClipGeometryValidator

    with :ok <- validate_affine(matrix, viewport),
         {:ok, fields} <- compose(descriptor, {1, 1, 0, 0, nil, nil}) do
      {a, b, c, d, e, f} = matrix

      Enum.reduce_while(fields, {:ok, []}, fn field, {:ok, projected} ->
        [left, bottom, right, top] = field.rect

        corners =
          Enum.map([{left, bottom}, {right, bottom}, {right, top}, {left, top}], fn {x, y} ->
            {a * x + c * y + e, b * x + d * y + f}
          end)

        with {:ok, quads} <- ClipGeometryValidator.project(clip_geometry, corners, viewport) do
          if quads == [] do
            {:cont, {:ok, projected}}
          else
            polygons =
              Enum.map(quads, fn [x1, y1, x2, y2, x3, y3, x4, y4] ->
                [{x1, y1}, {x2, y2}, {x4, y4}, {x3, y3}]
              end)

            xs = Enum.flat_map(polygons, &Enum.map(&1, fn {x, _y} -> x end))
            ys = Enum.flat_map(polygons, &Enum.map(&1, fn {_x, y} -> y end))
            bounds = {Enum.min(xs), Enum.min(ys), Enum.max(xs), Enum.max(ys)}
            transform = {a, b, c, d, a * left + c * bottom + e, b * left + d * bottom + f}

            with :ok <-
                   WriterValidator.validate_pdf_geometry(
                     Tuple.to_list(bounds) ++ Tuple.to_list(bounds)
                   ),
                 {:ok, states} <-
                   Enum.reduce_while(
                     NativeElixirPdfUtilities.Pdf.FormWriter.appearance_frames(field),
                     {:ok, []},
                     fn frame, {:ok, states} ->
                       prepared =
                         try do
                           FormAppearanceComposition.project_appearance(
                             frame,
                             transform,
                             polygons,
                             bounds
                           )
                         catch
                           {:render_resource_limit, failure} -> failure
                         end

                       case prepared do
                         {:ok, appearance} -> {:cont, {:ok, [appearance | states]}}
                         failure -> {:halt, failure}
                       end
                     end
                   ) do
              states = Enum.reverse(states)

              appearance =
                if field.type in [:checkbox, :radio],
                  do: Enum.at(states, if(field.value, do: 1, else: 0)),
                  else: hd(states)

              field =
                field
                |> Map.put(:rect, Tuple.to_list(bounds))
                |> Map.put(:appearance, appearance)
                |> Map.put(:appearance_states, states)

              field =
                NativeElixirPdfUtilities.Validators.FormAppearanceValidator.project_field(
                  field,
                  transform,
                  polygons,
                  bounds
                )

              {:cont, {:ok, [field | projected]}}
            else
              failure -> {:halt, failure}
            end
          end
        else
          failure -> {:halt, failure}
        end
      end)
      |> case do
        {:ok, fields} -> {:ok, Enum.reverse(fields)}
        failure -> failure
      end
    end
  end

  defp validate_affine(matrix, viewport) do
    case {matrix, viewport} do
      {{a, b, c, d, e, f}, {left, bottom, right, top}} ->
        values = [a, b, c, d, e, f, left, bottom, right, top]

        if Enum.all?(values, &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))) and
             right >= left and top >= bottom and abs(a * d - b * c) > 0 do
          :ok
        else
          error(
            "child field affine placement must have bounded nondegenerate geometry and an ordered viewport"
          )
        end

      _ ->
        error(
          "child field affine placement requires six matrix values and a rectangular viewport"
        )
    end
  end

  defp validate_axis_composition(composition) do
    case composition do
      {sx, sy, tx, ty, clip, height} ->
        clip_values =
          case clip do
            nil -> []
            {left, bottom, right, top} -> [left, bottom, right, top]
            _ -> [:invalid]
          end

        values = [sx, sy, tx, ty] ++ clip_values ++ if(height == nil, do: [], else: [height])

        if Enum.all?(values, &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))) and
             sx > 0 and sy > 0 and (height == nil or height >= 0) do
          :ok
        else
          error(
            "child field axis placement requires bounded positive scales, numeric offsets and a numeric rectangle clip"
          )
        end

      _ ->
        error(
          "child field axis placement requires two scales, two offsets, a rectangle clip and source height"
        )
    end
  end

  defp projected_bounds([left, bottom, right, top], {sx, sy, tx, ty, clip, _height}) do
    full = {sx * left + tx, sy * bottom + ty, sx * right + tx, sy * top + ty}
    visible = DrawingGeometry.intersect_clip(full, clip)

    with :ok <-
           WriterValidator.validate_pdf_geometry(Tuple.to_list(full) ++ Tuple.to_list(visible)),
         do: {:ok, full, visible}
  end

  defp axis_appearance(appearance, sx, sy, offset_x, offset_y, projected_width, projected_height) do
    {width, height} = appearance.size

    boxes = Enum.flat_map(appearance.boxes, &appearance_paint/1)

    boxes =
      case Map.get(appearance, :paint_frame) do
        nil ->
          boxes

        frame ->
          [
            %{
              type: :drawing_group,
              width: frame.width,
              height: frame.height,
              transform: {1, 0, 0, 1, -frame.x, -frame.y},
              boxes: boxes
            }
          ]
      end

    %{
      size: {projected_width, projected_height},
      boxes: [
        %{
          type: :drawing_group,
          width: width,
          height: height,
          transform: {sx, 0, 0, sy, offset_x, offset_y},
          boxes: boxes
        }
      ]
    }
  end

  defp appearance_paint(box) do
    marker =
      box.type == :rect and is_map(Map.get(box, :form_control)) and
        is_nil(Map.get(box, :fill_color)) and not WriterValidator.visible_border?(box)

    box = Map.drop(box, [:form_control, :form_owner])

    if marker do
      []
    else
      case Map.fetch(box, :boxes) do
        {:ok, children} -> [Map.put(box, :boxes, Enum.flat_map(children, &appearance_paint/1))]
        :error -> [box]
      end
    end
  end

  defp error(message) do
    Diagnostics.error(:forms, :invalid_form, message,
      module: __MODULE__,
      operation: :compose_child_widgets
    )
  end
end
