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

  @doc "Composes canonical issued field rectangles and real appearance paint into a validated axis-aligned clipped parent group."
  @spec compose(
          descriptor(),
          {number(), number(), number(), number(), tuple() | nil, number() | nil}
        ) ::
          {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def compose(descriptor, {sx, sy, _tx, _ty, _clip, _height} = composition) do
    with {:ok, fields} <- resolve(descriptor) do
      Enum.reduce_while(fields, {:ok, []}, fn field, {:ok, acc} ->
        with {:ok, {full_x, full_y, _right, _top}, {x, y, r, t}} <-
               projected_bounds(field.rect, composition) do
          if r > x and t > y do
            appearance = field.appearance
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

            appearance = %{
              size: {r - x, t - y},
              boxes: [
                %{
                  type: :drawing_group,
                  width: width,
                  height: height,
                  transform: {sx, 0, 0, sy, full_x - x, full_y - y},
                  boxes: boxes
                }
              ]
            }

            projected = field |> Map.put(:rect, [x, y, r, t]) |> Map.put(:appearance, appearance)
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
  @spec validate_composition(descriptor(), tuple()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_composition(descriptor, composition) do
    with {:ok, fields} <- resolve(descriptor) do
      Enum.reduce_while(fields, :ok, fn field, :ok ->
        case projected_bounds(field.rect, composition) do
          {:ok, _full, _visible} -> {:cont, :ok}
          failure -> {:halt, failure}
        end
      end)
    end
  end

  defp projected_bounds([left, bottom, right, top], {sx, sy, tx, ty, clip, _height}) do
    full = {sx * left + tx, sy * bottom + ty, sx * right + tx, sy * top + ty}
    visible = DrawingGeometry.intersect_clip(full, clip)

    with :ok <-
           WriterValidator.validate_pdf_geometry(Tuple.to_list(full) ++ Tuple.to_list(visible)),
         do: {:ok, full, visible}
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
