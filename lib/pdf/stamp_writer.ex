defmodule NativeElixirPdfUtilities.Pdf.StampWriter do
  @moduledoc false

  alias NativeElixirPdfUtilities.Pdf.IncrementalWriter
  @doc false
  @spec write(map()) :: {:ok, binary()} | {:error, {atom(), map()}}
  def write(plan) do
    case plan do
      %{
        target_context: target_context,
        placements: placements,
        copied_objects: copied_objects,
        opacity: opacity
      }
      when is_list(placements) and is_list(copied_objects) and is_number(opacity) ->
        objects = build_objects(target_context, placements, copied_objects, opacity)
        IncrementalWriter.write(target_context, objects)

      _ ->
        NativeElixirPdfUtilities.Diagnostics.error(
          :incremental_write,
          :invalid_pdf_input,
          "prepared stamp write plan is malformed",
          module: __MODULE__
        )
    end
  end

  defp build_objects(target_context, placements, copied_objects, opacity) do
    first_id = target_context.document.trailer["Size"]

    {reference_map, next_id} =
      Enum.reduce(copied_objects, {%{}, first_id}, fn {{object, generation}, _parsed},
                                                      {mapping, next_id} ->
        {Map.put(mapping, {object, generation}, next_id), next_id + 1}
      end)

    source_pages =
      placements
      |> Enum.map(& &1.source)
      |> Enum.uniq_by(& &1.page_number)

    {form_ids, next_id} =
      Enum.reduce(source_pages, {%{}, next_id}, fn page, {ids, next_id} ->
        {Map.put(ids, page.page_number, next_id), next_id + 1}
      end)

    {graphics_state_id, next_id} =
      case opacity < 1.0 do
        true -> {next_id, next_id + 1}
        false -> {nil, next_id}
      end

    # Save the initial page state before existing content changes its transform or clip.
    save_state_id = next_id

    {invocation_ids, _next_id} =
      Enum.map_reduce(placements, next_id + 1, fn _placement, next_id ->
        {next_id, next_id + 1}
      end)

    copied = copied_object_entries(copied_objects, reference_map)
    forms = form_entries(source_pages, form_ids, reference_map)

    graphics_state =
      case graphics_state_id do
        nil ->
          []

        id ->
          [{id, 0, {:value, %{"Type" => {:name, "ExtGState"}, "ca" => opacity, "CA" => opacity}}}]
      end

    page_entries =
      placements
      |> Enum.zip(invocation_ids)
      |> Enum.flat_map(fn {placement, invocation_id} ->
        form_id = Map.fetch!(form_ids, placement.source.page_number)

        {resources, xobject_name, graphics_state_name} =
          stamped_resources(
            placement.target.resources,
            form_id,
            graphics_state_id
          )

        matrix = placement_matrix(placement)

        invocation =
          "\nQ\n" <> invocation_stream(matrix, xobject_name, graphics_state_name)

        page_dictionary =
          placement.target.dictionary
          |> Map.put("Resources", resources)
          |> Map.put(
            "Contents",
            [{:ref, {save_state_id, 0}} | placement.target.contents] ++
              [{:ref, {invocation_id, 0}}]
          )

        {page_object, page_generation} = placement.target.ref

        [
          {invocation_id, 0, {:stream, %{}, invocation}},
          {page_object, page_generation, {:value, page_dictionary}}
        ]
      end)

    [{save_state_id, 0, {:stream, %{}, "q\n"}}] ++
      copied ++ forms ++ graphics_state ++ page_entries
  end

  defp copied_object_entries(copied_objects, reference_map) do
    Enum.map(copied_objects, fn {reference, parsed} ->
      id = Map.fetch!(reference_map, reference)
      {_object, generation} = reference

      body =
        case parsed.stream do
          stream when is_binary(stream) ->
            {:stream, remap_value(Map.delete(parsed.value, "Length"), reference_map), stream}

          nil ->
            {:value, remap_value(parsed.value, reference_map)}
        end

      {id, generation, body}
    end)
  end

  defp form_entries(source_pages, form_ids, reference_map) do
    Enum.map(source_pages, fn source ->
      dictionary =
        %{
          "Type" => {:name, "XObject"},
          "Subtype" => {:name, "Form"},
          "FormType" => 1,
          "BBox" => source.crop_box,
          "Matrix" => source.normalization,
          "Resources" => remap_value(source.resources, reference_map)
        }
        |> put_optional("Group", remap_value(source.group, reference_map))

      {Map.fetch!(form_ids, source.page_number), 0, {:stream, dictionary, source.content}}
    end)
  end

  defp stamped_resources(resources, form_id, graphics_state_id) do
    xobjects = Map.get(resources, "XObject", %{})
    xobject_name = available_name(xobjects, "NEPUStamp")

    resources =
      Map.put(resources, "XObject", Map.put(xobjects, xobject_name, {:ref, {form_id, 0}}))

    case graphics_state_id do
      nil ->
        {resources, xobject_name, nil}

      graphics_state_id ->
        states = Map.get(resources, "ExtGState", %{})
        state_name = available_name(states, "NEPUStampGS")
        states = Map.put(states, state_name, {:ref, {graphics_state_id, 0}})
        {Map.put(resources, "ExtGState", states), xobject_name, state_name}
    end
  end

  defp available_name(dictionary, base) do
    Stream.iterate(1, &(&1 + 1))
    |> Enum.find_value(fn index ->
      name = if index == 1, do: base, else: base <> Integer.to_string(index)
      if Map.has_key?(dictionary, name), do: nil, else: name
    end)
  end

  defp invocation_stream(matrix, xobject_name, graphics_state_name) do
    graphics_state =
      case graphics_state_name do
        nil -> []
        name -> ["/", name, " gs "]
      end

    [
      "q ",
      graphics_state,
      Enum.map_join(matrix, " ", &format_number/1),
      " cm /",
      xobject_name,
      " Do Q"
    ]
    |> IO.iodata_to_binary()
  end

  defp placement_matrix(placement) do
    fit = fit_matrix(placement.source, placement.target, placement.fit)

    transformed =
      case placement.transform do
        %{rotation: rotation, pivot: pivot} ->
          multiply(rotation_matrix(rotation, pivot), fit)

        nil ->
          fit
      end

    multiply(placement.target.inverse_normalization, transformed)
  end

  defp fit_matrix(source, target, mode) do
    case mode do
      :exact ->
        identity()

      :stretch ->
        [target.width / source.width, 0.0, 0.0, target.height / source.height, 0.0, 0.0]

      mode when mode in [:contain, :cover] ->
        scale =
          case mode do
            :contain -> min(target.width / source.width, target.height / source.height)
            :cover -> max(target.width / source.width, target.height / source.height)
          end

        width = source.width * scale
        height = source.height * scale
        [scale, 0.0, 0.0, scale, (target.width - width) / 2, (target.height - height) / 2]
    end
  end

  defp rotation_matrix(rotation, {pivot_x, pivot_y}) do
    radians = -rotation * :math.pi() / 180
    cosine = :math.cos(radians)
    sine = :math.sin(radians)

    [
      cosine,
      sine,
      -sine,
      cosine,
      pivot_x - cosine * pivot_x + sine * pivot_y,
      pivot_y - sine * pivot_x - cosine * pivot_y
    ]
  end

  defp identity, do: [1.0, 0.0, 0.0, 1.0, 0.0, 0.0]

  defp multiply([a, b, c, d, e, f], [a2, b2, c2, d2, e2, f2]) do
    [
      a * a2 + c * b2,
      b * a2 + d * b2,
      a * c2 + c * d2,
      b * c2 + d * d2,
      a * e2 + c * f2 + e,
      b * e2 + d * f2 + f
    ]
  end

  defp remap_value(value, reference_map) do
    case value do
      {:ref, reference} ->
        {:ref, {Map.fetch!(reference_map, reference), elem(reference, 1)}}

      values when is_list(values) ->
        Enum.map(values, &remap_value(&1, reference_map))

      dictionary when is_map(dictionary) ->
        Map.new(dictionary, fn {key, item} -> {key, remap_value(item, reference_map)} end)

      value ->
        value
    end
  end

  defp put_optional(dictionary, key, value) do
    case value do
      nil -> dictionary
      value -> Map.put(dictionary, key, value)
    end
  end

  defp format_number(number) do
    case number == 0 do
      true ->
        "0"

      false ->
        number
        |> Kernel.*(1.0)
        |> :erlang.float_to_binary(decimals: 10)
        |> String.trim_trailing("0")
        |> String.trim_trailing(".")
    end
  end
end
