defmodule NativeElixirPdfUtilities.Pdf.AssemblyWriter do
  @moduledoc false

  alias NativeElixirPdfUtilities.Pdf.InfoCodec
  alias NativeElixirPdfUtilities.Validators.MergeValidator

  @doc false
  @spec write([map()]) :: {:ok, binary()}
  def write(inputs) do
    page_ids =
      Enum.flat_map(inputs, fn %{pages: pages, map: id_map} ->
        Enum.map(pages, fn {object, generation} ->
          {Map.fetch!(id_map, {object, generation}), generation}
        end)
      end)

    pages_object_id = 1
    catalog_object_id = 2
    {pieces, offsets, position} = add_piece([], pdf_header(), %{}, 0)

    {pieces, offsets, position} =
      add_object(
        pieces,
        offsets,
        position,
        pages_object_id,
        0,
        render_pages_object(page_ids)
      )

    {pieces, offsets, position} =
      add_object(
        pieces,
        offsets,
        position,
        catalog_object_id,
        0,
        render_catalog_object(pages_object_id)
      )

    {pieces, offsets, position} =
      Enum.reduce(inputs, {pieces, offsets, position}, fn input, output ->
        Enum.reduce(input.objects, output, fn object, {pieces, offsets, position} ->
          new_id = Map.fetch!(input.map, {object.obj, object.gen})
          page_context = page_context(object, input, pages_object_id)
          body = render_object_body(object, input.map, page_context)
          add_object(pieces, offsets, position, new_id, object.gen, body)
        end)
      end)

    maximum_object_id = Enum.max([catalog_object_id, pages_object_id | Map.keys(offsets)])

    xref =
      xref_and_trailer(offsets, position, maximum_object_id, catalog_object_id)

    {:ok, IO.iodata_to_binary([Enum.reverse(pieces), xref])}
  end

  defp page_context(object, %{inherited: inheritances}, parent_id) do
    case Map.fetch(inheritances, {object.obj, object.gen}) do
      {:ok, inherited} -> Map.put(inherited, :parent_id, parent_id)
      :error -> nil
    end
  end

  defp pdf_header do
    ["%PDF-1.7\n%\xE2\xE3\xCF\xD3\n"]
  end

  defp add_piece(pieces, piece, offsets, position) do
    {[piece | pieces], offsets, position + :erlang.iolist_size(piece)}
  end

  defp add_object(pieces, offsets, position, id, generation, body) do
    piece = [
      Integer.to_string(id),
      " ",
      Integer.to_string(generation),
      " obj\n",
      body,
      "\nendobj\n"
    ]

    {
      [piece | pieces],
      Map.put(offsets, id, {position, generation}),
      position + :erlang.iolist_size(piece)
    }
  end

  defp render_pages_object(page_ids) do
    kids =
      page_ids
      |> Enum.map(fn {id, generation} ->
        [Integer.to_string(id), " ", Integer.to_string(generation), " R"]
      end)
      |> Enum.intersperse(" ")

    ["<< /Type /Pages /Kids [ ", kids, " ] /Count ", Integer.to_string(length(page_ids)), " >>\n"]
  end

  defp render_catalog_object(pages_object_id) do
    ["<< /Type /Catalog /Pages ", Integer.to_string(pages_object_id), " 0 R >>\n"]
  end

  defp render_object_body(object, id_map, page_context) do
    case Map.get(object, :value_override) do
      nil ->
        tokens =
          case page_context do
            nil -> object.tokens
            %{parent_id: _parent_id} -> rewrite_page_tokens(object.tokens, page_context)
          end

        render_tokens(tokens, id_map)

      value ->
        render_value(value, id_map, page_context.parent_id)
    end
  end

  defp rewrite_page_tokens(tokens, %{
         parent_id: parent_id,
         resources: resources,
         mediabox: media_box,
         cropbox: crop_box,
         rotate: rotate
       }) do
    [:dict_start | rest] = tokens
    {dictionary, remaining} = take_dictionary(rest, 1, [])

    dictionary =
      dictionary
      |> put_key("Parent", [{:generated_reference, parent_id}])
      |> put_key("Type", [{:name, "Page"}])
      |> put_optional_key("Resources", resources)
      |> put_key("MediaBox", media_box)
      |> put_optional_key("CropBox", crop_box)
      |> put_optional_key("Rotate", rotate)

    [:dict_start | dictionary] ++ [:dict_end | remaining]
  end

  defp take_dictionary(tokens, depth, collected) do
    case tokens do
      [:dict_start | rest] ->
        take_dictionary(rest, depth + 1, [:dict_start | collected])

      [:dict_end | rest] when depth == 1 ->
        {Enum.reverse(collected), rest}

      [:dict_end | rest] ->
        take_dictionary(rest, depth - 1, [:dict_end | collected])

      [token | rest] ->
        take_dictionary(rest, depth, [token | collected])
    end
  end

  defp put_key(tokens, name, value_tokens) do
    case MergeValidator.split_dictionary_value(tokens, name) do
      {:ok, left, _old, right} -> left ++ [{:name, name} | value_tokens] ++ right
      :error -> tokens ++ [{:name, name} | value_tokens]
    end
  end

  defp put_optional_key(tokens, name, value_tokens) do
    case value_tokens do
      nil -> tokens
      value_tokens -> put_key(tokens, name, value_tokens)
    end
  end

  defp render_tokens(tokens, id_map) do
    tokens
    |> do_render_tokens(id_map, [])
    |> Enum.reverse()
  end

  defp do_render_tokens(tokens, id_map, rendered) do
    case tokens do
      [] ->
        rendered

      [{:name, name} | rest] ->
        do_render_tokens(rest, id_map, [["/", InfoCodec.encode_name(name)] | separator(rendered)])

      [{:generated_reference, object} | rest] ->
        do_render_tokens(
          rest,
          id_map,
          [[Integer.to_string(object), " 0 R"] | separator(rendered)]
        )

      [{:int, object}, {:int, generation}, :R | rest] ->
        mapped = Map.fetch!(id_map, {object, generation})

        do_render_tokens(
          rest,
          id_map,
          [
            [Integer.to_string(mapped), " ", Integer.to_string(generation), " R"]
            | separator(rendered)
          ]
        )

      [:dict_start | rest] ->
        do_render_tokens(rest, id_map, ["<<" | separator(rendered)])

      [:dict_end | rest] ->
        do_render_tokens(rest, id_map, [">>" | separator(rendered)])

      [:lbracket | rest] ->
        do_render_tokens(rest, id_map, ["[" | separator(rendered)])

      [:rbracket | rest] ->
        do_render_tokens(rest, id_map, ["]" | separator(rendered)])

      [:stream, {:stream_data, data}, :endstream | rest] ->
        do_render_tokens(rest, id_map, [["\nstream\n", data, "\nendstream"] | rendered])

      [{:string, string} | rest] ->
        do_render_tokens(
          rest,
          id_map,
          [["(", InfoCodec.escape_literal(string), ")"] | separator(rendered)]
        )

      [{:hex_string, string} | rest] ->
        do_render_tokens(rest, id_map, [["<", Base.encode16(string), ">"] | separator(rendered)])

      [{:int, integer} | rest] ->
        do_render_tokens(rest, id_map, [Integer.to_string(integer) | separator(rendered)])

      [{:real, real} | rest] ->
        do_render_tokens(rest, id_map, [format_real(real) | separator(rendered)])

      [true | rest] ->
        do_render_tokens(rest, id_map, ["true" | separator(rendered)])

      [false | rest] ->
        do_render_tokens(rest, id_map, ["false" | separator(rendered)])

      [:null | rest] ->
        do_render_tokens(rest, id_map, ["null" | separator(rendered)])
    end
  end

  defp render_value(value, id_map, parent_id) do
    case value do
      nil ->
        "null"

      true ->
        "true"

      false ->
        "false"

      value when is_integer(value) ->
        Integer.to_string(value)

      value when is_float(value) ->
        format_real(value)

      {:name, name} ->
        ["/", InfoCodec.encode_name(name)]

      {:string, bytes} ->
        ["(", InfoCodec.escape_literal(bytes), ")"]

      {:hex, bytes} ->
        ["<", Base.encode16(bytes), ">"]

      {:ref, ref} ->
        render_reference(ref, id_map)

      :generated_parent ->
        [Integer.to_string(parent_id), " 0 R"]

      values when is_list(values) ->
        [
          "[ ",
          values |> Enum.map(&render_value(&1, id_map, parent_id)) |> Enum.intersperse(" "),
          " ]"
        ]

      dictionary when is_map(dictionary) ->
        render_dictionary(dictionary, id_map, parent_id)
    end
  end

  defp render_reference({object, generation}, id_map) do
    [
      Integer.to_string(Map.fetch!(id_map, {object, generation})),
      " ",
      Integer.to_string(generation),
      " R"
    ]
  end

  defp render_dictionary(dictionary, id_map, parent_id) do
    entries =
      dictionary
      |> Enum.sort_by(fn {key, _value} -> key end)
      |> Enum.map(fn {key, value} ->
        [["/", InfoCodec.encode_name(key)], " ", render_value(value, id_map, parent_id)]
      end)
      |> Enum.intersperse(" ")

    ["<< ", entries, " >>"]
  end

  defp separator(rendered) do
    case rendered do
      [] -> []
      _ -> [" " | rendered]
    end
  end

  defp format_real(real) do
    integer = trunc(real)

    case abs(real - integer) < 1.0e-9 do
      true ->
        Integer.to_string(integer)

      false ->
        real
        |> :erlang.float_to_binary(decimals: 10)
        |> String.trim_trailing("0")
        |> String.trim_trailing(".")
    end
  end

  defp xref_and_trailer(offsets, position, maximum_id, root_id) do
    entries =
      [[pad(0, 10), " 65535 f \n"]] ++
        Enum.map(1..maximum_id, fn id ->
          {offset, generation} = Map.fetch!(offsets, id)
          [pad(offset, 10), " ", pad(generation, 5), " n \n"]
        end)

    [
      "xref\n0 ",
      Integer.to_string(maximum_id + 1),
      "\n",
      entries,
      "trailer\n<< /Size ",
      Integer.to_string(maximum_id + 1),
      " /Root ",
      Integer.to_string(root_id),
      " 0 R >>\nstartxref\n",
      Integer.to_string(position),
      "\n%%EOF\n"
    ]
  end

  defp pad(integer, length) do
    integer |> Integer.to_string() |> String.pad_leading(length, "0")
  end
end
