defmodule NativeElixirPdfUtilities.Pdf.Reader do
  @moduledoc """
  Strict, native PDF object reader shared by PDF utilities.

  The reader resolves direct and compressed objects, validates the final xref
  pointer, decodes the standard non-encryption stream filters, and walks the
  page tree with inherited resources. It intentionally does not decrypt PDFs.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Tokenizer
  import Bitwise

  @max_input_bytes 50_000_000
  @max_objects 100_000
  @max_pages 10_000
  @max_decoded_stream_bytes 25_000_000
  @max_decompression_ratio 100

  @type ref :: {non_neg_integer(), non_neg_integer()}
  @type value ::
          nil
          | boolean()
          | integer()
          | float()
          | {:name, binary()}
          | {:string, binary()}
          | {:hex, binary()}
          | {:ref, ref()}
          | [value()]
          | %{optional(binary()) => value()}
  @type object :: %{
          value: value(),
          stream: binary() | nil,
          offset: non_neg_integer() | nil,
          tokens: [Tokenizer.token()]
        }
  @type page :: %{
          ref: ref(),
          resources: value() | nil,
          rotate: number() | nil,
          media_box: [number()] | nil
        }
  @type document :: %{
          required(:objects) => %{optional(ref()) => object()},
          optional(atom()) => term()
        }
  @type xref_entry ::
          {:free, non_neg_integer(), non_neg_integer()}
          | {:uncompressed, non_neg_integer(), non_neg_integer()}
          | {:compressed, pos_integer(), non_neg_integer()}
  @type error_reason ::
          :encrypted_pdf
          | :invalid_pdf_input
          | :unsupported_pdf_feature
          | :resource_limit_exceeded

  @doc """
  Parses a PDF binary into a resolver document.
  """
  @spec read(binary()) :: {:ok, document()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def read(pdf) do
    case pdf do
      pdf when is_binary(pdf) ->
        with :ok <- validate_header(pdf),
             {:ok, xref_offset} <- final_xref_offset(pdf),
             {:ok, xref, trailer} <- parse_xref_chain(pdf, xref_offset, %{}, 0),
             :ok <- validate_xref(xref, trailer, pdf),
             :ok <- reject_encryption(trailer),
             {:ok, objects} <- load_objects(pdf, xref),
             {:ok, pages} <- collect_pages(objects, trailer) do
          {:ok, %{binary: pdf, objects: objects, trailer: trailer, pages: pages, xref: xref}}
        else
          {:error, {_reason, _diagnostic}} = reader_error -> reader_error
        end

      _ ->
        error(:input, :invalid_pdf_input, "PDF input must be a binary")
    end
  end

  @doc """
  Resolves an indirect value in a parsed document.
  """
  @spec resolve(document(), value()) ::
          {:ok, value()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def resolve(document, value) do
    case value do
      {:ref, ref} ->
        resolve_reference(document, ref, %{})

      value ->
        {:ok, value}
    end
  end

  @doc """
  Resolves an indirect stream and decodes its declared filters.
  """
  @spec decoded_stream(document(), value()) ::
          {:ok, binary()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def decoded_stream(document, value) do
    case value do
      {:ref, ref} ->
        with {:ok, stream_ref, %{value: dictionary, stream: stream}} <-
               resolve_stream_object(document, ref, %{}),
             true <- is_map(dictionary) and is_binary(stream) do
          decode_stream(stream, dictionary, document, stream_ref)
        else
          false -> error(:stream, :invalid_pdf_input, "object is not a stream", object: ref)
          {:error, _} = stream_error -> stream_error
        end

      _ ->
        error(:stream, :invalid_pdf_input, "stream must be an indirect object")
    end
  end

  @doc """
  Resolves a dictionary value, returning an error when it is not a dictionary.
  """
  @spec dictionary(document(), value()) ::
          {:ok, map()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def dictionary(document, value) do
    with {:ok, value} <- resolve(document, value),
         true <- is_map(value) do
      {:ok, value}
    else
      false -> error(:resolution, :invalid_pdf_input, "expected a PDF dictionary")
      {:error, _} = error -> error
    end
  end

  @doc """
  Returns a dictionary key after resolving the dictionary itself.
  """
  @spec fetch(document(), value(), binary()) ::
          {:ok, value() | nil} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def fetch(document, dictionary, key) do
    case key do
      key when is_binary(key) ->
        with {:ok, dictionary} <- dictionary(document, dictionary) do
          {:ok, Map.get(dictionary, key)}
        end

      _ ->
        error(:resolution, :invalid_pdf_input, "dictionary key must be a binary")
    end
  end

  defp validate_header(pdf) do
    cond do
      byte_size(pdf) > @max_input_bytes ->
        error(:limits, :resource_limit_exceeded, "PDF input exceeds the byte limit")

      Regex.match?(~r/\A%PDF-(1\.[0-7]|2\.0)(?:\s|\r|\n)/, pdf) ->
        :ok

      true ->
        error(:header, :invalid_pdf_input, "PDF header is missing or has an unsupported version")
    end
  end

  defp final_xref_offset(pdf) do
    case Regex.scan(~r/startxref\s*(\d+)\s*%%EOF\s*\z/s, pdf) do
      [[_, offset_text]] ->
        {offset, ""} = Integer.parse(offset_text)

        if offset < byte_size(pdf) do
          {:ok, offset}
        else
          error(:xref, :invalid_pdf_input, "final startxref offset is outside the PDF")
        end

      _ ->
        error(:xref, :invalid_pdf_input, "PDF is missing a valid final startxref section")
    end
  end

  defp parse_xref_chain(pdf, offset, seen, depth) do
    cond do
      Map.has_key?(seen, offset) ->
        error(:xref, :invalid_pdf_input, "xref revision chain contains a cycle")

      depth >= 1_000 ->
        error(:limits, :resource_limit_exceeded, "xref revision count exceeds the limit")

      true ->
        with {:ok, entries, trailer} <- parse_xref_revision(pdf, offset),
             {:ok, previous_entries, previous_trailer} <-
               previous_xref_revision(
                 pdf,
                 Map.get(trailer, "Prev"),
                 Map.put(seen, offset, true),
                 depth
               ) do
          {:ok, Map.merge(previous_entries, entries), Map.merge(previous_trailer, trailer)}
        end
    end
  end

  defp previous_xref_revision(pdf, offset, seen, depth) do
    case offset do
      nil ->
        {:ok, %{}, %{}}

      offset when is_integer(offset) and offset >= 0 ->
        parse_xref_chain(pdf, offset, seen, depth + 1)

      _ ->
        error(:xref, :invalid_pdf_input, "xref Prev offset is malformed")
    end
  end

  defp parse_xref_revision(pdf, offset) do
    if offset >= 0 and offset < byte_size(pdf) do
      tail = binary_part(pdf, offset, byte_size(pdf) - offset)

      cond do
        String.starts_with?(tail, "xref") ->
          parse_classic_xref(pdf, offset, tail)

        Regex.match?(~r/\A\d+\s+\d+\s+obj\b/, tail) ->
          parse_xref_stream(pdf, offset)

        true ->
          error(:xref, :invalid_pdf_input, "xref offset does not point to an xref section")
      end
    else
      error(:xref, :invalid_pdf_input, "xref offset is outside the PDF")
    end
  end

  defp parse_classic_xref(pdf, offset, tail) do
    tokens = Tokenizer.new(tail) |> Tokenizer.tokenize_all()

    with [:xref | rest] <- tokens,
         {:ok, entries, trailer} <- parse_classic_sections(rest, %{}),
         {:ok, supplemental_entries, supplemental_trailer} <-
           supplemental_xref_stream(pdf, Map.get(trailer, "XRefStm"), offset) do
      {:ok, Map.merge(entries, supplemental_entries), Map.merge(supplemental_trailer, trailer)}
    else
      {:error, _} = xref_error -> xref_error
      _ -> error(:xref, :invalid_pdf_input, "classic xref table is malformed")
    end
  end

  defp parse_classic_sections(tokens, entries) do
    case tokens do
      [:trailer | rest] ->
        with {:ok, trailer, _rest} <- parse_value(rest),
             true <- is_map(trailer) do
          {:ok, entries, trailer}
        else
          _ -> error(:trailer, :invalid_pdf_input, "trailer dictionary is malformed")
        end

      [{:int, first}, {:int, count} | rest]
      when first >= 0 and count >= 0 and first + count <= @max_objects + 1 ->
        with {:ok, entries, rest} <- parse_classic_entries(rest, first, count, entries) do
          parse_classic_sections(rest, entries)
        end

      _ ->
        error(:xref, :invalid_pdf_input, "classic xref subsection is malformed")
    end
  end

  defp parse_classic_entries(tokens, object, remaining, entries) do
    case remaining do
      0 ->
        {:ok, entries, tokens}

      _ ->
        case tokens do
          [{:int, offset}, {:int, generation}, {:op, marker} | rest]
          when offset >= 0 and generation in 0..65_535 and marker in ["n", "f"] ->
            if Map.has_key?(entries, object) do
              error(:xref, :invalid_pdf_input, "classic xref subsections overlap")
            else
              entry =
                if marker == "n",
                  do: {:uncompressed, offset, generation},
                  else: {:free, offset, generation}

              parse_classic_entries(
                rest,
                object + 1,
                remaining - 1,
                Map.put(entries, object, entry)
              )
            end

          _ ->
            error(:xref, :invalid_pdf_input, "classic xref entry is malformed")
        end
    end
  end

  defp supplemental_xref_stream(pdf, stream_offset, classic_offset) do
    case stream_offset do
      nil ->
        {:ok, %{}, %{}}

      stream_offset
      when is_integer(stream_offset) and stream_offset >= 0 and stream_offset != classic_offset ->
        parse_xref_stream(pdf, stream_offset)

      _ ->
        error(:xref, :invalid_pdf_input, "trailer XRefStm offset is malformed")
    end
  end

  defp parse_xref_stream(pdf, offset) do
    with {:ok, ref, object} <- parse_indirect_object_at(pdf, offset, byte_size(pdf), nil),
         true <- is_map(object.value) and name?(Map.get(object.value, "Type"), "XRef"),
         true <- is_binary(object.stream),
         {:ok, decoded} <-
           decode_stream(object.stream, object.value, %{objects: %{ref => object}}, ref),
         {:ok, entries} <- xref_stream_entries(decoded, object.value) do
      {:ok, entries, object.value}
    else
      false -> error(:xref, :invalid_pdf_input, "xref stream object is malformed")
      {:error, _} = xref_error -> xref_error
    end
  end

  defp xref_stream_entries(data, dictionary) do
    size = Map.get(dictionary, "Size")
    widths = Map.get(dictionary, "W")
    index = Map.get(dictionary, "Index", if(is_integer(size), do: [0, size], else: nil))

    with true <- is_integer(size) and size > 0 and size <= @max_objects + 1,
         [type_width, field_width, generation_width] <- widths,
         true <-
           Enum.all?(
             [type_width, field_width, generation_width],
             &(is_integer(&1) and &1 >= 0)
           ),
         row_width when row_width > 0 <- type_width + field_width + generation_width,
         {:ok, object_numbers} <- xref_index_objects(index, size),
         true <- byte_size(data) == length(object_numbers) * row_width do
      object_numbers
      |> Enum.with_index()
      |> Enum.reduce_while({:ok, %{}}, fn {object, row}, {:ok, entries} ->
        record = binary_part(data, row * row_width, row_width)

        case xref_stream_entry(record, type_width, field_width, generation_width) do
          {:ok, entry} -> {:cont, {:ok, Map.put(entries, object, entry)}}
          {:error, _} = entry_error -> {:halt, entry_error}
        end
      end)
    else
      false -> error(:xref, :invalid_pdf_input, "xref stream dimensions are malformed")
      _ -> error(:xref, :invalid_pdf_input, "xref stream dictionary is malformed")
    end
  end

  defp xref_index_objects(index, size) do
    case index do
      index when is_list(index) and rem(length(index), 2) == 0 ->
        index
        |> Enum.chunk_every(2)
        |> Enum.reduce_while({:ok, [], MapSet.new()}, fn pair, {:ok, objects, seen} ->
          case pair do
            [first, count]
            when is_integer(first) and first >= 0 and is_integer(count) and count >= 0 and
                   first + count <= size ->
              range = if count == 0, do: [], else: Enum.to_list(first..(first + count - 1))

              if Enum.any?(range, &MapSet.member?(seen, &1)) do
                {:halt, error(:xref, :invalid_pdf_input, "xref stream Index ranges overlap")}
              else
                {:cont, {:ok, objects ++ range, Enum.reduce(range, seen, &MapSet.put(&2, &1))}}
              end

            _ ->
              {:halt, error(:xref, :invalid_pdf_input, "xref stream Index is malformed")}
          end
        end)
        |> case do
          {:ok, objects, _seen} -> {:ok, objects}
          {:error, _} = index_error -> index_error
        end

      _ ->
        error(:xref, :invalid_pdf_input, "xref stream Index is malformed")
    end
  end

  defp xref_stream_entry(record, type_width, field_width, generation_width) do
    <<type_bytes::binary-size(type_width), field_bytes::binary-size(field_width),
      generation_bytes::binary-size(generation_width)>> = record

    type = if type_width == 0, do: 1, else: :binary.decode_unsigned(type_bytes)
    field = if field_width == 0, do: 0, else: :binary.decode_unsigned(field_bytes)
    generation = if generation_width == 0, do: 0, else: :binary.decode_unsigned(generation_bytes)

    case type do
      0 when generation <= 65_535 ->
        {:ok, {:free, field, generation}}

      1 when generation <= 65_535 ->
        {:ok, {:uncompressed, field, generation}}

      type when type in [0, 1] ->
        error(:xref, :invalid_pdf_input, "xref stream generation is malformed")

      2 ->
        {:ok, {:compressed, field, generation}}

      _ ->
        error(:xref, :unsupported_pdf_feature, "xref stream entry type is unsupported")
    end
  end

  defp parse_optional_stream(tokens) do
    case tokens do
      [:stream, {:stream_data, stream}, :endstream | rest] -> {:ok, stream, rest}
      [:stream | _] -> :error
      rest -> {:ok, nil, rest}
    end
  end

  defp parse_value(tokens) do
    case tokens do
      [{:int, first}, {:int, second}, :R | rest] -> {:ok, {:ref, {first, second}}, rest}
      [{:int, value} | rest] -> {:ok, value, rest}
      [{:real, value} | rest] -> {:ok, value, rest}
      [{:name, name} | rest] -> {:ok, {:name, name}, rest}
      [{:string, value} | rest] -> {:ok, {:string, value}, rest}
      [{:hex_string, value} | rest] -> {:ok, {:hex, value}, rest}
      [true | rest] -> {:ok, true, rest}
      [false | rest] -> {:ok, false, rest}
      [:null | rest] -> {:ok, nil, rest}
      [:lbracket | rest] -> parse_array(rest, [])
      [:dict_start | rest] -> parse_dictionary(rest, %{})
      _ -> :error
    end
  end

  defp parse_array(tokens, values) do
    case tokens do
      [:rbracket | rest] ->
        {:ok, Enum.reverse(values), rest}

      _ ->
        with {:ok, value, rest} <- parse_value(tokens) do
          parse_array(rest, [value | values])
        end
    end
  end

  defp parse_dictionary(tokens, dictionary) do
    case tokens do
      [:dict_end | rest] ->
        {:ok, dictionary, rest}

      [{:name, key} | rest] ->
        with {:ok, value, rest} <- parse_value(rest) do
          parse_dictionary(rest, Map.put(dictionary, key, value))
        end

      _ ->
        :error
    end
  end

  defp validate_xref(entries, trailer, pdf) do
    size = Map.get(trailer, "Size")

    cond do
      not is_integer(size) or size <= 0 or size > @max_objects + 1 ->
        error(:xref, :invalid_pdf_input, "xref Size is malformed")

      map_size(entries) > @max_objects ->
        error(:limits, :resource_limit_exceeded, "PDF object count exceeds the limit")

      not match?({:ref, _}, Map.get(trailer, "Root")) ->
        error(:trailer, :invalid_pdf_input, "trailer does not contain a catalog reference")

      Enum.any?(entries, fn {object, entry} ->
        object < 0 or object >= size or not valid_xref_entry?(entry, pdf, size)
      end) ->
        error(:xref, :invalid_pdf_input, "xref entry is outside its declared bounds")

      true ->
        :ok
    end
  end

  defp valid_xref_entry?(entry, pdf, size) do
    case entry do
      {:free, next, generation} ->
        next >= 0 and next < size and generation in 0..65_535

      {:uncompressed, offset, generation} ->
        offset >= 0 and offset < byte_size(pdf) and generation in 0..65_535

      {:compressed, object_stream, index} ->
        object_stream > 0 and object_stream < size and index >= 0
    end
  end

  defp load_objects(pdf, xref) do
    uncompressed =
      xref
      |> Enum.flat_map(fn
        {object, {:uncompressed, offset, generation}} -> [{offset, object, generation}]
        _ -> []
      end)
      |> Enum.sort()

    with {:ok, objects} <- load_uncompressed_objects(pdf, uncompressed, %{}),
         {:ok, objects} <- load_compressed_objects(xref, objects) do
      {:ok, objects}
    end
  end

  defp load_uncompressed_objects(pdf, pending, objects) do
    case pending do
      [] ->
        {:ok, objects}

      [{offset, object, generation} | rest] ->
        limit =
          case rest do
            [{next_offset, _next_object, _next_generation} | _] -> next_offset
            [] -> byte_size(pdf)
          end

        with {:ok, {^object, ^generation}, parsed} <-
               parse_indirect_object_at(pdf, offset, limit, {object, generation}) do
          load_uncompressed_objects(pdf, rest, Map.put(objects, {object, generation}, parsed))
        else
          {:ok, actual_ref, _parsed} ->
            error(
              :xref,
              :invalid_pdf_input,
              "xref entry points to object #{elem(actual_ref, 0)} #{elem(actual_ref, 1)} instead of #{object} #{generation}"
            )

          {:error, {reason, diagnostic}} ->
            {:error,
             {reason, Map.update!(diagnostic, :message, &"#{&1}; object #{object} #{generation}")}}
        end
    end
  end

  defp parse_indirect_object_at(pdf, offset, limit, _expected_ref) do
    if offset >= 0 and limit > offset and limit <= byte_size(pdf) do
      slice = binary_part(pdf, offset, limit - offset)
      header = Regex.run(~r/\A[\x00\t\n\f\r ]*(\d+)\s+(\d+)\s+obj\b/, slice)

      case header do
        [_, object_text, generation_text] ->
          object = String.to_integer(object_text)
          generation = String.to_integer(generation_text)
          tokens = Tokenizer.new(slice) |> Tokenizer.tokenize_all()

          [{:int, ^object}, {:int, ^generation}, :obj | rest] = tokens
          {body, tail} = Enum.split_while(rest, &(&1 != :endobj))

          with true <- generation in 0..65_535,
               [_endobj | _] <- tail,
               false <- Enum.any?(body, &match?({:error, _}, &1)),
               {:ok, value, value_rest} <- parse_value(body),
               {:ok, stream, []} <- parse_optional_stream(value_rest) do
            ref = {object, generation}
            {:ok, ref, %{value: value, stream: stream, offset: offset, tokens: body}}
          else
            _ -> error(:object, :invalid_pdf_input, "indirect object boundary is malformed")
          end

        _ ->
          error(:object, :invalid_pdf_input, "xref offset does not point to an indirect object")
      end
    else
      error(:xref, :invalid_pdf_input, "indirect object offset is outside the PDF")
    end
  end

  defp load_compressed_objects(xref, objects) do
    xref
    |> Enum.flat_map(fn
      {object, {:compressed, object_stream, index}} -> [{object, object_stream, index}]
      _ -> []
    end)
    |> Enum.group_by(fn {_object, object_stream, _index} -> object_stream end)
    |> Enum.reduce_while({:ok, objects}, fn {object_stream, requested}, {:ok, objects} ->
      with {:ok, stream_ref} <- object_stream_ref(xref, object_stream),
           %{value: dictionary, stream: stream} <- Map.get(objects, stream_ref),
           true <-
             is_map(dictionary) and is_binary(stream) and
               name?(Map.get(dictionary, "Type"), "ObjStm"),
           {:ok, decoded} <- decode_stream(stream, dictionary, %{objects: objects}, stream_ref),
           {:ok, contained} <- parse_object_stream(decoded, dictionary, stream_ref),
           {:ok, objects} <- add_compressed_objects(requested, contained, objects, stream_ref) do
        {:cont, {:ok, objects}}
      else
        {:error, _} = stream_error ->
          {:halt, stream_error}

        _ ->
          {:halt,
           error(:object_stream, :invalid_pdf_input, "object stream is malformed",
             object: {object_stream, 0}
           )}
      end
    end)
  end

  defp object_stream_ref(xref, object_stream) do
    case Map.get(xref, object_stream) do
      {:uncompressed, _offset, generation} -> {:ok, {object_stream, generation}}
      _ -> error(:object_stream, :invalid_pdf_input, "object stream has no direct xref entry")
    end
  end

  defp parse_object_stream(data, dictionary, ref) do
    with count when is_integer(count) and count >= 0 <- Map.get(dictionary, "N"),
         first when is_integer(first) and first >= 0 and first <= byte_size(data) <-
           Map.get(dictionary, "First"),
         header <- binary_part(data, 0, first),
         numbers <-
           Regex.scan(~r/\d+/, header) |> List.flatten() |> Enum.map(&String.to_integer/1),
         true <- String.trim(Regex.replace(~r/\d+/, header, "")) == "",
         true <- length(numbers) == count * 2 do
      pairs = Enum.chunk_every(numbers, 2)

      pairs
      |> Enum.with_index()
      |> Enum.reduce_while({:ok, %{}}, fn {[object, relative_offset], index}, {:ok, acc} ->
        next_offset =
          case Enum.at(pairs, index + 1) do
            nil -> byte_size(data) - first
            [_next_object, next_relative_offset] -> next_relative_offset
          end

        length = next_offset - relative_offset

        if length >= 0 and first + relative_offset + length <= byte_size(data) do
          slice = binary_part(data, first + relative_offset, length)

          tokens = Tokenizer.new(slice) |> Tokenizer.tokenize_all()

          case parse_value(tokens) do
            {:ok, value, []} when object > 0 and not is_map_key(acc, object) ->
              parsed = %{value: value, stream: nil, offset: nil, tokens: tokens, index: index}
              {:cont, {:ok, Map.put(acc, object, parsed)}}

            _ ->
              {:halt,
               error(:object_stream, :invalid_pdf_input, "compressed object is malformed",
                 object: ref
               )}
          end
        else
          {:halt,
           error(:object_stream, :invalid_pdf_input, "compressed object offset is invalid",
             object: ref
           )}
        end
      end)
    else
      _ ->
        error(:object_stream, :invalid_pdf_input, "object stream header is invalid", object: ref)
    end
  end

  defp add_compressed_objects(requested, contained, objects, stream_ref) do
    Enum.reduce_while(requested, {:ok, objects}, fn {object, _object_stream, index},
                                                    {:ok, objects} ->
      case Enum.find(contained, fn {_contained_object, parsed} -> parsed.index == index end) do
        {^object, parsed} ->
          parsed = Map.delete(parsed, :index)
          {:cont, {:ok, Map.put(objects, {object, 0}, parsed)}}

        _ ->
          {:halt,
           error(
             :object_stream,
             :invalid_pdf_input,
             "compressed xref entry does not match object stream index #{index}",
             object: stream_ref
           )}
      end
    end)
  end

  defp reject_encryption(trailer) do
    if Map.has_key?(trailer, "Encrypt") do
      error(:encryption, :encrypted_pdf, "encrypted PDFs are not supported")
    else
      :ok
    end
  end

  defp resolve_reference(document, ref, seen) do
    if Map.has_key?(seen, ref) do
      error(:resolution, :invalid_pdf_input, "indirect reference chain contains a cycle",
        object: ref
      )
    else
      case Map.get(document.objects, ref) do
        %{value: {:ref, next_ref}} ->
          resolve_reference(document, next_ref, Map.put(seen, ref, true))

        %{value: value} ->
          {:ok, value}

        nil ->
          error(:resolution, :invalid_pdf_input, "indirect object reference is missing",
            object: ref
          )
      end
    end
  end

  defp resolve_stream_object(document, ref, seen) do
    if Map.has_key?(seen, ref) do
      error(:resolution, :invalid_pdf_input, "indirect stream reference contains a cycle",
        object: ref
      )
    else
      case Map.get(document.objects, ref) do
        %{value: {:ref, next_ref}, stream: nil} ->
          resolve_stream_object(document, next_ref, Map.put(seen, ref, true))

        %{stream: _stream} = object ->
          {:ok, ref, object}

        nil ->
          error(:resolution, :invalid_pdf_input, "stream reference is missing", object: ref)
      end
    end
  end

  defp collect_pages(objects, trailer) do
    document = %{objects: objects}

    with {:ok, catalog} <- resolve(document, Map.get(trailer, "Root")),
         true <- is_map(catalog),
         true <- name?(Map.get(catalog, "Type"), "Catalog"),
         {:ok, pages_ref} <- required_ref(catalog, "Pages"),
         {:ok, pages} <- walk_page_tree(document, pages_ref, nil, nil, nil, %{}, []) do
      {:ok, Enum.reverse(pages)}
    else
      false -> error(:page_tree, :invalid_pdf_input, "catalog object is malformed")
      {:error, _} = error -> error
    end
  end

  defp walk_page_tree(
         document,
         {:ref, ref} = page_ref,
         inherited_resources,
         inherited_rotate,
         inherited_media_box,
         seen,
         pages
       ) do
    cond do
      Map.has_key?(seen, ref) ->
        error(:page_tree, :invalid_pdf_input, "page tree contains a cycle", object: ref)

      length(pages) >= @max_pages ->
        error(:limits, :resource_limit_exceeded, "PDF page count exceeds the limit")

      true ->
        with {:ok, dictionary} <- dictionary(document, page_ref) do
          resources = Map.get(dictionary, "Resources", inherited_resources)
          rotate = Map.get(dictionary, "Rotate", inherited_rotate)
          media_box = Map.get(dictionary, "MediaBox", inherited_media_box)
          seen = Map.put(seen, ref, true)

          case Map.get(dictionary, "Type") do
            {:name, "Page"} ->
              {:ok,
               [
                 %{ref: ref, resources: resources, rotate: rotate, media_box: media_box}
                 | pages
               ]}

            {:name, "Pages"} ->
              walk_kids(
                document,
                Map.get(dictionary, "Kids"),
                resources,
                rotate,
                media_box,
                seen,
                pages
              )

            _ ->
              error(:page_tree, :invalid_pdf_input, "page tree node has an invalid Type",
                object: ref
              )
          end
        else
          {:error, _} = error ->
            error
        end
    end
  end

  defp walk_kids(document, kids, resources, rotate, media_box, seen, pages) do
    case kids do
      kids when is_list(kids) ->
        Enum.reduce_while(kids, {:ok, pages}, fn kid, {:ok, pages} ->
          case kid do
            {:ref, _} ->
              case walk_page_tree(document, kid, resources, rotate, media_box, seen, pages) do
                {:ok, pages} -> {:cont, {:ok, pages}}
                {:error, _} = page_error -> {:halt, page_error}
              end

            _ ->
              {:halt,
               error(:page_tree, :invalid_pdf_input, "Pages Kids array contains a non-reference")}
          end
        end)

      _ ->
        error(:page_tree, :invalid_pdf_input, "Pages node is missing a valid Kids array")
    end
  end

  defp required_ref(dictionary, key) do
    case Map.get(dictionary, key) do
      {:ref, _} = ref -> {:ok, ref}
      _ -> error(:page_tree, :invalid_pdf_input, "required #{key} reference is missing")
    end
  end

  defp decode_stream(stream, dictionary, document, ref) do
    with :ok <- validate_stream_length(stream, dictionary, document, ref),
         {:ok, filter_value} <- resolve(document, Map.get(dictionary, "Filter")),
         {:ok, parameter_value} <- resolve(document, Map.get(dictionary, "DecodeParms")),
         {:ok, filters} <- filter_list(filter_value),
         {:ok, parameters} <- parameter_list(document, parameter_value, length(filters)),
         {:ok, decoded} <-
           Enum.zip(filters, parameters)
           |> Enum.reduce_while({:ok, stream}, &decode_filter(&1, &2, ref)),
         :ok <- validate_decoded_size(stream, decoded) do
      {:ok, decoded}
    else
      {:error, _} = error -> error
    end
  end

  defp validate_stream_length(stream, dictionary, document, ref) do
    case Map.get(dictionary, "Length") do
      length when is_integer(length) and length == byte_size(stream) ->
        :ok

      {:ref, _} = length_ref ->
        with {:ok, length} <- resolve(document, length_ref),
             true <- is_integer(length) and length == byte_size(stream) do
          :ok
        else
          _ ->
            error(:stream, :invalid_pdf_input, "stream length does not match stream bytes",
              object: ref
            )
        end

      _ ->
        error(:stream, :invalid_pdf_input, "stream has an invalid Length", object: ref)
    end
  end

  defp filter_list(value) do
    case value do
      nil ->
        {:ok, []}

      {:name, filter} ->
        {:ok, [filter]}

      filters when is_list(filters) ->
        if Enum.all?(filters, &match?({:name, _}, &1)) do
          {:ok, Enum.map(filters, fn {:name, filter} -> filter end)}
        else
          error(:filter, :invalid_pdf_input, "Filter array is malformed")
        end

      _ ->
        error(:filter, :invalid_pdf_input, "Filter is malformed")
    end
  end

  defp parameter_list(document, parameters, count) do
    case parameters do
      nil ->
        {:ok, List.duplicate(nil, count)}

      parameters when is_map(parameters) and count == 1 ->
        {:ok, [parameters]}

      parameters when is_list(parameters) and length(parameters) == count ->
        parameters
        |> Enum.reduce_while({:ok, []}, fn parameter, {:ok, resolved} ->
          case resolve(document, parameter) do
            {:ok, parameter} when is_map(parameter) or is_nil(parameter) ->
              {:cont, {:ok, [parameter | resolved]}}

            _ ->
              {:halt, error(:filter, :invalid_pdf_input, "DecodeParms array is malformed")}
          end
        end)
        |> case do
          {:ok, resolved} -> {:ok, Enum.reverse(resolved)}
          {:error, _} = parameter_error -> parameter_error
        end

      _ ->
        error(:filter, :invalid_pdf_input, "DecodeParms does not match Filter")
    end
  end

  defp decode_filter({filter, parameters}, {:ok, data}, ref) do
    result =
      case filter do
        "FlateDecode" ->
          inflate(data, ref)

        "Fl" ->
          inflate(data, ref)

        "ASCIIHexDecode" ->
          ascii_hex(data, ref)

        "AHx" ->
          ascii_hex(data, ref)

        "ASCII85Decode" ->
          ascii85(data, ref)

        "A85" ->
          ascii85(data, ref)

        "RunLengthDecode" ->
          run_length(data, ref)

        "RL" ->
          run_length(data, ref)

        "LZWDecode" ->
          lzw(data, parameters, ref)

        "LZW" ->
          lzw(data, parameters, ref)

        _ ->
          error(:filter, :unsupported_pdf_feature, "unsupported PDF stream filter #{filter}",
            object: ref
          )
      end

    with {:ok, data} <- result,
         {:ok, data} <- apply_predictor(data, parameters, ref) do
      {:cont, {:ok, data}}
    else
      {:error, _} = error -> {:halt, error}
    end
  end

  defp inflate(data, ref) do
    zlib = :zlib.open()

    try do
      :ok = :zlib.inflateInit(zlib)

      limit =
        min(
          @max_decoded_stream_bytes,
          max(byte_size(data) * @max_decompression_ratio, 1)
        )

      inflate_chunks(zlib, data, limit, 0, [], ref)
    rescue
      _ -> error(:filter, :invalid_pdf_input, "FlateDecode data is invalid", object: ref)
    after
      :zlib.close(zlib)
    end
  end

  defp inflate_chunks(zlib, data, limit, decoded_size, decoded, ref) do
    {chunk, rest} =
      if byte_size(data) > 16_384 do
        {binary_part(data, 0, 16_384), binary_part(data, 16_384, byte_size(data) - 16_384)}
      else
        {data, <<>>}
      end

    output = :zlib.inflate(zlib, chunk)
    decoded_size = decoded_size + IO.iodata_length(output)

    cond do
      decoded_size > limit ->
        decoded_stream_limit_error(ref)

      rest == <<>> ->
        final = :zlib.inflate(zlib, <<>>)
        {:ok, IO.iodata_to_binary(Enum.reverse([final, output | decoded]))}

      true ->
        inflate_chunks(zlib, rest, limit, decoded_size, [output | decoded], ref)
    end
  end

  defp decoded_stream_limit_error(ref) do
    error(:limits, :resource_limit_exceeded, "decoded stream exceeds its safety limit",
      object: ref
    )
  end

  defp ascii_hex(data, ref) do
    cleaned = data |> :binary.bin_to_list() |> Enum.reject(&(&1 in [0, 9, 10, 12, 13, 32]))
    {digits, remainder} = Enum.split_while(cleaned, &(&1 != ?>))

    cond do
      remainder == [] ->
        error(:filter, :invalid_pdf_input, "ASCIIHexDecode data is missing its terminator",
          object: ref
        )

      Enum.any?(digits, &(not hex_digit?(&1))) ->
        error(:filter, :invalid_pdf_input, "ASCIIHexDecode data contains a non-hex digit",
          object: ref
        )

      true ->
        digits = if rem(length(digits), 2) == 1, do: digits ++ [?0], else: digits

        {:ok,
         digits
         |> Enum.chunk_every(2)
         |> Enum.map(fn [a, b] -> hex_value(a) * 16 + hex_value(b) end)
         |> :erlang.list_to_binary()}
    end
  end

  defp ascii85(data, ref) do
    data = data |> :binary.bin_to_list() |> Enum.reject(&(&1 in [0, 9, 10, 12, 13, 32]))
    data = if Enum.take(data, 2) == [?<, ?~], do: Enum.drop(data, 2), else: data

    case Enum.split_while(data, fn byte -> byte != ?~ end) do
      {body, [?~, ?> | _]} ->
        decode_ascii85(body, [], ref)

      _ ->
        error(:filter, :invalid_pdf_input, "ASCII85Decode data is missing its terminator",
          object: ref
        )
    end
  end

  defp decode_ascii85(bytes, acc, ref) do
    case {bytes, acc} do
      {[], acc} ->
        {:ok, acc |> Enum.reverse() |> IO.iodata_to_binary()}

      {[?z | rest], []} ->
        decode_ascii85(rest, [<<0, 0, 0, 0>>], ref)

      {[?z | _], _acc} ->
        error(:filter, :invalid_pdf_input, "ASCII85Decode z appears inside a group", object: ref)

      {bytes, acc} ->
        {group, rest} = Enum.split(bytes, min(5, length(bytes)))

        cond do
          Enum.any?(group, &(&1 < 33 or &1 > 117)) ->
            error(:filter, :invalid_pdf_input, "ASCII85Decode byte is out of range", object: ref)

          length(group) == 1 ->
            error(:filter, :invalid_pdf_input, "ASCII85Decode final group is too short",
              object: ref
            )

          true ->
            padded = group ++ List.duplicate(?u, 5 - length(group))
            value = Enum.reduce(padded, 0, fn byte, value -> value * 85 + byte - 33 end)
            <<decoded::binary-size(4)>> = <<value::unsigned-big-integer-size(32)>>
            take = if rest == [], do: length(group) - 1, else: 4
            decode_ascii85(rest, [binary_part(decoded, 0, take) | acc], ref)
        end
    end
  end

  defp run_length(data, ref) do
    limit =
      min(
        @max_decoded_stream_bytes,
        max(byte_size(data) * @max_decompression_ratio, 1)
      )

    decode_run_length(data, 0, limit, [], ref)
  end

  defp decode_run_length(bytes, decoded_size, limit, acc, ref) do
    case bytes do
      <<128, _rest::binary>> ->
        {:ok, acc |> Enum.reverse() |> IO.iodata_to_binary()}

      <<>> ->
        error(:filter, :invalid_pdf_input, "RunLengthDecode data has no end marker", object: ref)

      <<length, rest::binary>> when length <= 127 ->
        count = length + 1

        if byte_size(rest) >= count do
          <<literal::binary-size(count), rest::binary>> = rest
          decoded_size = decoded_size + count

          if decoded_size > limit do
            decoded_stream_limit_error(ref)
          else
            decode_run_length(rest, decoded_size, limit, [literal | acc], ref)
          end
        else
          error(:filter, :invalid_pdf_input, "RunLengthDecode literal run is truncated",
            object: ref
          )
        end

      <<length, byte, rest::binary>> ->
        count = 257 - length
        decoded_size = decoded_size + count

        if decoded_size > limit do
          decoded_stream_limit_error(ref)
        else
          decode_run_length(
            rest,
            decoded_size,
            limit,
            [:binary.copy(<<byte>>, count) | acc],
            ref
          )
        end

      _ ->
        error(:filter, :invalid_pdf_input, "RunLengthDecode repeat run is truncated", object: ref)
    end
  end

  defp lzw(data, parameters, ref) do
    early_change = if is_map(parameters), do: Map.get(parameters, "EarlyChange", 1), else: 1

    if early_change in [0, 1] do
      decode_lzw(data, early_change, ref)
    else
      error(:filter, :invalid_pdf_input, "LZW EarlyChange must be 0 or 1", object: ref)
    end
  end

  defp decode_lzw(data, early_change, ref) do
    dictionary = Enum.into(0..255, %{}, fn value -> {value, <<value>>} end)

    limit =
      min(
        @max_decoded_stream_bytes,
        max(byte_size(data) * @max_decompression_ratio, 1)
      )

    decode_lzw_bits(data, 0, 9, dictionary, 258, nil, early_change, 0, limit, [], ref)
  end

  defp decode_lzw_bits(
         data,
         bit_offset,
         width,
         dictionary,
         next_code,
         previous,
         early_change,
         decoded_size,
         limit,
         acc,
         ref
       ) do
    if bit_offset + width > bit_size(data) do
      error(:filter, :invalid_pdf_input, "LZWDecode data ends before its end code", object: ref)
    else
      <<_::bitstring-size(bit_offset), code::unsigned-big-integer-size(width), _::bitstring>> =
        data

      case code do
        256 ->
          decode_lzw_bits(
            data,
            bit_offset + width,
            9,
            Enum.into(0..255, %{}, fn value -> {value, <<value>>} end),
            258,
            nil,
            early_change,
            decoded_size,
            limit,
            acc,
            ref
          )

        257 ->
          {:ok, acc |> Enum.reverse() |> IO.iodata_to_binary()}

        _ ->
          value =
            case Map.fetch(dictionary, code) do
              {:ok, value} ->
                value

              :error when is_binary(previous) and code == next_code ->
                previous <> binary_part(previous, 0, 1)

              :error ->
                nil
            end

          if is_binary(value) do
            decoded_size = decoded_size + byte_size(value)

            if decoded_size > limit do
              error(:limits, :resource_limit_exceeded, "decoded stream exceeds its safety limit",
                object: ref
              )
            else
              {dictionary, next_code} =
                if previous && next_code < 4096 do
                  {Map.put(dictionary, next_code, previous <> binary_part(value, 0, 1)),
                   next_code + 1}
                else
                  {dictionary, next_code}
                end

              next_width =
                if width < 12 and next_code + early_change == 1 <<< width,
                  do: width + 1,
                  else: width

              decode_lzw_bits(
                data,
                bit_offset + width,
                next_width,
                dictionary,
                next_code,
                value,
                early_change,
                decoded_size,
                limit,
                [value | acc],
                ref
              )
            end
          else
            error(:filter, :invalid_pdf_input, "LZWDecode dictionary reference is invalid",
              object: ref
            )
          end
      end
    end
  end

  defp apply_predictor(data, parameters, ref) do
    case parameters do
      nil ->
        {:ok, data}

      parameters when is_map(parameters) ->
        case Map.get(parameters, "Predictor", 1) do
          1 ->
            {:ok, data}

          2 ->
            tiff_predictor(data, parameters, ref)

          predictor when predictor in 10..15 ->
            png_predictor(data, parameters, ref)

          _ ->
            error(:filter, :unsupported_pdf_feature, "unsupported stream predictor", object: ref)
        end
    end
  end

  defp tiff_predictor(data, parameters, ref) do
    with {:ok, row_bytes, _bytes_per_pixel} <- predictor_dimensions(parameters, ref),
         true <- rem(byte_size(data), row_bytes) == 0 do
      colors = Map.get(parameters, "Colors", 1)
      bits = Map.get(parameters, "BitsPerComponent", 8)
      columns = Map.get(parameters, "Columns", 1)

      rows =
        for <<row::binary-size(row_bytes) <- data>>,
          do: tiff_row(row, colors, bits, columns)

      {:ok, IO.iodata_to_binary(rows)}
    else
      false ->
        error(:filter, :invalid_pdf_input, "TIFF predictor rows are truncated", object: ref)

      {:error, _} = error ->
        error
    end
  end

  defp tiff_row(row, colors, bits, columns) do
    sample_count = colors * columns
    sample_bits = sample_count * bits
    padding_bits = bit_size(row) - sample_bits
    <<samples::bitstring-size(sample_bits), padding::bitstring-size(padding_bits)>> = row

    {decoded, _values} =
      samples
      |> then(fn samples -> for <<sample::unsigned-big-size(bits) <- samples>>, do: sample end)
      |> Enum.with_index()
      |> Enum.reduce({[], %{}}, fn {sample, index}, {decoded, values} ->
        value = rem(sample + Map.get(values, index - colors, 0), 1 <<< bits)
        {[value | decoded], Map.put(values, index, value)}
      end)

    decoded
    |> Enum.reverse()
    |> Enum.reduce(<<>>, fn sample, row ->
      <<row::bitstring, sample::unsigned-big-size(bits)>>
    end)
    |> then(fn decoded -> <<decoded::bitstring, padding::bitstring>> end)
  end

  defp png_predictor(data, parameters, ref) do
    with {:ok, row_bytes, bytes_per_pixel} <- predictor_dimensions(parameters, ref) do
      decode_png_rows(
        data,
        row_bytes,
        bytes_per_pixel,
        :binary.copy(<<0>>, row_bytes),
        [],
        ref
      )
    end
  end

  defp decode_png_rows(data, row_bytes, bytes_per_pixel, previous, acc, ref) do
    case data do
      <<>> ->
        {:ok, acc |> Enum.reverse() |> IO.iodata_to_binary()}

      _ ->
        if byte_size(data) >= row_bytes + 1 do
          <<filter, encoded::binary-size(row_bytes), rest::binary>> = data

          case png_row(filter, encoded, previous, bytes_per_pixel) do
            {:ok, row} ->
              decode_png_rows(rest, row_bytes, bytes_per_pixel, row, [row | acc], ref)

            :error ->
              error(:filter, :invalid_pdf_input, "PNG predictor row has an invalid filter",
                object: ref
              )
          end
        else
          error(:filter, :invalid_pdf_input, "PNG predictor rows are truncated", object: ref)
        end
    end
  end

  defp png_row(filter, encoded, previous, bytes_per_pixel) do
    case filter do
      filter when filter in 0..4 ->
        encoded
        |> :binary.bin_to_list()
        |> Enum.with_index()
        |> Enum.reduce({[], %{}}, fn {byte, index}, {decoded, values} ->
          left = Map.get(values, index - bytes_per_pixel, 0)
          up = :binary.at(previous, index)

          up_left =
            if index < bytes_per_pixel,
              do: 0,
              else: :binary.at(previous, index - bytes_per_pixel)

          value =
            case filter do
              0 -> byte
              1 -> rem(byte + left, 256)
              2 -> rem(byte + up, 256)
              3 -> rem(byte + div(left + up, 2), 256)
              4 -> rem(byte + paeth(left, up, up_left), 256)
            end

          {[value | decoded], Map.put(values, index, value)}
        end)
        |> then(fn {decoded, _values} ->
          {:ok, decoded |> Enum.reverse() |> :erlang.list_to_binary()}
        end)

      _ ->
        :error
    end
  end

  defp paeth(left, up, up_left) do
    prediction = left + up - up_left

    distances = [
      {abs(prediction - left), left},
      {abs(prediction - up), up},
      {abs(prediction - up_left), up_left}
    ]

    distances |> Enum.min_by(fn {distance, _} -> distance end) |> elem(1)
  end

  defp predictor_dimensions(parameters, ref) do
    colors = Map.get(parameters, "Colors", 1)
    bits = Map.get(parameters, "BitsPerComponent", 8)
    columns = Map.get(parameters, "Columns", 1)

    if is_integer(colors) and colors > 0 and bits in [1, 2, 4, 8, 16] and
         is_integer(columns) and columns > 0 do
      {:ok, div(colors * columns * bits + 7, 8), max(1, div(colors * bits + 7, 8))}
    else
      error(:filter, :invalid_pdf_input, "predictor dimensions are invalid", object: ref)
    end
  end

  defp validate_decoded_size(input, decoded) do
    cond do
      byte_size(decoded) > @max_decoded_stream_bytes ->
        error(:limits, :resource_limit_exceeded, "decoded stream exceeds the byte limit")

      byte_size(input) > 0 and byte_size(decoded) > byte_size(input) * @max_decompression_ratio ->
        error(:limits, :resource_limit_exceeded, "stream decompression ratio exceeds the limit")

      true ->
        :ok
    end
  end

  defp name?(value, expected) do
    case value do
      {:name, name} -> name == expected
      _ -> false
    end
  end

  defp hex_digit?(byte), do: byte in ?0..?9 or byte in ?A..?F or byte in ?a..?f

  defp hex_value(byte) do
    cond do
      byte in ?0..?9 -> byte - ?0
      byte in ?A..?F -> byte - ?A + 10
      true -> byte - ?a + 10
    end
  end

  defp error(stage, reason, message, details \\ []) do
    {pdf_details, diagnostic_options} = Keyword.split(details, [:object])

    message =
      Enum.reduce(pdf_details, message, fn {:object, {object, generation}}, message ->
        "#{message}; object #{object} #{generation}"
      end)

    Diagnostics.error(
      stage,
      reason,
      message,
      Keyword.merge([operation: :read, module: __MODULE__], diagnostic_options)
    )
  end
end
