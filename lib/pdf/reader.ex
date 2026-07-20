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
  @type object :: %{value: value(), stream: binary() | nil, offset: non_neg_integer() | nil}
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
             :ok <- validate_final_xref_pointer(pdf),
             {:ok, objects, trailers} <- parse_objects(pdf),
             {:ok, objects} <- expand_object_streams(objects),
             {:ok, trailer} <- final_trailer(trailers, objects),
             :ok <- reject_encryption(trailer),
             {:ok, pages} <- collect_pages(objects, trailer) do
          {:ok, %{binary: pdf, objects: objects, trailer: trailer, pages: pages}}
        else
          {:error, {_reason, _diagnostic}} = reader_error -> reader_error
          :error -> error(:structure, :invalid_pdf_input, "PDF syntax is malformed")
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
        case Map.get(document.objects, ref) do
          %{value: value} ->
            {:ok, value}

          nil ->
            error(:resolution, :invalid_pdf_input, "indirect object reference is missing",
              object: ref
            )
        end

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
        case Map.get(document.objects, ref) do
          %{value: dictionary, stream: stream}
          when is_map(dictionary) and is_binary(stream) ->
            decode_stream(stream, dictionary, document, ref)

          %{value: _value} ->
            error(:stream, :invalid_pdf_input, "object is not a stream", object: ref)

          nil ->
            error(:resolution, :invalid_pdf_input, "stream reference is missing", object: ref)
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

  defp validate_final_xref_pointer(pdf) do
    case Regex.scan(~r/startxref\s*(\d+)\s*%%EOF\s*\z/s, pdf) do
      [[_, offset_text]] ->
        {offset, ""} = Integer.parse(offset_text)

        if offset < byte_size(pdf) and xref_target?(pdf, offset) do
          :ok
        else
          error(
            :xref,
            :invalid_pdf_input,
            "final startxref offset does not point to an xref section"
          )
        end

      _ ->
        error(:xref, :invalid_pdf_input, "PDF is missing a valid final startxref section")
    end
  end

  defp xref_target?(pdf, offset) do
    tail = binary_part(pdf, offset, byte_size(pdf) - offset)
    String.starts_with?(tail, "xref") or Regex.match?(~r/\A\d+\s+\d+\s+obj(?:\s|\r|\n)/, tail)
  end

  defp parse_objects(pdf) do
    tokens = Tokenizer.new(pdf) |> Tokenizer.tokenize_all()

    if Enum.any?(tokens, &match?({:error, _}, &1)) do
      error(:syntax, :invalid_pdf_input, "PDF contains invalid lexical syntax")
    else
      scan_top_level(tokens, %{}, [])
    end
  end

  defp scan_top_level(tokens, objects, trailers) do
    case tokens do
      [] ->
        {:ok, objects, Enum.reverse(trailers)}

      [{:int, object}, {:int, generation}, :obj | rest]
      when object >= 0 and generation >= 0 ->
        with {:ok, value, rest} <- parse_value(rest),
             {:ok, stream, rest} <- parse_optional_stream(rest),
             [:endobj | rest] <- rest,
             true <- map_size(objects) < @max_objects do
          scan_top_level(
            rest,
            Map.put(objects, {object, generation}, %{value: value, stream: stream, offset: nil}),
            trailers
          )
        else
          false ->
            error(:limits, :resource_limit_exceeded, "PDF object count exceeds the limit")

          _ ->
            error(:object, :invalid_pdf_input, "indirect object boundary is malformed",
              object: {object, generation}
            )
        end

      [:trailer | rest] ->
        with {:ok, trailer, rest} <- parse_value(rest),
             true <- is_map(trailer) do
          scan_top_level(rest, objects, [trailer | trailers])
        else
          _ -> error(:trailer, :invalid_pdf_input, "trailer dictionary is malformed")
        end

      [_token | rest] ->
        scan_top_level(rest, objects, trailers)
    end
  end

  defp parse_optional_stream([:stream, {:stream_data, stream}, :endstream | rest]),
    do: {:ok, stream, rest}

  defp parse_optional_stream([:stream | _]), do: :error
  defp parse_optional_stream(rest), do: {:ok, nil, rest}

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

  defp parse_array([:rbracket | rest], values), do: {:ok, Enum.reverse(values), rest}

  defp parse_array(tokens, values) do
    with {:ok, value, rest} <- parse_value(tokens) do
      parse_array(rest, [value | values])
    end
  end

  defp parse_dictionary([:dict_end | rest], dictionary), do: {:ok, dictionary, rest}

  defp parse_dictionary([{:name, key} | rest], dictionary) do
    with {:ok, value, rest} <- parse_value(rest) do
      parse_dictionary(rest, Map.put(dictionary, key, value))
    end
  end

  defp parse_dictionary(_, _), do: :error

  defp expand_object_streams(objects) do
    Enum.reduce_while(objects, {:ok, objects}, fn {ref, %{value: dictionary, stream: stream}},
                                                  {:ok, acc} ->
      if is_map(dictionary) and is_binary(stream) and
           name?(Map.get(dictionary, "Type"), "ObjStm") do
        with {:ok, decoded} <- decode_stream(stream, dictionary, %{objects: acc}, ref),
             {:ok, expanded} <- parse_object_stream(decoded, dictionary, ref),
             true <- map_size(acc) + map_size(expanded) <= @max_objects do
          {:cont, {:ok, Map.merge(acc, expanded)}}
        else
          false ->
            {:halt,
             error(:limits, :resource_limit_exceeded, "PDF object count exceeds the limit")}

          {:error, _} = error ->
            {:halt, error}

          _ ->
            {:halt,
             error(:object_stream, :invalid_pdf_input, "object stream is malformed", object: ref)}
        end
      else
        {:cont, {:ok, acc}}
      end
    end)
  end

  defp parse_object_stream(data, dictionary, ref) do
    with count when is_integer(count) and count >= 0 <- Map.get(dictionary, "N"),
         first when is_integer(first) and first >= 0 and first <= byte_size(data) <-
           Map.get(dictionary, "First"),
         header <- binary_part(data, 0, first),
         numbers <-
           Regex.scan(~r/\d+/, header) |> List.flatten() |> Enum.map(&String.to_integer/1),
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

          case Tokenizer.new(slice) |> Tokenizer.tokenize_all() do
            tokens when is_list(tokens) ->
              case parse_value(tokens) do
                {:ok, value, []} ->
                  {:cont,
                   {:ok, Map.put(acc, {object, 0}, %{value: value, stream: nil, offset: nil})}}

                _ ->
                  {:halt,
                   error(:object_stream, :invalid_pdf_input, "compressed object is malformed",
                     object: ref
                   )}
              end
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

  defp final_trailer(trailers, objects) do
    case Enum.find(Enum.reverse(trailers), &Map.has_key?(&1, "Root")) do
      nil ->
        case objects
             |> Map.values()
             |> Enum.map(& &1.value)
             |> Enum.find(
               &(is_map(&1) and name?(Map.get(&1, "Type"), "XRef") and Map.has_key?(&1, "Root"))
             ) do
          nil ->
            error(:trailer, :invalid_pdf_input, "trailer does not contain a catalog reference")

          trailer ->
            {:ok, trailer}
        end

      trailer ->
        {:ok, trailer}
    end
  end

  defp reject_encryption(trailer) do
    if Map.has_key?(trailer, "Encrypt") do
      error(:encryption, :encrypted_pdf, "encrypted PDFs are not supported")
    else
      :ok
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

  defp walk_page_tree(
         _document,
         _page_ref,
         _resources,
         _rotate,
         _media_box,
         _seen,
         _pages
       ),
       do: :error

  defp walk_kids(document, kids, resources, rotate, media_box, seen, pages) do
    case kids do
      kids when is_list(kids) ->
        Enum.reduce_while(kids, {:ok, pages}, fn kid, {:ok, pages} ->
          case walk_page_tree(document, kid, resources, rotate, media_box, seen, pages) do
            {:ok, pages} -> {:cont, {:ok, pages}}
            {:error, _} = page_error -> {:halt, page_error}
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

  defp filter_list(nil), do: {:ok, []}
  defp filter_list({:name, filter}), do: {:ok, [filter]}

  defp filter_list(filters) when is_list(filters) do
    if Enum.all?(filters, &match?({:name, _}, &1)) do
      {:ok, Enum.map(filters, fn {:name, filter} -> filter end)}
    else
      error(:filter, :invalid_pdf_input, "Filter array is malformed")
    end
  end

  defp filter_list(_) do
    error(:filter, :invalid_pdf_input, "Filter is malformed")
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
        error(:limits, :resource_limit_exceeded, "decoded stream exceeds its safety limit",
          object: ref
        )

      rest == <<>> ->
        final = :zlib.inflate(zlib, <<>>)
        final_size = decoded_size + IO.iodata_length(final)

        if final_size <= limit do
          {:ok, IO.iodata_to_binary(Enum.reverse([final, output | decoded]))}
        else
          error(:limits, :resource_limit_exceeded, "decoded stream exceeds its safety limit",
            object: ref
          )
        end

      true ->
        inflate_chunks(zlib, rest, limit, decoded_size, [output | decoded], ref)
    end
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

  defp decode_ascii85([], acc, _ref), do: {:ok, acc |> Enum.reverse() |> IO.iodata_to_binary()}
  defp decode_ascii85([?z | rest], [], ref), do: decode_ascii85(rest, [<<0, 0, 0, 0>>], ref)

  defp decode_ascii85([?z | _], _group, ref),
    do: error(:filter, :invalid_pdf_input, "ASCII85Decode z appears inside a group", object: ref)

  defp decode_ascii85(bytes, acc, ref) do
    {group, rest} = Enum.split(bytes, min(5, length(bytes)))

    cond do
      Enum.any?(group, &(&1 < 33 or &1 > 117)) ->
        error(:filter, :invalid_pdf_input, "ASCII85Decode byte is out of range", object: ref)

      length(group) == 1 ->
        error(:filter, :invalid_pdf_input, "ASCII85Decode final group is too short", object: ref)

      true ->
        padded = group ++ List.duplicate(?u, 5 - length(group))
        value = Enum.reduce(padded, 0, fn byte, value -> value * 85 + byte - 33 end)
        <<bytes::binary-size(4)>> = <<value::unsigned-big-integer-size(32)>>
        take = if rest == [], do: length(group) - 1, else: 4
        decode_ascii85(rest, [binary_part(bytes, 0, take) | acc], ref)
    end
  end

  defp run_length(data, ref), do: run_length(:binary.bin_to_list(data), [], ref)
  defp run_length([128 | _], acc, _ref), do: {:ok, acc |> Enum.reverse() |> IO.iodata_to_binary()}

  defp run_length([], _acc, ref),
    do: error(:filter, :invalid_pdf_input, "RunLengthDecode data has no end marker", object: ref)

  defp run_length([length | rest], acc, ref) when length <= 127 do
    count = length + 1

    if length(rest) >= count do
      {bytes, rest} = Enum.split(rest, count)
      run_length(rest, [bytes | acc], ref)
    else
      error(:filter, :invalid_pdf_input, "RunLengthDecode literal run is truncated", object: ref)
    end
  end

  defp run_length([length, byte | rest], acc, ref) do
    run_length(rest, [List.duplicate(byte, 257 - length) | acc], ref)
  end

  defp run_length(_, _acc, ref),
    do: error(:filter, :invalid_pdf_input, "RunLengthDecode repeat run is truncated", object: ref)

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

    decode_lzw_bits(data, 0, 9, dictionary, nil, early_change, 0, limit, [], ref)
  end

  defp decode_lzw_bits(
         data,
         bit_offset,
         width,
         dictionary,
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
            Map.get(dictionary, code) || if previous, do: previous <> binary_part(previous, 0, 1)

          if is_binary(value) do
            decoded_size = decoded_size + byte_size(value)

            if decoded_size > limit do
              error(:limits, :resource_limit_exceeded, "decoded stream exceeds its safety limit",
                object: ref
              )
            else
              dictionary =
                if previous && map_size(dictionary) < 4096 do
                  Map.put(dictionary, map_size(dictionary), previous <> binary_part(value, 0, 1))
                else
                  dictionary
                end

              next_width =
                if width < 12 and map_size(dictionary) + early_change == 1 <<< width,
                  do: width + 1,
                  else: width

              decode_lzw_bits(
                data,
                bit_offset + width,
                next_width,
                dictionary,
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

  defp apply_predictor(data, nil, _ref), do: {:ok, data}

  defp apply_predictor(data, parameters, ref) when is_map(parameters) do
    case Map.get(parameters, "Predictor", 1) do
      1 -> {:ok, data}
      2 -> tiff_predictor(data, parameters, ref)
      predictor when predictor in 10..15 -> png_predictor(data, parameters, ref)
      _ -> error(:filter, :unsupported_pdf_feature, "unsupported stream predictor", object: ref)
    end
  end

  defp apply_predictor(_data, _parameters, ref),
    do: error(:filter, :invalid_pdf_input, "DecodeParms must be a dictionary", object: ref)

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

  defp decode_png_rows(<<>>, _row_bytes, _bytes_per_pixel, _previous, acc, _ref),
    do: {:ok, acc |> Enum.reverse() |> IO.iodata_to_binary()}

  defp decode_png_rows(data, row_bytes, bytes_per_pixel, previous, acc, ref) do
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

  defp png_row(filter, encoded, previous, bytes_per_pixel) when filter in 0..4 do
    encoded
    |> :binary.bin_to_list()
    |> Enum.with_index()
    |> Enum.reduce({[], %{}}, fn {byte, index}, {decoded, values} ->
      left = Map.get(values, index - bytes_per_pixel, 0)
      up = :binary.at(previous, index)

      up_left =
        if index < bytes_per_pixel, do: 0, else: :binary.at(previous, index - bytes_per_pixel)

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
  end

  defp png_row(_, _encoded, _previous, _bytes_per_pixel), do: :error

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

  defp name?({:name, name}, expected), do: name == expected
  defp name?(_, _), do: false
  defp hex_digit?(byte), do: byte in ?0..?9 or byte in ?A..?F or byte in ?a..?f
  defp hex_value(byte) when byte in ?0..?9, do: byte - ?0
  defp hex_value(byte) when byte in ?A..?F, do: byte - ?A + 10
  defp hex_value(byte), do: byte - ?a + 10

  defp error(stage, reason, message, details \\ []) do
    {pdf_details, diagnostic_options} = Keyword.split(details, [:object])

    message =
      Enum.reduce(pdf_details, message, fn detail, message ->
        case detail do
          {:object, {object, generation}} ->
            "#{message}; object #{object} #{generation}"

          _ ->
            message
        end
      end)

    Diagnostics.error(
      stage,
      reason,
      message,
      Keyword.merge([operation: :read, module: __MODULE__], diagnostic_options)
    )
  end
end
