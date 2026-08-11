defmodule NativeElixirPdfUtilities.Merge do
  @moduledoc """
  PDF utilities for merging documents through the shared native reader.

  Notes and constraints:
  - Emits a classic PDF 1.7 header and builds a fresh `xref` + `trailer`.
  - Resolves classic xref tables, xref streams, and object streams before copying
    active objects with fresh identifiers.
  - Adjusts indirect references (`n g R`) to the new numbering.
  - Collects Page objects and builds a new `Catalog` + `Pages` tree that references them.
  - Leaves stream bytes untouched and preserves declared `/Length` (direct or indirect ref),
    only renumbering indirect references as needed.

  The merger is conservative and pragmatic, targeting structural correctness for common PDFs.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Tokenizer
  alias NativeElixirPdfUtilities.Validators.MergeValidator

  @type pdf_bin :: binary()
  @typedoc "A single token as produced by `NativeElixirPdfUtilities.Tokenizer`."
  @type token :: Tokenizer.token()
  @typedoc "A list of PDF tokens."
  @type tokens :: [token()]
  @typedoc "Object record captured while indexing inputs."
  @type obj_rec :: %{obj: integer(), gen: integer(), tokens: tokens(), value: Reader.value()}
  @typedoc "Mapping from original object id to new object id."
  @type id_map :: %{optional(integer()) => integer()}
  @typedoc "Byte-offset table for xref: object id -> {byte_offset, generation}."
  @type offsets_map :: %{optional(integer()) => {non_neg_integer(), non_neg_integer()}}
  @type error_reason :: :empty_pdf_list | Reader.error_reason()

  @doc """
  Merge a list of PDF binaries into a single PDF binary.

  It resolves active objects through the shared reader, renumbers them, collects
  Page objects, and emits a new Catalog/Pages tree referencing all input pages.
  """
  @spec merge([pdf_bin()]) ::
          {:ok, pdf_bin()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def merge(bins) do
    case bins do
      [] ->
        Diagnostics.error(:merge, :empty_pdf_list, "merge/1 expects at least one PDF binary",
          operation: :merge,
          module: __MODULE__
        )

      bins when is_list(bins) ->
        case Enum.all?(bins, &is_binary/1) do
          true ->
            do_merge(bins)

          false ->
            Diagnostics.error(
              :merge,
              :invalid_pdf_input,
              "merge/1 expects a list of PDF binaries",
              operation: :merge,
              module: __MODULE__
            )
        end

      _ ->
        Diagnostics.error(:merge, :invalid_pdf_input, "merge/1 expects a list of PDF binaries",
          operation: :merge,
          module: __MODULE__
        )
    end
  end

  defp do_merge(bins) do
    case Enum.reduce_while(bins, {:ok, []}, fn bin, {:ok, inputs} ->
           case index_pdf(bin) do
             {:ok, input} ->
               {:cont, {:ok, [input | inputs]}}

             {:reader_error, {reason, diagnostic}} ->
               {:halt, {:reader_error, {reason, diagnostic}}}

             {:error, {reason, diagnostic}} ->
               {:halt, {:error, {reason, diagnostic}}}
           end
         end) do
      {:ok, inputs} ->
        with {:ok, inputs} <-
               inputs |> Enum.reverse() |> MergeValidator.prepare_remapping(3) do
          build_merged_pdf(inputs)
        end

      {:reader_error, {reader_reason, diagnostic}} ->
        {:error,
         {reader_reason,
          diagnostic
          |> Map.put(:operation, :merge)
          |> Map.put(:module, __MODULE__)}}

      {:error, {merge_reason, diagnostic}} ->
        Diagnostics.error(
          :merge,
          :invalid_pdf_input,
          "merge/1 received an invalid PDF (#{merge_reason} at #{diagnostic.stage}): #{diagnostic.message}",
          operation: :merge,
          module: __MODULE__,
          source: Map.get(diagnostic, :source)
        )
    end
  end

  defp build_merged_pdf(inputs) do
    # 3) Collect all page ids in new numbering (flatten Pages)
    page_ids =
      inputs
      |> Enum.flat_map(fn %{pages: pages, map: map} ->
        Enum.map(pages, fn {object, generation} ->
          {Map.fetch!(map, {object, generation}), generation}
        end)
      end)
      |> Enum.reduce({[], MapSet.new()}, fn id, {acc, seen} ->
        if MapSet.member?(seen, id), do: {acc, seen}, else: {[id | acc], MapSet.put(seen, id)}
      end)
      |> then(fn {acc, _} -> Enum.reverse(acc) end)

    pages_obj_id = 1
    catalog_obj_id = 2

    # 4) Render all objects with rewritten refs
    # We'll render: new Pages, new Catalog, then all rewritten input objects
    {pieces, offsets, pos} = add_piece([], pdf_header(), %{}, 0)
    render_pages = render_pages_object(pages_obj_id, page_ids)
    {pieces, offsets, pos} = add_object(pieces, offsets, pos, pages_obj_id, 0, render_pages)
    render_catalog = render_catalog_object(catalog_obj_id, pages_obj_id)

    {pieces, offsets, pos} =
      add_object(pieces, offsets, pos, catalog_obj_id, 0, render_catalog)

    {pieces, offsets, pos} =
      Enum.reduce(inputs, {pieces, offsets, pos}, fn input = %{objects: objs, map: map}, acc ->
        Enum.reduce(objs, acc, fn obj, {pieces, offsets, pos} ->
          new_id = Map.fetch!(map, {obj.obj, obj.gen})
          page_ctx = page_injection_ctx(obj, input, pages_obj_id)
          body = render_object_body(obj.tokens, map, page_ctx)
          add_object(pieces, offsets, pos, new_id, obj.gen, body)
        end)
      end)

    # 5) Xref + trailer
    max_obj_id = Enum.max([catalog_obj_id, pages_obj_id | Map.keys(offsets)])
    {xref_io, _xref_pos} = xref_and_trailer(offsets, pos, max_obj_id, catalog_obj_id)

    final_io = [Enum.reverse(pieces), xref_io]
    {:ok, IO.iodata_to_binary(final_io)}
  end

  # === Indexing ===

  # Index a PDF binary into objects, page ids and inherited attributes.
  defp index_pdf(bin) do
    case Reader.read_validated(bin) do
      {:ok, pdf_context} ->
        MergeValidator.prepare(pdf_context)

      {:error, error} ->
        {:reader_error, error}
    end
  end

  # Build a page-rewrite context for Page objects, else nil.
  defp page_injection_ctx(object, %{inherited: inheritances}, parent_id) do
    case Map.fetch(inheritances, object.obj) do
      {:ok, inherited} ->
        %{
          parent_id: parent_id,
          resources_tokens: inherited.resources,
          mediabox_tokens: inherited.mediabox,
          cropbox_tokens: inherited.cropbox,
          rotate_tokens: inherited.rotate
        }

      :error ->
        nil
    end
  end

  # === Rendering ===

  # Return a fixed classic PDF header with a binary-comment line.
  defp pdf_header do
    ["%PDF-1.7\n%\xE2\xE3\xCF\xD3\n"]
  end

  # Append a chunk to the output pieces and update the running byte position.
  defp add_piece(pieces, piece, offsets, pos) do
    len = :erlang.iolist_size(piece)
    {[piece | pieces], offsets, pos + len}
  end

  # Append a fully formatted object to the output, recording its starting offset.
  defp add_object(pieces, offsets, pos, id, gen, body_io) do
    header = [Integer.to_string(id), " ", Integer.to_string(gen), " obj\n"]
    footer = "\nendobj\n"
    piece = [header, body_io, footer]
    len = :erlang.iolist_size(piece)
    offsets2 = Map.put(offsets, id, {pos, gen})
    {[piece | pieces], offsets2, pos + len}
  end

  # Render the top-level Pages dictionary referencing all collected Page kids.
  defp render_pages_object(_pages_obj_id, page_ids) do
    kids_refs =
      page_ids
      |> Enum.map(fn {id, generation} ->
        [Integer.to_string(id), " ", Integer.to_string(generation), " R"]
      end)
      |> Enum.intersperse(" ")

    [
      "<< /Type /Pages /Kids [ ",
      kids_refs,
      " ] /Count ",
      Integer.to_string(length(page_ids)),
      " >>\n"
    ]
  end

  # Render the top-level Catalog referencing the generated Pages object.
  defp render_catalog_object(_catalog_obj_id, pages_obj_id) do
    [
      "<< /Type /Catalog /Pages ",
      Integer.to_string(pages_obj_id),
      " 0 R >>\n"
    ]
  end

  # Render an object's body while optionally rewriting Page dict content and remapping refs.
  defp render_object_body(tokens, id_map, page_ctx) do
    # If this is a Page dict, rewrite Parent and ensure Resources/MediaBox
    tokens2 =
      case page_ctx do
        nil -> tokens
        %{parent_id: _} -> rewrite_page_tokens(tokens, page_ctx)
      end

    # Replace indirect references with mapped ids; render tokens with spaces
    render_tokens(tokens2, id_map)
  end

  # Rewrite a Page dictionary with its new parent and effective inheritable attributes.
  defp rewrite_page_tokens(tokens, %{
         parent_id: parent_id,
         resources_tokens: inh_res,
         mediabox_tokens: inh_mb,
         cropbox_tokens: inh_crop,
         rotate_tokens: inh_rotate
       }) do
    # We expect a single top-level dict in a Page object. Split it out, sanitize, and put it back.
    [:dict_start | rest] = tokens
    {dict_inner, before, afterr} = do_take_dict(rest, 1, [], [])

    dict_inner =
      dict_inner
      |> drop_key("Parent")
      |> put_key("Parent", [{:generated_reference, parent_id}])
      |> ensure_type_page()
      |> ensure_resources(inh_res)
      |> put_key("MediaBox", inh_mb)
      |> put_optional_key("CropBox", inh_crop)
      |> put_optional_key("Rotate", inh_rotate)

    before ++ [:dict_start | dict_inner] ++ [:dict_end | afterr]
  end

  # Worker for top-level dict extraction.
  defp do_take_dict(tokens, depth, acc, before) do
    case tokens do
      [:dict_start | rest] ->
        do_take_dict(rest, depth + 1, [:dict_start | acc], before)

      [:dict_end | rest] when depth == 1 ->
        {Enum.reverse(acc), Enum.reverse(before), rest}

      [:dict_end | rest] ->
        do_take_dict(rest, depth - 1, [:dict_end | acc], before)

      [token | rest] ->
        do_take_dict(rest, depth, [token | acc], before)
    end
  end

  # Drop a key (and its value) from a flat dict token list if present.
  defp drop_key(tokens, name) do
    case MergeValidator.split_dictionary_value(tokens, name) do
      {:ok, left, _val, right} -> left ++ right
      :error -> tokens
    end
  end

  # Put or replace a key with the given value tokens, appending near the end by default.
  defp put_key(tokens, name, value_tokens) do
    # Put/replace near the end so it’s visible
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

  # Ensure /Type /Page is set.
  defp ensure_type_page(tokens) do
    put_key(tokens, "Type", [{:name, "Page"}])
  end

  # Keep existing /Resources if non-empty; otherwise inject inherited /Resources when available.
  defp ensure_resources(tokens, inh_res) do
    case MergeValidator.split_dictionary_value(tokens, "Resources") do
      {:ok, left, val, right} when is_list(val) and val != [] ->
        left ++ [{:name, "Resources"} | val] ++ right

      _ ->
        if is_list(inh_res) and inh_res != [] do
          put_key(tokens, "Resources", inh_res)
        else
          tokens
        end
    end
  end

  # Render tokens back into iodata while remapping indirect references using id_map.
  defp render_tokens(tokens, id_map) do
    do_render_tokens(tokens, id_map, [], nil) |> Enum.reverse()
  end

  defp do_render_tokens(tokens, id_map, acc, _last_name) do
    case tokens do
      [] ->
        acc

      [{:name, name} | rest] ->
        do_render_tokens(rest, id_map, [["/", encode_pdf_name(name)] | add_sep(acc)], name)

      [{:generated_reference, obj} | rest] ->
        io = [Integer.to_string(obj), " 0 R"]
        do_render_tokens(rest, id_map, [io | add_sep(acc)], nil)

      [{:int, obj}, {:int, gen}, :R | rest] ->
        new_obj = Map.fetch!(id_map, {obj, gen})
        io = [Integer.to_string(new_obj), " ", Integer.to_string(gen), " R"]
        do_render_tokens(rest, id_map, [io | add_sep(acc)], nil)

      [:dict_start | rest] ->
        do_render_tokens(rest, id_map, ["<<" | add_sep(acc)], nil)

      [:dict_end | rest] ->
        do_render_tokens(rest, id_map, [">>" | add_sep(acc)], nil)

      [:lbracket | rest] ->
        do_render_tokens(rest, id_map, ["[" | add_sep(acc)], nil)

      [:rbracket | rest] ->
        do_render_tokens(rest, id_map, ["]" | add_sep(acc)], nil)

      [:stream, {:stream_data, data}, :endstream | rest] ->
        do_render_tokens(rest, id_map, [["\nstream\n", data, "\nendstream"] | acc], nil)

      [{:string, string} | rest] ->
        do_render_tokens(rest, id_map, [["(", escape_literal(string), ")"] | add_sep(acc)], nil)

      [{:hex_string, string} | rest] ->
        do_render_tokens(rest, id_map, [["<", to_hex(string), ">"] | add_sep(acc)], nil)

      [{:int, int} | rest] ->
        do_render_tokens(rest, id_map, [Integer.to_string(int) | add_sep(acc)], nil)

      [{:real, real} | rest] ->
        do_render_tokens(rest, id_map, [format_pdf_real(real) | add_sep(acc)], nil)

      [true | rest] ->
        do_render_tokens(rest, id_map, ["true" | add_sep(acc)], nil)

      [false | rest] ->
        do_render_tokens(rest, id_map, ["false" | add_sep(acc)], nil)

      [:null | rest] ->
        do_render_tokens(rest, id_map, ["null" | add_sep(acc)], nil)
    end
  end

  # Add a separating space in the output unless at the beginning.
  defp add_sep(acc) do
    case acc do
      [] -> []
      _ -> [" " | acc]
    end
  end

  # PDF names use # followed by two hexadecimal digits for bytes that cannot be
  # written literally. Tokenization decodes those escapes, so rendering must
  # restore them to avoid changing token boundaries or the logical name value.
  defp encode_pdf_name(name) do
    for <<byte <- name>> do
      case byte do
        byte
        when byte >= 33 and byte <= 126 and
               byte not in [?#, ?(, ?), ?<, ?>, ?[, ?], ?{, ?}, ?/, ?%] ->
          <<byte>>

        byte ->
          ["#", :io_lib.format("~2.16.0B", [byte])]
      end
    end
  end

  # Ensure reals are rendered as plain decimal (no scientific notation)
  defp format_pdf_real(f) do
    # If the value is essentially an integer, emit as integer
    i = trunc(f)

    if abs(f - i) < 1.0e-9 do
      Integer.to_string(i)
    else
      # Fixed decimals with trimming
      s = :erlang.float_to_binary(f, [{:decimals, 10}])
      trim_trailing_zeros_and_dot(s)
    end
  end

  defp trim_trailing_zeros_and_dot(bin) do
    bin
    |> String.replace_trailing("0", "")
    |> String.replace_trailing(".", "")
  end

  # Escape a literal string for inclusion in (...) with PDF-compliant escapes.
  defp escape_literal(bin) do
    bin
    |> :binary.bin_to_list()
    |> Enum.map(fn
      ?\n ->
        "\\n"

      ?\r ->
        "\\r"

      ?\t ->
        "\\t"

      ?\b ->
        "\\b"

      ?\f ->
        "\\f"

      ?( ->
        "\\("

      ?) ->
        "\\)"

      ?\\ ->
        "\\\\"

      c when c < 32 or c > 126 ->
        # Use octal escape for non-printable
        :io_lib.format("\\~.3.0b", [c])

      c ->
        <<c>>
    end)
  end

  # Convert bytes to uppercase hex pairs iodata.
  defp to_hex(bin) do
    for <<c <- bin>>, into: [], do: :io_lib.format("~2.16.0B", [c])
  end

  # Build a classic xref table and trailer for the accumulated object offsets.
  defp xref_and_trailer(offsets, pos, max_id, root_id) do
    xref_pos = pos
    size = max_id + 1

    header = ["xref\n0 ", Integer.to_string(size), "\n"]

    all_ids = Enum.to_list(0..max_id)

    # Free ids are those without offsets (0 is always free)
    nonzero_free =
      all_ids
      |> Enum.reject(&(&1 == 0))
      |> Enum.filter(&(not Map.has_key?(offsets, &1)))

    # Build free-list mapping for nonzero free objects: id -> next_id (last points to 0)
    next_of =
      nonzero_free
      |> Enum.zip(Enum.drop(nonzero_free, 1) ++ [0])
      |> Map.new()

    # Object 0 must point to the first free object (or 0 if none)
    first_free = List.first(nonzero_free) || 0

    entries =
      Enum.map(all_ids, fn id ->
        case Map.fetch(offsets, id) do
          {:ok, {off, gen}} ->
            [pad10(off), " ", pad5(gen), " n \n"]

          :error when id == 0 ->
            [pad10(first_free), " 65535 f \n"]

          :error ->
            next = Map.get(next_of, id, 0)
            [pad10(next), " 00000 f \n"]
        end
      end)

    trailer = [
      "trailer\n<< /Size ",
      Integer.to_string(size),
      " /Root ",
      Integer.to_string(root_id),
      " 0 R >>\n",
      "startxref\n",
      Integer.to_string(xref_pos),
      "\n%%EOF\n"
    ]

    {[header, entries, trailer], xref_pos}
  end

  # Pad an integer to 10 digits with leading zeroes.
  defp pad10(int) do
    s = Integer.to_string(int)
    pad = 10 - byte_size(s)
    if pad > 0, do: :binary.copy("0", pad) <> s, else: s
  end

  # Pad an integer to 5 digits with leading zeroes.
  defp pad5(int) do
    s = Integer.to_string(int)
    pad = 5 - byte_size(s)
    if pad > 0, do: :binary.copy("0", pad) <> s, else: s
  end
end
