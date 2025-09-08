defmodule NativeElixirPdfUtilities.Merge do
  @moduledoc """
  Minimal PDF utilities for merging PDF binaries using the tokenizer.

  Notes and constraints:
  - Emits a classic PDF 1.7 header and builds a fresh `xref` + `trailer`.
  - Copies all objects from inputs, renumbering to avoid collisions.
  - Adjusts indirect references (`n g R`) to the new numbering.
  - Collects Page objects and builds a new `Catalog` + `Pages` tree that references them.
  - Leaves stream bytes untouched and preserves declared `/Length` (direct or indirect ref),
    only renumbering indirect references as needed.
  - Does not decode filters or resolve xref streams; expects classic PDFs tokenizable by the tokenizer.

  The merger is conservative and pragmatic, targeting structural correctness for common PDFs.
  """

  alias NativeElixirPdfUtilities.Tokenizer

  @type pdf_bin :: binary()
  @typedoc "A single token as produced by `NativeElixirPdfUtilities.Tokenizer`."
  @type token :: Tokenizer.token()
  @typedoc "A list of PDF tokens."
  @type tokens :: [token()]
  @typedoc "Object record captured while indexing inputs."
  @type obj_rec :: %{obj: integer(), gen: integer(), tokens: tokens()}
  @typedoc "Mapping from original object id to new object id."
  @type id_map :: %{optional(integer()) => integer()}
  @typedoc "Byte-offset table for xref: object id -> {byte_offset, generation}."
  @type offsets_map :: %{optional(integer()) => {non_neg_integer(), non_neg_integer()}}

  @doc """
  Merge a list of PDF binaries into a single PDF binary.

  This is a best-effort merger for classic PDFs. It renumbers all objects of each input,
  collects Page objects, and emits a new Catalog/Pages tree that references all pages.
  """
  @spec merge([pdf_bin()]) :: {:ok, pdf_bin()}
  def merge([]), do: raise(ArgumentError, "merge/1 expects at least one PDF binary")

  def merge(bins) when is_list(bins) do
    # 1) Index all inputs -> objects and page ids
    inputs = Enum.map(bins, &index_pdf/1)

    # 2) Assign id offsets so object ids won't collide; reserve 1,2 for Pages,Catalog
    {inputs2, _next_id} = assign_offsets(inputs, 3)

    # 3) Collect all page ids in new numbering (flatten Pages)
    page_ids =
      inputs2
      |> Enum.flat_map(fn %{pages: pages, map: map} ->
        Enum.map(pages, &Map.fetch!(map, &1))
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
    {pieces, offsets, pos} = add_object(pieces, offsets, pos, catalog_obj_id, 0, render_catalog)

    {pieces, offsets, pos} =
      Enum.reduce(inputs2, {pieces, offsets, pos}, fn input = %{objects: objs, map: map}, acc ->
        Enum.reduce(objs, acc, fn obj, {pieces, offsets, pos} ->
          new_id = Map.fetch!(map, obj.obj)
          page_ctx = page_injection_ctx(obj.tokens, input, pages_obj_id)
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
    st = Tokenizer.new(bin)
    toks = Tokenizer.tokenize_all(st)

    {objects, pages} = parse_objects_and_pages(toks)
    root_pages = find_root_pages_id(objects)
    inherited = extract_inherited_from_root_pages(objects, root_pages)

    %{
      objects: objects,
      pages: pages,
      root_pages: root_pages,
      inherited: inherited,
      max_obj: Enum.reduce(objects, 0, fn o, acc -> max(acc, o.obj) end)
    }
  end

  # Parse the flat token stream into object records and collect Page object ids.
  defp parse_objects_and_pages(tokens) do
    parse_objects_and_pages(tokens, 0, [], [], nil)
  end

  # Internal worker for object scanning.
  defp parse_objects_and_pages(tokens, idx, objects, pages, current) do
    if idx >= length(tokens) do
      {Enum.reverse(objects), Enum.reverse(pages)}
    else
      case Enum.slice(tokens, idx, 3) do
        [{:int, obj}, {:int, gen}, :obj] ->
          # collect until :endobj (exclusive)
          {body, next_idx} = take_until_endobj(tokens, idx + 3, [])
          is_page = object_is_page?(body)
          pages2 = if is_page, do: [obj | pages], else: pages
          obj_rec = %{obj: obj, gen: gen, tokens: body}
          parse_objects_and_pages(tokens, next_idx + 1, [obj_rec | objects], pages2, current)

        _ ->
          parse_objects_and_pages(tokens, idx + 1, objects, pages, current)
      end
    end
  end

  # Take tokens until encountering :endobj (exclusive), returning collected tokens and index.
  defp take_until_endobj(tokens, idx, acc) do
    if idx >= length(tokens) do
      {Enum.reverse(acc), idx}
    else
      case Enum.at(tokens, idx) do
        :endobj -> {Enum.reverse(acc), idx}
        tok -> take_until_endobj(tokens, idx + 1, [tok | acc])
      end
    end
  end

  # Best-effort predicate to detect a Page dictionary by scanning for /Type /Page.
  defp object_is_page?(tokens) do
    # Best-effort: look for /Type /Page anywhere
    Enum.chunk_every(tokens, 2, 1, :discard)
    |> Enum.any?(fn
      [{:name, "Type"}, {:name, "Page"}] -> true
      _ -> false
    end)
  end

  # Find the object id of the root Pages tree via the Catalog's /Pages reference.
  defp find_root_pages_id(objects) do
    case Enum.find(objects, fn %{tokens: toks} ->
           Enum.chunk_every(toks, 2, 1, :discard)
           |> Enum.any?(fn
             [{:name, "Type"}, {:name, "Catalog"}] -> true
             _ -> false
           end)
         end) do
      nil -> nil
      %{tokens: toks} -> find_pages_ref_in_tokens(toks)
    end
  end

  # Look for '/Pages <obj> <gen> R' in a token sequence and return <obj>.
  defp find_pages_ref_in_tokens(tokens) do
    tokens
    |> Enum.chunk_every(5, 1, :discard)
    |> Enum.find_value(fn
      [{:name, "Pages"}, {:int, obj}, {:int, _gen}, :R, _] -> obj
      _ -> nil
    end)
  end

  # When there's no root pages id, return empty inherited attributes.
  defp extract_inherited_from_root_pages(_objects, nil), do: %{resources: nil, mediabox: nil}

  # Extract inherited attributes (/Resources and /MediaBox) from the root Pages object.
  defp extract_inherited_from_root_pages(objects, root_pages_id) do
    case Enum.find(objects, &(&1.obj == root_pages_id)) do
      nil ->
        %{resources: nil, mediabox: nil}

      %{tokens: toks} ->
        %{
          resources: find_value_after_name(toks, "Resources"),
          mediabox: find_value_after_name(toks, "MediaBox")
        }
    end
  end

  # Find the value tokens immediately following a given name key in a token list.
  defp find_value_after_name(tokens, name) do
    case Enum.split_while(tokens, fn t -> t != {:name, name} end) do
      {_prefix, []} ->
        nil

      {_prefix, [_name | rest]} ->
        case read_value_tokens(rest) do
          {val, _} when is_list(val) and val != [] -> val
          _ -> nil
        end
    end
  end

  # Read a single value (dict/array/ref/atom) from a token stream and also return the rest.
  defp read_value_tokens([:dict_start | rest]),
    do: take_until_matching_dict_end(rest, 1, [:dict_start])

  defp read_value_tokens([:lbracket | rest]),
    do: take_until_matching_bracket(rest, 1, [:lbracket])

  defp read_value_tokens([{:int, a}, {:int, b}, :R | rest]),
    do: {[{:int, a}, {:int, b}, :R], rest}

  defp read_value_tokens([tok | rest]), do: {[tok], rest}
  defp read_value_tokens([]), do: {[], []}

  # Collect tokens until the matching top-level :dict_end while preserving nesting.
  defp take_until_matching_dict_end(rest, 0, acc), do: {Enum.reverse([:dict_start | acc]), rest}
  defp take_until_matching_dict_end([], _n, acc), do: {Enum.reverse([:dict_start | acc]), []}

  defp take_until_matching_dict_end([:dict_start | r], n, acc),
    do: take_until_matching_dict_end(r, n + 1, [:dict_start | acc])

  defp take_until_matching_dict_end([:dict_end | r], 1, acc),
    do: {Enum.reverse([:dict_start | acc]) ++ [:dict_end], r}

  defp take_until_matching_dict_end([:dict_end | r], n, acc),
    do: take_until_matching_dict_end(r, n - 1, [:dict_end | acc])

  defp take_until_matching_dict_end([t | r], n, acc),
    do: take_until_matching_dict_end(r, n, [t | acc])

  # Collect tokens until the matching top-level :rbracket while preserving nesting.
  defp take_until_matching_bracket(rest, 0, acc), do: {Enum.reverse([:lbracket | acc]), rest}
  defp take_until_matching_bracket([], _n, acc), do: {Enum.reverse([:lbracket | acc]), []}

  defp take_until_matching_bracket([:lbracket | r], n, acc),
    do: take_until_matching_bracket(r, n + 1, [:lbracket | acc])

  defp take_until_matching_bracket([:rbracket | r], 1, acc),
    do: {Enum.reverse([:lbracket | acc]) ++ [:rbracket], r}

  defp take_until_matching_bracket([:rbracket | r], n, acc),
    do: take_until_matching_bracket(r, n - 1, [:rbracket | acc])

  defp take_until_matching_bracket([t | r], n, acc),
    do: take_until_matching_bracket(r, n, [t | acc])

  # Build a page-rewrite context for Page objects (to set Parent/Resources/MediaBox), else nil.
  defp page_injection_ctx(tokens, %{inherited: inh}, parent_id) do
    if object_is_page?(tokens) do
      %{parent_id: parent_id, resources_tokens: inh.resources, mediabox_tokens: inh.mediabox}
    else
      nil
    end
  end

  # Assign id ranges to each input to avoid collisions; returns inputs augmented with :map.
  defp assign_offsets(inputs, start_id) do
    Enum.map_reduce(inputs, start_id, fn %{max_obj: max_obj} = input, next_id ->
      base = next_id
      map = fn orig -> base + orig end
      # Build id map for all ids 1..max_obj that actually appear
      ids_present = MapSet.new(Enum.map(input.objects, & &1.obj))

      id_map =
        ids_present
        |> Enum.into(%{}, fn id -> {id, map.(id)} end)

      {Map.put(input, :map, id_map), base + max_obj + 1}
    end)
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
      |> Enum.map(fn id -> [Integer.to_string(id), " 0 R"] end)
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

  # Replace your existing rewrite_page_tokens/2 and helpers with this tighter version.

  # Rewrite top-level Page dictionary to set Parent, ensure /Type /Page, /Resources and /MediaBox.
  defp rewrite_page_tokens(tokens, %{
         parent_id: parent_id,
         resources_tokens: inh_res,
         mediabox_tokens: inh_mb
       }) do
    # We expect a single top-level dict in a Page object. Split it out, sanitize, and put it back.
    {dict_inner, before, afterr} = take_top_level_dict(tokens)

    dict_inner =
      dict_inner
      |> drop_key("Parent")
      |> put_key("Parent", [{:int, parent_id}, {:int, 0}, :R])
      |> ensure_type_page()
      |> ensure_resources(inh_res)
      |> ensure_mediabox(inh_mb || default_mediabox())
      |> drop_top_level_empty_arrays()

    before ++ [:dict_start | dict_inner] ++ [:dict_end | afterr]
  end

  # Extract the single top-level dictionary tokens (if present), plus leading/trailing tokens.
  defp take_top_level_dict([:dict_start | rest]), do: do_take_dict(rest, 1, [], [])
  defp take_top_level_dict(ts), do: {ts, [], []}

  # Worker for top-level dict extraction.
  defp do_take_dict([:dict_start | r], n, acc, before),
    do: do_take_dict(r, n + 1, [:dict_start | acc], before)

  defp do_take_dict([:dict_end | r], 1, acc, before),
    do: {Enum.reverse(acc), Enum.reverse(before), r}

  defp do_take_dict([:dict_end | r], n, acc, before),
    do: do_take_dict(r, n - 1, [:dict_end | acc], before)

  defp do_take_dict([t | r], n, acc, before), do: do_take_dict(r, n, [t | acc], before)

  # Drop a key (and its value) from a flat dict token list if present.
  defp drop_key(tokens, name) do
    case split_on_name(tokens, name) do
      {:ok, left, _val, right} -> left ++ right
      :error -> tokens
    end
  end

  # Put or replace a key with the given value tokens, appending near the end by default.
  defp put_key(tokens, name, value_tokens) do
    # Put/replace near the end so it’s visible
    case split_on_name(tokens, name) do
      {:ok, left, _old, right} -> left ++ [{:name, name} | value_tokens] ++ right
      :error -> tokens ++ [{:name, name} | value_tokens]
    end
  end

  # Ensure /Type /Page is set.
  defp ensure_type_page(tokens) do
    put_key(tokens, "Type", [{:name, "Page"}])
  end

  # Keep existing /Resources if non-empty; otherwise inject inherited /Resources when available.
  defp ensure_resources(tokens, inh_res) do
    case split_on_name(tokens, "Resources") do
      {:ok, left, val, right} when is_list(val) and val != [] ->
        left ++ [{:name, "Resources"} | val] ++ right

      _ ->
        if is_list(inh_res) and inh_res != [],
          do: put_key(tokens, "Resources", inh_res),
          else: tokens
    end
  end

  # Keep a valid /MediaBox array; otherwise use provided fallback.
  defp ensure_mediabox(tokens, fallback) do
    case split_on_name(tokens, "MediaBox") do
      {:ok, left, val, right} ->
        if valid_box?(val),
          do: left ++ [{:name, "MediaBox"} | val] ++ right,
          else: left ++ [{:name, "MediaBox"} | fallback] ++ right

      :error ->
        put_key(tokens, "MediaBox", fallback)
    end
  end

  # A4
  defp default_mediabox,
    do: [:lbracket, {:int, 0}, {:int, 0}, {:int, 595}, {:int, 842}, :rbracket]

  # Validate a box array [a b c d] of numbers.
  defp valid_box?([:lbracket, a, b, c, d, :rbracket])
       when is_tuple(a) and is_tuple(b) and is_tuple(c) and is_tuple(d),
       do: true

  defp valid_box?(_), do: false

  # Drop empty [] arrays at the top level in a dict, unless they are a value for a key.
  defp drop_top_level_empty_arrays(tokens) do
    # within top-level dict (no nested dicts/arrays here), drop any [] that is not a value after a name
    do_drop_empty_arrays(tokens, 0, false, [])
  end

  # Worker for empty-array dropping.
  defp do_drop_empty_arrays([], _depth, _after_name?, acc), do: Enum.reverse(acc)

  defp do_drop_empty_arrays([:dict_start | r], d, a?, acc),
    do: do_drop_empty_arrays(r, d + 1, a?, [:dict_start | acc])

  defp do_drop_empty_arrays([:dict_end | r], d, a?, acc),
    do: do_drop_empty_arrays(r, max(d - 1, 0), a?, [:dict_end | acc])

  defp do_drop_empty_arrays([:lbracket, :rbracket | r], 1, false, acc),
    do: do_drop_empty_arrays(r, 1, false, acc)

  defp do_drop_empty_arrays([:lbracket, :rbracket | r], d, a?, acc),
    do: do_drop_empty_arrays(r, d, a?, [:lbracket, :rbracket | acc])

  defp do_drop_empty_arrays([{:name, n} | r], d, _a?, acc),
    do: do_drop_empty_arrays(r, d, true, [{:name, n} | acc])

  defp do_drop_empty_arrays([t | r], d, _a?, acc),
    do: do_drop_empty_arrays(r, d, false, [t | acc])

  # Split a flat dict token list on a name key; returns left, value tokens, right or :error.
  defp split_on_name(tokens, name) do
    # returns {:ok, left, value_tokens, right} or :error
    case Enum.split_while(tokens, fn t -> t != {:name, name} end) do
      {_left, []} ->
        :error

      {left, [_name | rest]} ->
        {val, rest2} = read_value_tokens(rest)
        {:ok, left, val, rest2}
    end
  end

  # Replace your whole render_tokens/do_render_tokens block with this:

  # Render tokens back into iodata while remapping indirect references using id_map.
  defp render_tokens(tokens, id_map) do
    do_render_tokens(tokens, id_map, [], nil) |> Enum.reverse()
  end

  # last_name: nil or the most recent name (e.g., "Parent")
  defp do_render_tokens([], _id_map, acc, _last_name), do: acc

  defp do_render_tokens([{:name, n} | rest], id_map, acc, _last_name) do
    do_render_tokens(rest, id_map, [["/", n] | add_sep(acc)], n)
  end

  # Indirect ref: don't remap when it's the value of /Parent
  defp do_render_tokens([{:int, a}, {:int, b}, :R | rest], id_map, acc, last_name) do
    new_a = if last_name == "Parent", do: a, else: Map.get(id_map, a, a)
    io = [Integer.to_string(new_a), " ", Integer.to_string(b), " R"]
    do_render_tokens(rest, id_map, [io | add_sep(acc)], nil)
  end

  # Tolerant path in case R came through as an operator token
  defp do_render_tokens([{:int, a}, {:int, b}, {:op, "R"} | rest], id_map, acc, last_name) do
    new_a = if last_name == "Parent", do: a, else: Map.get(id_map, a, a)
    io = [Integer.to_string(new_a), " ", Integer.to_string(b), " R"]
    do_render_tokens(rest, id_map, [io | add_sep(acc)], nil)
  end

  defp do_render_tokens([:dict_start | rest], id_map, acc, _last_name),
    do: do_render_tokens(rest, id_map, ["<<" | add_sep(acc)], nil)

  defp do_render_tokens([:dict_end | rest], id_map, acc, _last_name),
    do: do_render_tokens(rest, id_map, [">>" | add_sep(acc)], nil)

  defp do_render_tokens([:lbracket | rest], id_map, acc, _last_name),
    do: do_render_tokens(rest, id_map, ["[" | add_sep(acc)], nil)

  defp do_render_tokens([:rbracket | rest], id_map, acc, _last_name),
    do: do_render_tokens(rest, id_map, ["]" | add_sep(acc)], nil)

  defp do_render_tokens(
         [:stream, {:stream_data, data}, :endstream | rest],
         id_map,
         acc,
         _last_name
       ) do
    io = ["\nstream\n", data, "\nendstream"]
    do_render_tokens(rest, id_map, [io | acc], nil)
  end

  defp do_render_tokens([{:string, s} | rest], id_map, acc, _last_name) do
    do_render_tokens(rest, id_map, [["(", escape_literal(s), ")"] | add_sep(acc)], nil)
  end

  defp do_render_tokens([{:hex_string, s} | rest], id_map, acc, _last_name) do
    do_render_tokens(rest, id_map, [["<", to_hex(s), ">"] | add_sep(acc)], nil)
  end

  defp do_render_tokens([{:int, i} | rest], id_map, acc, _last_name) do
    do_render_tokens(rest, id_map, [Integer.to_string(i) | add_sep(acc)], nil)
  end

  defp do_render_tokens([{:real, f} | rest], id_map, acc, _last_name) do
    s = format_pdf_real(f)
    do_render_tokens(rest, id_map, [s | add_sep(acc)], nil)
  end

  defp do_render_tokens([:R | rest], id_map, acc, _last_name),
    do: do_render_tokens(rest, id_map, ["R" | add_sep(acc)], nil)

  defp do_render_tokens([:obj | rest], id_map, acc, _last_name),
    do: do_render_tokens(rest, id_map, acc, nil)

  defp do_render_tokens([:endobj | rest], id_map, acc, _last_name),
    do: do_render_tokens(rest, id_map, acc, nil)

  defp do_render_tokens([:xref | rest], id_map, acc, _last_name),
    do: do_render_tokens(rest, id_map, acc, nil)

  defp do_render_tokens([:trailer | rest], id_map, acc, _last_name),
    do: do_render_tokens(rest, id_map, acc, nil)

  defp do_render_tokens([:startxref | rest], id_map, acc, _last_name),
    do: do_render_tokens(rest, id_map, acc, nil)

  defp do_render_tokens([true | rest], id_map, acc, _last_name),
    do: do_render_tokens(rest, id_map, ["true" | add_sep(acc)], nil)

  defp do_render_tokens([false | rest], id_map, acc, _last_name),
    do: do_render_tokens(rest, id_map, ["false" | add_sep(acc)], nil)

  defp do_render_tokens([:null | rest], id_map, acc, _last_name),
    do: do_render_tokens(rest, id_map, ["null" | add_sep(acc)], nil)

  defp do_render_tokens([{:op, word} | rest], id_map, acc, _last_name) do
    do_render_tokens(rest, id_map, [word | add_sep(acc)], nil)
  end

  # Add a separating space in the output unless at the beginning.
  defp add_sep([]), do: []
  defp add_sep(acc), do: [" " | acc]

  # Ensure reals are rendered as plain decimal (no scientific notation)
  defp format_pdf_real(f) when is_float(f) do
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

  defp trim_trailing_zeros_and_dot(bin) when is_binary(bin) do
    case String.split(bin, ".", parts: 2) do
      [int] ->
        int

      [int, frac] ->
        frac2 = String.replace_trailing(frac, "0", "")

        cond do
          frac2 == "" -> int
          true -> int <> "." <> frac2
        end
    end
  end

  # Escape a literal string for inclusion in (...) with PDF-compliant escapes.
  defp escape_literal(bin) when is_binary(bin) do
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
