defmodule NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidator do
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme
  @moduledoc false

  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  @scalar_lists [:source_glyph_characters, :bidi_characters, :hyphenation_characters]

  @doc "Retains authored text identities in the actual issued native HTML subtree."
  @spec prepare_tree(map()) :: map()
  def prepare_tree(node) do
    HtmlValidator.reserve_render_resource(:max_work, 1, :font)

    case node do
      %{type: :text, text: text, style: style} ->
        %{node | style: Map.put(style, :_source_glyph_characters, source_characters(node, text))}

      %{children: children} ->
        %{node | children: Enum.map(children, &prepare_tree/1)}

      _ ->
        node
    end
  end

  @doc "Retains original text owners and scalar offsets through native Math text transformation."
  @spec source_characters(map(), String.t()) :: [map()] | nil
  def source_characters(node, displayed) do
    if Map.has_key?(node, :node_id) do
      owner = Map.drop(node, [:style, :children])

      Enum.zip(String.codepoints(displayed), String.codepoints(node.text))
      |> Enum.with_index()
      |> Enum.map(fn {{displayed, text}, offset} ->
        %{
          displayed: displayed,
          fragments: [%{owner: owner, text: text, offset: offset}]
        }
      end)
    end
  end

  @doc "Carries a validated field scalar through the shared character, whitespace and ligature pipeline without a DOM owner."
  @spec field_scalar_characters(map()) :: [map()]
  def field_scalar_characters(authority) do
    authority.scalar
    |> String.codepoints()
    |> Enum.with_index()
    |> Enum.map(fn {text, offset} ->
      %{displayed: text, fragments: [%{scalar_owner: authority, text: text, offset: offset}]}
    end)
  end

  @doc "Normalizes native text with the shared CSS whitespace operations."
  @spec normalize_text(String.t(), map()) :: String.t()
  def normalize_text(text, style) do
    # A single ordinary space is already normalized under every white-space mode.
    patterns =
      if Map.get(style, :white_space_collapse) == :discard,
        do: [" ", "\r", "\n", "\t", "\f"],
        else: ["\r", "\n", "\t", "\f", "  "]

    case :binary.match(text, patterns) do
      :nomatch ->
        text

      _ ->
        Enum.reduce(
          NativeElixirPdfUtilities.Validators.CssWhiteSpaceValidator.normalizations(style),
          text,
          fn {pattern, replacement}, text ->
            String.replace(text, pattern, replacement)
          end
        )
    end
  end

  @doc "Carries exact scalar provenance through the same whitespace normalization."
  @spec normalize_characters([map()], map()) :: [map()]
  def normalize_characters(characters, style) do
    Enum.reduce(
      NativeElixirPdfUtilities.Validators.CssWhiteSpaceValidator.normalizations(style),
      characters,
      fn {pattern, replacement}, characters ->
        text = Enum.map_join(characters, & &1.displayed)
        HtmlValidator.reserve_render_resource(:max_work, byte_size(text), :font)

        {result, remaining, cursor} =
          Regex.scan(pattern, text, return: :index)
          |> Enum.reduce({[], characters, 0}, fn [{offset, bytes}], {result, remaining, cursor} ->
            prefix_length =
              text |> binary_part(cursor, offset - cursor) |> String.codepoints() |> length()

            selected_length =
              text |> binary_part(offset, bytes) |> String.codepoints() |> length()

            {prefix, remaining} = Enum.split(remaining, prefix_length)
            {selected, remaining} = Enum.split(remaining, selected_length)

            record = %{
              displayed: replacement,
              fragments: Enum.flat_map(selected, & &1.fragments)
            }

            result = Enum.reverse(prefix, result)
            result = if replacement == "", do: result, else: [record | result]
            {result, remaining, offset + bytes}
          end)

        _ = cursor
        Enum.reverse(result) ++ remaining
      end
    )
  end

  @doc "Attaches the normalized source characters to an actual inline text run."
  @spec prepare_run(map(), String.t()) :: map()
  def prepare_run(node, text) do
    run = %{text: text, style: node.style}

    case Map.get(node.style, :_source_glyph_characters) do
      characters when is_list(characters) ->
        run
        |> Map.put(:style, Map.delete(run.style, :_source_glyph_characters))
        |> Map.put(:source_glyph_characters, normalize_characters(characters, node.style))

      _ ->
        run
    end
  end

  @doc "Slices an actual text run and its scalar provenance at the same UTF-8 byte boundaries."
  @spec slice_run(map(), non_neg_integer(), String.t()) :: map()
  def slice_run(run, offset, text) do
    result = Map.put(run, :text, text)

    result =
      if Map.has_key?(run, :_paragraph_end_length),
        do: Map.put(result, :_paragraph_offset, Map.get(run, :_paragraph_offset, 0) + offset),
        else: result

    if Enum.any?(@scalar_lists, &is_list(Map.get(run, &1))) do
      first = run.text |> binary_part(0, offset) |> String.codepoints() |> length()
      count = length(String.codepoints(text))

      Enum.reduce(@scalar_lists, result, fn field, result ->
        case Map.get(run, field) do
          characters when is_list(characters) ->
            Map.put(result, field, Enum.slice(characters, first, count))

          _ ->
            result
        end
      end)
    else
      result
    end
  end

  @doc "Partitions an admitted contiguous run while consuming parallel scalar ownership lists once."
  @spec partition_run(map(), [String.t()]) :: [map()]
  def partition_run(run, texts) do
    HtmlValidator.reserve_render_resource(:max_work, byte_size(run.text), :layout)
    fields = Map.take(run, @scalar_lists)

    Enum.map_reduce(texts, {fields, 0}, fn text, {remaining, offset} ->
      count = if map_size(fields) == 0, do: 0, else: length(String.codepoints(text))

      {selected, rest} =
        Enum.reduce(remaining, {%{}, %{}}, fn {field, characters}, {selected, rest} ->
          {part, tail} = Enum.split(characters, count)
          {Map.put(selected, field, part), Map.put(rest, field, tail)}
        end)

      part = run |> Map.put(:text, text) |> Map.merge(selected)

      part =
        if Map.has_key?(run, :_paragraph_end_length),
          do: Map.put(part, :_paragraph_offset, Map.get(run, :_paragraph_offset, 0) + offset),
          else: part

      {part, {rest, offset + byte_size(text)}}
    end)
    |> elem(0)
  end

  @doc "Splits a run at a known newline byte offset, consuming source scalars once."
  @spec split_line(map(), non_neg_integer()) :: {map(), map()}
  def split_line(run, offset) do
    <<part::binary-size(^offset), "\n", rest::binary>> = run.text
    {slice_run(run, 0, part), slice_run(run, offset + 1, rest)}
  end

  @doc "Removes selected scalar positions from an admitted run and all of its parallel source records."
  @spec remove_scalars(map(), MapSet.t(non_neg_integer())) :: map()
  def remove_scalars(run, positions) do
    text =
      run.text
      |> String.codepoints()
      |> Enum.with_index()
      |> Enum.reject(fn {_, index} -> MapSet.member?(positions, index) end)
      |> Enum.map_join(&elem(&1, 0))

    Enum.reduce(@scalar_lists, Map.put(run, :text, text), fn field, result ->
      case Map.get(run, field) do
        characters when is_list(characters) ->
          selected =
            characters
            |> Enum.with_index()
            |> Enum.reject(fn {_, index} -> MapSet.member?(positions, index) end)
            |> Enum.map(&elem(&1, 0))

          Map.put(result, field, selected)

        _ ->
          result
      end
    end)
  end

  @doc "Retains source scalar offsets when layout trims an actual run."
  @spec trim_run(map(), String.t()) :: map()
  def trim_run(run, text) do
    cond do
      run.text == text ->
        run

      Map.has_key?(run, :_paragraph_end_length) or
          Enum.any?(@scalar_lists, &is_list(Map.get(run, &1))) ->
        if text == "" do
          slice_run(run, 0, text)
        else
          {offset, _} = :binary.match(run.text, text)
          slice_run(run, offset, text)
        end

      true ->
        Map.put(run, :text, text)
    end
  end

  @doc "Retains original tab scalar owners through the actual tab-stop expansion."
  @spec expand_tabs(map(), String.t(), [non_neg_integer()]) :: map()
  def expand_tabs(run, text, counts) do
    result = Map.put(run, :text, text)

    Enum.reduce(@scalar_lists, result, fn field, result ->
      case Map.get(run, field) do
        characters when is_list(characters) ->
          {expanded, []} =
            Enum.map_reduce(characters, counts, fn character, remaining ->
              tab? =
                if field == :source_glyph_characters,
                  do: character.displayed == "\t",
                  else: character.codepoint == 9

              case {tab?, remaining} do
                {true, [count | remaining]} ->
                  selected =
                    if field == :source_glyph_characters,
                      do: %{character | displayed: " "},
                      else: %{character | codepoint: 32}

                  {List.duplicate(selected, count), remaining}

                _ ->
                  {[character], remaining}
              end
            end)

          Map.put(result, field, List.flatten(expanded))

        _ ->
          result
      end
    end)
  end

  @doc "Combines actual adjacent text runs while retaining every consumed source scalar."
  @spec merge_runs(map(), map()) :: map()
  def merge_runs(first, second) do
    result = Map.put(first, :text, first.text <> second.text)

    case first do
      first
      when not is_map_key(first, :source_glyph_characters) and
             not is_map_key(first, :bidi_characters) and
             not is_map_key(first, :hyphenation_characters) ->
        result

      _ ->
        Enum.reduce(@scalar_lists, result, fn field, result ->
          case {Map.get(first, field), Map.get(second, field)} do
            {left, right} when is_list(left) and is_list(right) ->
              Map.put(result, field, left ++ right)

            _ ->
              result
          end
        end)
    end
  end

  @doc "Maps actual selected-font ligature shaping to the consumed original scalar fragments."
  @spec shape_characters([map()], map(), number()) :: [map()]
  def shape_characters(characters, font, spacing) do
    text = Enum.map_join(characters, & &1.displayed)
    shaped = Font.shape_ligatures(text, font, spacing)

    {selected, []} =
      Enum.map_reduce(String.codepoints(shaped), characters, fn displayed, remaining ->
        <<codepoint::utf8>> = displayed

        count =
          if hd(remaining).displayed == displayed do
            1
          else
            {letters, _} =
              Enum.find(font.latin_ligatures, fn {letters, value} ->
                value == codepoint and
                  String.starts_with?(Enum.map_join(remaining, & &1.displayed), letters)
              end)

            length(String.codepoints(letters))
          end

        {consumed, remaining} = Enum.split(remaining, count)

        {%{
           displayed: displayed,
           fragments: Enum.flat_map(consumed, & &1.fragments),
           consumed: count
         }, remaining}
      end)

    selected
  end

  @doc "Returns source byte boundaries shared by extended graphemes and the selected font's actual shaping clusters."
  @spec cluster_boundaries(map()) :: [non_neg_integer()]
  def cluster_boundaries(run) do
    HtmlValidator.reserve_render_resource(:max_work, byte_size(run.text), :layout)

    if NativeElixirPdfUtilities.Validators.TextShapingValidator.required?(
         run.style.font_face,
         run.text
       ) do
      case Font.css_glyph_positions(
             run.text,
             run.style.font_face,
             Map.get(run.style, :font_size, 12),
             Map.get(run.style, :letter_spacing, 0)
           ) do
        {:ok, shaped} -> NativeElixirPdfUtilities.HtmlToPdf.TextShaper.cluster_boundaries(shaped)
        error -> throw({:render_resource_limit, error})
      end
    else
      scalars = String.codepoints(run.text)
      characters = Enum.map(scalars, &%{displayed: &1, fragments: []})

      shaped =
        shape_characters(
          characters,
          run.style.font_face,
          Map.get(run.style, :letter_spacing, 0.0)
        )

      {ends, _, _} =
        Enum.reduce(shaped, {[], scalars, 0}, fn cluster, {ends, remaining, offset} ->
          {consumed, remaining} = Enum.split(remaining, cluster.consumed)
          offset = offset + Enum.reduce(consumed, 0, &(byte_size(&1) + &2))
          {[offset | ends], remaining, offset}
        end)

      {grapheme_ends, _} =
        Enum.map_reduce(UnicodeGrapheme.graphemes(run.text), 0, fn grapheme, offset ->
          offset = offset + byte_size(grapheme)
          {offset, offset}
        end)

      boundaries = MapSet.new(ends)
      [0 | Enum.filter(grapheme_ends, &MapSet.member?(boundaries, &1))]
    end
  end

  @doc "Returns the actual embedded-font glyph clusters selected for a native paint text box."
  @spec paint_runs(map(), map(), number()) :: [map()]
  @spec paint_runs(map(), map(), number(), map() | nil) :: [map()]
  def paint_runs(run, face, spacing, display_plan \\ nil) do
    case {Map.get(run, :source_glyph_characters), face} do
      {characters, %{type: :embedded}} when is_list(characters) ->
        characters = display_characters(characters, display_plan)

        text = Enum.map_join(characters, & &1.displayed)

        selected =
          if NativeElixirPdfUtilities.Validators.TextShapingValidator.required?(face, text) do
            case Font.css_glyph_positions(
                   text,
                   face,
                   Map.get(Map.get(run, :style, %{}), :font_size, 12),
                   spacing,
                   characters: characters
                 ) do
              {:ok, shaped} ->
                shaped.glyphs
                |> Enum.reject(& &1.ignorable?)
                |> Enum.map(fn glyph ->
                  %{
                    displayed: <<hd(glyph.codepoints)::utf8>>,
                    fragments: glyph.fragments,
                    glyph_ids: [glyph.glyph]
                  }
                end)

              error ->
                throw({:render_resource_limit, error})
            end
          else
            shape_characters(characters, face, spacing)
          end

        selected
        |> Enum.map(fn character ->
          <<codepoint::utf8>> = character.displayed

          %{
            source_fragments: Enum.filter(character.fragments, &Map.has_key?(&1, :owner)),
            field_scalar_fragments:
              Enum.filter(character.fragments, &Map.has_key?(&1, :scalar_owner)),
            text: character.displayed,
            displayed_unicode: codepoint,
            glyph_ids:
              case Map.fetch(character, :glyph_ids) do
                {:ok, glyphs} -> glyphs
                :error -> [Map.fetch!(face.cmap, codepoint)]
              end
          }
        end)

      _ ->
        []
    end
  end

  @doc "Projects scalar owners through an admitted discretionary display plan."
  @spec display_characters([map()], map() | nil) :: [map()]
  def display_characters(characters, display_plan) do
    if display_plan && display_plan.discretionary? do
      visible =
        Enum.reject(characters, &(&1.displayed in ["\u00AD", "\u200B"]))
        |> Enum.map(fn character ->
          if character.displayed in ["\u2028", "\u2029"],
            do: %{character | displayed: " "},
            else: character
        end)

      prefix = Enum.map_join(visible, & &1.displayed)

      suffix =
        binary_part(
          display_plan.text,
          byte_size(prefix),
          byte_size(display_plan.text) - byte_size(prefix)
        )

      soft = List.last(characters)

      previous_level =
        case List.last(visible) do
          %{fragments: fragments} -> Enum.find_value(fragments, &Map.get(&1, :level))
          _ -> nil
        end

      extra =
        Enum.map(String.codepoints(suffix), fn scalar ->
          fragments =
            Enum.map(soft.fragments, fn fragment ->
              if previous_level != nil and Map.has_key?(fragment, :level),
                do: Map.put(fragment, :level, previous_level),
                else: fragment
            end)

          %{soft | displayed: scalar, fragments: fragments}
        end)

      visible ++ extra
    else
      characters
    end
  end
end
