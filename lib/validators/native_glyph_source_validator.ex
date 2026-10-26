defmodule NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Retains authored text identities in the actual issued native HTML subtree."
  @spec prepare_tree(map()) :: map()
  def prepare_tree(node) do
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :font)

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
    case :binary.match(text, ["\r", "\n", "\t", "\f", "  "]) do
      :nomatch ->
        text

      _ ->
        Enum.reduce(normalizations(style), text, fn {pattern, replacement}, text ->
          String.replace(text, pattern, replacement)
        end)
    end
  end

  @doc "Carries exact scalar provenance through the same whitespace normalization."
  @spec normalize_characters([map()], map()) :: [map()]
  def normalize_characters(characters, style) do
    Enum.reduce(normalizations(style), characters, fn {pattern, replacement}, characters ->
      text = Enum.map_join(characters, & &1.displayed)
      HtmlValidator.reserve_render_resource(:max_layout_text_work, byte_size(text), :font)

      {result, remaining, cursor} =
        Regex.scan(pattern, text, return: :index)
        |> Enum.reduce({[], characters, 0}, fn [{offset, bytes}], {result, remaining, cursor} ->
          prefix_length =
            text |> binary_part(cursor, offset - cursor) |> String.codepoints() |> length()

          selected_length = text |> binary_part(offset, bytes) |> String.codepoints() |> length()
          {prefix, remaining} = Enum.split(remaining, prefix_length)
          {selected, remaining} = Enum.split(remaining, selected_length)

          replacement = %{
            displayed: replacement,
            fragments: Enum.flat_map(selected, & &1.fragments)
          }

          {Enum.reverse(prefix, result) |> then(&[replacement | &1]), remaining, offset + bytes}
        end)

      _ = cursor
      Enum.reverse(result) ++ remaining
    end)
  end

  defp normalizations(style) do
    newline = {~r/\r\n?|\n/u, "\n"}

    case Map.get(style, :white_space, :normal) do
      mode when mode in [:pre, :pre_wrap, :break_spaces] -> [newline]
      :pre_line -> [newline, {~r/[ \t\f]+/u, " "}]
      _ -> [{~r/[ \t\r\n\f]+/u, " "}]
    end
  end

  @doc "Attaches the normalized source characters to an actual inline text run."
  @spec prepare_run(map(), String.t()) :: map()
  def prepare_run(node, text) do
    run = %{text: text, style: node.style}

    case Map.get(node.style, :_source_glyph_characters) do
      characters when is_list(characters) ->
        Map.put(run, :source_glyph_characters, normalize_characters(characters, node.style))

      _ ->
        run
    end
  end

  @doc "Slices an actual text run and its scalar provenance at the same UTF-8 byte boundaries."
  @spec slice_run(map(), non_neg_integer(), String.t()) :: map()
  def slice_run(run, offset, text) do
    result = Map.put(run, :text, text)

    case Map.get(run, :source_glyph_characters) do
      characters when is_list(characters) ->
        first = run.text |> binary_part(0, offset) |> String.codepoints() |> length()

        Map.put(
          result,
          :source_glyph_characters,
          Enum.slice(characters, first, length(String.codepoints(text)))
        )

      _ ->
        result
    end
  end

  @doc "Splits a run at a known newline byte offset, consuming source scalars once."
  @spec split_line(map(), non_neg_integer()) :: {map(), map()}
  def split_line(run, offset) do
    <<part::binary-size(^offset), "\n", rest::binary>> = run.text
    first = Map.put(run, :text, part)
    last = Map.put(run, :text, rest)

    case Map.get(run, :source_glyph_characters) do
      characters when is_list(characters) ->
        {before, after_newline} = Enum.split(characters, length(String.codepoints(part)))

        {Map.put(first, :source_glyph_characters, before),
         Map.put(last, :source_glyph_characters, Enum.drop(after_newline, 1))}

      _ ->
        {first, last}
    end
  end

  @doc "Retains source scalar offsets when layout trims an actual run."
  @spec trim_run(map(), String.t()) :: map()
  def trim_run(run, text) do
    case Map.get(run, :source_glyph_characters) do
      characters when is_list(characters) ->
        if text == "" do
          slice_run(run, 0, text)
        else
          {offset, _} = :binary.match(run.text, text)
          slice_run(run, offset, text)
        end

      _ ->
        Map.put(run, :text, text)
    end
  end

  @doc "Retains original tab scalar owners through the actual tab-stop expansion."
  @spec expand_tabs(map(), String.t(), [non_neg_integer()]) :: map()
  def expand_tabs(run, text, counts) do
    result = Map.put(run, :text, text)

    case Map.get(run, :source_glyph_characters) do
      characters when is_list(characters) ->
        {expanded, []} =
          Enum.map_reduce(characters, counts, fn character, remaining ->
            case {character.displayed, remaining} do
              {"\t", [count | remaining]} ->
                {List.duplicate(%{character | displayed: " "}, count), remaining}

              _ ->
                {[character], remaining}
            end
          end)

        Map.put(result, :source_glyph_characters, List.flatten(expanded))

      _ ->
        result
    end
  end

  @doc "Combines actual adjacent text runs while retaining every consumed source scalar."
  @spec merge_runs(map(), map()) :: map()
  def merge_runs(first, second) do
    result = Map.put(first, :text, first.text <> second.text)

    case {Map.get(first, :source_glyph_characters), Map.get(second, :source_glyph_characters)} do
      {left, right} when is_list(left) and is_list(right) ->
        Map.put(result, :source_glyph_characters, left ++ right)

      _ ->
        result
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

  @doc "Returns the actual embedded-font glyph clusters selected for a native paint text box."
  @spec paint_runs(map(), map(), number()) :: [map()]
  @spec paint_runs(map(), map(), number(), map() | nil) :: [map()]
  def paint_runs(run, face, spacing, display_plan \\ nil) do
    case {Map.get(run, :source_glyph_characters), face} do
      {characters, %{type: :embedded}} when is_list(characters) ->
        characters =
          if display_plan && display_plan.discretionary? do
            visible = Enum.reject(characters, &(&1.displayed in ["\u00AD", "\u200B"]))

            if Enum.map_join(visible, & &1.displayed) == display_plan.text do
              visible
            else
              # The validated display plan selects one real hyphen for a taken soft break.
              soft = List.last(characters)
              visible ++ [%{soft | displayed: String.last(display_plan.text)}]
            end
          else
            characters
          end

        shape_characters(characters, face, spacing)
        |> Enum.map(fn character ->
          <<codepoint::utf8>> = character.displayed

          %{
            source_fragments: Enum.filter(character.fragments, &Map.has_key?(&1, :owner)),
            field_scalar_fragments:
              Enum.filter(character.fragments, &Map.has_key?(&1, :scalar_owner)),
            text: character.displayed,
            displayed_unicode: codepoint,
            glyph_ids: [Map.fetch!(face.cmap, codepoint)]
          }
        end)

      _ ->
        []
    end
  end
end
