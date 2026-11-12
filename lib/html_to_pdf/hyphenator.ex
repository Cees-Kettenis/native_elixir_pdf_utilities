defmodule NativeElixirPdfUtilities.HtmlToPdf.Hyphenator do
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, HyphenationValidator}
  @words ~r/[\p{L}\p{M}]+(?:['’\x{AD}]*[\p{L}\p{M}]+)*\x{AD}*/u

  @doc "Scopes the admitted page capacity used to avoid unplaceable hyphenation chains during layout."
  @spec with_fragmentainer(number() | nil, (-> result)) :: result when result: term()
  def with_fragmentainer(height, operation) do
    key = {__MODULE__, :fragmentainer_height}
    previous = Process.get(key)
    Process.put(key, height)

    try do
      operation.()
    after
      if previous == nil, do: Process.delete(key), else: Process.put(key, previous)
    end
  end

  @doc "Returns the current page capacity, or nil for independently laid out surfaces."
  @spec fragmentainer_height() :: number() | nil
  def fragmentainer_height, do: Process.get({__MODULE__, :fragmentainer_height})

  @doc "Adds automatic discretionary opportunities across adjacent inline source owners before bidi and fitting."
  @spec prepare_runs([map()]) :: {:ok, [map()]} | {:error, {atom(), map()}}
  def prepare_runs(runs) do
    result =
      if Enum.any?(runs, &(context(&1) != :unchanged)) do
        prepared =
          runs
          |> Enum.chunk_by(&context/1)
          |> Enum.flat_map(fn group ->
            case context(hd(group)) do
              :unchanged ->
                group

              {language, dictionaries, _limits} ->
                if select_dictionary(dictionaries, String.split(language, "-")) == nil,
                  do: group,
                  else: prepare_group(group, dictionaries, language)
            end
          end)

        if Enum.any?(prepared, &Map.has_key?(&1, :hyphenation_characters)),
          do: retain_source_ranges(runs, prepared),
          else: prepared
      else
        runs
      end

    {:ok, result}
  catch
    {:hyphenation_error, error} -> error
    {:render_resource_limit, error} -> error
  end

  @doc "Discards rejected generated opportunities without consuming or renumbering original source scalars."
  @spec discard_breaks([map()], MapSet.t()) :: [map()]
  def discard_breaks(runs, rejected) do
    Enum.map(runs, fn run ->
      case {:maps.get(:_hyphenation_source, run, nil),
            :maps.get(:hyphenation_characters, run, nil)} do
        {%{id: id}, characters} when is_list(characters) ->
          HtmlValidator.reserve_render_resource(
            :max_work,
            length(characters),
            :layout
          )

          positions =
            characters
            |> Enum.with_index()
            |> Enum.reduce(MapSet.new(), fn {character, index}, positions ->
              if character.generated? and MapSet.member?(rejected, {id, character.start}),
                do: MapSet.put(positions, index),
                else: positions
            end)

          NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidator.remove_scalars(
            run,
            positions
          )

        _ ->
          run
      end
    end)
  end

  @doc "Returns cluster-safe dictionary opportunities and their caller-supplied provenance."
  @spec opportunities(String.t(), map(), String.t()) :: {:ok, map()} | {:error, {atom(), map()}}
  def opportunities(word, dictionaries, language) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, word} <- HyphenationValidator.prepare_word(word, language) do
        dictionary = select_dictionary(dictionaries, String.split(word.language, "-"))

        if dictionary == nil do
          {:ok, %{breaks: [], language: nil, provenance: nil, word: word}}
        else
          positions =
            case Map.fetch(dictionary.exceptions, word.normalized) do
              {:ok, positions} -> positions
              :error -> pattern_positions(word.normalized, dictionary.trie)
            end

          count = length(word.boundaries)
          boundaries = List.to_tuple(word.boundaries)

          breaks =
            for position <- positions,
                position > 0 and position < count,
                do: elem(boundaries, position - 1)

          {:ok,
           %{
             breaks: breaks,
             language: dictionary.language,
             provenance: dictionary.provenance,
             word: word
           }}
        end
      end
    end)
  catch
    {:render_resource_limit, error} -> error
  end

  defp context(run) do
    style = run.style

    if :maps.get(:type, run, nil) in [nil, :text] and :maps.get(:hyphens, style, nil) == :auto and
         :maps.get(:white_space, style, :normal) not in [:pre, :nowrap] do
      {:maps.get(:_text_language, style, "und"),
       :maps.get(:_hyphenation_dictionaries, style, %{}),
       Map.get(style, :hyphenate_limit_chars, {:auto, :auto, :auto})}
    else
      :unchanged
    end
  end

  defp retain_source_ranges(original, prepared) do
    source = %{
      id: make_ref(),
      text:
        Enum.map_join(original, fn run ->
          if :maps.get(:hard_break, run, false), do: "\n", else: run.text
        end)
    }

    HtmlValidator.reserve_render_resource(:max_work, byte_size(source.text), :layout)

    {prepared, _} =
      Enum.zip(original, prepared)
      |> Enum.map_reduce(0, fn {original, run}, offset ->
        characters =
          Map.get_lazy(run, :hyphenation_characters, fn ->
            for <<codepoint::utf8 <- run.text>>, do: %{codepoint: codepoint, generated?: false}
          end)

        {characters, finish} =
          Enum.map_reduce(characters, offset, fn character, offset ->
            finish =
              offset +
                if(character.generated?, do: 0, else: byte_size(<<character.codepoint::utf8>>))

            {Map.merge(character, %{start: offset, end: finish}), finish}
          end)

        finish = if :maps.get(:hard_break, original, false), do: finish + 1, else: finish

        {run
         |> Map.put(:hyphenation_characters, characters)
         |> Map.put(:_hyphenation_source, source), finish}
      end)

    prepared
  end

  defp select_dictionary(dictionaries, parts) do
    case parts do
      [] ->
        nil

      _ ->
        case Map.get(dictionaries, Enum.join(parts, "-")) do
          nil ->
            prefix = Enum.drop(parts, -1)

            prefix =
              if prefix != [] and byte_size(List.last(prefix)) == 1,
                do: Enum.drop(prefix, -1),
                else: prefix

            select_dictionary(dictionaries, prefix)

          dictionary ->
            dictionary
        end
    end
  end

  defp prepare_group(runs, dictionaries, language) do
    text = Enum.map_join(runs, & &1.text)
    HtmlValidator.reserve_render_resource(:max_work, byte_size(text), :layout)

    breaks =
      Regex.scan(@words, text, return: :index)
      |> Enum.reduce(MapSet.new(), fn [{offset, bytes}], breaks ->
        word = binary_part(text, offset, bytes)

        if not String.contains?(word, "\u00AD") do
          {:ok, model} =
            case opportunities(word, dictionaries, language) do
              {:ok, model} -> {:ok, model}
              error -> throw({:hyphenation_error, error})
            end

          NativeElixirPdfUtilities.Validators.CssHyphenationValidator.allowed_points(
            model.word,
            model.breaks,
            hd(runs).style
          )
          |> Enum.reduce(breaks, &MapSet.put(&2, offset + &1))
        else
          breaks
        end
      end)

    if MapSet.size(breaks) == 0 do
      runs
    else
      HtmlValidator.reserve_render_resource(
        :max_output_bytes,
        2 * MapSet.size(breaks),
        :layout
      )

      {runs, _} =
        Enum.map_reduce(runs, 0, fn run, offset ->
          source =
            case :maps.get(:source_glyph_characters, run, nil) do
              characters when is_list(characters) -> List.to_tuple(characters)
              _ -> nil
            end

          {characters, finish} =
            run.text
            |> String.codepoints()
            |> Enum.with_index()
            |> Enum.map_reduce(offset, fn {scalar, index}, offset ->
              offset = offset + byte_size(scalar)

              record = %{
                displayed: scalar,
                generated?: false,
                source: if(source != nil, do: elem(source, index))
              }

              characters =
                if MapSet.member?(breaks, offset),
                  do: [
                    record,
                    %{
                      displayed: "\u00AD",
                      generated?: true,
                      source: %{displayed: "\u00AD", fragments: []}
                    }
                  ],
                  else: [record]

              {characters, offset}
            end)

          characters = List.flatten(characters)

          run =
            run
            |> Map.put(:text, Enum.map_join(characters, & &1.displayed))
            |> Map.put(
              :hyphenation_characters,
              Enum.map(characters, fn character ->
                <<codepoint::utf8>> = character.displayed
                %{codepoint: codepoint, generated?: character.generated?}
              end)
            )

          run =
            if source == nil,
              do: run,
              else: Map.put(run, :source_glyph_characters, Enum.map(characters, & &1.source))

          {run, finish}
        end)

      runs
    end
  end

  defp pattern_positions(word, trie) do
    characters = [?. | String.to_charlist(word)] ++ [?.]
    count = length(characters)
    characters = List.to_tuple(characters)
    scores = :array.new(count + 1, default: 0)
    scores = Enum.reduce(0..(count - 1), scores, &match_pattern(trie, characters, &1, &1, &2))

    {positions, _} =
      word
      |> UnicodeGrapheme.graphemes()
      |> Enum.with_index(1)
      |> Enum.reduce({[], 1}, fn {cluster, index}, {positions, scalar} ->
        scalar = scalar + length(String.codepoints(cluster))

        positions =
          if rem(:array.get(scalar, scores), 2) == 1, do: [index | positions], else: positions

        {positions, scalar}
      end)

    Enum.reverse(positions)
  end

  defp match_pattern(node, characters, index, start, scores) do
    HtmlValidator.reserve_render_resource(:max_work, 1, :layout)

    scores =
      case :maps.get(:weights, node, nil) do
        nil ->
          scores

        weights ->
          weights
          |> Enum.with_index(start)
          |> Enum.reduce(scores, fn {weight, position}, scores ->
            HtmlValidator.reserve_render_resource(:max_work, 1, :layout)
            :array.set(position, max(weight, :array.get(position, scores)), scores)
          end)
      end

    if index == tuple_size(characters) do
      scores
    else
      case Map.get(node, elem(characters, index)) do
        nil -> scores
        child -> match_pattern(child, characters, index + 1, start, scores)
      end
    end
  end
end
