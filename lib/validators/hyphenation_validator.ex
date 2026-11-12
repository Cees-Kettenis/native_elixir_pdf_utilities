defmodule NativeElixirPdfUtilities.Validators.HyphenationValidator do
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{RenderCache, TextTransform}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, LanguageValidator}

  @pattern ~r/\A[0-9]?\.?(?:[0-9]?[\p{L}\p{M}'’])+[0-9]?\.?[0-9]?\z/u
  @exception ~r/\A[\p{L}\p{M}'’]+(?:-[\p{L}\p{M}'’]+)*\z/u

  @doc "Admits caller-owned Liang patterns, exceptions and provenance without loading dictionary files."
  @spec prepare_dictionaries(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_dictionaries(input) do
    HtmlValidator.with_render_budget(fn ->
      if input == %{} do
        {:ok, %{}}
      else
        # A shared source term avoids hashing all pattern strings on every
        # style validation. Child documents with different input are admitted anew.
        {previous, result} =
          RenderCache.fetch_stylesheet({__MODULE__, :dictionaries}, fn ->
            {input, compile_dictionaries(input)}
          end)

        if previous == input, do: result, else: compile_dictionaries(input)
      end
    end)
  catch
    {:hyphenation_error, error} -> error
    {:render_resource_limit, error} -> error
  end

  @doc "Checks bounded prepared dictionaries against their original admitted definitions on styled surfaces."
  @spec validate_prepared(term()) :: :ok | {:error, {atom(), map()}}
  def validate_prepared(dictionaries) do
    if dictionaries == %{} do
      :ok
    else
      {previous, result} =
        RenderCache.fetch_stylesheet({__MODULE__, :prepared_models}, fn ->
          {dictionaries, validate_dictionary_models(dictionaries)}
        end)

      if previous == dictionaries, do: result, else: validate_dictionary_models(dictionaries)
    end
  end

  defp validate_dictionary_models(dictionaries) do
    cond do
      is_map(dictionaries) ->
        HtmlValidator.check_render_resource(
          :max_entries,
          map_size(dictionaries),
          :layout
        )

        definitions =
          Enum.reduce_while(dictionaries, {:ok, %{}}, fn {language, model}, {:ok, definitions} ->
            case model do
              %{definition: definition} ->
                {:cont, {:ok, Map.put(definitions, language, definition)}}

              _ ->
                {:halt, :error}
            end
          end)

        case definitions do
          {:ok, definitions} ->
            case prepare_dictionaries(definitions) do
              {:ok, prepared} when prepared == dictionaries -> :ok
              {:error, {:resource_limit_exceeded, _}} = error -> error
              _ -> invalid_prepared()
            end

          :error ->
            invalid_prepared()
        end

      true ->
        invalid_prepared()
    end
  catch
    {:render_resource_limit, error} -> error
  end

  @doc "Admits a word and returns its normalized dictionary key with original grapheme byte boundaries."
  @spec prepare_word(term(), binary()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_word(word, language) do
    with {:ok, parts} <- LanguageValidator.prepare_identifier(language) do
      if is_binary(word),
        do: HtmlValidator.reserve_render_resource(:max_work, byte_size(word), :layout)

      if is_binary(word) and String.valid?(word) and Regex.match?(@exception, word) and
           not String.contains?(word, "-") do
        HtmlValidator.check_render_resource(
          :max_entries,
          length(String.codepoints(word)),
          :layout
        )

        normalized =
          TextTransform.convert(word, :lowercase, parts)
          |> String.normalize(:nfc)

        {boundaries, _} =
          Enum.map_reduce(UnicodeGrapheme.graphemes(word), 0, fn cluster, offset ->
            offset = offset + byte_size(cluster)
            {offset, offset}
          end)

        {:ok,
         %{
           text: word,
           normalized: normalized,
           boundaries: boundaries,
           language: Enum.join(parts, "-")
         }}
      else
        Diagnostics.error(
          :layout,
          :invalid_text,
          "hyphenation requires a UTF-8 word of letters and marks",
          operation: :hyphenate_word,
          module: __MODULE__
        )
      end
    end
  catch
    {:render_resource_limit, error} -> error
  end

  defp compile_dictionaries(input) do
    unless is_map(input),
      do: invalid("hyphenation dictionaries must map language identifiers to dictionaries", nil)

    HtmlValidator.check_render_resource(:max_entries, map_size(input), :options)

    {dictionaries, _} =
      Enum.reduce(input, {%{}, %{bytes: 0, entries: 0}}, fn {language, dictionary},
                                                            {output, cost} ->
        parts =
          case LanguageValidator.prepare_identifier(language) do
            {:ok, parts} -> parts
            _ -> invalid("dictionary languages require UTF-8 identifiers", nil)
          end

        tag = Enum.join(parts, "-")

        if tag == "" or Map.has_key?(output, tag),
          do:
            invalid(
              "dictionary languages must be nonempty and unique after canonicalization",
              tag
            )

        unless is_map(dictionary) and
                 Enum.all?(Map.keys(dictionary), &(&1 in [:patterns, :exceptions, :provenance])),
               do:
                 invalid("a dictionary accepts only :patterns, :exceptions and :provenance", tag)

        provenance = Map.get(dictionary, :provenance)

        unless is_map(provenance) and is_binary(provenance[:source]) and provenance[:source] != "" and
                 Enum.all?(provenance, fn {key, value} ->
                   key in [:source, :version, :license] and is_binary(value) and
                     String.valid?(value)
                 end),
               do:
                 invalid(
                   "dictionary provenance requires a UTF-8 :source; optional :version and :license are strings",
                   tag
                 )

        patterns = Map.get(dictionary, :patterns, [])
        exceptions = Map.get(dictionary, :exceptions, [])

        unless is_list(patterns) and is_list(exceptions),
          do: invalid("dictionary patterns and exceptions must be lists", tag)

        entries = cost.entries + length(patterns) + length(exceptions)
        HtmlValidator.check_render_resource(:max_entries, entries, :options)
        cost = %{cost | entries: entries}
        cost = account_bytes(tag, cost)
        cost = Enum.reduce(Map.values(provenance), cost, &account_bytes/2)

        {trie, cost} =
          Enum.reduce(patterns, {%{}, cost}, fn pattern, {trie, cost} ->
            cost = if is_binary(pattern), do: account_bytes(pattern, cost), else: cost

            unless is_binary(pattern) and String.valid?(pattern) and
                     Regex.match?(@pattern, pattern),
                   do:
                     invalid(
                       "dictionary patterns require letters, optional boundary dots and single digit weights",
                       tag
                     )

            {letters, weights} =
              pattern
              |> String.to_charlist()
              |> Enum.reduce({[], [0]}, fn character, {letters, weights} ->
                if character in ?0..?9,
                  do: {letters, [character - ?0 | tl(weights)]},
                  else: {[character | letters], [0 | weights]}
              end)

            letters = Enum.reverse(letters)
            weights = Enum.reverse(weights)
            key = List.to_string(letters)

            unless String.normalize(key, :nfc) == key and
                     TextTransform.convert(key, :lowercase, parts) == key,
                   do:
                     invalid(
                       "dictionary patterns must use lowercase NFC letters for their language",
                       tag
                     )

            HtmlValidator.check_render_resource(
              :max_entries,
              length(letters) -
                Enum.count(letters, &(&1 == ?.)),
              :options
            )

            {insert(trie, letters, weights), cost}
          end)

        {exceptions, cost} =
          Enum.reduce(exceptions, {%{}, cost}, fn exception, {exceptions, cost} ->
            cost = if is_binary(exception), do: account_bytes(exception, cost), else: cost

            unless is_binary(exception) and String.valid?(exception) and
                     Regex.match?(@exception, exception),
                   do:
                     invalid(
                       "dictionary exceptions require words with optional internal hyphens",
                       tag
                     )

            plain = String.replace(exception, "-", "")

            HtmlValidator.check_render_resource(
              :max_entries,
              length(String.codepoints(plain)),
              :options
            )

            word = TextTransform.convert(plain, :lowercase, parts) |> String.normalize(:nfc)

            if Map.has_key?(exceptions, word),
              do: invalid("dictionary exceptions must name unique words", tag)

            {positions, _} =
              exception
              |> UnicodeGrapheme.graphemes()
              |> Enum.reduce({[], 0}, fn character, {positions, offset} ->
                if character == "-",
                  do: {[offset | positions], offset},
                  else: {positions, offset + 1}
              end)

            {Map.put(exceptions, word, Enum.reverse(positions)), cost}
          end)

        {Map.put(output, tag, %{
           language: tag,
           trie: trie,
           exceptions: exceptions,
           provenance: provenance,
           definition: dictionary
         }), cost}
      end)

    {:ok, dictionaries}
  end

  defp insert(trie, letters, weights) do
    HtmlValidator.reserve_render_resource(:max_work, 1, :options)

    case letters do
      [] ->
        Map.update(trie, :weights, weights, fn previous ->
          Enum.zip_with(previous, weights, &max/2)
        end)

      [letter | rest] ->
        Map.put(trie, letter, insert(Map.get(trie, letter, %{}), rest, weights))
    end
  end

  defp account_bytes(text, cost) do
    bytes = cost.bytes + byte_size(text)
    HtmlValidator.check_render_resource(:max_input_bytes, bytes, :options)
    HtmlValidator.reserve_render_resource(:max_work, byte_size(text), :options)
    %{cost | bytes: bytes}
  end

  defp invalid(message, source),
    do:
      throw(
        {:hyphenation_error,
         Diagnostics.error(:options, :invalid_options, message,
           operation: :prepare_hyphenation,
           module: __MODULE__,
           source: source
         )}
      )

  defp invalid_prepared,
    do:
      Diagnostics.error(
        :layout,
        :invalid_layout,
        "prepared hyphenation dictionaries must match their bounded admitted definitions",
        operation: :layout,
        module: __MODULE__,
        source: "hyphenation dictionaries"
      )
end
