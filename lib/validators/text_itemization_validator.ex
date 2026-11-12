defmodule NativeElixirPdfUtilities.Validators.TextItemizationValidator do
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeTextData
  alias NativeElixirPdfUtilities.Validators.{FontValidator, LanguageValidator}
  @weak_scripts ["Zyyy", "Zinh", "Zzzz"]

  @doc "Merges consumed source fragments once in logical occurrence order without hashing complete DOM owners."
  @spec merge_fragments([[map()]]) :: [map()]
  def merge_fragments(lists) do
    case List.flatten(lists) do
      [] ->
        []

      [fragment] ->
        [fragment]

      fragments ->
        FontValidator.with_budget(fn ->
          {result, _} =
            Enum.reduce(fragments, {[], :gb_sets.empty()}, fn fragment, {result, seen} ->
              FontValidator.reserve(
                :max_work,
                4 * ceil(:math.log2(:gb_sets.size(seen) + 2))
              )

              if :gb_sets.is_member(fragment, seen),
                do: {result, seen},
                else: {[fragment | result], :gb_sets.add(fragment, seen)}
            end)

          Enum.reverse(result)
        end)
    end
  end

  @doc "Itemizes UTF-8 text or adjacent inline pieces into shared script/language clusters and exact scalar owners."
  @spec prepare(term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(input, language \\ "und") do
    FontValidator.with_budget(fn ->
      with {:ok, language} <- prepare_language(language) do
        pieces = if is_binary(input), do: [%{text: input}], else: input

        if is_list(pieces) do
          Enum.reduce_while(pieces, {:ok, []}, fn piece, {:ok, characters} ->
            FontValidator.reserve(:max_work, 1)

            case piece do
              %{text: text} when is_binary(text) ->
                FontValidator.reserve(:max_work, byte_size(text))

                piece_language =
                  case Map.get(piece, :language) do
                    nil -> language
                    value -> value
                  end

                with true <- String.valid?(text),
                     {:ok, language} <- prepare_language(piece_language) do
                  scalars = String.codepoints(text)
                  FontValidator.reserve(:max_entries, length(scalars))

                  characters =
                    scalars
                    |> Enum.with_index()
                    |> Enum.reduce(characters, fn {scalar, index}, characters ->
                      fragments =
                        case Map.fetch(piece, :owner) do
                          {:ok, owner} -> [%{owner: owner, text: scalar, offset: index}]
                          :error -> []
                        end

                      [
                        %{displayed: scalar, language: language, fragments: fragments}
                        | characters
                      ]
                    end)

                  {:cont, {:ok, characters}}
                else
                  false -> {:halt, failure("text itemization requires valid UTF-8 scalar text")}
                  error -> {:halt, error}
                end

              _ ->
                {:halt, failure("inline text pieces require a UTF-8 :text value")}
            end
          end)
          |> case do
            {:ok, characters} -> {:ok, itemize(Enum.reverse(characters))}
            error -> error
          end
        else
          failure("text itemization requires UTF-8 text or a list of inline text pieces")
        end
      end
    end)
  end

  @doc "Itemizes canonical scalar fragments after whitespace and text transformation, retaining every original owner."
  @spec prepare_characters(term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_characters(characters, language \\ "und") do
    FontValidator.with_budget(fn ->
      with {:ok, language} <- prepare_language(language) do
        if is_list(characters) do
          Enum.reduce_while(characters, {:ok, []}, fn character, {:ok, result} ->
            FontValidator.reserve(:max_work, 1)
            FontValidator.reserve(:max_entries, 1)

            case character do
              %{displayed: <<_scalar::utf8>>, fragments: fragments} when is_list(fragments) ->
                FontValidator.reserve(:max_work, length(fragments))

                valid? =
                  Enum.all?(fragments, fn fragment ->
                    case fragment do
                      %{text: <<_source_scalar::utf8>>, offset: offset}
                      when is_integer(offset) and offset >= 0 ->
                        Map.has_key?(fragment, :owner) or Map.has_key?(fragment, :scalar_owner)

                      _ ->
                        false
                    end
                  end)

                if valid?,
                  do: {:cont, {:ok, [Map.put(character, :language, language) | result]}},
                  else:
                    {:halt,
                     failure(
                       "source fragments require one UTF-8 scalar, an owner and a non-negative offset"
                     )}

              _ ->
                {:halt, failure("source characters require one UTF-8 scalar and a fragment list")}
            end
          end)
          |> case do
            {:ok, result} -> {:ok, itemize(Enum.reverse(result))}
            error -> error
          end
        else
          failure("source characters must be a list")
        end
      end
    end)
  end

  @doc "Admits an exact displayed-scalar projection of logical UTF-8 text, allowing Unicode bidi mirrors."
  @spec prepare_projection(term(), term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_projection(text, characters, language) do
    FontValidator.with_budget(fn ->
      with {:ok, plan} <- prepare_characters(characters, language) do
        if is_binary(text) do
          FontValidator.reserve(:max_work, byte_size(text))

          remaining =
            Enum.reduce_while(characters, text, fn %{displayed: <<displayed::utf8>>}, source ->
              case source do
                <<logical::utf8, rest::binary>> ->
                  if displayed == logical or displayed == UnicodeTextData.mirror(logical),
                    do: {:cont, rest},
                    else: {:halt, false}

                _ ->
                  {:halt, false}
              end
            end)

          if remaining == "",
            do: {:ok, plan},
            else:
              failure(
                "source characters must match the logical text's scalars or their Unicode bidi mirrors"
              )
        else
          failure("source-character shaping requires logical UTF-8 text as a binary")
        end
      end
    end)
  end

  @doc "Admits a shaping language through the shared language grammar and returns its canonical identifier."
  @spec prepare_language(term()) :: {:ok, String.t()} | {:error, {atom(), map()}}
  def prepare_language(language) do
    result =
      NativeElixirPdfUtilities.Validators.HtmlValidator.with_render_budget(fn ->
        LanguageValidator.prepare_identifier(language)
      end)

    case result do
      {:ok, parts} ->
        {:ok, Enum.join(parts, "-")}

      {:error, {reason, diagnostic}} ->
        {:error, {reason, %{diagnostic | stage: :font}}}
    end
  end

  defp itemize(characters) do
    text = Enum.map_join(characters, & &1.displayed)
    FontValidator.reserve(:max_work, byte_size(text))

    {clusters, {[], _, _}} =
      text
      |> UnicodeGrapheme.graphemes()
      |> Enum.map_reduce({characters, 0, 0}, fn grapheme, {characters, byte, scalar} ->
        count = length(String.codepoints(grapheme))
        {consumed, remaining} = Enum.split(characters, count)

        scalars =
          Enum.map(consumed, fn character ->
            <<codepoint::utf8>> = character.displayed

            %{
              codepoint: codepoint,
              properties: UnicodeTextData.properties(codepoint),
              source: character
            }
          end)

        strong =
          Enum.find_value(scalars, fn %{properties: properties} ->
            script = elem(properties, 0)
            if script not in @weak_scripts, do: script
          end)

        candidates =
          Enum.flat_map(scalars, fn %{codepoint: codepoint} ->
            codepoint
            |> UnicodeTextData.scripts()
            |> Tuple.to_list()
            |> Enum.reject(&(&1 in @weak_scripts))
          end)
          |> Enum.uniq()

        cluster = %{
          text: grapheme,
          start: byte,
          end: byte + byte_size(grapheme),
          scalar_start: scalar,
          scalar_end: scalar + count,
          script: strong,
          script_candidates: candidates,
          language: hd(consumed).language,
          scalars: scalars,
          fragments: Enum.flat_map(consumed, & &1.fragments)
        }

        {cluster, {remaining, cluster.end, cluster.scalar_end}}
      end)

    {following, _} =
      clusters
      |> Enum.reverse()
      |> Enum.map_reduce(nil, fn cluster, next -> {next, cluster.script || next} end)

    {clusters, _} =
      Enum.zip(clusters, Enum.reverse(following))
      |> Enum.map_reduce(nil, fn {cluster, next}, previous ->
        script =
          cluster.script ||
            Enum.find([previous, next], fn script ->
              script != nil and
                (cluster.script_candidates == [] or script in cluster.script_candidates)
            end) || List.first(cluster.script_candidates) || "Zyyy"

        {Map.put(cluster, :script, script),
         if(script in @weak_scripts, do: previous, else: script)}
      end)

    runs =
      clusters
      |> Enum.with_index()
      |> Enum.reduce([], fn {cluster, index}, runs ->
        case runs do
          [%{script: script, language: language} = previous | rest]
          when script == cluster.script and language == cluster.language ->
            [%{previous | end: cluster.end, cluster_end: index + 1} | rest]

          _ ->
            [
              %{
                script: cluster.script,
                language: cluster.language,
                start: cluster.start,
                end: cluster.end,
                cluster_start: index,
                cluster_end: index + 1
              }
              | runs
            ]
        end
      end)
      |> Enum.reverse()

    %{text: text, clusters: clusters, runs: runs}
  end

  defp failure(message),
    do:
      Diagnostics.error(:font, :invalid_text, message,
        operation: :itemize_text,
        module: __MODULE__,
        source: "text itemization"
      )
end
