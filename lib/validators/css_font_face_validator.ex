defmodule NativeElixirPdfUtilities.Validators.CssFontFaceValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.CssReferenceValidator
  alias NativeElixirPdfUtilities.Validators.CssFontValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @control_descriptors ~w(font-feature-settings font-variation-settings font-language-override font-named-instance)
  @metric_descriptors Map.keys(NativeElixirPdfUtilities.Validators.FontMetricValidator.fields())

  @doc false
  @spec validate([NativeElixirPdfUtilities.HtmlToPdf.CssParser.declaration()]) ::
          {:ok, NativeElixirPdfUtilities.HtmlToPdf.CssParser.font_face()}
          | {:error, %{property: String.t(), index: non_neg_integer() | nil}}
  def validate(declarations) do
    case Enum.find(declarations, &(not supported_font_descriptor?(&1))) do
      nil ->
        family = font_family_descriptor(declarations)
        sources = font_source_descriptor(declarations)
        selection = font_selection_descriptors(declarations)

        case {family, sources, selection} do
          {{:ok, family}, {:ok, sources}, {:ok, properties}} ->
            {:ok, Map.merge(%{family: family, sources: sources}, properties)}

          _ ->
            property =
              case selection do
                {:error, property} -> property
                _ -> if family == {:error, :invalid_css}, do: "font-family", else: "src"
              end

            index =
              declarations
              |> Enum.with_index()
              |> Enum.reverse()
              |> Enum.find_value(fn {declaration, index} ->
                if elem(declaration, 0) == property, do: index
              end)

            {:error, %{property: property, index: index}}
        end

      declaration ->
        {:error,
         %{
           property: elem(declaration, 0),
           index: Enum.find_index(declarations, &(&1 == declaration))
         }}
    end
  end

  @doc false
  @spec prepare_original_resources([map()]) ::
          {:ok, [map()]} | {:error, {atom(), map()}}
  def prepare_original_resources(entries) do
    Enum.reduce_while(entries, {:ok, []}, fn entry, {:ok, selected} ->
      HtmlValidator.reserve_render_resource(:max_work, 1, :font)

      case entry do
        %{prepared: %{authored_fonts: records} = prepared} when is_list(records) ->
          result =
            Enum.reduce_while(records, {:ok, selected}, fn record, {:ok, selected} ->
              HtmlValidator.reserve_render_resource(:max_work, 1, :font)

              Enum.reduce_while(record.declarations, {:ok, selected}, fn declaration,
                                                                         {:ok, selected} ->
                HtmlValidator.reserve_render_resource(:max_work, 1, :font)

                if elem(declaration.declaration, 0) == "src" do
                  # Every original src descriptor keeps its normal ordered fallback
                  # policy, including superseded descriptors and inactive faces.
                  case font_source_descriptor([declaration.declaration]) do
                    {:ok, sources} ->
                      face = Map.put(record.face, :sources, sources)

                      prepared =
                        prepared
                        |> Map.put(:font_faces, [face])
                        |> Map.delete(:font_source_tickets)

                      font_entry =
                        entry
                        |> Map.put(:prepared, prepared)
                        |> Map.put(:original_font_source, declaration.source)

                      {:cont, {:ok, [font_entry | selected]}}

                    _ ->
                      {:halt,
                       original_resource_error(
                         declaration.source,
                         "original font source has no supported normal loading context; authorization and contents remain pending"
                       )}
                  end
                else
                  {:cont, {:ok, selected}}
                end
              end)
              |> case do
                {:ok, selected} -> {:cont, {:ok, selected}}
                error -> {:halt, error}
              end
            end)

          case result do
            {:ok, selected} -> {:cont, {:ok, selected}}
            error -> {:halt, error}
          end

        _ ->
          {:halt,
           original_resource_error(
             %{},
             "original font resources require validated authored stylesheet records"
           )}
      end
    end)
    |> case do
      {:ok, selected} -> {:ok, Enum.reverse(selected)}
      error -> error
    end
  end

  @doc false
  @spec with_original_resource(map(), (-> term())) :: :ok | {:error, {atom(), map()}}
  def with_original_resource(entry, loader) do
    try do
      case loader.() do
        :ok -> :ok
        failure -> original_resource_failure(entry, failure)
      end
    catch
      {:font_resource_limit, failure} -> original_resource_failure(entry, failure)
      {:render_resource_limit, failure} -> original_resource_failure(entry, failure)
    end
  end

  @doc false
  @spec original_resource_failure(map(), term()) :: {:error, {atom(), map()}}
  def original_resource_failure(entry, failure) do
    case failure do
      {:error, {reason, diagnostic}} when is_map(diagnostic) ->
        source = entry.original_font_source

        {:error,
         {reason,
          NativeElixirPdfUtilities.Diagnostics.diagnostic(
            diagnostic.stage,
            reason,
            diagnostic.message,
            operation: Map.get(diagnostic, :operation, :original_font_sources),
            module: Map.get(diagnostic, :module, __MODULE__),
            source: Map.get(source, :text),
            line: Map.get(source, :line),
            column: Map.get(source, :column)
          )}}

      _ ->
        original_resource_error(
          entry.original_font_source,
          "original CSS font source could not be resolved, parsed or embedded; provide supported authorized font bytes before template replacement"
        )
    end
  end

  defp original_resource_error(source, message) do
    NativeElixirPdfUtilities.Diagnostics.error(:font, :invalid_document, message,
      module: __MODULE__,
      operation: :original_font_sources,
      source: Map.get(source, :text),
      line: Map.get(source, :line),
      column: Map.get(source, :column)
    )
  end

  @doc false
  @spec validate_authored([NativeElixirPdfUtilities.HtmlToPdf.CssParser.declaration()]) ::
          {:ok, NativeElixirPdfUtilities.HtmlToPdf.CssParser.font_face()}
          | {:error, %{property: String.t(), index: non_neg_integer() | nil}}
  def validate_authored(declarations) do
    HtmlValidator.with_render_budget(fn ->
      # Existing validation owns supported descriptors, font-display and final
      # selection. Only earlier supported values need this additional check.
      with {:ok, face} <- validate(declarations) do
        last =
          declarations
          |> Enum.with_index()
          |> Map.new(fn {declaration, index} -> {elem(declaration, 0), index} end)

        result =
          declarations
          |> Enum.with_index()
          |> Enum.reduce_while(:ok, fn {declaration, index}, :ok ->
            HtmlValidator.reserve_render_resource(:max_work, 1, :css)
            property = elem(declaration, 0)

            valid? =
              if index != last[property] do
                case property do
                  "font-family" ->
                    match?({:ok, _}, font_family_descriptor([declaration]))

                  property
                  when property in [
                         "font-weight",
                         "font-style",
                         "font-width",
                         "font-stretch",
                         "unicode-range"
                       ] ->
                    match?({:ok, _}, descriptor_properties(property, elem(declaration, 1)))

                  _ ->
                    true
                end
              else
                true
              end

            if valid?,
              do: {:cont, :ok},
              else: {:halt, {:error, %{property: property, index: index}}}
          end)

        with :ok <- result, do: {:ok, face}
      end
    end)
  end

  defp supported_font_descriptor?(declaration) do
    case declaration do
      {property, _value}
      when property in [
             "font-family",
             "src",
             "font-weight",
             "font-style",
             "font-width",
             "font-stretch",
             "unicode-range"
           ] or property in @metric_descriptors or property in @control_descriptors ->
        true

      {"font-display", value} ->
        String.downcase(value) in ["auto", "block", "swap", "fallback", "optional"]

      _ ->
        false
    end
  end

  defp font_family_descriptor(declarations) do
    case declarations |> Enum.reverse() |> List.keyfind("font-family", 0) do
      {"font-family", value} ->
        family = value |> String.trim() |> String.trim("\"") |> String.trim("'")
        if family == "", do: {:error, :invalid_css}, else: {:ok, family}

      _ ->
        {:error, :invalid_css}
    end
  end

  defp font_source_descriptor(declarations) do
    case declarations |> Enum.reverse() |> List.keyfind("src", 0) do
      {"src", value} ->
        sources =
          value
          |> source_candidates()
          |> Enum.flat_map(fn candidate ->
            case candidate_info(candidate) do
              %{supported: true, reference: reference} -> [reference]
              _ -> []
            end
          end)

        case sources do
          [] -> {:error, :invalid_css}
          sources -> {:ok, sources}
        end

      _ ->
        {:error, :invalid_css}
    end
  end

  @doc false
  @spec validate_source_candidate(String.t()) :: {:ok, map()} | {:error, {atom(), map()}}
  @spec validate_source_candidate(
          String.t(),
          (non_neg_integer(), non_neg_integer() -> map()) | nil
        ) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def validate_source_candidate(candidate, mapper \\ nil) do
    info = candidate_info(candidate)

    with {:ok, references} <- CssReferenceValidator.scan(candidate, mapper) do
      info = Map.put(info, :source_references, references)

      case {Map.get(info, :url), references} do
        {nil, []} ->
          {:ok, Map.put(info, :reference_policy, :unresolved_syntax)}

        {url, [reference | _]} when not is_nil(url) ->
          {:ok,
           info
           |> Map.put(:decoded_reference, reference.decoded)
           |> Map.put(:reference_policy, :static_url_syntax_only)}

        _ ->
          {:ok, Map.put(info, :reference_policy, :static_url_syntax_only)}
      end
    end
  end

  defp candidate_info(candidate) do
    tokens = HtmlValidator.split_css_tokens(candidate)

    {candidate, technologies} =
      case List.last(tokens) do
        token when is_binary(token) ->
          case Regex.run(~r/\Atech\((.*)\)\z/isu, token, capture: :all_but_first) do
            [body] ->
              technologies =
                HtmlValidator.split_css_comma(body, true)
                |> Enum.map(&String.downcase(String.trim(&1), :ascii))

              {Enum.drop(tokens, -1) |> Enum.join(" "), technologies}

            _ ->
              {candidate, []}
          end

        _ ->
          {candidate, []}
      end

    captures =
      Regex.named_captures(
        ~r/^(?<url>url\(\s*(?:"(?<double>[^"]+)"|'(?<single>[^']+)'|(?<bare>[^)'"\s]+))\s*\))(?:\s+format\(\s*(?:"(?<format_double>[^"]+)"|'(?<format_single>[^']+)'|(?<format_bare>[^)'"\s]+))\s*\))?$/ui,
        candidate
      )

    case captures do
      captures when is_map(captures) ->
        reference = first_capture(captures, ["double", "single", "bare"])
        format = first_capture(captures, ["format_double", "format_single", "format_bare"])

        %{
          url: captures["url"],
          reference: reference,
          format: format,
          supported:
            supported_font_source?(reference, format) and
              Enum.all?(technologies, &supports_technology?/1),
          technologies: technologies,
          syntax_policy: :recognized_url_candidate_syntax
        }

      _ ->
        %{supported: false, syntax_policy: :unresolved_candidate_syntax}
    end
  end

  @doc false
  @spec source_candidates(String.t()) :: [String.t()]
  def source_candidates(value) do
    {candidates, current, _quote, _depth} =
      value
      |> HtmlValidator.css_syntax_units()
      |> Enum.reduce({[], [], nil, 0}, fn character, {candidates, current, quote, depth} ->
        cond do
          character in ["\"", "'"] and is_nil(quote) ->
            {candidates, [character | current], character, depth}

          character == quote ->
            {candidates, [character | current], nil, depth}

          is_nil(quote) and character == "(" ->
            {candidates, [character | current], quote, depth + 1}

          is_nil(quote) and character == ")" ->
            {candidates, [character | current], quote, max(depth - 1, 0)}

          is_nil(quote) and depth == 0 and character == "," ->
            candidate = current |> Enum.reverse() |> Enum.join() |> String.trim()
            {[candidate | candidates], [], quote, depth}

          true ->
            {candidates, [character | current], quote, depth}
        end
      end)

    final_candidate = current |> Enum.reverse() |> Enum.join() |> String.trim()

    [final_candidate | candidates]
    |> Enum.reverse()
    |> Enum.reject(&(&1 == ""))
  end

  @doc "Reports the static outline font formats accepted by the native font source validator."
  @spec supports_format?(String.t()) :: boolean()
  def supports_format?(format),
    do:
      String.downcase(format, :ascii) in [
        "truetype",
        "opentype",
        "woff",
        "woff2",
        "collection",
        "truetype-variations",
        "opentype-variations",
        "woff-variations",
        "woff2-variations"
      ]

  @doc "Reports font technologies used by native layout, including Latin and Math OpenType features."
  @spec supports_technology?(String.t()) :: boolean()
  def supports_technology?(technology),
    do:
      String.downcase(technology, :ascii) in [
        "features-opentype",
        "variations",
        "color-colrv0",
        "color-colrv1",
        "color-svg",
        "color-sbix",
        "color-cbdt"
      ]

  defp supported_font_source?(source, format) do
    normalized_format = String.downcase(format)
    extension = source |> Path.extname() |> String.downcase()

    supported_reference? = source != "" and not String.contains?(source, "\0")

    format_supported? =
      case normalized_format do
        "" -> true
        format -> supports_format?(format)
      end

    supported_reference? and extension in [".ttf", ".otf", ".woff", ".woff2", ".ttc", ".otc"] and
      format_supported?
  end

  defp first_capture(captures, names) do
    Enum.find_value(names, "", fn name ->
      case Map.get(captures, name) do
        value when is_binary(value) and value != "" -> value
        _ -> nil
      end
    end)
  end

  defp font_selection_descriptors(declarations) do
    declarations
    |> Enum.reverse()
    |> Enum.uniq_by(&elem(&1, 0))
    |> Enum.reverse()
    |> Enum.reduce_while({:ok, %{weight: 400, style: :normal}}, fn {property, value},
                                                                   {:ok, properties} ->
      if property in ["font-weight", "font-style", "font-width", "font-stretch", "unicode-range"] or
           property in @metric_descriptors or property in @control_descriptors do
        case descriptor_properties(property, value) do
          {:ok, parsed} ->
            keys =
              case property do
                "font-weight" ->
                  [:weight, :weight_range]

                "font-style" ->
                  [:style, :oblique]

                "unicode-range" ->
                  [:unicode_range]

                property when property in @control_descriptors ->
                  case property do
                    "font-feature-settings" -> [:feature_settings]
                    "font-variation-settings" -> [:variation_settings]
                    "font-language-override" -> [:language_override]
                    "font-named-instance" -> [:named_instance]
                  end

                property when property in @metric_descriptors ->
                  [
                    Map.fetch!(
                      NativeElixirPdfUtilities.Validators.FontMetricValidator.fields(),
                      property
                    )
                  ]

                _ ->
                  [:width, :width_range]
              end

            {:cont, {:ok, Map.merge(Map.drop(properties, keys), parsed)}}

          :error ->
            {:halt, {:error, property}}
        end
      else
        {:cont, {:ok, properties}}
      end
    end)
  end

  defp descriptor_properties(property, value) do
    case property do
      "font-named-instance" ->
        value = String.trim(value)

        cond do
          String.downcase(value, :ascii) == "auto" ->
            {:ok, %{named_instance: nil}}

          Regex.match?(
            Regex.compile!("\\A" <> HtmlValidator.css_string_pattern() <> "\\z", "s"),
            value
          ) ->
            {:ok, %{named_instance: HtmlValidator.decode_css_string(value)}}

          true ->
            :error
        end

      property when property in @control_descriptors ->
        case NativeElixirPdfUtilities.Validators.CssFontControlsValidator.prepare(property, value) do
          {:ok, control} ->
            {key, value} = hd(Map.to_list(control))

            option =
              case key do
                :"feature-settings" -> :feature_settings
                :"variation-settings" -> :variation_settings
                :"language-override" -> :language_override
              end

            {:ok, %{option => value}}

          _ ->
            :error
        end

      property when property in @metric_descriptors ->
        NativeElixirPdfUtilities.Validators.FontMetricValidator.prepare_descriptor(
          property,
          value
        )

      "font-weight" ->
        with {:ok, {first, last}} <-
               CssFontValidator.prepare_range(value, &CssFontValidator.prepare_absolute_weight/1) do
          {:ok,
           if(first == last,
             do: %{weight: first},
             else: %{weight: first, weight_range: {first, last}}
           )}
        end

      "font-style" ->
        CssFontValidator.prepare_style(value, true)

      "unicode-range" ->
        with {:ok, ranges} <- CssFontValidator.prepare_unicode_range(value),
             do: {:ok, %{unicode_range: ranges}}

      _ ->
        with {:ok, {first, last}} <-
               CssFontValidator.prepare_range(value, &CssFontValidator.prepare_width/1) do
          {:ok,
           if(first == last,
             do: %{width: first},
             else: %{width: first, width_range: {first, last}}
           )}
        end
    end
  end
end
