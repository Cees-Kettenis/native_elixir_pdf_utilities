defmodule NativeElixirPdfUtilities.Validators.CssFontFaceValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc false
  @spec validate([NativeElixirPdfUtilities.HtmlToPdf.CssParser.declaration()]) ::
          {:ok, NativeElixirPdfUtilities.HtmlToPdf.CssParser.font_face()}
          | {:error, %{property: String.t(), index: non_neg_integer() | nil}}
  def validate(declarations) do
    case Enum.find(declarations, &(not supported_font_descriptor?(&1))) do
      nil ->
        family = font_family_descriptor(declarations)
        sources = font_source_descriptor(declarations)
        weight = font_weight_descriptor(declarations)
        style = font_style_descriptor(declarations)

        case {family, sources, weight, style} do
          {{:ok, family}, {:ok, sources}, {:ok, weight}, {:ok, style}} ->
            {:ok, %{family: family, sources: sources, weight: weight, style: style}}

          _ ->
            property =
              Enum.find_value(
                [
                  {"font-family", family},
                  {"src", sources},
                  {"font-weight", weight},
                  {"font-style", style}
                ],
                fn {property, result} -> if result == {:error, :invalid_css}, do: property end
              )

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

  defp supported_font_descriptor?(declaration) do
    case declaration do
      {property, _value} when property in ["font-family", "src", "font-weight", "font-style"] ->
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
  def validate_source_candidate(candidate) do
    info = candidate_info(candidate)

    case Map.get(info, :url) do
      nil ->
        {:ok, Map.put(info, :reference_policy, :unresolved_syntax)}

      url ->
        with {:ok, decoded} <- HtmlValidator.parse_background_image_reference(url) do
          {:ok,
           info
           |> Map.put(:decoded_reference, decoded)
           |> Map.put(:reference_policy, :static_url_syntax_only)}
        end
    end
  end

  defp candidate_info(candidate) do
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
          supported: supported_font_source?(reference, format)
        }

      _ ->
        %{supported: false}
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

  defp supported_font_source?(source, format) do
    normalized_format = String.downcase(format)
    extension = source |> Path.extname() |> String.downcase()

    supported_reference? = source != "" and not String.contains?(source, "\0")

    format_supported? =
      case normalized_format do
        "" -> true
        format -> format in ["truetype", "opentype"]
      end

    supported_reference? and extension in [".ttf", ".otf"] and format_supported?
  end

  defp first_capture(captures, names) do
    Enum.find_value(names, "", fn name ->
      case Map.get(captures, name) do
        value when is_binary(value) and value != "" -> value
        _ -> nil
      end
    end)
  end

  defp font_weight_descriptor(declarations) do
    case declarations |> Enum.reverse() |> List.keyfind("font-weight", 0) do
      nil ->
        {:ok, 400}

      {"font-weight", value} ->
        case String.downcase(String.trim(value)) do
          "normal" -> {:ok, 400}
          "bold" -> {:ok, 700}
          value -> parsed_font_weight(Integer.parse(value))
        end
    end
  end

  defp parsed_font_weight(parsed) do
    case parsed do
      {weight, ""} when weight >= 100 and weight <= 900 -> {:ok, weight}
      _ -> {:error, :invalid_css}
    end
  end

  defp font_style_descriptor(declarations) do
    case declarations |> Enum.reverse() |> List.keyfind("font-style", 0) do
      nil ->
        {:ok, :normal}

      {"font-style", value} ->
        case String.downcase(String.trim(value)) do
          "normal" -> {:ok, :normal}
          "italic" -> {:ok, :italic}
          _ -> {:error, :invalid_css}
        end
    end
  end
end
