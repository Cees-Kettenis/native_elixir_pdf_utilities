defmodule NativeElixirPdfUtilities.Validators.HtmlLegacyMarkupValidator do
  @moduledoc "HTML font and body presentation hints before the authored CSS cascade."

  alias NativeElixirPdfUtilities.Validators.{CssValueValidator, HtmlValidator}

  # HTML's seven legacy font sizes are a fixed rendering vocabulary, not limits.
  @font_sizes {7.5, 9.75, 12.0, 13.5, 18.0, 24.0, 36.0}

  @doc "Resolves applicable legacy hints using the shared HTML number and color rules."
  @spec presentation_hints(map()) :: map()
  def presentation_hints(node) do
    if Map.get(node, :namespace, :html) == :html do
      attributes = node.attributes

      case node.tag do
        "font" ->
          hints =
            case HtmlValidator.parse_html_legacy_color(Map.get(attributes, "color")) do
              {:ok, color} -> %{color: color}
              :error -> %{}
            end

          hints =
            case Map.get(attributes, "face") do
              value when is_binary(value) and value != "" -> Map.put(hints, :font_families, value)
              _ -> hints
            end

          case HtmlValidator.parse_html_integer(Map.get(attributes, "size")) do
            {:ok, size} ->
              source = HtmlValidator.trim_ascii_whitespace(attributes["size"])
              size = if String.starts_with?(source, ["+", "-"]), do: size + 3, else: size
              Map.put(hints, :font_size, elem(@font_sizes, min(max(size, 1), 7) - 1))

            :error ->
              hints
          end

        "body" ->
          hints =
            Enum.reduce(
              [{"bgcolor", :background_color}, {"text", :color}, {"link", :_body_link_color}],
              %{},
              fn {attribute, key}, hints ->
                case HtmlValidator.parse_html_legacy_color(Map.get(attributes, attribute)) do
                  {:ok, color} -> Map.put(hints, key, color)
                  :error -> hints
                end
              end
            )

          hints =
            case Map.get(attributes, "background") do
              value when is_binary(value) and value != "" ->
                Map.put(hints, :background_image_source, value)

              _ ->
                hints
            end

          margin =
            Enum.reduce(
              [
                {"marginwidth", [:left, :right]},
                {"marginheight", [:top, :bottom]},
                {"leftmargin", [:left]},
                {"topmargin", [:top]},
                {"rightmargin", [:right]},
                {"bottommargin", [:bottom]}
              ],
              %{},
              fn {attribute, sides}, margin ->
                case HtmlValidator.parse_html_integer(
                       Map.get(attributes, attribute),
                       :nonnegative
                     ) do
                  {:ok, size} ->
                    case HtmlValidator.parse_css_number(Integer.to_string(size)) do
                      {size, ""} -> Enum.reduce(sides, margin, &Map.put(&2, &1, size * 0.75))
                      _ -> margin
                    end

                  :error ->
                    margin
                end
              end
            )

          if margin == %{},
            do: hints,
            else: Map.put(hints, :margin, Map.merge(CssValueValidator.edges(6.0), margin))

        _ ->
          %{}
      end
    else
      %{}
    end
  end
end
