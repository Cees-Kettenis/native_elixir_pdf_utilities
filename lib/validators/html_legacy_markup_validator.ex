defmodule NativeElixirPdfUtilities.Validators.HtmlLegacyMarkupValidator do
  @moduledoc "HTML font and body presentation hints before the authored CSS cascade."

  alias NativeElixirPdfUtilities.Validators.{
    CssValueValidator,
    HtmlValidator,
    HtmlMetadataValidator
  }

  # HTML's seven legacy font sizes are a fixed rendering vocabulary, not limits.
  @font_sizes {7.5, 9.75, 12.0, 13.5, 18.0, 24.0, 36.0}

  @doc "Resolves applicable legacy hints using the shared HTML number and color rules."
  @spec presentation_hints(map(), map()) :: map()
  def presentation_hints(node, default_margin \\ CssValueValidator.edges(6.0)) do
    if Map.get(node, :namespace, :html) == :html do
      attributes = HtmlMetadataValidator.unqualified_attributes(node)

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

          # Metadata-free caller trees have no authored ordering information.
          # Use the fixed hint vocabulary in that case, never map iteration.
          order =
            Map.get(node, :attribute_order, ~w(marginwidth marginheight leftmargin topmargin))

          HtmlValidator.reserve_render_resource(:max_html_tree_work, length(order), :html)

          margin =
            Enum.reduce(
              order,
              %{},
              fn name, margin ->
                sides =
                  case name do
                    name when name in ~w(marginwidth leftmargin) -> [:left, :right]
                    name when name in ~w(marginheight topmargin) -> [:top, :bottom]
                    _ -> []
                  end

                if sides != [] and Map.has_key?(attributes, name) do
                  # Both aliases set both sides of their axis. Invalid hints
                  # leave earlier authored hints and normalized defaults intact.
                  case HtmlValidator.parse_html_integer(attributes[name], :nonnegative) do
                    {:ok, size} ->
                      case HtmlValidator.parse_css_number(Integer.to_string(size)) do
                        {size, ""} -> Enum.reduce(sides, margin, &Map.put(&2, &1, size * 0.75))
                        _ -> margin
                      end

                    :error ->
                      margin
                  end
                else
                  margin
                end
              end
            )

          if margin == %{},
            do: hints,
            else: Map.put(hints, :margin, Map.merge(default_margin, margin))

        _ ->
          %{}
      end
    else
      %{}
    end
  end
end
