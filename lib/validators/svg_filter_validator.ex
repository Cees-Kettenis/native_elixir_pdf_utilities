defmodule NativeElixirPdfUtilities.Validators.SvgFilterValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @primitives ~w(feBlend feColorMatrix feComponentTransfer feComposite feConvolveMatrix feDiffuseLighting feDisplacementMap feDropShadow feFlood feGaussianBlur feImage feMerge feMorphology feOffset feSpecularLighting feTile feTurbulence)
  @children ~w(feMergeNode feFuncR feFuncG feFuncB feFuncA feDistantLight fePointLight feSpotLight)
  @doc "Lists SVG filter element names admitted by the canonical source policy."
  @spec tags() :: [String.t()]
  def tags, do: ["filter" | @primitives ++ @children]

  @doc "Validates filter source attributes without preparing or executing a filter renderer."
  @spec validate_source_attributes(term(), term()) :: :ok | {:error, {atom(), map()}}
  def validate_source_attributes(tag, attributes) do
    case {tag, attributes} do
      {"feGaussianBlur", attributes} when is_map(attributes) ->
        value = Map.get(attributes, "stdDeviation", "0")

        valid? =
          if is_binary(value) do
            value = HtmlValidator.trim_ascii_whitespace(value)
            parts = String.split(value, ~r/[ \t\r\n,]+/, trim: true)

            length(parts) in [1, 2] and
              not Regex.match?(~r/\A,|,\z|,[ \t\r\n]*,/, value) and
              Enum.all?(parts, fn part ->
                case HtmlValidator.parse_css_number(part) do
                  {number, ""} -> number >= 0
                  _ -> false
                end
              end)
          else
            false
          end

        if valid? do
          :ok
        else
          Diagnostics.error(
            :layout,
            :invalid_document,
            "feGaussianBlur stdDeviation requires one or two finite nonnegative unitless numbers; got #{inspect(value)}",
            module: __MODULE__,
            operation: :prepare,
            source: "feGaussianBlur"
          )
        end

      {tag, attributes} when is_binary(tag) and is_map(attributes) ->
        :ok

      _ ->
        Diagnostics.error(
          :layout,
          :invalid_document,
          "SVG filter source requires an element name and attribute map",
          module: __MODULE__,
          operation: :prepare,
          source: "SVG filter attributes"
        )
    end
  end
end
