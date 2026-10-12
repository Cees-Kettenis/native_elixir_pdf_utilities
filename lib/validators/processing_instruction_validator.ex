defmodule NativeElixirPdfUtilities.Validators.ProcessingInstructionValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  # XML 1.0 Fifth Edition Name/S and the dated XML stylesheet PseudoAtts grammar.
  # These character ranges are syntax invariants, not configurable resource limits.
  @name_start ~S(:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF})
  @name "[#{@name_start}][#{@name_start}\\-.0-9\\x{B7}\\x{300}-\\x{36F}\\x{203F}-\\x{2040}]*+"
  @reference ~S"&(?:amp|lt|gt|quot|apos);|&#[0-9]++;|&#x[0-9A-Fa-f]++;"
  @value "\"(?:[^\"<&]++|#{@reference})*+\"|'(?:[^'<&]++|#{@reference})*+'"
  @attribute "(?<name>#{@name})[ \\t\\r\\n]*+=[ \\t\\r\\n]*+(?<value>#{@value})"
  @plain_attribute String.replace(@attribute, ["(?<name>", "(?<value>"], "(?:")
  @pseudo_attributes Regex.compile!(
                       "\\A[ \\t\\r\\n]*+(?:#{@plain_attribute}(?:[ \\t\\r\\n]++#{@plain_attribute})*+[ \\t\\r\\n]*+)?\\z",
                       "u"
                     )
  @attribute_regex Regex.compile!(@attribute, "u")
  @reference_regex Regex.compile!("(#{@reference})", "u")
  @predefined %{"&amp;" => "&", "&lt;" => "<", "&gt;" => ">", "&quot;" => "\"", "&apos;" => "'"}

  @doc "Returns the complete XML pseudo-attribute map of a validated HTML processing instruction. Malformed pseudo-attribute data yields an empty map."
  @spec attributes(term()) ::
          {:ok, %{String.t() => String.t()}} | {:error, {atom(), Diagnostics.diagnostic()}}
  def attributes(node) do
    HtmlValidator.with_render_budget(fn ->
      # Bound transport string checks before calling the shared PI validator.
      for key <- [:target, :data], is_map(node), value = Map.get(node, key), is_binary(value) do
        HtmlValidator.check_render_resource(:max_html_source_bytes, byte_size(value), :html)
        HtmlValidator.reserve_render_resource(:max_html_tree_work, 3 * byte_size(value), :html)
      end

      with {:ok, node} <- HtmlValidator.validate_html_processing_instruction(node) do
        # Bound the grammar and attribute scans, their UTF-8 checks, reference
        # splitting, numeric conversion, name hashing and output copying. Reserve
        # the linear traversal allowance even when the complete data is malformed.
        HtmlValidator.reserve_render_resource(
          :max_html_tree_work,
          10 * byte_size(node.data),
          :html
        )

        result =
          if Regex.match?(@pseudo_attributes, node.data) do
            @attribute_regex
            |> Regex.scan(node.data, capture: ["name", "value"])
            |> Enum.reduce_while(%{}, fn [name, quoted], attributes ->
              value = binary_part(quoted, 1, byte_size(quoted) - 2)

              case {Map.has_key?(attributes, name), decode_value(value)} do
                {false, {:ok, value}} -> {:cont, Map.put(attributes, name, value)}
                _ -> {:halt, %{}}
              end
            end)
          else
            %{}
          end

        {:ok, result}
      end
    end)
  end

  @doc "Returns a case-sensitive PI pseudo-attribute value, or nil for an absent name or malformed data."
  @spec attribute(term(), term()) ::
          {:ok, String.t() | nil} | {:error, {atom(), Diagnostics.diagnostic()}}
  def attribute(node, name) do
    HtmlValidator.with_render_budget(fn ->
      valid_name? =
        case name do
          name when is_binary(name) ->
            HtmlValidator.check_render_resource(:max_html_source_bytes, byte_size(name), :html)
            HtmlValidator.reserve_render_resource(:max_html_tree_work, byte_size(name), :html)
            String.valid?(name)

          _ ->
            false
        end

      if valid_name? do
        with {:ok, attributes} <- attributes(node), do: {:ok, Map.get(attributes, name)}
      else
        Diagnostics.error(
          :style,
          :invalid_document,
          "processing instruction attribute names must be valid UTF-8 strings",
          module: __MODULE__,
          source: "processing_instruction"
        )
      end
    end)
  end

  defp decode_value(value) do
    @reference_regex
    |> Regex.split(value, include_captures: true, trim: true)
    |> Enum.reduce_while([], fn part, decoded ->
      result =
        case part do
          "&#" <> digits ->
            digits = binary_part(digits, 0, byte_size(digits) - 1)

            {digits, base} =
              case digits do
                "x" <> hex -> {hex, 16}
                decimal -> {decimal, 10}
              end

            digits = String.trim_leading(digits, "0")

            # XML Char ends at U+10FFFF. Its 7 decimal / 6 hex digits are fixed
            # protocol bounds; avoid constructing unbounded caller integers.
            codepoint =
              cond do
                digits == "" -> 0
                byte_size(digits) > if(base == 16, do: 6, else: 7) -> 0
                true -> elem(Integer.parse(digits, base), 0)
              end

            if codepoint in [9, 10, 13] or codepoint in 0x20..0xD7FF or
                 codepoint in 0xE000..0xFFFD or codepoint in 0x10000..0x10FFFF do
              {:ok, <<codepoint::utf8>>}
            else
              :error
            end

          _ ->
            {:ok, Map.get(@predefined, part, part)}
        end

      case result do
        {:ok, text} -> {:cont, [text | decoded]}
        :error -> {:halt, :error}
      end
    end)
    |> case do
      :error -> :error
      decoded -> {:ok, decoded |> Enum.reverse() |> IO.iodata_to_binary()}
    end
  end
end
