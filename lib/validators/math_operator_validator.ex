defmodule NativeElixirPdfUtilities.Validators.MathOperatorValidator do
  @moduledoc """
  Prepares MathML Core operator semantics independently of glyph measurement.

  The source is the 24 June 2025 MathML Core snapshot, sections 3.2.4.1,
  3.2.4.2 and B.1. Invalid authored attribute syntax uses Core's dictionary
  fallback. Invalid API arguments and exceeded resource limits return diagnostics.
  This module does not render MathML or select stretch glyphs.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.MathOperatorDictionary
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @dictionary MathOperatorDictionary.data()
  @row_parents [
    :grouping,
    "maction",
    "math",
    "mrow",
    "mstyle",
    "merror",
    "mphantom",
    "mprescripts",
    "semantics",
    "mpadded",
    "msqrt"
  ]
  @script_parents ["msub", "msup", "msubsup", "munder", "mover", "munderover", "mmultiscripts"]
  @boolean_properties [:stretchy, :symmetric, :largeop, :movablelimits, :fence, :separator]
  @length_properties [:lspace, :rspace, :minsize, :maxsize]
  # CSS units requiring viewport, container, or additional selected-font metrics.
  # These are syntax names, not configurable resource limits.
  @contextual_units ~w(ch cap ic lh rlh rex rch rcap ric vw vh vi vb vmin vmax svw svh svi svb svmin svmax lvw lvh lvi lvb lvmin lvmax dvw dvh dvi dvb dvmin dvmax cqw cqh cqi cqb cqmin cqmax)
  @context_keys [
    :parent,
    :index,
    :count,
    :child_index,
    :font_size,
    :root_font_size,
    :x_height,
    :text_only
  ]

  @typedoc "Point lengths, glyph-height percentages, or the unbounded Core maximum."
  @type size_constraint :: number() | {:percentage, number()} | :infinity

  @typedoc "Validated operator properties for a prepared MathML core operator."
  @type semantics :: %{
          text: String.t(),
          form: :infix | :prefix | :postfix,
          explicit_form: boolean(),
          category: atom(),
          lspace: number(),
          rspace: number(),
          stretchy: boolean(),
          symmetric: boolean(),
          fence: boolean(),
          separator: boolean(),
          largeop: boolean(),
          movablelimits: boolean(),
          minsize: size_constraint(),
          maxsize: size_constraint(),
          stretch_axis: :horizontal | :vertical,
          single_character: boolean(),
          invisible: boolean()
        }

  @doc """
  Prepares the dictionary and authored properties of an `mo` core operator.

  `attributes` is a map of binary attribute names and values. `text` is preserved
  exactly, including invisible operators, combining overlays and ASCII pairs.
  The caller supplies the already prepared token text, without further cleanup.

  `context` describes the outer embellished operator, not an inner wrapper:

  * `parent` is the MathML parent tag name, or `nil` for a detached operator.
    Use `:grouping` for prepared unknown MathML elements or anonymous rows.
  * `index` and `count` describe in-flow siblings excluding space-like children.
    They default to zero and one. Empty sibling lists use index zero/count zero.
  * `child_index` is the index amongst all in-flow siblings and defaults to `index`.
    It determines postfix form for a script child that is not the base.
  * `font_size` defaults to 12 points; `root_font_size` defaults to `font_size`.
    These sizes belong to the core operator. Optional `x_height` supplies the
    selected core font's x-height in points for `ex` lengths.
  * `text_only` defaults to true. Set false when the core contains element
    children, so it cannot use the text-keyed dictionary or single-glyph layout.

  Spacing is returned in points. Percentage spacing uses the dictionary spacing
  as its base. Percentage min/max sizes remain tagged until natural glyph height
  is available. These are parsed constraints, not stretch targets. Negative
  constraints are valid Core values. Follow-up stretch preparation in the math
  validator must resolve percentages and clamp min/max against natural glyph
  height; consumers must not independently reinterpret these constraints.
  Fence and separator are semantic metadata and do not change Core geometry.
  This slice supports numeric percentages and absolute, em, rem and ex lengths.
  CSS calculations and units requiring other geometry are diagnosed explicitly.
  `stretch_axis` uses the names accepted by `MathLayout.stretch/6`, even when
  `stretchy` is false. Multi-character operators need text layout, not stretch/6.
  """
  @spec prepare(term(), term(), term()) ::
          {:ok, semantics()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(text, attributes, context \\ %{}) do
    with :ok <- validate_arguments(text, attributes, context) do
      size = Map.get(context, :font_size, 12)
      context = Map.put_new(context, :root_font_size, size)
      explicit = Map.get(attributes, "form", "") |> String.downcase(:ascii)
      explicit? = explicit in ["infix", "prefix", "postfix"]
      index = Map.get(context, :index, 0)
      count = Map.get(context, :count, 1)
      parent = Map.get(context, :parent)

      form =
        cond do
          explicit? -> String.to_existing_atom(explicit)
          parent in @row_parents and count > 1 and index == 0 -> :prefix
          parent in @row_parents and count > 1 and index == count - 1 -> :postfix
          parent in @script_parents and Map.get(context, :child_index, index) > 0 -> :postfix
          true -> :infix
        end

      text_only? = Map.get(context, :text_only, true)
      dictionary_key = if text_only?, do: dictionary_key(text)
      category = category(dictionary_key, form)

      category =
        if category == :default and not explicit? and text_only? do
          Enum.reduce_while([:infix, :postfix, :prefix], :default, fn candidate, _ ->
            case category(dictionary_key, candidate) do
              :default -> {:cont, :default}
              found -> {:halt, found}
            end
          end)
        else
          category
        end

      defaults = Map.fetch!(@dictionary.categories, category)
      single? = text_only? and match?(<<_::utf8>>, text)

      codepoint =
        case text do
          <<codepoint::utf8>> when text_only? -> codepoint
          _ -> nil
        end

      defaults =
        Map.merge(defaults, %{
          fence: MapSet.member?(@dictionary.fences, dictionary_key),
          separator: MapSet.member?(@dictionary.separators, dictionary_key),
          minsize: {:percentage, 100.0},
          maxsize: :infinity
        })

      properties =
        Enum.reduce(@boolean_properties, defaults, fn key, properties ->
          value = Map.get(attributes, Atom.to_string(key), "") |> String.downcase(:ascii)

          case value do
            "true" -> Map.put(properties, key, true)
            "false" -> Map.put(properties, key, false)
            _ -> properties
          end
        end)

      with {:ok, properties} <- prepare_lengths(properties, defaults, attributes, context, size) do
        {:ok,
         Map.merge(properties, %{
           text: text,
           form: form,
           explicit_form: explicit?,
           category: category,
           single_character: single?,
           invisible: codepoint in 0x2061..0x2064,
           stretch_axis:
             if(MapSet.member?(@dictionary.inline_axes, codepoint),
               do: :horizontal,
               else: :vertical
             )
         })}
      end
    end
  end

  # Centralize API shape and context checks so preparation can rely on them.
  defp validate_arguments(text, attributes, context) do
    limit = Limits.get(:max_css_numeric_magnitude)

    valid? =
      is_binary(text) and String.valid?(text) and is_map(attributes) and
        Enum.all?(Map.to_list(attributes), fn {key, value} ->
          is_binary(key) and String.valid?(key) and is_binary(value) and String.valid?(value)
        end) and is_map(context) and Enum.all?(Map.keys(context), &(&1 in @context_keys))

    if valid? do
      index = Map.get(context, :index, 0)
      count = Map.get(context, :count, 1)
      child_index = Map.get(context, :child_index, index)
      size = Map.get(context, :font_size, 12)
      root = Map.get(context, :root_font_size, size)
      x_height = Map.get(context, :x_height)
      parent = Map.get(context, :parent)

      cond do
        byte_size(text) > Limits.get(:max_rendered_text_bytes) ->
          failure(:resource_limit_exceeded, "operator text exceeds max_rendered_text_bytes", nil)

        not (is_integer(index) and index >= 0 and is_integer(count) and count >= 0 and
               (index < count or (index == 0 and count == 0)) and
               is_integer(child_index) and child_index >= 0 and
               (is_nil(parent) or parent == :grouping or
                  (is_binary(parent) and String.valid?(parent))) and
               is_boolean(Map.get(context, :text_only, true)) and
               Enum.all?([size, root], &(is_number(&1) and &1 >= 0 and &1 <= limit)) and
                 (is_nil(x_height) or
                    (is_number(x_height) and x_height >= 0 and x_height <= limit))) ->
          failure(:invalid_document, "invalid operator position or core font context", text)

        count > Limits.get(:max_html_nodes) or child_index > Limits.get(:max_html_nodes) ->
          failure(
            :resource_limit_exceeded,
            "operator sibling context exceeds max_html_nodes",
            text
          )

        true ->
          :ok
      end
    else
      failure(
        :invalid_document,
        "operator preparation requires valid UTF-8 text, binary attributes, and supported context",
        if(is_binary(text) and String.valid?(text), do: text)
      )
    end
  end

  # B.1 keys use UTF-16 code units, not graphemes. Overlays and the ASCII-pair
  # aliases affect lookup only. Never rewrite the original extraction text.
  defp dictionary_key(text) do
    case text do
      <<codepoint::utf8>> when codepoint < 0x0320 or codepoint > 0x03FF ->
        codepoint

      <<first::utf8, overlay::utf8>> when first <= 0xFFFF and overlay in [0x0338, 0x20D2] ->
        first

      _ ->
        Map.get(@dictionary.ascii_pairs, text)
    end
  end

  defp category(codepoint, form) do
    cond do
      codepoint in [0x1EEF0, 0x1EEF1] and form == :postfix -> :i
      codepoint in [0x007C, 0x223C] and form == :infix -> :force_default
      true -> Map.get(@dictionary.entries, {codepoint, form}, :default)
    end
  end

  # The same signed CSS length grammar applies to all four attributes. Percentage
  # spacing resolves against dictionary values; percentage sizes await glyphs.
  defp prepare_lengths(properties, defaults, attributes, context, size) do
    Enum.reduce_while(@length_properties, {:ok, properties}, fn key, {:ok, properties} ->
      default = Map.fetch!(defaults, key)
      value = Map.get(attributes, Atom.to_string(key))
      spacing? = key in [:lspace, :rspace]
      fallback = if spacing?, do: default * size, else: default

      if is_binary(value) and byte_size(value) > Limits.get(:max_html_numeric_token_bytes) do
        {:halt,
         failure(
           :resource_limit_exceeded,
           "operator #{key} exceeds max_html_numeric_token_bytes",
           value
         )}
      else
        normalized =
          if is_binary(value),
            do: HtmlValidator.trim_ascii_whitespace(value) |> String.downcase(:ascii)

        prepared =
          case HtmlValidator.parse_css_number(normalized) do
            :error ->
              :error

            {number, "%"} ->
              if spacing?,
                do: {:ok, default * size * (number / 100)},
                else: {:ok, {:percentage, number}}

            {_number, "ex"} ->
              if is_number(Map.get(context, :x_height)) do
                case HtmlValidator.parse_css_length(
                       String.replace_suffix(normalized, "ex", "em"),
                       :margin,
                       context.x_height
                     ) do
                  :error -> :numeric_limit
                  result -> result
                end
              else
                :missing_x_height
              end

            {_number, unit} when unit in @contextual_units ->
              :unsupported_geometry

            {_number, unit} when unit in ["pc", "q"] ->
              # Fixed CSS absolute-unit conversions: 1pc = 12pt, 1q = 1/4mm.
              case HtmlValidator.parse_css_number(
                     normalized,
                     if(unit == "pc", do: 12, else: 72 / 101.6)
                   ) do
                {points, _unit} -> {:ok, points}
                :error -> :numeric_limit
              end

            {_number, unit} ->
              case HtmlValidator.parse_css_length(normalized, :margin, size) do
                {:ok, {:rem, number}} ->
                  resolved = number * context.root_font_size

                  if abs(resolved) <= Limits.get(:max_css_numeric_magnitude),
                    do: {:ok, resolved},
                    else: :numeric_limit

                other ->
                  if other == :error and unit in ["pt", "px", "mm", "cm", "in", "em", "rem"],
                    do: :numeric_limit,
                    else: other
              end
          end

        case prepared do
          :unsupported_geometry ->
            {:halt,
             failure(
               :invalid_document,
               "operator #{key} requires unsupported CSS length geometry",
               value
             )}

          :numeric_limit ->
            {:halt,
             failure(
               :resource_limit_exceeded,
               "operator #{key} exceeds max_css_numeric_magnitude",
               value
             )}

          :missing_x_height ->
            {:halt,
             failure(
               :invalid_document,
               "operator #{key} in ex requires selected-font x_height",
               value
             )}

          {:ok, number} when is_number(number) ->
            if abs(number) <= Limits.get(:max_css_numeric_magnitude) do
              {:cont, {:ok, Map.put(properties, key, number)}}
            else
              {:halt,
               failure(
                 :resource_limit_exceeded,
                 "operator #{key} exceeds max_css_numeric_magnitude",
                 value
               )}
            end

          {:ok, percentage} ->
            {:cont, {:ok, Map.put(properties, key, percentage)}}

          :error ->
            if is_binary(normalized) and
                 String.starts_with?(normalized, ["calc(", "min(", "max(", "clamp("]) do
              {:halt,
               failure(
                 :invalid_document,
                 "operator #{key} CSS calculations are not supported by this slice",
                 value
               )}
            else
              {:cont, {:ok, Map.put(properties, key, fallback)}}
            end
        end
      end
    end)
  end

  defp failure(reason, message, source) do
    Diagnostics.error(:layout, reason, message,
      operation: :prepare_math_operator,
      module: __MODULE__,
      source: source
    )
  end
end
