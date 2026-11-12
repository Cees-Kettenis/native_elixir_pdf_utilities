defmodule NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator do
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeLineBreak

  @discretionary_characters ["\u00AD", "\u200B"]

  @doc "Preserves hard and soft HTML break semantics after the validated CSS cascade."
  @spec element_style(String.t(), map()) :: map()
  def element_style(tag, style) do
    display =
      case {tag, style.display} do
        {_, :none} -> :none
        {"br", _} -> :line_break
        {"wbr", _} -> :soft_break
      end

    style |> Map.put(:display, display) |> Map.delete(:background_image_source)
  end

  @doc "Identifies layout controls whose original codepoint needs no font glyph."
  @spec layout_whitespace?(term(), map()) :: boolean()
  def layout_whitespace?(grapheme, style) do
    grapheme in ["\t", "\n", "\r", "\r\n"] or
      (grapheme in ["\u2028", "\u2029"] and not Map.has_key?(style, :_math_native_group))
  end

  @doc "Returns validated Unicode byte offsets, optionally adding CSS preserved-space opportunities."
  @spec opportunities(term()) ::
          {:ok, [{non_neg_integer(), boolean()}]} | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec opportunities(term(), term()) ::
          {:ok, [{non_neg_integer(), boolean()}]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def opportunities(text, options \\ false) do
    with {:ok, policy} <- prepare_policy(options) do
      if is_binary(text) do
        HtmlValidator.reserve_render_resource(:max_work, byte_size(text), :layout)

        if String.valid?(text, :fast_ascii) do
          result =
            if policy.line_break == :anywhere do
              {breaks, _} =
                Enum.map_reduce(UnicodeGrapheme.graphemes(text), 0, fn cluster, offset ->
                  offset = offset + byte_size(cluster)
                  {{offset, offset == byte_size(text)}, offset}
                end)

              breaks
            else
              UnicodeLineBreak.opportunities(text, policy)
            end

          HtmlValidator.reserve_render_resource(
            :max_work,
            byte_size(text) + length(result),
            :layout
          )

          {:ok,
           css_space_opportunities(
             text,
             result,
             word_hyphen_opportunities(text),
             policy
           )}
        else
          failure("line breaking requires valid UTF-8 text")
        end
      else
        failure("line breaking requires UTF-8 text")
      end
    end
  catch
    {:render_resource_limit, error} -> error
  end

  @doc "Admits one shared CSS line-breaking policy for style validation and every inline consumer."
  @spec prepare_policy(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_policy(options) do
    case options do
      flag when is_boolean(flag) ->
        {:ok,
         %{
           line_break: :normal,
           word_break: :normal,
           language: "und",
           break_spaces?: flag,
           hyphens: :manual
         }}

      style when is_map(style) ->
        line =
          case :maps.get(:line_break, style, :normal) do
            :auto -> :normal
            value -> value
          end

        word = :maps.get(:word_break, style, :normal)
        overflow = :maps.get(:overflow_wrap, style, :normal)

        if line in [:normal, :strict, :loose, :anywhere, :break_word] and
             word in [:normal, :break_all, :keep_all, :break_word] and
             overflow in [:normal, :anywhere, :break_word] do
          with :ok <-
                 NativeElixirPdfUtilities.Validators.CssHyphenationValidator.validate_style(
                   style,
                   "HTML inline text"
                 ),
               {:ok, parts} <-
                 NativeElixirPdfUtilities.Validators.LanguageValidator.prepare_identifier(
                   :maps.get(:_text_language, style, "und")
                 ) do
            {:ok,
             %{
               line_break: line,
               word_break: word,
               language: List.first(parts) || "und",
               break_spaces?: :maps.get(:white_space, style, nil) == :break_spaces,
               hyphens: :maps.get(:hyphens, style, :manual)
             }}
          end
        else
          failure(
            "styled text requires a supported line_break, word_break and overflow_wrap policy; received #{inspect({line, word, overflow})}"
          )
        end

      _ ->
        failure("preserved-space line breaking requires a boolean option or a styled text policy")
    end
  end

  @doc "Validates a native Unicode break protocol result against its UTF-8 source and shared work budget."
  @spec validate_native_opportunities(term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_native_opportunities(text, result) do
    with {:ok, _word_hyphens} <- native_opportunity_plan(text, result), do: :ok
  end

  # Retained protocol admission also serves callers validating an offset list.
  defp native_opportunity_plan(text, result) do
    case {text, result} do
      {text, result} when is_binary(text) and is_list(result) and length(result) >= 0 ->
        HtmlValidator.reserve_render_resource(
          :max_work,
          byte_size(text) + length(result),
          :layout
        )

        if String.valid?(text, :fast_ascii) do
          {grapheme_offsets, _offset} =
            text
            |> UnicodeGrapheme.graphemes()
            |> Enum.reduce({MapSet.new(), 0}, fn cluster, {offsets, offset} ->
              offset = offset + byte_size(cluster)
              {MapSet.put(offsets, offset), offset}
            end)

          {valid?, _last} =
            Enum.reduce(result, {true, -1}, fn item, {valid?, previous} ->
              case item do
                {offset, mandatory} when is_integer(offset) and is_boolean(mandatory) ->
                  {valid? and offset > previous and offset <= byte_size(text) and
                     (MapSet.member?(grapheme_offsets, offset) or text == ""), offset}

                _ ->
                  {false, previous}
              end
            end)

          if valid? and (List.last(result) == {byte_size(text), true} or text == ""),
            do: {:ok, word_hyphen_opportunities(text)},
            else: failure("native Unicode line breaking returned an invalid grapheme boundary")
        else
          failure("native Unicode line breaking requires valid UTF-8 source text")
        end

      _ ->
        failure("native Unicode line breaking requires UTF-8 source text and a result list")
    end
  catch
    {:render_resource_limit, error} -> error
  end

  defp word_hyphen_opportunities(text) do
    if :binary.match(text, "-") == :nomatch do
      []
    else
      Regex.scan(~r/\p{L}-(?=\p{Nd})/u, text, return: :index)
      |> Enum.map(fn [{offset, bytes}] -> {offset + bytes, false} end)
    end
  end

  @doc "Segments at shared Unicode and optional preserved-space opportunities, retaining exact text and preceding context."
  @spec segments(term()) :: {:ok, [binary()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec segments(term(), term()) ::
          {:ok, [binary()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec segments(term(), term(), term()) ::
          {:ok, [binary()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def segments(text, before \\ "", break_spaces? \\ false) do
    case {text, before} do
      {text, before} when is_binary(text) and is_binary(before) ->
        with {:ok, opportunities} <- opportunities(before <> text, break_spaces?) do
          start = byte_size(before)

          {segments, _} =
            opportunities
            |> Enum.filter(fn {offset, _mandatory} -> offset > start end)
            |> Enum.map_reduce(0, fn {offset, _mandatory}, previous ->
              offset = offset - start
              {binary_part(text, previous, offset - previous), offset}
            end)

          {:ok, segments}
        end

      _ ->
        failure("line breaking requires UTF-8 text and preceding inline context")
    end
  end

  @doc "Determines whether a styled text boundary is a Unicode line break opportunity."
  @spec boundary?(binary(), binary()) ::
          {:ok, boolean()} | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec boundary?(binary(), binary(), term()) ::
          {:ok, boolean()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def boundary?(before, after_text, break_spaces? \\ false) do
    with {:ok, breaks} <- opportunities(before <> after_text, break_spaces?) do
      {:ok, Enum.any?(breaks, fn {offset, _mandatory} -> offset == byte_size(before) end)}
    end
  end

  @doc "Prepares visible ordinary glyph text and exact authored selection text for discretionary formatting."
  @spec display_plan(term(), Font.font_face(), term()) ::
          {:ok, %{text: binary(), actual_text: binary(), discretionary?: boolean()}}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec display_plan(term(), Font.font_face()) ::
          {:ok, %{text: binary(), actual_text: binary(), discretionary?: boolean()}}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec display_plan(term(), Font.font_face(), term(), map()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def display_plan(text, font, break_after? \\ false, style \\ %{}) do
    with {:ok, visible} <- display_text(text, font, break_after?, style) do
      {:ok, %{text: visible, actual_text: text, discretionary?: visible != text}}
    end
  end

  @doc "Prepares the same visible glyph text for measurement without allocating a selection/paint plan."
  @spec display_text(term(), Font.font_face(), term()) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec display_text(term(), Font.font_face()) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec display_text(term(), Font.font_face(), term(), map()) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  def display_text(text, font, break_after? \\ false, style \\ %{}) do
    with :ok <-
           NativeElixirPdfUtilities.Validators.CssHyphenationValidator.validate_style(
             style,
             "HTML inline text"
           ) do
      case {text, break_after?} do
        {text, selected?} when is_binary(text) and is_boolean(selected?) ->
          HtmlValidator.reserve_render_resource(:max_work, byte_size(text), :layout)

          if String.valid?(text, :fast_ascii) do
            # U+00AD and U+200B remain in UAX14 input and ActualText, not in unbroken glyph paint.
            visible = strip_discretionary_characters(text)

            # Chromium paints LS/PS as spaces, not LF breaks. Keep their source in ActualText.
            visible =
              if :binary.match(visible, <<0xE2>>) == :nomatch,
                do: visible,
                else: String.replace(visible, ["\u2028", "\u2029"], " ")

            hyphen =
              if selected? and String.ends_with?(text, "\u00AD") and
                   :maps.get(:hyphens, style, nil) != :none do
                # CSS Text 3 uses a real selected-font Unicode hyphen at a taken manual break.
                case :maps.get(:hyphenate_character, style, :auto) do
                  :auto ->
                    Enum.find(["\u2010", "-", "\u00AD"], &Font.supports_text?(font, &1))

                  text ->
                    if NativeElixirPdfUtilities.Validators.FontRenderingValidator.supports_text?(
                         font,
                         text
                       ),
                       do: text
                end
              else
                ""
              end

            case hyphen do
              nil ->
                failure("the selected font cannot paint a conditional hyphen")

              hyphen ->
                {:ok, visible <> hyphen}
            end
          else
            failure("discretionary text requires valid UTF-8")
          end

        {_, flag} when not is_boolean(flag) ->
          failure("discretionary line selection requires a boolean option")

        _ ->
          failure("discretionary text requires UTF-8 text")
      end
    end
  catch
    {:render_resource_limit, error} -> error
  end

  @doc "Identifies otherwise identical inline boundaries within one unbroken shaping context."
  @spec transparent_format_boundary?(term(), term()) ::
          {:ok, boolean()} | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec transparent_format_boundary?(term(), term(), map() | nil) ::
          {:ok, boolean()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def transparent_format_boundary?(before, after_text, style \\ nil) do
    case {before, after_text} do
      {before, after_text} when is_binary(before) and is_binary(after_text) ->
        HtmlValidator.reserve_render_resource(
          :max_work,
          byte_size(before) + byte_size(after_text),
          :layout
        )

        if String.valid?(before, :fast_ascii) and String.valid?(after_text, :fast_ascii) do
          discretionary? =
            String.ends_with?(before, @discretionary_characters) or
              (after_text != "" and strip_discretionary_characters(after_text) == "")

          if discretionary? or before == "" or after_text == "" or style == nil do
            {:ok, discretionary?}
          else
            NativeElixirPdfUtilities.Validators.TextShapingValidator.shared_context?(
              before,
              after_text,
              style
            )
          end
        else
          failure("discretionary inline boundaries require valid UTF-8")
        end

      _ ->
        failure("discretionary inline boundaries require UTF-8 text")
    end
  catch
    {:render_resource_limit, error} -> error
  end

  @compile {:inline, strip_discretionary_characters: 1}
  defp strip_discretionary_characters(text) do
    if :binary.match(text, "\u00AD") == :nomatch and
         :binary.match(text, "\u200B") == :nomatch,
       do: text,
       else: String.replace(text, @discretionary_characters, "")
  end

  @doc "Rounds a validated inline fragment width upward to the fixed CSS layout unit."
  @spec snap_fragment_width(number()) :: float()
  def snap_fragment_width(width) do
    Float.ceil(width / 0.75 * 64) / 64 * 0.75
  end

  @doc "Applies the fixed one-layout-unit fitting allowance to validated inline widths."
  @spec fits_width?(number(), number()) :: boolean()
  def fits_width?(width, available) do
    # CSS 1/64-pixel layout units are a format precision invariant, not a resource limit.
    width <= available + 0.75 / 64
  end

  # CSS Text 3 keeps the Unicode breaks and adds a break after every preserved
  # space, tab and other Zs separator. NBSP and extended graphemes stay intact.
  defp css_space_opportunities(text, opportunities, word_hyphens, policy) do
    # Chromium's printed CSS keeps the break after an alphabetic word's
    # hyphen before digits (for example STYLE-5376). The native Unicode
    # numeric rule otherwise joins that boundary to the following number.
    # Retain numeric prefixes and numeric ranges as indivisible sequences.
    opportunities =
      (opportunities ++ word_hyphens)
      # Chromium's ASCII compatibility rules keep slash-separated words intact,
      # including literal markup in a text document. Unicode rules still apply
      # to the boundary between a slash and a non-ASCII character.
      |> Enum.reject(fn {offset, mandatory?} ->
        not mandatory? and policy.word_break != :break_all and policy.line_break != :anywhere and
          offset > 0 and offset < byte_size(text) and
          :binary.at(text, offset - 1) == ?/ and :binary.at(text, offset) in 0x21..0x7F
      end)
      |> Map.new()
      |> Enum.sort_by(&elem(&1, 0))

    opportunities =
      Enum.reject(opportunities, fn {offset, mandatory?} ->
        not mandatory? and :maps.get(:hyphens, policy, :manual) == :none and offset >= 2 and
          binary_part(text, offset - 2, 2) == "\u00AD"
      end)

    opportunities =
      if Regex.match?(~r/[^\x00-\x7F]/u, text) do
        {boundaries, _} =
          Enum.reduce(UnicodeGrapheme.graphemes(text), {MapSet.new(), 0}, fn cluster,
                                                                             {boundaries, offset} ->
            offset = offset + byte_size(cluster)
            {MapSet.put(boundaries, offset), offset}
          end)

        Enum.filter(opportunities, fn {offset, mandatory?} ->
          mandatory? or MapSet.member?(boundaries, offset)
        end)
      else
        opportunities
      end

    if policy.break_spaces? do
      HtmlValidator.reserve_render_resource(:max_work, byte_size(text), :layout)
      breaks = Map.new(opportunities)

      {result, _} =
        text
        |> UnicodeGrapheme.graphemes()
        |> Enum.reduce({[], 0}, fn cluster, {result, previous} ->
          offset = previous + byte_size(cluster)
          space? = cluster != "\u00A0" and Regex.match?(~r/^[\t\p{Zs}]$/u, cluster)

          result =
            if space? or Map.has_key?(breaks, offset),
              do: [{offset, Map.get(breaks, offset, false)} | result],
              else: result

          {result, offset}
        end)

      Enum.reverse(result)
    else
      opportunities
    end
  end

  defp failure(message) do
    Diagnostics.error(:layout, :invalid_layout, message,
      operation: :line_break,
      module: __MODULE__,
      source: "HTML inline text"
    )
  end
end
