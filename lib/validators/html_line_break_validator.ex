defmodule NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeLineBreak

  @discretionary_characters ["\u00AD", "\u200B"]

  @doc "Returns validated Unicode byte offsets, optionally adding CSS preserved-space opportunities."
  @spec opportunities(term()) ::
          {:ok, [{non_neg_integer(), boolean()}]} | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec opportunities(term(), term()) ::
          {:ok, [{non_neg_integer(), boolean()}]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def opportunities(text, break_spaces? \\ false) do
    case {text, break_spaces?} do
      {text, break_spaces?} when is_binary(text) and is_boolean(break_spaces?) ->
        HtmlValidator.reserve_render_resource(:max_layout_text_work, byte_size(text), :layout)

        if String.valid?(text, :fast_ascii) do
          result = UnicodeLineBreak.opportunities(text)

          HtmlValidator.reserve_render_resource(
            :max_layout_text_work,
            byte_size(text) + length(result),
            :layout
          )

          # The Elixir scanner produces these offsets directly. Protocol
          # admission is needed only for externally supplied offset lists.
          {:ok,
           css_space_opportunities(text, result, word_hyphen_opportunities(text), break_spaces?)}
        else
          failure("line breaking requires valid UTF-8 text")
        end

      {_, flag} when not is_boolean(flag) ->
        failure("preserved-space line breaking requires a boolean option")

      _ ->
        failure("line breaking requires UTF-8 text")
    end
  catch
    {:render_resource_limit, error} -> error
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
          :max_layout_text_work,
          byte_size(text) + length(result),
          :layout
        )

        if String.valid?(text, :fast_ascii) do
          {grapheme_offsets, _offset} =
            text
            |> String.graphemes()
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
  def display_plan(text, font, break_after? \\ false) do
    case {text, break_after?} do
      {text, selected?} when is_binary(text) and is_boolean(selected?) ->
        HtmlValidator.reserve_render_resource(:max_layout_text_work, byte_size(text), :layout)

        if String.valid?(text, :fast_ascii) do
          # U+00AD and U+200B remain in UAX14 input and ActualText, not in unbroken glyph paint.
          visible = strip_discretionary_characters(text)

          hyphen =
            if selected? and String.ends_with?(text, "\u00AD") do
              # CSS Text 3 uses a real selected-font Unicode hyphen at a taken manual break.
              Enum.find(["\u2010", "-", "\u00AD"], &Font.supports_text?(font, &1))
            else
              ""
            end

          case hyphen do
            nil ->
              failure("the selected font cannot paint a conditional hyphen")

            hyphen ->
              {:ok,
               %{
                 text: visible <> hyphen,
                 actual_text: text,
                 discretionary?: visible <> hyphen != text
               }}
          end
        else
          failure("discretionary text requires valid UTF-8")
        end

      {_, flag} when not is_boolean(flag) ->
        failure("discretionary line selection requires a boolean option")

      _ ->
        failure("discretionary text requires UTF-8 text")
    end
  catch
    {:render_resource_limit, error} -> error
  end

  @doc "Identifies an otherwise identical inline boundary made transparent by discretionary formatting."
  @spec transparent_format_boundary?(term(), term()) ::
          {:ok, boolean()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def transparent_format_boundary?(before, after_text) do
    case {before, after_text} do
      {before, after_text} when is_binary(before) and is_binary(after_text) ->
        HtmlValidator.reserve_render_resource(
          :max_layout_text_work,
          byte_size(before) + byte_size(after_text),
          :layout
        )

        if String.valid?(before, :fast_ascii) and String.valid?(after_text, :fast_ascii) do
          {:ok,
           String.ends_with?(before, @discretionary_characters) or
             (after_text != "" and
                strip_discretionary_characters(after_text) == "")}
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
  defp css_space_opportunities(text, opportunities, word_hyphens, break_spaces?) do
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
        not mandatory? and offset > 0 and offset < byte_size(text) and
          :binary.at(text, offset - 1) == ?/ and :binary.at(text, offset) in 0x21..0x7F
      end)
      |> Map.new()
      |> Enum.sort_by(&elem(&1, 0))

    if break_spaces? do
      HtmlValidator.reserve_render_resource(:max_layout_text_work, byte_size(text), :layout)
      breaks = Map.new(opportunities)

      {result, _} =
        text
        |> String.graphemes()
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
