defmodule NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{UnicodeCharacterProperty, UnicodeTextData}

  @extended Regex.compile!(
              "\\A(?:" <>
                UnicodeCharacterProperty.expression("Extended_Pictographic", false) <> ")\\z",
              "u"
            )
  @ascii ~r/\A[\x00-\x7F]*\z/

  @doc "Segments admitted UTF-8 into Unicode 17 extended grapheme clusters using UAX29 GB3–GB999."
  @spec graphemes(String.t()) :: [String.t()]
  def graphemes(text) do
    # ASCII properties have not changed between Unicode versions. Keep the
    # runtime's optimized byte path, including its CRLF pair handling.
    if Regex.match?(@ascii, text), do: String.graphemes(text), else: segments(text, [])
  end

  @doc "Returns the first Unicode 17 extended grapheme and remaining admitted UTF-8, or nil."
  @spec next(String.t()) :: {String.t(), String.t()} | nil
  def next(text) do
    case text do
      "" ->
        nil

      <<codepoint::utf8, tail::binary>> ->
        properties = UnicodeTextData.grapheme_break(codepoint)

        state =
          advance(
            %{previous: nil, regional: 0, pictographic: false, zwj: false, conjunct: :none},
            codepoint,
            properties
          )

        grow(text, tail, state)
    end
  end

  defp segments(text, clusters) do
    case next(text) do
      nil -> Enum.reverse(clusters)
      {cluster, tail} -> segments(tail, [cluster | clusters])
    end
  end

  defp grow(original, tail, state) do
    case tail do
      "" ->
        {original, ""}

      <<codepoint::utf8, rest::binary>> ->
        properties = UnicodeTextData.grapheme_break(codepoint)

        if boundary?(state, codepoint, properties) do
          {binary_part(original, 0, byte_size(original) - byte_size(tail)), tail}
        else
          grow(original, rest, advance(state, codepoint, properties))
        end
    end
  end

  # Keep the ordered UAX29 rules explicit: control boundaries precede Extend,
  # and Indic/emoji lookbehind states are independent of regional-indicator parity.
  defp boundary?(state, codepoint, {current, conjunct}) do
    previous = state.previous

    cond do
      previous == :CR and current == :LF ->
        false

      previous in [:Control, :CR, :LF] or current in [:Control, :CR, :LF] ->
        true

      previous == :L and current in [:L, :V, :LV, :LVT] ->
        false

      previous in [:LV, :V] and current in [:V, :T] ->
        false

      previous in [:LVT, :T] and current == :T ->
        false

      current in [:Extend, :ZWJ, :SpacingMark] or previous == :Prepend ->
        false

      conjunct == :Consonant and state.conjunct == :linked ->
        false

      previous == :ZWJ and state.zwj and Regex.match?(@extended, <<codepoint::utf8>>) ->
        false

      previous == :Regional_Indicator and current == :Regional_Indicator and
          rem(state.regional, 2) == 1 ->
        false

      true ->
        true
    end
  end

  defp advance(state, codepoint, {current, conjunct}) do
    indic =
      case conjunct do
        :Consonant -> :consonant
        :Linker -> if state.conjunct in [:consonant, :linked], do: :linked, else: :none
        :Extend -> state.conjunct
        _ -> :none
      end

    %{
      previous: current,
      regional: if(current == :Regional_Indicator, do: state.regional + 1, else: 0),
      pictographic:
        Regex.match?(@extended, <<codepoint::utf8>>) or
          (current == :Extend and state.pictographic),
      zwj: current == :ZWJ and state.pictographic,
      conjunct: indic
    }
  end
end
