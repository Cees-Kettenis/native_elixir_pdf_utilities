defmodule NativeElixirPdfUtilities.Validators.EmojiPresentationValidator do
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{
    UnicodeCharacterProperty,
    UnicodeStringProperty,
    UnicodeTextData
  }

  @default Regex.compile!(
             "\\A(?:" <>
               UnicodeCharacterProperty.expression("Emoji_Presentation", false) <> ")\\z",
             "u"
           )
  @sequences UnicodeStringProperty.strings("RGI_Emoji")
             |> Enum.filter(
               &(String.length(&1) == 1 and
                   length(String.codepoints(&1)) > 1)
             )
             |> MapSet.new()

  @doc "Determines a cluster's presentation preference, with explicit selectors overriding CSS defaults."
  @spec preference(String.t(), map()) :: :normal | :text | :emoji
  def preference(text, font) do
    case String.to_charlist(text) do
      [base | rest] ->
        if UnicodeTextData.emoji_variant?(base) do
          cond do
            0xFE0E in rest ->
              :text

            0xFE0F in rest ->
              :emoji

            Map.has_key?(font, :emoji_presentation) ->
              font.emoji_presentation

            true ->
              case get_in(font, [:feature_plan, :emoji]) do
                :text -> :text
                :emoji -> :emoji
                :unicode -> if Regex.match?(@default, <<base::utf8>>), do: :emoji, else: :text
                _ -> :normal
              end
          end
        else
          :normal
        end

      [] ->
        :normal
    end
  end

  @doc "Retains a selected cluster's presentation through shaping and color painting."
  @spec prepare(map(), String.t()) :: map()
  def prepare(font, text) do
    case preference(text, font) do
      :normal ->
        font

      :text ->
        font
        |> Map.put(:emoji_presentation, :text)
        |> Map.delete(:bitmap_widths)
        |> then(fn selected ->
          if Map.has_key?(font, :bitmap_widths),
            do: Map.update!(selected, :id, &(&1 <> "-text-presentation")),
            else: selected
        end)

      value ->
        Map.put(font, :emoji_presentation, value)
    end
  end

  @doc "Identifies complete registered multi-scalar emoji clusters for shaped fallback preference."
  @spec sequence?(String.t()) :: boolean()
  def sequence?(text), do: MapSet.member?(@sequences, text)

  @doc "Identifies runs that need per-cluster presentation matching before the whole-face shortcut."
  @spec matching_required?(String.t(), map()) :: boolean()
  def matching_required?(text, font) do
    String.contains?(text, ["\uFE0E", "\uFE0F", "\u200D", "\u20E3"]) or
      get_in(font, [:feature_plan, :emoji]) in [:text, :emoji, :unicode] or
      (Regex.match?(~r/[^\x00-\x7F]/u, text) and
         Enum.any?(UnicodeGrapheme.graphemes(text), &sequence?/1))
  end
end
