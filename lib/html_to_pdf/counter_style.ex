defmodule NativeElixirPdfUtilities.HtmlToPdf.CounterStyle do
  @moduledoc false

  @type style ::
          :decimal
          | :decimal_leading_zero
          | :lower_alpha
          | :upper_alpha
          | :lower_roman
          | :upper_roman
          | :disc
          | :circle
          | :square
          | :disclosure_open
          | :disclosure_closed
          | :none

  @doc false
  @spec marker(style(), integer()) :: String.t()
  def marker(style, ordinal) do
    case style do
      :none ->
        ""

      :disc ->
        "•"

      :circle ->
        "◦"

      :square ->
        "▪"

      :disclosure_open ->
        "▼"

      :disclosure_closed ->
        "▶"

      :decimal ->
        "#{ordinal}."

      :decimal_leading_zero ->
        sign = if ordinal < 0, do: "-", else: ""

        sign <>
          String.pad_leading(Integer.to_string(abs(ordinal)), 2 - byte_size(sign), "0") <> "."

      style when style in [:lower_alpha, :upper_alpha] and ordinal > 0 ->
        letters =
          Stream.unfold(ordinal, fn value ->
            if value == 0, do: nil, else: {97 + rem(value - 1, 26), div(value - 1, 26)}
          end)
          |> Enum.reverse()
          |> List.to_string()

        if(style == :upper_alpha, do: String.upcase(letters), else: letters) <> "."

      # CSS defines Roman styles only for 1..3999. This is a fixed counter-style range.
      style when style in [:lower_roman, :upper_roman] and ordinal in 1..3999 ->
        {letters, 0} =
          Enum.reduce(
            [
              {1000, "M"},
              {900, "CM"},
              {500, "D"},
              {400, "CD"},
              {100, "C"},
              {90, "XC"},
              {50, "L"},
              {40, "XL"},
              {10, "X"},
              {9, "IX"},
              {5, "V"},
              {4, "IV"},
              {1, "I"}
            ],
            {[], ordinal},
            fn {value, letters}, {parts, remaining} ->
              {[String.duplicate(letters, div(remaining, value)) | parts], rem(remaining, value)}
            end
          )

        letters = letters |> Enum.reverse() |> IO.iodata_to_binary()
        if(style == :lower_roman, do: String.downcase(letters), else: letters) <> "."

      _ ->
        "#{ordinal}."
    end
  end
end
