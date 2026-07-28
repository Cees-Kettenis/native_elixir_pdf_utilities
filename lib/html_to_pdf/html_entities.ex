defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlEntities do
  @moduledoc false

  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlEntityData

  @numeric_replacements %{
    0x80 => 0x20AC,
    0x82 => 0x201A,
    0x83 => 0x0192,
    0x84 => 0x201E,
    0x85 => 0x2026,
    0x86 => 0x2020,
    0x87 => 0x2021,
    0x88 => 0x02C6,
    0x89 => 0x2030,
    0x8A => 0x0160,
    0x8B => 0x2039,
    0x8C => 0x0152,
    0x8E => 0x017D,
    0x91 => 0x2018,
    0x92 => 0x2019,
    0x93 => 0x201C,
    0x94 => 0x201D,
    0x95 => 0x2022,
    0x96 => 0x2013,
    0x97 => 0x2014,
    0x98 => 0x02DC,
    0x99 => 0x2122,
    0x9A => 0x0161,
    0x9B => 0x203A,
    0x9C => 0x0153,
    0x9E => 0x017E,
    0x9F => 0x0178
  }

  @doc false
  @spec decode(String.t(), :text | :attribute) :: String.t()
  def decode(text, context) do
    case {text, context} do
      {text, context} when is_binary(text) and context in [:text, :attribute] ->
        decode_text(text, context, [])
    end
  end

  defp decode_text(text, context, decoded) do
    case text do
      "" ->
        decoded
        |> Enum.reverse()
        |> IO.iodata_to_binary()

      <<"&", rest::binary>> ->
        case decode_reference(rest, context) do
          {:ok, characters, remaining} ->
            decode_text(remaining, context, [characters | decoded])

          :not_found ->
            decode_text(rest, context, ["&" | decoded])
        end

      <<byte, rest::binary>> ->
        decode_text(rest, context, [byte | decoded])
    end
  end

  defp decode_reference(rest, context) do
    case rest do
      <<"#", numeric::binary>> -> decode_numeric_reference(numeric)
      _ -> decode_named_reference(rest, context)
    end
  end

  defp decode_numeric_reference(numeric) do
    case numeric do
      <<"x", hexadecimal::binary>> -> decode_number(hexadecimal, 16)
      <<"X", hexadecimal::binary>> -> decode_number(hexadecimal, 16)
      decimal -> decode_number(decimal, 10)
    end
  end

  defp decode_number(source, base) do
    pattern =
      case base do
        16 -> ~r/^[0-9A-Fa-f]+/u
        10 -> ~r/^[0-9]+/u
      end

    case Regex.run(pattern, source) do
      [digits] ->
        remaining = binary_part(source, byte_size(digits), byte_size(source) - byte_size(digits))

        remaining =
          case remaining do
            <<";", rest::binary>> -> rest
            _ -> remaining
          end

        codepoint =
          digits
          |> String.to_integer(base)
          |> normalize_numeric_codepoint()

        {:ok, <<codepoint::utf8>>, remaining}

      nil ->
        :not_found
    end
  end

  defp normalize_numeric_codepoint(codepoint) do
    cond do
      codepoint == 0 ->
        0xFFFD

      codepoint > 0x10FFFF or codepoint in 0xD800..0xDFFF ->
        0xFFFD

      true ->
        Map.get(@numeric_replacements, codepoint, codepoint)
    end
  end

  defp decode_named_reference(rest, context) do
    max_length = min(byte_size(rest), HtmlEntityData.max_name_length())

    case longest_named_reference(rest, max_length) do
      {name, characters, remaining} ->
        semicolon? = String.ends_with?(name, ";")

        case context == :attribute and not semicolon? and
               attribute_legacy_reference_continuation?(remaining) do
          true -> :not_found
          false -> {:ok, characters, remaining}
        end

      nil ->
        :not_found
    end
  end

  defp longest_named_reference(rest, length) do
    case length do
      0 ->
        nil

      length ->
        name = binary_part(rest, 0, length)

        case HtmlEntityData.lookup(name) do
          nil ->
            longest_named_reference(rest, length - 1)

          characters ->
            remaining = binary_part(rest, length, byte_size(rest) - length)
            {name, characters, remaining}
        end
    end
  end

  defp attribute_legacy_reference_continuation?(remaining) do
    case remaining do
      <<"=", _::binary>> -> true
      <<character, _::binary>> when character in ?0..?9 -> true
      <<character, _::binary>> when character in ?A..?Z -> true
      <<character, _::binary>> when character in ?a..?z -> true
      _ -> false
    end
  end
end
