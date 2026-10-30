defmodule NativeElixirPdfUtilities.Pdf.InfoCodec do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.TextEncoding
  alias NativeElixirPdfUtilities.Validators.PdfDateValidator
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @type pdf_date :: PdfDateValidator.pdf_date()

  @doc false
  @spec decode_text(binary()) :: {:ok, String.t()} | :error
  def decode_text(bytes) do
    case bytes do
      <<0xFE, 0xFF, encoded::binary>> ->
        unicode_to_utf8(encoded, {:utf16, :big})

      <<0xEF, 0xBB, 0xBF, encoded::binary>> ->
        case String.valid?(encoded) do
          true -> {:ok, encoded}
          false -> :error
        end

      bytes when is_binary(bytes) ->
        decode_pdf_doc(bytes)
    end
  end

  @doc false
  @spec encode_text(String.t()) :: {:string, binary()} | {:hex, binary()}
  def encode_text(text) do
    case text do
      text when is_binary(text) ->
        case decode_pdf_doc(text) do
          {:ok, ^text} ->
            {:string, text}

          _ ->
            {:hex, <<0xFE, 0xFF>> <> :unicode.characters_to_binary(text, :utf8, {:utf16, :big})}
        end
    end
  end

  @doc false
  @spec parse_date(binary()) :: {:ok, NaiveDateTime.t(), pdf_date()} | :error
  def parse_date(value), do: PdfDateValidator.parse_date(value)

  @doc false
  @spec normalize_date(term()) :: {:ok, binary()} | :error
  def normalize_date(value), do: PdfDateValidator.normalize_date(value)

  @doc false
  @spec serialize_value(term()) ::
          {:ok, iodata()} | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def serialize_value(value) do
    serialize_value(value, 0)
  end

  defp decode_pdf_doc(bytes) do
    bytes
    |> :binary.bin_to_list()
    |> Enum.reduce_while({:ok, []}, fn byte, {:ok, decoded} ->
      case TextEncoding.character("PDFDocEncoding", byte, %{}) do
        {:ok, character} -> {:cont, {:ok, [character | decoded]}}
        :error -> {:halt, :error}
      end
    end)
    |> case do
      {:ok, decoded} -> {:ok, decoded |> Enum.reverse() |> IO.iodata_to_binary()}
      :error -> :error
    end
  end

  defp unicode_to_utf8(encoded, source_encoding) do
    case :unicode.characters_to_binary(encoded, source_encoding, :utf8) do
      text when is_binary(text) -> if String.valid?(text), do: {:ok, text}, else: :error
      _ -> :error
    end
  end

  defp serialize_value(value, depth) do
    case value do
      nil ->
        {:ok, "null"}

      true ->
        {:ok, "true"}

      false ->
        {:ok, "false"}

      value when is_integer(value) ->
        {:ok, Integer.to_string(value)}

      value when is_float(value) ->
        # PDF numbers cannot use exponents. Expand the shortest round-trippable
        # float representation instead of rounding small transforms to zero.
        decimal =
          case String.split(:erlang.float_to_binary(value, [:short]), "e") do
            [decimal] ->
              decimal

            [coefficient, exponent] ->
              sign = if String.starts_with?(coefficient, "-"), do: "-", else: ""
              [whole, fraction] = String.split(String.trim_leading(coefficient, "-"), ".")
              digits = whole <> fraction
              point = byte_size(whole) + String.to_integer(exponent)

              expanded =
                String.duplicate("0", max(-point, 0)) <>
                  digits <>
                  String.duplicate("0", max(point - byte_size(digits), 0))

              {whole, fraction} = String.split_at(expanded, max(point, 0))

              sign <>
                if(whole == "", do: "0", else: whole) <>
                "." <>
                if(fraction == "", do: "0", else: fraction)
          end

        {:ok, decimal}

      {:name, name} when is_binary(name) ->
        {:ok, ["/", encode_name(name)]}

      {:string, bytes} when is_binary(bytes) ->
        {:ok, ["(", escape_literal(bytes), ")"]}

      {:hex, bytes} when is_binary(bytes) ->
        {:ok, ["<", Base.encode16(bytes), ">"]}

      {:ref, {object, generation}} when is_integer(object) and is_integer(generation) ->
        {:ok, "#{object} #{generation} R"}

      values when is_list(values) ->
        with :ok <- PdfValidator.validate_value_depth(depth + 1, module: __MODULE__) do
          serialize_array(values, depth + 1)
        end

      dictionary when is_map(dictionary) ->
        with :ok <- PdfValidator.validate_value_depth(depth + 1, module: __MODULE__) do
          serialize_dictionary(dictionary, depth + 1)
        end

      _ ->
        :error
    end
  end

  defp serialize_array(values, depth) do
    values
    |> Enum.reduce_while({:ok, []}, fn value, {:ok, encoded} ->
      case serialize_value(value, depth) do
        {:ok, value} -> {:cont, {:ok, [value | encoded]}}
        failure -> {:halt, failure}
      end
    end)
    |> case do
      {:ok, encoded} -> {:ok, ["[", encoded |> Enum.reverse() |> Enum.intersperse(" "), "]"]}
      failure -> failure
    end
  end

  defp serialize_dictionary(dictionary, depth) do
    dictionary
    |> Enum.sort_by(fn {key, _value} -> key end)
    |> Enum.reduce_while({:ok, []}, fn {key, value}, {:ok, encoded} ->
      case {is_binary(key), serialize_value(value, depth)} do
        {true, {:ok, value}} -> {:cont, {:ok, [[["/", encode_name(key)], " ", value] | encoded]}}
        {true, {:error, _} = failure} -> {:halt, failure}
        _ -> {:halt, :error}
      end
    end)
    |> case do
      {:ok, encoded} -> {:ok, ["<< ", encoded |> Enum.reverse() |> Enum.intersperse(" "), " >>"]}
      failure -> failure
    end
  end

  @doc false
  @spec encode_name(binary()) :: iodata()
  def encode_name(name) do
    name
    |> :binary.bin_to_list()
    |> Enum.map(fn byte ->
      case byte in 33..126 and byte not in ~c"()<>[]{}/%#" do
        true ->
          <<byte>>

        false ->
          ["#", byte |> Integer.to_string(16) |> String.upcase() |> String.pad_leading(2, "0")]
      end
    end)
  end

  @doc false
  @spec escape_literal(binary()) :: iodata()
  def escape_literal(value) do
    value
    |> :binary.bin_to_list()
    |> Enum.map(fn byte ->
      case byte do
        ?\\ ->
          "\\\\"

        ?( ->
          "\\("

        ?) ->
          "\\)"

        ?\n ->
          "\\n"

        ?\r ->
          "\\r"

        ?\t ->
          "\\t"

        ?\b ->
          "\\b"

        ?\f ->
          "\\f"

        byte when byte < 32 or byte > 126 ->
          "\\" <> (byte |> Integer.to_string(8) |> String.pad_leading(3, "0"))

        byte ->
          <<byte>>
      end
    end)
  end
end
