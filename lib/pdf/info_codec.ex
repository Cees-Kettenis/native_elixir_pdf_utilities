defmodule NativeElixirPdfUtilities.Pdf.InfoCodec do
  @moduledoc false

  alias NativeElixirPdfUtilities.Pdf.TextEncoding

  @type pdf_date :: %{
          required(:year) => pos_integer(),
          required(:month) => pos_integer(),
          required(:day) => pos_integer(),
          required(:hour) => non_neg_integer(),
          required(:minute) => non_neg_integer(),
          required(:second) => non_neg_integer(),
          required(:precision) => :year | :month | :day | :hour | :minute | :second,
          required(:timezone) => nil | :utc | {:- | :+, non_neg_integer(), non_neg_integer()}
        }

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
        case Enum.all?(:binary.bin_to_list(text), &(&1 <= 0x7F)) do
          true ->
            {:string, text}

          false ->
            {:hex, <<0xFE, 0xFF>> <> :unicode.characters_to_binary(text, :utf8, {:utf16, :big})}
        end
    end
  end

  @doc false
  @spec parse_date(binary()) :: {:ok, NaiveDateTime.t(), pdf_date()} | :error
  def parse_date(value) do
    pattern =
      ~r/\AD:(\d{4})(\d{2})?(\d{2})?(\d{2})?(\d{2})?(\d{2})?(?:(Z)|([+-])(\d{2})'?(\d{2})?'?)?\z/

    case Regex.run(pattern, value, capture: :all_but_first) do
      nil ->
        :error

      captures ->
        captures = captures ++ List.duplicate("", 10 - length(captures))

        [year, month, day, hour, minute, second, utc, sign, zone_hour, zone_minute] =
          captures

        with {:ok, components} <- date_components(year, month, day, hour, minute, second),
             {:ok, timezone} <- timezone(utc, sign, zone_hour, zone_minute),
             {:ok, date} <- Date.new(components.year, components.month, components.day),
             {:ok, time} <- Time.new(components.hour, components.minute, components.second),
             {:ok, date_time} <- NaiveDateTime.new(date, time) do
          {:ok, date_time, Map.put(components, :timezone, timezone)}
        else
          _ -> :error
        end
    end
  end

  @doc false
  @spec normalize_date(term()) :: {:ok, binary()} | :error
  def normalize_date(value) do
    case value do
      %DateTime{} = date_time ->
        offset = date_time.utc_offset + date_time.std_offset

        case rem(offset, 60) == 0 and abs(offset) <= 86_340 do
          true ->
            {:ok,
             "D:#{calendar_date(date_time)}#{calendar_time(date_time)}#{formatted_offset(offset)}"}

          false ->
            :error
        end

      %NaiveDateTime{} = date_time ->
        {:ok, "D:#{calendar_date(date_time)}#{calendar_time(date_time)}"}

      %Date{} = date ->
        {:ok, "D:#{calendar_date(date)}"}

      value when is_binary(value) ->
        normalize_date_string(value)

      _ ->
        :error
    end
  end

  @doc false
  @spec serialize_value(term()) :: {:ok, iodata()} | :error
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

  defp date_components(year, month, day, hour, minute, second) do
    supplied = [month, day, hour, minute, second]

    {:ok,
     %{
       year: String.to_integer(year),
       month: optional_integer(month, 1),
       day: optional_integer(day, 1),
       hour: optional_integer(hour, 0),
       minute: optional_integer(minute, 0),
       second: optional_integer(second, 0),
       precision: precision(supplied)
     }}
  end

  defp optional_integer(value, default) do
    case value do
      "" -> default
      value -> String.to_integer(value)
    end
  end

  defp precision(components) do
    names = [:month, :day, :hour, :minute, :second]

    components
    |> Enum.zip(names)
    |> Enum.reduce(:year, fn {value, name}, precision ->
      if value == "", do: precision, else: name
    end)
  end

  defp timezone(utc, sign, hour, minute) do
    case {utc, sign, hour, minute} do
      {"Z", "", "", ""} ->
        {:ok, :utc}

      {"", "", "", ""} ->
        {:ok, nil}

      {"", sign, hour, minute} when sign in ["+", "-"] and hour != "" and minute != "" ->
        hour = String.to_integer(hour)
        minute = String.to_integer(minute)

        case hour <= 23 and minute <= 59 do
          true -> {:ok, {String.to_atom(sign), hour, minute}}
          false -> :error
        end

      _ ->
        :error
    end
  end

  defp normalize_date_string(value) do
    case value do
      "D:" <> _rest ->
        case parse_date(value) do
          {:ok, _date_time, parsed} -> {:ok, canonical_pdf_date(parsed)}
          :error -> :error
        end

      value ->
        case DateTime.from_iso8601(value) do
          {:ok, date_time, offset} ->
            local = NaiveDateTime.add(DateTime.to_naive(date_time), offset, :second)

            {:ok, "D:#{calendar_date(local)}#{calendar_time(local)}#{formatted_offset(offset)}"}

          {:error, _reason} ->
            case NaiveDateTime.from_iso8601(value) do
              {:ok, date_time} ->
                {:ok, "D:#{calendar_date(date_time)}#{calendar_time(date_time)}"}

              {:error, _reason} ->
                normalize_iso_date(value)
            end
        end
    end
  end

  defp normalize_iso_date(value) do
    case Date.from_iso8601(value) do
      {:ok, date} -> {:ok, "D:#{calendar_date(date)}"}
      {:error, _reason} -> :error
    end
  end

  defp canonical_pdf_date(date) do
    base =
      "D:#{padded(date.year, 4)}" <>
        optional_date_component(date, :month) <>
        optional_date_component(date, :day) <>
        optional_date_component(date, :hour) <>
        optional_date_component(date, :minute) <>
        optional_date_component(date, :second)

    base <> canonical_timezone(date.timezone)
  end

  defp optional_date_component(date, component) do
    order = %{year: 0, month: 1, day: 2, hour: 3, minute: 4, second: 5}

    case order[date.precision] >= order[component] do
      true -> padded(Map.fetch!(date, component), 2)
      false -> ""
    end
  end

  defp canonical_timezone(timezone) do
    case timezone do
      nil -> ""
      :utc -> "Z"
      {sign, hour, minute} -> "#{sign}#{padded(hour, 2)}'#{padded(minute, 2)}'"
    end
  end

  defp calendar_date(value) do
    padded(value.year, 4) <> padded(value.month, 2) <> padded(value.day, 2)
  end

  defp calendar_time(value) do
    padded(value.hour, 2) <> padded(value.minute, 2) <> padded(value.second, 2)
  end

  defp formatted_offset(offset) do
    case offset do
      0 ->
        "+00'00'"

      offset ->
        sign = if offset < 0, do: "-", else: "+"
        offset = abs(offset)
        "#{sign}#{padded(div(offset, 3600), 2)}'#{padded(div(rem(offset, 3600), 60), 2)}'"
    end
  end

  defp padded(value, length) do
    value |> Integer.to_string() |> String.pad_leading(length, "0")
  end

  defp serialize_value(value, depth) do
    case depth <= 100 do
      true -> serialize_supported_value(value, depth)
      false -> :error
    end
  end

  defp serialize_supported_value(value, depth) do
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
        {:ok, :erlang.float_to_binary(value, [:compact, decimals: 10])}

      {:name, name} when is_binary(name) ->
        {:ok, ["/", encode_name(name)]}

      {:string, bytes} when is_binary(bytes) ->
        {:ok, ["(", escape_literal(bytes), ")"]}

      {:hex, bytes} when is_binary(bytes) ->
        {:ok, ["<", Base.encode16(bytes), ">"]}

      {:ref, {object, generation}} when is_integer(object) and is_integer(generation) ->
        {:ok, "#{object} #{generation} R"}

      values when is_list(values) ->
        serialize_array(values, depth + 1)

      dictionary when is_map(dictionary) ->
        serialize_dictionary(dictionary, depth + 1)

      _ ->
        :error
    end
  end

  defp serialize_array(values, depth) do
    values
    |> Enum.reduce_while({:ok, []}, fn value, {:ok, encoded} ->
      case serialize_value(value, depth) do
        {:ok, value} -> {:cont, {:ok, [value | encoded]}}
        :error -> {:halt, :error}
      end
    end)
    |> case do
      {:ok, encoded} -> {:ok, ["[", encoded |> Enum.reverse() |> Enum.intersperse(" "), "]"]}
      :error -> :error
    end
  end

  defp serialize_dictionary(dictionary, depth) do
    dictionary
    |> Enum.sort_by(fn {key, _value} -> key end)
    |> Enum.reduce_while({:ok, []}, fn {key, value}, {:ok, encoded} ->
      case {is_binary(key), serialize_value(value, depth)} do
        {true, {:ok, value}} -> {:cont, {:ok, [[["/", encode_name(key)], " ", value] | encoded]}}
        _ -> {:halt, :error}
      end
    end)
    |> case do
      {:ok, encoded} -> {:ok, ["<< ", encoded |> Enum.reverse() |> Enum.intersperse(" "), " >>"]}
      :error -> :error
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
