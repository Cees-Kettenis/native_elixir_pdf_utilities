defmodule NativeElixirPdfUtilities.Validators.PdfDateValidator do
  @moduledoc false

  @type pdf_date :: %{
          required(:year) => non_neg_integer(),
          required(:month) => pos_integer(),
          required(:day) => pos_integer(),
          required(:hour) => non_neg_integer(),
          required(:minute) => non_neg_integer(),
          required(:second) => non_neg_integer(),
          required(:precision) => :year | :month | :day | :hour | :minute | :second,
          required(:timezone) => nil | :utc | {:- | :+, non_neg_integer(), non_neg_integer()}
        }

  @doc false
  @spec parse_date(binary()) :: {:ok, NaiveDateTime.t(), pdf_date()} | :error
  def parse_date(value) do
    pattern =
      ~r/\AD:(\d{4})(\d{2})?(\d{2})?(\d{2})?(\d{2})?(\d{2})?(?:(Z)|([+-])(\d{2})(?:'?(\d{2}))?'?)?\z/

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

      {"", sign, hour, minute} when sign in ["+", "-"] and hour != "" ->
        hour = String.to_integer(hour)
        minute = optional_integer(minute, 0)

        case hour <= 23 and minute <= 59 do
          true -> {:ok, {String.to_atom(sign), hour, minute}}
          false -> :error
        end
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
end
