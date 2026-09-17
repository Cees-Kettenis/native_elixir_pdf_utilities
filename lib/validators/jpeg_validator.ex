defmodule NativeElixirPdfUtilities.Validators.JpegValidator do
  @moduledoc false

  @doc false
  @spec metadata(binary()) :: {:ok, map()} | :error
  def metadata(data) do
    case data do
      <<255, 216, rest::binary>> -> markers(rest, nil, nil)
      _ -> :error
    end
  end

  defp markers(data, frame, adobe) do
    case data do
      <<255, marker, _rest::binary>> when marker in [217, 218] ->
        color_metadata(frame, adobe)

      <<255, 255, rest::binary>> ->
        markers(<<255, rest::binary>>, frame, adobe)

      <<255, marker, rest::binary>> when marker in 208..215 ->
        markers(rest, frame, adobe)

      <<255, marker, length::16, segment::binary-size(length - 2), rest::binary>>
      when length >= 2 ->
        case {marker, segment} do
          {marker, <<8, height::16, width::16, count, descriptors::binary>>}
          when marker in [192, 194] and width > 0 and height > 0 and count in [1, 3, 4] ->
            components =
              for <<id, horizontal::4, vertical::4, table <- descriptors>>,
                do: {id, horizontal, vertical, table}

            if is_nil(frame) and byte_size(descriptors) == count * 3 and
                 length(Enum.uniq_by(components, &elem(&1, 0))) == count and
                 Enum.all?(components, fn {_id, h, v, table} ->
                   h in 1..4 and v in 1..4 and table <= 3
                 end) do
              markers(rest, %{width_px: width, height_px: height, components: components}, adobe)
            else
              :error
            end

          {marker, _}
          when marker in [192, 193, 194, 195, 197, 198, 199, 201, 202, 203, 205, 206, 207] ->
            :error

          {238, <<"Adobe", _version::16, _flags0::16, _flags1::16, transform>>}
          when transform in 0..2 ->
            if is_nil(adobe) or adobe == transform,
              do: markers(rest, frame, transform),
              else: :error

          {238, <<"Adobe", _::binary>>} ->
            :error

          _ ->
            markers(rest, frame, adobe)
        end

      _ ->
        :error
    end
  end

  defp color_metadata(frame, adobe) do
    case frame do
      %{components: components} ->
        ids = Enum.map(components, &elem(&1, 0))

        convention =
          case {ids, adobe} do
            {[_], value} when value in [nil, 0] -> :gray
            {_, 0} when length(ids) == 3 -> :rgb
            {_, 1} when length(ids) == 3 -> :ycbcr
            {[?R, ?G, ?B], nil} -> :rgb
            {[1, 2, 3], nil} -> :ycbcr
            {_, 0} when length(ids) == 4 -> :adobe_cmyk
            {_, 2} when length(ids) == 4 -> :ycck
            {[?C, ?M, ?Y, ?K], nil} -> :cmyk
            {[1, 2, 3, 4], nil} -> :cmyk
            _ -> :unsupported
          end

        if convention == :unsupported do
          :error
        else
          {:ok,
           Map.merge(frame, %{
             color_space:
               case length(ids) do
                 1 -> :device_gray
                 3 -> :device_rgb
                 4 -> :device_cmyk
               end,
             color_transform: convention,
             inverted_cmyk: convention in [:adobe_cmyk, :ycck]
           })}
        end

      nil ->
        :error
    end
  end
end
