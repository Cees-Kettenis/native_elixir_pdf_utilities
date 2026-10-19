defmodule NativeElixirPdfUtilities.HtmlToPdf.Font.MathTokenMetrics do
  @moduledoc false

  # Generated from the bundled font, independently of HTML/layout fixtures.
  # See scripts/data/math-token-metrics/README.md for the sampling contract.
  @metrics_file Path.expand("../../../priv/fonts/stix/token-ink-metrics.bin", __DIR__)
  @external_resource @metrics_file
  <<fingerprint::binary-size(32), glyph_count::16, maximum_size::16, records::binary>> =
    @metrics_file |> File.read!() |> :zlib.uncompress()

  @fingerprint fingerprint
  @maximum_size maximum_size
  @profiles for(<<count::16, profile::binary-size(count * 6) <- records>>, do: profile)
            |> List.to_tuple()
  true = tuple_size(@profiles) == glyph_count

  @doc "Returns the bundled device-metric profile for an exact font content fingerprint."
  @spec profile(binary()) :: tuple() | nil
  def profile(fingerprint) do
    if fingerprint == @fingerprint, do: @profiles
  end

  @doc "Looks up sampled CSS-pixel token ink extents, or nil outside the generated size range."
  @spec bounds(tuple() | nil, non_neg_integer(), number()) :: {number(), number()} | nil
  def bounds(profile, glyph, size) do
    # Skia converts the requested CSS size to FreeType's 26.6 fixed point.
    size = round(size / 0.75 * 64)

    if profile != nil and size >= 64 and size <= @maximum_size do
      records = elem(profile, glyph)
      find(records, size, 0, div(byte_size(records), 6) - 1)
    end
  end

  # Each record starts where its metrics change. Binary search keeps lookup
  # bounded by the profile's logarithmic depth, including at fractional sizes.
  defp find(records, size, first, last) do
    if first == last do
      <<_size::16, bottom::signed-16, top::signed-16>> = binary_part(records, first * 6, 6)
      {top * 0.75, -bottom * 0.75}
    else
      middle = div(first + last + 1, 2)
      <<start::16>> = binary_part(records, middle * 6, 2)

      if start <= size,
        do: find(records, size, middle, last),
        else: find(records, size, first, middle - 1)
    end
  end
end
