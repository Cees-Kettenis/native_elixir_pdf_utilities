defmodule NativeElixirPdfUtilities.HtmlToPdf.TextDecorationWave do
  @moduledoc false

  @doc "Returns a cubic wave's CSS pixel dimensions from its admitted point thickness."
  @spec geometry(number()) :: map()
  def geometry(thickness) do
    pixels = thickness / 0.75
    # These are UA paint proportions, not resource ceilings. The fixed one
    # pixel unit keeps thin waves legible; half-pixel controls reduce aliasing.
    unit = max(1.0, pixels)
    wavelength = 1 + 2 * round(2 * unit + 0.5)
    control = 0.5 + round(3 * unit + 0.5)
    amplitude = control / (2 * :math.sqrt(3))
    top = floor(0.5 - amplitude - pixels / 2)
    bottom = ceil(0.5 + amplitude + pixels / 2)
    %{control: control, step: wavelength / 2, height: (bottom - top) * 0.75, thickness: pixels}
  end

  @doc "Builds admitted repeated cubics; the caller clips them to their drawing rectangle."
  @spec commands(map()) :: [{:move | :curve, [number()]}]
  def commands(paint) do
    start = paint.x + paint.phase - 2 * paint.step
    middle = paint.y + paint.height / 2

    [
      {:move, [start, middle]}
      | Enum.map(0..paint.count, fn index ->
          x = start + index * 2 * paint.step

          {:curve,
           [
             x + paint.step,
             middle + paint.control,
             x + paint.step,
             middle - paint.control,
             x + 2 * paint.step,
             middle
           ]}
        end)
    ]
  end
end
