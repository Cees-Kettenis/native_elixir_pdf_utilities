defmodule NativeElixirPdfUtilities.HtmlToPdf.GaugeArtwork do
  @moduledoc """
  Static artwork for validator-prepared HTML gauges.

  The pinned HTML Standard sections 4.10.13, 4.10.14 and 15.5.14-15 define
  state, native dimensions and baseline, leaving platform artwork to the UA.
  This module follows the pinned Linux Chromium print palette and track geometry,
  including the primitive meter's four-stop gradient. Horizontal LTR rendering
  uses the ordinary CSS box paths for sizing, positioning and pagination.
  Relative frames defer native pixel alignment until the final page is known.

  Indeterminate progress uses a centered circular unknown-state mark. It carries
  position -1 and no measured fill. Chromium prints a time-dependent moving segment;
  PDF output intentionally conveys the unknown state without sampling animation.
  Chromium 152 also overflows the range of a meter spanning -1e308 to 1e308.
  Native preparation scales the endpoints before subtraction and retains its
  required midpoint geometry; parity compares an equivalent finite range.
  Fallback contents remain in the prepared DOM as semantics and paint no shadow
  content, as in Chromium. Gauges create no interactive PDF field.
  """

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc false
  @spec boxes(map(), map(), number(), number(), number(), number()) :: [map()]
  def boxes(gauge, style, x, y, width, height) do
    if width <= 0 or height <= 0 do
      []
    else
      native = Map.fetch!(style, :_gauge_native)
      track_height = if native or gauge.kind == :meter, do: height / 2, else: height
      track_y = y + (height - track_height) / 2

      radius = if native, do: track_height / 2, else: 0.0

      color =
        case gauge do
          %{kind: :progress} ->
            if native,
              do: Map.get(style, :accent_color, {0.0, 117 / 255, 1.0}),
              else: {0.0, 128 / 255, 0.0}

          %{region: :optimum} ->
            {16 / 255, 124 / 255, 16 / 255}

          %{region: :suboptimal} ->
            {1.0, 185 / 255, 0.0}

          %{region: :even_less_good} ->
            {216 / 255, 59 / 255, 1 / 255}
        end

      base = %{
        type: HtmlValidator.reserve_layout_box(:rect),
        role: :gauge_track,
        snap_to_css_pixel_grid: not native,
        x: x,
        y: track_y,
        width: width,
        height: track_height,
        fill_color:
          if(native or gauge.kind == :meter,
            do: {239 / 255, 239 / 255, 239 / 255},
            else: {128 / 255, 128 / 255, 128 / 255}
          ),
        stroke_color: nil,
        stroke_width: 0.0,
        border_radius: radius,
        gauge_state: gauge,
        gauge_clip_track: native and gauge.kind == :meter
      }

      base =
        if not native and gauge.kind == :meter do
          Map.put(base, :gauge_gradient, [
            {0.8667, 0.8667, 0.8667},
            {0.9333, 0.9333, 0.9333},
            {0.8, 0.8, 0.8},
            {0.8667, 0.8667, 0.8667}
          ])
        else
          base
        end

      boxes =
        cond do
          width == 0 or track_height == 0 ->
            []

          gauge.position < 0 ->
            # A static unknown-progress mark conveys indeterminacy without sampling
            # an animation or suggesting a measured completion fraction.
            mark = %{
              base
              | type: HtmlValidator.reserve_layout_box(:rect),
                role: :gauge_indeterminate,
                x: x + width / 2 - min(width, track_height) / 2,
                width: min(width, track_height),
                fill_color: color,
                stroke_width: 0.0,
                stroke_color: nil
            }

            [base, mark]

          gauge.position == 0 ->
            [base]

          true ->
            fill = %{
              base
              | type: HtmlValidator.reserve_layout_box(:rect),
                role: :gauge_value,
                width: width,
                fill_color: color,
                stroke_width: 0.0,
                stroke_color: nil
            }

            inset = if native and gauge.kind == :meter, do: min(0.75, track_height / 2), else: 0.0

            fill = %{
              fill
              | x: x + inset,
                y: track_y + inset,
                width: max(width - 2 * inset, 0.0),
                height: max(track_height - 2 * inset, 0.0),
                border_radius: max(radius - inset, 0.0)
            }

            fill =
              fill
              |> Map.put(:gauge_fraction, gauge.position)
              |> Map.put(:gauge_quantize_value, native)

            fill =
              if not native and gauge.kind == :meter do
                colors =
                  case gauge.region do
                    :optimum ->
                      [
                        {0.6667, 0.8667, 0.4667},
                        {0.8, 0.9333, 0.6667},
                        {0.4667, 0.6667, 0.2},
                        {0.6667, 0.8667, 0.4667}
                      ]

                    :suboptimal ->
                      [
                        {1.0, 0.9333, 0.4667},
                        {1.0, 1.0, 0.8},
                        {0.8667, 0.7333, 0.2},
                        {1.0, 0.9333, 0.4667}
                      ]

                    :even_less_good ->
                      [
                        {1.0, 0.4667, 0.4667},
                        {1.0, 0.8, 0.8},
                        {0.8667, 0.2667, 0.2667},
                        {1.0, 0.4667, 0.4667}
                      ]
                  end

                Map.put(fill, :gauge_gradient, colors)
              else
                fill
              end

            if fill.width > 0 and fill.height > 0, do: [base, fill], else: [base]
        end

      boxes =
        if native and boxes != [] do
          outline = %{
            base
            | type: HtmlValidator.reserve_layout_box(:rect),
              role: :gauge_outline,
              fill_color: nil,
              stroke_color:
                {118 / 255, 118 / 255, 118 / 255, if(gauge.kind == :progress, do: 0.5, else: 0.3)},
              stroke_width: 0.75
          }

          boxes ++ [outline]
        else
          boxes
        end

      if native do
        Enum.map(boxes, fn box ->
          Map.put(box, :gauge_frame, {box.x - x, box.y - y, width, height})
        end)
      else
        boxes
      end
    end
  end
end
