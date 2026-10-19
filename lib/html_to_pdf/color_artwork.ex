defmodule NativeElixirPdfUtilities.HtmlToPdf.ColorArtwork do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry

  @doc false
  @spec wrapper_padding() :: %{vertical: float(), horizontal: float()}
  def wrapper_padding, do: %{vertical: 3.0, horizontal: 1.5}

  @doc false
  @spec boxes(map(), number(), number(), number(), number()) :: [map()]
  def boxes(style, x, y, width, height) do
    border = style.border_widths
    padding = style.padding
    wrapper = wrapper_padding()
    # Chromium's fixed UA swatch wrapper uses 4px vertical/2px horizontal
    # padding. The swatch itself has a 1px solid rgb(119,119,119) border.
    swatch =
      %{
        type: HtmlValidator.reserve_layout_box(:rect),
        role: :color_swatch,
        x: x + border.left + padding.left + wrapper.horizontal,
        y: y + border.bottom + padding.bottom + wrapper.vertical,
        width:
          max(
            width - border.left - border.right - padding.left - padding.right -
              2 * wrapper.horizontal,
            0.0
          ),
        height:
          max(
            height - border.top - border.bottom - padding.top - padding.bottom -
              2 * wrapper.vertical,
            0.0
          ),
        border_radius: 0.0,
        fill_color: style._color_state.rgb,
        stroke_width: 0.75,
        stroke_color: {119 / 255, 119 / 255, 119 / 255},
        snap_to_css_pixel_grid: true,
        form_owner: Map.get(style, :_form_owner),
        clip:
          PageGeometry.inset_rounded_clip(
            %{x: x, y: y, width: width, height: height, border_radius: style.border_radius},
            border
          )
      }

    if swatch.width > 0 and swatch.height > 0, do: [swatch], else: []
  end
end
