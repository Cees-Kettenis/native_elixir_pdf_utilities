defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.SvgValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc false
  @spec rasterize(term(), term(), HtmlValidator.image_budget() | nil) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  def rasterize(svg, options, budget) do
    with {:ok, dimensions} <- SvgValidator.validate(svg, options, budget) do
      result =
        Resvg.svg_string_to_png_binary(
          svg,
          [
            resources_dir: System.tmp_dir!(),
            shape_rendering: :optimize_speed,
            text_rendering: :optimize_speed,
            image_rendering: :optimize_speed,
            skip_system_fonts: true
          ] ++ dimensions
        )

      SvgValidator.conversion_result(result)
    end
  end
end
