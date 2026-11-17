defmodule NativeElixirPdfUtilities.TestSupport.LayoutFixture do
  @moduledoc false
  @doc false
  @spec image_fixture(number(), number()) :: map()
  def image_fixture(width, height) do
    %{
      format: :png,
      data: <<255, 0, 0>>,
      width_px: round(width / 0.75),
      height_px: round(height / 0.75),
      width: width,
      height: height,
      color_space: :device_rgb,
      bits_per_component: 8
    }
  end

  @doc false
  @spec document([map()]) :: map()
  def document(children) do
    %{type: :document, children: children}
  end

  @doc false
  @spec block_style() :: map()
  def block_style do
    Map.merge(text_style(), %{
      display: :block,
      line_height: 14.4,
      margin_after: 12.0
    })
  end

  @doc false
  @spec text_style() :: map()
  def text_style do
    %{
      color: {0, 0, 0},
      font_face: %{type: :built_in, family: "Helvetica", pdf_name: "Helvetica"},
      font_family: "Helvetica",
      font_size: 12.0,
      font_style: :normal,
      font_weight: 400,
      line_height: 14.4
    }
  end
end
