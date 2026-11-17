defmodule NativeElixirPdfUtilities.HtmlToPdf.CssMask do
  @moduledoc "Mask layer painting and alpha composition over the existing effect raster."
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssBackground, CssBorderImage, CssEffectScene}
  alias NativeElixirPdfUtilities.Validators.CssMaskValidator
  @zero %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

  @doc "Composites equal admitted alpha planes using the specified Porter-Duff operator."
  @spec composite(term(), term(), term()) :: {:ok, binary()} | {:error, {atom(), map()}}
  def composite(source, backdrop, operator) do
    with :ok <- CssMaskValidator.reserve_composite(source, backdrop, operator) do
      if source == <<>> do
        {:ok, <<>>}
      else
        {:ok,
         for index <- 0..(byte_size(source) - 1), into: <<>> do
           s = :binary.at(source, index)
           b = :binary.at(backdrop, index)

           alpha =
             case operator do
               "add" -> s + b - s * b / 255
               "subtract" -> s * (1 - b / 255)
               "intersect" -> s * b / 255
               "exclude" -> s * (1 - b / 255) + b * (1 - s / 255)
             end

           <<round(alpha)>>
         end}
      end
    end
  end

  @doc "Applies ordered image masks and the independently composited nine-part mask border."
  @spec apply(map(), map(), map(), tuple()) :: {:ok, map()} | {:error, {atom(), map()}}
  def apply(image, effect, region, size) do
    style = Map.get(effect, :style, %{})

    if CssMaskValidator.active?(style) do
      mask = style.css_mask
      bounds = effect.bounds

      rect = %{
        type: :rect,
        x: bounds.x,
        y: bounds.top - bounds.height,
        width: bounds.width,
        height: bounds.height,
        fill_color: nil,
        stroke_color: nil,
        stroke_width: 0.0,
        border_radius: Map.get(style, :border_radius, 0.0),
        border_widths: Map.get(style, :border_widths, @zero)
      }

      images = Map.get(mask.background, :background_images, [nil])
      initial = :binary.copy(<<255>>, image.width_px * image.height_px)

      result =
        if Enum.any?(images, &(&1 != nil)) do
          images
          |> Enum.with_index()
          |> Enum.reverse()
          |> Enum.reduce_while({:ok, nil}, fn {source, index}, {:ok, alpha} ->
            layer =
              Enum.reduce(
                [
                  :background_positions,
                  :background_sizes,
                  :background_repeats,
                  :background_origins,
                  :background_clips
                ],
                %{background_images: [source], background_color: nil},
                fn key, layer ->
                  case Map.get(mask.background, key) do
                    nil -> layer
                    values -> Map.put(layer, key, [Enum.at(values, rem(index, length(values)))])
                  end
                end
              )

            background =
              style
              |> Map.merge(mask.background)
              |> Map.merge(layer)
              |> Map.put(:background_blend_modes, [:Normal])

            boxes = CssBackground.boxes(background, rect)

            with {:ok, %{image: layer}} <-
                   CssEffectScene.render(
                     boxes,
                     region,
                     [],
                     %{definitions: "", color: {0, 0, 0}},
                     size
                   ),
                 layer_alpha = alpha(layer, Enum.at(mask.modes, rem(index, length(mask.modes)))),
                 {:ok, combined} <-
                   if(alpha == nil,
                     do: {:ok, layer_alpha},
                     else:
                       composite(
                         layer_alpha,
                         alpha,
                         Enum.at(mask.composites, rem(index, length(mask.composites)))
                       )
                   ),
                 do: {:cont, {:ok, combined}},
                 else: (error -> {:halt, error})
          end)
        else
          {:ok, initial}
        end

      with {:ok, alpha} <- result,
           {:ok, alpha} <- border_alpha(alpha, mask, style, rect, region, size),
           {:ok, alpha} <- composite(Map.get(image, :alpha_data, initial), alpha, "intersect"),
           do: {:ok, Map.put(image, :alpha_data, alpha)}
    else
      {:ok, image}
    end
  end

  defp border_alpha(alpha, mask, style, rect, region, size) do
    if mask.border.border_image_source == nil do
      {:ok, alpha}
    else
      boxes = CssBorderImage.boxes(Map.merge(style, mask.border), rect, :mask)

      with {:ok, %{image: image}} <-
             CssEffectScene.render(boxes, region, [], %{definitions: "", color: {0, 0, 0}}, size),
           do: composite(alpha, alpha(image, mask.border_mode), "intersect")
    end
  end

  defp alpha(image, mode) do
    pixels = image.width_px * image.height_px
    alpha = Map.get_lazy(image, :alpha_data, fn -> :binary.copy(<<255>>, pixels) end)

    if mode == "luminance" do
      # CSS Masking 1 uses the luminance of the mask image multiplied by alpha.
      for index <- 0..(pixels - 1), into: <<>> do
        offset = index * 3

        luminance =
          0.2126 * :binary.at(image.data, offset) + 0.7152 * :binary.at(image.data, offset + 1) +
            0.0722 * :binary.at(image.data, offset + 2)

        <<round(luminance * :binary.at(alpha, index) / 255)>>
      end
    else
      alpha
    end
  end
end
