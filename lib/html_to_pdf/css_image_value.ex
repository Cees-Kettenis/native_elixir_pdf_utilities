defmodule NativeElixirPdfUtilities.HtmlToPdf.CssImageValue do
  @moduledoc "Loads selected static CSS image values through the owning resource loader."
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssColor, CssGradient}
  alias NativeElixirPdfUtilities.Validators.CssGradientValidator
  alias NativeElixirPdfUtilities.Validators.{CssImageValueValidator, ResponsiveImageValidator}

  @doc "Loads only the selected candidate, retaining resource denials and shared budget failures."
  @spec load(map() | nil, keyword(), (String.t() -> term())) ::
          {:ok, map() | nil} | {:error, term()}
  def load(value, opts, loader) do
    selected =
      CssImageValueValidator.select(value, Keyword.get(opts, :image_selection_density, 1))

    result =
      case selected do
        nil ->
          {:ok, nil}

        %{kind: :url, source: source} ->
          with {:ok, image} <- loader.(source),
               {:ok, image} <- crop_fragment(image, Map.get(selected, :spatial_fragment)),
               do: {:ok, %{kind: :decoded, image: image}}

        %{kind: :element, id: id} ->
          with {:ok, image} <- loader.({:element, id}),
               do: {:ok, %{kind: :decoded, image: image}}

        %{kind: :cross_fade, values: values} ->
          CssImageValueValidator.map_blend(values, selected, &load(&1, opts, loader))

        %{kind: :fallback, image: image, color: color} = fallback ->
          case load(image, opts, loader) do
            {:ok, loaded} ->
              {:ok,
               if(loaded == nil, do: nil, else: Map.put(loaded, :direction, fallback.direction))}

            {:error, {:invalid_document, _}} = error ->
              if color == nil,
                do: error,
                else: {:ok, %{kind: :solid, color: color, direction: fallback.direction}}

            error ->
              error
          end

        image ->
          {:ok, image}
      end

    with {:ok, result} <- result do
      case {result, selected} do
        {%{kind: :decoded, image: image}, %{density: density}} ->
          with {:ok, image} <-
                 ResponsiveImageValidator.correct_natural_dimensions(image, %{
                   density: density,
                   url: Map.get(selected, :source, "CSS image-set")
                 }),
               do: {:ok, %{result | image: image}}

        _ ->
          {:ok, result}
      end
    end
  end

  defp crop_fragment(image, fragment) do
    case {image, fragment} do
      {image, nil} -> {:ok, image}
      {%{format: :png}, fragment} -> crop_surface(image, fragment)
      _ -> fragment_error("CSS image fragments require a decoded PNG or SVG surface")
    end
  end

  defp crop_surface(image, {unit, x, y, w, h}) do
    {scale_x, scale_y} =
      if unit == :percent, do: {image.width_px / 100, image.height_px / 100}, else: {1, 1}

    left = floor(x * scale_x)
    top = floor(y * scale_y)
    width = min(ceil((x + w) * scale_x), image.width_px) - left
    height = min(ceil((y + h) * scale_y), image.height_px) - top

    if width > 0 and height > 0 do
      with :ok <- CssImageValueValidator.reserve_surface(width, height, width * height) do
        sample_bytes = div(image.bits_per_component, 8)
        color_bytes = sample_bytes * if(image.color_space == :device_gray, do: 1, else: 3)

        cropped = %{
          image
          | data: crop_rows(image.data, image.width_px, left, top, width, height, color_bytes),
            width_px: width,
            height_px: height,
            width: image.width * width / image.width_px,
            height: image.height * height / image.height_px,
            decoded_stride: width * (color_bytes + sample_bytes)
        }

        cropped = Map.delete(cropped, :svg_viewport)

        case Map.get(image, :alpha_data) do
          nil ->
            {:ok, cropped}

          alpha ->
            {:ok,
             Map.put(
               cropped,
               :alpha_data,
               crop_rows(alpha, image.width_px, left, top, width, height, sample_bytes)
             )}
        end
      end
    else
      fragment_error("CSS image fragment lies outside the source")
    end
  end

  defp crop_rows(data, stride, x, y, width, height, bytes) do
    for row <- y..(y + height - 1), into: <<>> do
      binary_part(data, (row * stride + x) * bytes, width * bytes)
    end
  end

  defp fragment_error(message),
    do: Diagnostics.error(:style, :invalid_document, message, source: "image()")

  @doc "Supplies a selected decoded image or paints a generated image at its used dimensions."
  @spec rasterize(map(), number(), number(), map()) :: {:ok, map()} | {:error, term()}
  def rasterize(value, width, height, style) do
    result =
      case value do
        %{kind: :decoded, image: image} ->
          {:ok, image}

        %{kind: :cross_fade, values: values} ->
          blend(values, width, height, style)

        %{kind: :gradient} ->
          CssGradient.rasterize(value, width, height, style)

        %{kind: :solid, color: color} ->
          with :ok <- CssImageValueValidator.reserve_surface(1, 1, 1) do
            [r, g, b] =
              CssColor.to_srgb(
                color.space,
                Enum.map(color.channels, fn c -> if c == :none, do: 0.0, else: c end)
              )

            alpha = if color.alpha == :none, do: 0.0, else: color.alpha

            image = %{
              format: :png,
              data: <<round(r * 255), round(g * 255), round(b * 255)>>,
              width_px: 1,
              height_px: 1,
              width: width,
              height: height,
              color_space: :device_rgb,
              bits_per_component: 8,
              decoded_stride: 4
            }

            {:ok,
             if(alpha == 1, do: image, else: Map.put(image, :alpha_data, <<round(alpha * 255)>>))}
          end
      end

    with {:ok, image} <- result do
      image = CssImageValueValidator.apply_rendering(image, style)

      if Map.get(value, :direction) in [:ltr, :rtl] and
           value.direction != Map.get(style, :direction, :ltr) do
        orientation = Map.get(image, :image_orientation, 1)
        mirror = %{1 => 2, 2 => 1, 3 => 4, 4 => 3, 5 => 6, 6 => 5, 7 => 8, 8 => 7}
        {:ok, Map.put(image, :image_orientation, Map.fetch!(mirror, orientation))}
      else
        {:ok, image}
      end
    end
  end

  defp blend(values, width, height, style) do
    with {:ok, {w, h}} <- CssGradientValidator.raster_plan(width, height, length(values)),
         {:ok, prepared} <-
           CssImageValueValidator.map_blend(values, %{values: values}, fn value ->
             with {:ok, image} <- rasterize(value, width, height, style) do
               case image do
                 %{format: :png, color_space: space} when space in [:device_gray, :device_rgb] ->
                   {:ok, image}

                 _ ->
                   Diagnostics.error(
                     :style,
                     :not_implemented,
                     "cross-fade requires a raster image with decoded sample channels",
                     source: "cross-fade()"
                   )
               end
             end
           end) do
      rgba =
        for row <- 0..(h - 1), column <- 0..(w - 1), into: <<>> do
          {r, g, b, a} =
            Enum.reduce(prepared.values, {0.0, 0.0, 0.0, 0.0}, fn {image, alpha}, {r, g, b, a} ->
              {pr, pg, pb, pa} = image_pixel(image, (column + 0.5) / w, (row + 0.5) / h)
              coverage = pa * alpha
              {r + pr * coverage, g + pg * coverage, b + pb * coverage, a + coverage}
            end)

          if a == 0,
            do: <<0, 0, 0, 0>>,
            else: <<round(r / a), round(g / a), round(b / a), round(min(1.0, a) * 255)>>
        end

      image = %{
        format: :png,
        data: for(<<r, g, b, _ <- rgba>>, into: <<>>, do: <<r, g, b>>),
        width_px: w,
        height_px: h,
        width: width,
        height: height,
        color_space: :device_rgb,
        bits_per_component: 8,
        decoded_stride: w * 4
      }

      alpha = for <<_, _, _, a <- rgba>>, into: <<>>, do: <<a>>

      {:ok,
       if(alpha == :binary.copy(<<255>>, w * h),
         do: image,
         else: Map.put(image, :alpha_data, alpha)
       )}
    end
  end

  defp image_pixel(image, x, y) do
    {x, y} =
      case Map.get(image, :image_orientation, 1) do
        2 -> {1 - x, y}
        3 -> {1 - x, 1 - y}
        4 -> {x, 1 - y}
        5 -> {y, x}
        6 -> {y, 1 - x}
        7 -> {1 - y, 1 - x}
        8 -> {1 - y, x}
        _ -> {x, y}
      end

    at = trunc(y * image.height_px) * image.width_px + trunc(x * image.width_px)
    bytes = div(image.bits_per_component, 8)
    channels = if image.color_space == :device_gray, do: 1, else: 3
    offset = at * channels * bytes

    {red, green, blue} =
      case binary_part(image.data, offset, channels * bytes) do
        <<gray>> -> {gray, gray, gray}
        <<gray, _>> -> {gray, gray, gray}
        <<r, g, b>> -> {r, g, b}
        <<r, _, g, _, b, _>> -> {r, g, b}
      end

    alpha =
      case Map.get(image, :alpha_data) do
        nil -> 1.0
        values -> :binary.at(values, at * bytes) / 255
      end

    {red, green, blue, alpha}
  end

  @doc "Returns the PDF unit-square transform for the fixed TIFF orientation enumeration."
  @spec orientation_matrix(1..8) :: [integer()]
  def orientation_matrix(orientation) do
    case orientation do
      1 -> [1, 0, 0, 1, 0, 0]
      2 -> [-1, 0, 0, 1, 1, 0]
      3 -> [-1, 0, 0, -1, 1, 1]
      4 -> [1, 0, 0, -1, 0, 1]
      5 -> [0, -1, -1, 0, 1, 1]
      6 -> [0, -1, 1, 0, 0, 1]
      7 -> [0, 1, 1, 0, 0, 0]
      8 -> [0, 1, -1, 0, 1, 0]
    end
  end
end
