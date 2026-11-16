defmodule NativeElixirPdfUtilities.HtmlToPdf.CssImageValue do
  @moduledoc "Loads selected static CSS image values through the owning resource loader."
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssColor, CssGradient}
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
          with {:ok, image} <- loader.(source), do: {:ok, %{kind: :decoded, image: image}}

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

  @doc "Supplies a selected decoded image or paints a generated image at its used dimensions."
  @spec rasterize(map(), number(), number(), map()) :: {:ok, map()} | {:error, term()}
  def rasterize(value, width, height, style) do
    result =
      case value do
        %{kind: :decoded, image: image} ->
          {:ok, image}

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
