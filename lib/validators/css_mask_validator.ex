defmodule NativeElixirPdfUtilities.Validators.CssMaskValidator do
  @moduledoc "Layered CSS alpha/luminance masks and nine-part mask borders through existing image geometry."
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.CssImageValue

  alias NativeElixirPdfUtilities.Validators.{
    CssBackgroundValidator,
    CssBorderImageValidator,
    CssImageValueValidator,
    HtmlValidator
  }

  @properties ~w(mask mask-image mask-mode mask-repeat mask-position mask-size mask-origin mask-clip mask-composite mask-border mask-border-source mask-border-slice mask-border-width mask-border-outset mask-border-repeat mask-border-mode)
  @background_fields CssBackgroundValidator.canvas_fields() ++ [:background_image_source]
  @border_fields Map.keys(CssBorderImageValidator.defaults())
  @modes ~w(alpha luminance match-source)
  @operators ~w(add subtract intersect exclude)

  @doc "Lists mask and mask-border declarations using shared background and border-image grammar."
  @spec properties() :: [binary()]
  def properties, do: @properties

  @doc "Returns noninherited mask defaults without populating ordinary styles."
  @spec defaults() :: map()
  def defaults do
    %{
      css_mask: %{
        background: %{
          background_images: [nil],
          background_image: nil,
          background_color: nil,
          background_position: {{:percent, 0.0}, {:percent, 0.0}},
          background_size: {:auto, :auto},
          background_repeat: :repeat,
          background_origins: [:border_box],
          background_clips: [:border_box]
        },
        modes: ["match-source"],
        composites: ["add"],
        border: CssBorderImageValidator.defaults(),
        border_mode: "alpha"
      }
    }
  end

  @doc "Prepares mask layers and border images through the existing source grammars."
  @spec prepare(map(), binary(), binary()) :: {:ok, map()} | :error | {:error, term()}
  def prepare(style, property, source) do
    mask = mask_defaults(Map.get(style, :css_mask, %{}))

    cond do
      property in ["mask-mode", "mask-composite"] ->
        choices = if property == "mask-mode", do: @modes, else: @operators

        values =
          source
          |> HtmlValidator.split_css_comma(true)
          |> Enum.map(&(String.trim(&1) |> String.downcase(:ascii)))

        HtmlValidator.check_render_resource(:max_entries, length(values), :style)
        key = if property == "mask-mode", do: :modes, else: :composites

        if values != [] and Enum.all?(values, &(&1 in choices)),
          do: {:ok, Map.put(style, :css_mask, Map.put(mask, key, values))},
          else: :error

      property == "mask-border-mode" ->
        value = source |> String.trim() |> String.downcase(:ascii)

        if value in ~w(alpha luminance),
          do: {:ok, Map.put(style, :css_mask, %{mask | border_mode: value})},
          else: :error

      String.starts_with?(property, "mask-border") ->
        translated = String.replace_prefix(property, "mask-border", "border-image")

        with {:ok, prepared} <-
               CssBorderImageValidator.prepare(Map.merge(style, mask.border), translated, source),
             do:
               {:ok,
                Map.put(style, :css_mask, %{mask | border: Map.take(prepared, @border_fields)})}

      property == "mask-image" ->
        values = HtmlValidator.split_css_comma(source, true)

        Enum.reduce_while(values, {:ok, []}, fn source, {:ok, images} ->
          case CssImageValueValidator.prepare(source, style) do
            {:ok, image} -> {:cont, {:ok, [image | images]}}
            error -> {:halt, error}
          end
        end)
        |> case do
          {:ok, [_ | _] = images} ->
            {:ok,
             Map.put(style, :css_mask, %{
               mask
               | background:
                   mask.background
                   |> Map.delete(:background_image_source)
                   |> Map.put(:background_images, Enum.reverse(images))
             })}

          _ ->
            :error
        end

      property == "mask" ->
        layers = HtmlValidator.split_css_comma(source, true)

        parsed =
          Enum.map(layers, fn layer ->
            tokens = HtmlValidator.split_css_tokens(layer)
            modes = Enum.filter(tokens, &(&1 in @modes))
            operators = Enum.filter(tokens, &(&1 in @operators))

            if length(modes) <= 1 and length(operators) <= 1 do
              %{
                source:
                  tokens |> Enum.reject(&(&1 in @modes or &1 in @operators)) |> Enum.join(" "),
                mode: List.first(modes, "match-source"),
                composite: List.first(operators, "add")
              }
            end
          end)

        if parsed != [] and Enum.all?(parsed, &is_map/1) do
          with {:ok, prepared} <-
                 CssBackgroundValidator.prepare(
                   Map.drop(style, @background_fields),
                   "background",
                   Enum.map_join(parsed, ", ", & &1.source)
                 ),
               true <- Map.get(prepared, :background_color) == nil do
            background =
              Map.take(prepared, @background_fields)
              |> Map.put_new(:background_origins, [:border_box])
              |> Map.put_new(:background_clips, [:border_box])

            {:ok,
             Map.put(style, :css_mask, %{
               defaults().css_mask
               | background: background,
                 modes: Enum.map(parsed, & &1.mode),
                 composites: Enum.map(parsed, & &1.composite)
             })}
          else
            _ -> :error
          end
        else
          :error
        end

      true ->
        translated = String.replace_prefix(property, "mask-", "background-")

        with {:ok, prepared} <-
               CssBackgroundValidator.prepare(
                 Map.merge(Map.drop(style, @background_fields), mask.background),
                 translated,
                 source
               ),
             do:
               {:ok,
                Map.put(style, :css_mask, %{
                  mask
                  | background: Map.take(prepared, @background_fields)
                })}
    end
  end

  @doc "Loads only selected mask resources using the declaring element's existing authorized image loader."
  @spec load(map(), keyword(), function()) :: {:ok, map()} | {:error, term()}
  def load(style, opts, loader) do
    case Map.get(style, :css_mask) do
      nil ->
        {:ok, style}

      mask ->
        mask = mask_defaults(mask)

        images =
          case Map.get(mask.background, :background_image_source) do
            nil -> Map.get(mask.background, :background_images, [nil])
            source -> [%{kind: :url, source: source}]
          end

        Enum.reduce_while(images, {:ok, []}, fn image, {:ok, images} ->
          result =
            if Map.get(style, :display) == :none,
              do: {:ok, nil},
              else: CssImageValue.load(image, opts, loader)

          case result do
            {:ok, image} -> {:cont, {:ok, [image | images]}}
            error -> {:halt, error}
          end
        end)
        |> case do
          {:ok, images} ->
            with {:ok, border} <-
                   if(Map.get(style, :display) == :none,
                     do: {:ok, nil},
                     else: CssImageValue.load(mask.border.border_image_source, opts, loader)
                   ),
                 background =
                   mask.background
                   |> Map.delete(:background_image_source)
                   |> Map.put(:background_images, Enum.reverse(images)),
                 {:ok, background} <-
                   CssBackgroundValidator.compute_style(
                     Map.merge(Map.drop(style, @background_fields), background)
                   ),
                 do:
                   {:ok,
                    Map.put(style, :css_mask, %{
                      mask
                      | background: Map.take(background, @background_fields),
                        border: %{mask.border | border_image_source: border}
                    })}

          error ->
            error
        end
    end
  end

  defp mask_defaults(mask) do
    initial = defaults().css_mask

    initial
    |> Map.merge(mask)
    |> Map.put(:background, Map.merge(initial.background, Map.get(mask, :background, %{})))
    |> Map.put(:border, Map.merge(initial.border, Map.get(mask, :border, %{})))
  end

  @doc "Identifies a mask that establishes an isolated paint scope."
  @spec active?(map()) :: boolean()
  def active?(style) do
    case Map.get(style, :css_mask) do
      nil ->
        false

      mask ->
        Enum.any?(Map.get(mask.background, :background_images, [nil]), &(&1 != nil)) or
          Map.get(mask.background, :background_image_source) != nil or
          mask.border.border_image_source != nil
    end
  end

  @doc "Validates mask resource and geometry facts through the shared image/background/border validators."
  @spec valid_style?(map()) :: boolean()
  def valid_style?(style) do
    case Map.get(style, :css_mask) do
      nil ->
        true

      %{
        background: background,
        modes: modes,
        composites: composites,
        border: border,
        border_mode: mode
      }
      when is_map(background) and is_map(border) and is_list(modes) and is_list(composites) ->
        mode in ~w(alpha luminance) and modes != [] and composites != [] and
          length(modes) <= Limits.get(:max_entries) and
          length(composites) <= Limits.get(:max_entries) and Enum.all?(modes, &(&1 in @modes)) and
          Enum.all?(composites, &(&1 in @operators)) and
          CssBackgroundValidator.validate_style(background, "mask") == :ok and
          CssBorderImageValidator.validate_style(border, "mask-border") == :ok

      _ ->
        false
    end
  end

  @doc "Admits matching mask planes and one standard compositing operator before byte arithmetic."
  @spec reserve_composite(term(), term(), term()) :: :ok | {:error, {atom(), map()}}
  def reserve_composite(source, backdrop, operator) do
    if is_binary(source) and is_binary(backdrop) and byte_size(source) == byte_size(backdrop) and
         operator in @operators do
      HtmlValidator.reserve_render_resource(:max_raster_work, byte_size(source), :writer)
      HtmlValidator.reserve_render_resource(:max_decoded_bytes, byte_size(source), :writer)
      :ok
    else
      invalid(
        "mask composition requires equal-length alpha planes and add/subtract/intersect/exclude"
      )
    end
  end

  @doc "Maps mask cascade longhands to their shared nested image and geometry facts."
  @spec cascade_fields() :: map()
  def cascade_fields do
    background = %{
      "mask-image" => [:background_images, :background_image_source, :background_image],
      "mask-position" => [:background_positions, :background_position],
      "mask-size" => [:background_sizes, :background_size],
      "mask-repeat" => [:background_repeats, :background_repeat],
      "mask-origin" => [:background_origins],
      "mask-clip" => [:background_clips]
    }

    Map.new(background, fn {property, fields} ->
      {property, Enum.map(fields, &[:css_mask, :background, &1])}
    end)
    |> Map.merge(%{
      "mask-mode" => [[:css_mask, :modes]],
      "mask-composite" => [[:css_mask, :composites]],
      "mask-border-mode" => [[:css_mask, :border_mode]]
    })
    |> Map.merge(
      Map.new(CssBorderImageValidator.properties() -- ["border-image"], fn property ->
        {String.replace_prefix(property, "border-image", "mask-border"),
         [[:css_mask, :border, String.to_existing_atom(String.replace(property, "-", "_"))]]}
      end)
    )
  end

  @doc "Builds an actionable malformed mask diagnostic at the shared layout boundary."
  @spec invalid(binary()) :: {:error, {atom(), map()}}
  def invalid(message),
    do:
      Diagnostics.error(:layout, :invalid_layout, message,
        module: __MODULE__,
        operation: :prepare_css_mask,
        source: "mask"
      )
end
