defmodule NativeElixirPdfUtilities.Validators.CssCompositingValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.Validators.{CssMathValidator, HtmlValidator, SvgStyleValidator}

  @blend_modes %{
    "normal" => :Normal,
    "multiply" => :Multiply,
    "screen" => :Screen,
    "overlay" => :Overlay,
    "darken" => :Darken,
    "lighten" => :Lighten,
    "color-dodge" => :ColorDodge,
    "color-burn" => :ColorBurn,
    "hard-light" => :HardLight,
    "soft-light" => :SoftLight,
    "difference" => :Difference,
    "exclusion" => :Exclusion,
    "hue" => :Hue,
    "saturation" => :Saturation,
    "color" => :Color,
    "luminosity" => :Luminosity
  }
  @pdf_modes Map.values(@blend_modes)
  @css_modes Map.new(@blend_modes, fn {keyword, mode} -> {mode, keyword} end)

  @doc "Admits CSS opacity and the static PDF blend-mode vocabulary."
  @spec apply(map(), binary(), binary()) :: {:ok, map()} | :error
  def apply(style, property, source) do
    case property do
      "opacity" ->
        value =
          case SvgStyleValidator.parse("opacity", source) do
            {:ok, value} ->
              {:ok, value}

            _ ->
              CssMathValidator.scalar(
                source,
                :number,
                Map.merge(style, %{percentage_basis: 1.0, percentage_dimension: :number})
              )
          end

        with {:ok, value} <- value,
             do: {:ok, Map.put(style, :opacity, min(1.0, max(0.0, value)))}

      "mix-blend-mode" ->
        with {:ok, mode} <- blend_mode(source),
             do: {:ok, Map.put(style, :mix_blend_mode, mode)},
             else: (_ -> :error)

      "background-blend-mode" ->
        source
        |> HtmlValidator.split_css_comma(true)
        |> Enum.reduce_while({:ok, []}, fn token, {:ok, modes} ->
          case blend_mode(token) do
            {:ok, mode} -> {:cont, {:ok, [mode | modes]}}
            _ -> {:halt, :error}
          end
        end)
        |> case do
          {:ok, modes} -> {:ok, Map.put(style, :background_blend_modes, Enum.reverse(modes))}
          error -> error
        end
    end
  end

  defp blend_mode(source) do
    with {:ok, identifier, ""} <-
           source |> HtmlValidator.trim_css_whitespace() |> HtmlValidator.parse_css_identifier(),
         {:ok, mode} <- Map.fetch(@blend_modes, String.downcase(identifier, :ascii)),
         do: {:ok, mode},
         else: (_ -> :error)
  end

  @doc "Returns the CSS spelling of a validated PDF blend mode for the SVG paint converter."
  @spec keyword(atom()) :: binary()
  def keyword(mode), do: Map.fetch!(@css_modes, mode)

  @doc "Identifies an element whose complete artwork needs one compositing surface."
  @spec group?(map()) :: boolean()
  def group?(style) do
    case style do
      style
      when not is_map_key(style, :opacity) and not is_map_key(style, :mix_blend_mode) and
             not is_map_key(style, :isolation) and not is_map_key(style, :css_clip_path) and
             not is_map_key(style, :css_mask) ->
        false

      _ ->
        :maps.get(:opacity, style, 1.0) < 1 or
          :maps.get(:mix_blend_mode, style, :Normal) != :Normal or
          :maps.get(:isolation, style, :auto) == :isolate or
          NativeElixirPdfUtilities.Validators.CssClipValidator.active?(style) or
          NativeElixirPdfUtilities.Validators.CssMaskValidator.active?(style)
    end
  end

  @doc "Identifies paint scopes that need a transparency surface; neutral isolated stacks without descendant blends keep ordinary paint."
  @spec paint_group?(map(), [map()]) :: boolean()
  def paint_group?(style, boxes) do
    group?(style) and
      (Map.get(style, :opacity, 1.0) < 1 or
         Map.get(style, :mix_blend_mode, :Normal) != :Normal or blend_descendants?(boxes))
  end

  defp blend_descendants?(boxes) do
    Enum.any?(boxes, fn box ->
      HtmlValidator.reserve_render_resource(:max_work, 1, :layout)

      Enum.any?(Map.get(box, :css_compositing_scopes, []), &(&1.mix_blend_mode != :Normal)) or
        case box do
          %{boxes: children} -> blend_descendants?(children)
          _ -> false
        end
    end)
  end

  @doc "Checks computed compositing values at the styled-tree boundary."
  @spec valid_style?(map()) :: boolean()
  def valid_style?(style) do
    case style do
      style
      when not is_map_key(style, :opacity) and not is_map_key(style, :mix_blend_mode) and
             not is_map_key(style, :background_blend_modes) and
             not is_map_key(style, :_css_compositing_contexts) ->
        true

      _ ->
        opacity = :maps.get(:opacity, style, 1.0)

        is_number(opacity) and opacity >= 0 and opacity <= 1 and
          :maps.get(:mix_blend_mode, style, :Normal) in @pdf_modes and
          case Map.get(style, :background_blend_modes, [:Normal]) do
            modes when is_list(modes) and modes != [] ->
              length(modes) <= Limits.get(:max_entries) and Enum.all?(modes, &(&1 in @pdf_modes))

            _ ->
              false
          end and validate_scopes(Map.get(style, :_css_compositing_contexts, [])) == :ok
    end
  end

  @doc "Rejects private resource fields and validates admitted writer compositing metadata."
  @spec validate_box(term()) :: :ok | {:error, {atom(), map()}}
  def validate_box(box) do
    case box do
      %{css_compositing_paint: _} ->
        Diagnostics.error(
          :writer,
          :invalid_layout,
          "CSS compositing paint flags are private writer metadata; supply source paint scopes instead",
          module: __MODULE__,
          operation: :write,
          source: "css_compositing_paint"
        )

      %{css_inner_opacity_key: _} ->
        Diagnostics.error(
          :writer,
          :invalid_layout,
          "CSS compositing resource keys are private writer metadata; supply source paint scopes instead",
          module: __MODULE__,
          operation: :write,
          source: "css_inner_opacity_key"
        )

      %{css_compositing_scopes: scopes} ->
        validate_scopes(scopes)

      _ ->
        :ok
    end
  end

  @doc "Checks shared resource ancestry across a paint plane so one group cannot acquire conflicting parents or state."
  @spec validate_ancestry([map()]) :: :ok | {:error, {atom(), map()}}
  def validate_ancestry(boxes) do
    HtmlValidator.with_render_budget(fn ->
      scopes = for %{css_compositing_scopes: [_ | _] = scopes} <- boxes, do: scopes

      Enum.reduce_while(scopes, %{}, fn scopes, groups ->
        {result, _} =
          Enum.reduce_while(scopes, {groups, []}, fn scope, {groups, path} ->
            HtmlValidator.reserve_render_resource(:max_work, 1, :writer)
            descriptor = {path, scope.opacity, scope.mix_blend_mode}

            case Map.fetch(groups, scope.id) do
              {:ok, ^descriptor} ->
                {:cont, {groups, [scope.id | path]}}

              :error ->
                HtmlValidator.check_render_resource(
                  :max_entries,
                  map_size(groups) + 1,
                  :writer
                )

                {:cont, {Map.put(groups, scope.id, descriptor), [scope.id | path]}}

              _ ->
                {:halt, {:conflict, []}}
            end
          end)

        if result == :conflict, do: {:halt, :conflict}, else: {:cont, result}
      end)
      |> case do
        :conflict ->
          Diagnostics.error(
            :writer,
            :invalid_layout,
            "CSS compositing group identities require one consistent parent ancestry, opacity and blend mode; recompute source paint scopes",
            module: __MODULE__,
            operation: :write,
            source: "CSS compositing ancestry"
          )

        _ ->
          :ok
      end
    end)
  end

  @doc "Checks bounded, unique compositing ancestry before PDF resource preparation."
  @spec validate_scopes(term()) :: :ok | {:error, {atom(), map()}}
  def validate_scopes(scopes) do
    valid? =
      scopes == [] or
        (is_list(scopes) and length(scopes) <= Limits.get(:max_depth) and
           Enum.all?(scopes, fn scope ->
             case scope do
               %{id: id, opacity: opacity, mix_blend_mode: mode} ->
                 case id do
                   {:background, owner, layer}
                   when is_reference(owner) and is_integer(layer) and layer >= 0 ->
                     true

                   _ ->
                     NativeElixirPdfUtilities.Validators.CssStackingValidator.valid_source_order?(
                       id
                     )
                 end and
                   valid_style?(%{opacity: opacity, mix_blend_mode: mode})

               _ ->
                 false
             end
           end) and length(Enum.uniq_by(scopes, & &1.id)) == length(scopes))

    if valid?,
      do: :ok,
      else:
        Diagnostics.error(
          :writer,
          :invalid_layout,
          "CSS compositing requires bounded, unique source ancestry, opacity in [0,1] and an admitted blend mode",
          module: __MODULE__,
          operation: :write,
          source: "CSS compositing"
        )
  end
end
