defmodule NativeElixirPdfUtilities.Validators.CssColorAdjustmentValidator do
  @moduledoc "Static print color environment and adjustment controls."
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Prepares color-scheme and print/forced adjustment in the deterministic light-preferred print environment."
  @spec prepare(String.t(), String.t()) :: {:ok, map()} | :error
  def prepare(property, value) do
    tokens = value |> String.downcase(:ascii) |> HtmlValidator.split_css_tokens()

    case {property, tokens} do
      {"color-scheme", ["normal"]} ->
        {:ok, %{color_scheme: :light}}

      {"color-scheme", tokens} ->
        schemes = Enum.reject(tokens, &(&1 == "only"))

        if schemes != [] and length(Enum.uniq(tokens)) == length(tokens) and
             Enum.all?(schemes, &(&1 in ["light", "dark"])),
           do: {:ok, %{color_scheme: if("light" in schemes, do: :light, else: :dark)}},
           else: :error

      {"print-color-adjust", [value]} when value in ["economy", "exact"] ->
        {:ok, %{print_color_adjust: String.to_existing_atom(value)}}

      {"forced-color-adjust", [value]} when value in ["auto", "none", "preserve-parent-color"] ->
        {:ok,
         %{
           forced_color_adjust:
             case value do
               "auto" -> :auto
               "none" -> :none
               _ -> :preserve_parent_color
             end
         }}

      _ ->
        :error
    end
  end

  @doc "Applies the selected dark UA control palette before authored color and box declarations."
  @spec control_defaults(map()) :: map()
  def control_defaults(style) do
    case style do
      %{color_scheme: :dark, form_kind: kind} when kind != nil ->
        button? = Map.get(style, :form_kind) in [:button, :input_button, :color]

        {:ok, foreground} =
          HtmlValidator.parse_css_color(
            if(Map.get(style, :_control_disabled, false),
              do: "GrayText",
              else: if(button?, do: "ButtonText", else: "FieldText")
            ),
            style
          )

        {:ok, background} =
          HtmlValidator.parse_css_color(if(button?, do: "ButtonFace", else: "Field"), style)

        border = {133 / 255, 133 / 255, 133 / 255}

        style
        |> Map.put(:color, foreground)
        |> Map.put(
          :background_color,
          if(Map.get(style, :background_color) == nil, do: nil, else: background)
        )
        |> Map.put(:border_color, border)
        |> Map.put(:border_colors, Map.new([:top, :right, :bottom, :left], &{&1, border}))

      _ ->
        style
    end
  end

  @doc "Checks the selected print color controls without adding a device or host-theme dependency."
  @spec validate_style(map(), String.t()) :: :ok | {:error, term()}
  def validate_style(style, source) do
    case style do
      style
      when not is_map_key(style, :color_scheme) and not is_map_key(style, :print_color_adjust) and
             not is_map_key(style, :forced_color_adjust) ->
        :ok

      _ ->
        if Map.get(style, :color_scheme, :light) in [:light, :dark] and
             Map.get(style, :print_color_adjust, :economy) in [:economy, :exact] and
             Map.get(style, :forced_color_adjust, :auto) in [:auto, :none, :preserve_parent_color],
           do: :ok,
           else:
             Diagnostics.error(
               :layout,
               :invalid_layout,
               "color controls require a light/dark scheme and supported print/forced adjustment values",
               operation: :layout,
               module: __MODULE__,
               source: source
             )
    end
  end
end
