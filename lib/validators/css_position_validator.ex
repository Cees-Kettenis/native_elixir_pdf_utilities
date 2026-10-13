defmodule NativeElixirPdfUtilities.Validators.CssPositionValidator do
  @moduledoc "Resolves validated CSS positions against actual fitted or background image space."

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{CssMathValidator, HtmlValidator, SourceSizeValidator}

  @doc "Checks the numeric type of a position calculation without loading fonts or images."
  @spec prepare_math(String.t()) :: {:ok, {:css_position_math, String.t()}} | :error
  def prepare_math(value) do
    environment = %{width: 1.0, height: 1.0, type_check: true, percentage_basis: 1.0}

    result =
      try do
        if is_binary(value) and String.valid?(value),
          do: CssMathValidator.evaluate(value, environment),
          else: :error
      rescue
        ArithmeticError -> :error
      end

    case result do
      {:ok, _, %{length: 1} = type} when map_size(type) == 1 ->
        {:ok, {:css_position_math, value}}

      {:ok, zero, type} when zero == 0 and map_size(type) == 0 ->
        {:ok, {:css_position_math, value}}

      _ ->
        :error
    end
  end

  @doc "Resolves a position in points using actual remaining space and the selected font style."
  @spec resolve(term(), number(), map()) :: {:ok, float()} | {:error, {atom(), map()}}
  def resolve(position, remaining, style) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, remaining} <- HtmlValidator.normalize_geometry_number(remaining),
           true <- is_map(style) do
        result =
          case position do
            {:percent, ratio} when is_number(ratio) ->
              {:ok, remaining * ratio}

            {:end, offset} ->
              with {:ok, offset} <- resolve(offset, remaining, style),
                   do: {:ok, remaining - offset}

            {:css_position_math, source} when is_binary(source) ->
              source = if String.valid?(source), do: source, else: "invalid"
              font = Map.get(style, :font_face)
              root_font = Map.get(style, :_root_font_face) || font
              size = Map.get(style, :font_size)
              root_size = Map.get(style, :_root_font_size, size)

              with true <-
                     is_number(size) and size >= 0 and is_number(root_size) and root_size >= 0,
                   true <- is_number(Map.get(style, :line_height)),
                   {:ok, metrics} <-
                     SourceSizeValidator.prepare_font_metrics(font, size, style.line_height),
                   {:ok, root_metrics} <-
                     SourceSizeValidator.prepare_font_metrics(
                       root_font,
                       root_size,
                       Map.get(style, :_root_line_height, style.line_height)
                     ) do
                viewport = Map.get(style, :_position_viewport)

                environment = %{
                  width: if(is_map(viewport), do: Map.get(viewport, :width), else: nil),
                  height: if(is_map(viewport), do: Map.get(viewport, :height), else: nil),
                  font_size: size / 0.75,
                  root_font_size: root_size / 0.75,
                  source_metrics: metrics,
                  root_metrics: root_metrics,
                  percentage_basis: remaining / 0.75
                }

                evaluated =
                  try do
                    CssMathValidator.evaluate(source, environment)
                  rescue
                    ArithmeticError -> :error
                  end

                case evaluated do
                  {:ok, :nan, type} when type == %{length: 1} ->
                    {:ok, 0.0}

                  {:ok, value, type} when is_number(value) and type == %{length: 1} ->
                    {:ok, value * 0.75}

                  {:ok, value, type} when value == 0 and map_size(type) == 0 ->
                    {:ok, 0.0}

                  {:ok, special, type}
                  when special in [:infinity, :negative_infinity] and type == %{length: 1} ->
                    Diagnostics.error(
                      :layout,
                      :resource_limit_exceeded,
                      "CSS position exceeds the finite layout numeric range",
                      module: __MODULE__,
                      source: source
                    )

                  _ ->
                    :error
                end
              else
                false -> :error
                error -> error
              end

            number when is_number(number) ->
              {:ok, number}

            _ ->
              :error
          end

        case result do
          {:ok, value} ->
            HtmlValidator.check_render_resource(:max_css_numeric_magnitude, abs(value), :layout)
            HtmlValidator.normalize_geometry_number(value)

          {:error, _} = error ->
            error

          :error ->
            Diagnostics.error(
              :layout,
              :invalid_layout,
              "CSS position requires a finite length or percentage calculation and selected font metrics",
              module: __MODULE__,
              source: inspect(position)
            )
        end
      else
        _ ->
          Diagnostics.error(
            :layout,
            :invalid_layout,
            "CSS position requires finite remaining image space and a selected style",
            module: __MODULE__,
            source: "object-position/background-position"
          )
      end
    end)
  end
end
