defmodule NativeElixirPdfUtilities.Validators.CssPositionValidator do
  @moduledoc "Resolves validated CSS positions against actual fitted or background image space."

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{CssMathValidator, HtmlValidator, SourceSizeValidator}

  @doc "Checks the numeric type of a position calculation without loading fonts or images."
  @spec prepare_math(String.t()) ::
          {:ok, {:css_position_math, String.t()}} | :error | {:error, {atom(), map()}}
  def prepare_math(value) do
    HtmlValidator.with_render_budget(fn ->
      environment = %{
        width: 1.0,
        height: 1.0,
        type_check: true,
        percentage_basis: 1.0,
        _depth_limit: :max_css_calculation_depth
      }

      case CssMathValidator.length_dependencies(value) do
        {:ok, _} ->
          {:ok, {:css_position_math, value}}

        {:error, {:resource_limit_exceeded, _}} = error ->
          error

        _ ->
          case if(is_binary(value) and String.valid?(value),
                 do: CssMathValidator.evaluate(value, environment),
                 else: :error
               ) do
            {:ok, zero, type} when zero == 0 and map_size(type) == 0 ->
              {:ok, {:css_position_math, value}}

            _ ->
              :error
          end
      end
    end)
  end

  @doc "Prepares the selected-font CSS math environment using a real point percentage basis."
  @spec math_environment(map(), number()) :: {:ok, map()} | {:error, {atom(), map()}}
  def math_environment(style, remaining) do
    case Map.fetch(style, :_css_unit_scales) do
      {:ok, scales} ->
        {:ok,
         %{
           unit_scales: scales,
           percentage_basis: remaining / 0.75,
           _intrinsic_basis: Map.get(style, :_intrinsic_basis),
           _depth_limit: :max_css_calculation_depth
         }}

      :error ->
        selected_math_environment(style, remaining)
    end
  end

  defp selected_math_environment(style, remaining) do
    font = Map.get(style, :font_face)
    root_font = Map.get(style, :_root_font_face) || font
    size = Map.get(style, :font_size)
    root_size = Map.get(style, :_root_font_size, size)

    line_height =
      NativeElixirPdfUtilities.Validators.CssLengthValidator.computed_line_height(style)

    with true <- is_number(size) and size >= 0 and is_number(root_size) and root_size >= 0,
         true <- is_number(line_height),
         {:ok, metrics} <- SourceSizeValidator.prepare_font_metrics(font, size, line_height),
         {:ok, root_metrics} <-
           SourceSizeValidator.prepare_font_metrics(
             root_font,
             root_size,
             Map.get(style, :_root_line_height, style.line_height)
           ) do
      viewport = Map.get(style, :_position_viewport)

      {:ok,
       %{
         width: if(is_map(viewport), do: Map.get(viewport, :width), else: nil),
         height: if(is_map(viewport), do: Map.get(viewport, :height), else: nil),
         font_size: size / 0.75,
         root_font_size: root_size / 0.75,
         writing_mode: Map.get(style, :writing_mode, :horizontal_tb),
         _intrinsic_basis: Map.get(style, :_intrinsic_basis),
         source_metrics: metrics,
         root_metrics: root_metrics,
         percentage_basis: remaining / 0.75,
         _depth_limit: :max_css_calculation_depth
       }}
    else
      {:error, _} = error ->
        error

      _ ->
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "CSS calculation requires finite selected font and root font metrics",
          module: __MODULE__,
          operation: :resolve_css_math
        )
    end
  end

  @doc "Computes position units at their declaring element while retaining the used percentage space."
  @spec compute_style(map()) :: {:ok, map()} | :error | {:error, {atom(), map()}}
  def compute_style(style) do
    Enum.reduce_while(
      [:offsets, :object_position, :background_position, :transform_origin],
      {:ok, style},
      fn property, {:ok, result} ->
        case compute_position(Map.get(result, property), style) do
          {:ok, value} ->
            {:cont,
             {:ok,
              if(value == Map.get(result, property),
                do: result,
                else: Map.put(result, property, value)
              )}}

          error ->
            {:halt, error}
        end
      end
    )
  end

  defp compute_position(position, style) do
    case position do
      {:css_position_math, source} when is_binary(source) ->
        case CssMathValidator.length_dependencies(source) do
          {:ok, %{percentage?: true}} ->
            with {:ok, calculation} <- CssMathValidator.compute_length(source, style),
                 do: {:ok, {:css_position_math, calculation}}

          _ ->
            resolve(position, 0.0, style)
        end

      {:end, offset} ->
        with {:ok, offset} <- compute_position(offset, style), do: {:ok, {:end, offset}}

      {x, y} when not is_atom(x) ->
        with {:ok, x} <- compute_position(x, style),
             {:ok, y} <- compute_position(y, style),
             do: {:ok, {x, y}}

      map when is_map(map) ->
        Enum.reduce_while(map, {:ok, map}, fn {edge, value}, {:ok, result} ->
          case compute_position(value, style) do
            {:ok, value} -> {:cont, {:ok, Map.put(result, edge, value)}}
            error -> {:halt, error}
          end
        end)

      _ ->
        {:ok, position}
    end
  end

  @doc "Checks prepared CSS position dimensions through the shared math grammar and numeric bounds."
  @spec valid_prepared?(term()) :: boolean()
  def valid_prepared?(position) do
    valid_prepared_position?(position, 0)
  end

  defp valid_prepared_position?(position, depth) do
    HtmlValidator.check_render_resource(:max_css_calculation_depth, depth, :layout)

    case position do
      {:css_position_math, {:css_calculation, _} = calculation} ->
        CssMathValidator.valid_prepared_length?(calculation)

      {:percent, ratio} ->
        match?({:ok, _}, HtmlValidator.normalize_geometry_number(ratio))

      {:end, offset} ->
        valid_prepared_position?(offset, depth + 1)

      {:css_position_math, source} when is_binary(source) ->
        case prepare_math(source) do
          {:error, {:resource_limit_exceeded, _}} = error ->
            throw({:render_resource_limit, error})

          prepared ->
            prepared == {:ok, position}
        end

      number when is_number(number) ->
        match?({:ok, _}, HtmlValidator.normalize_geometry_number(number))

      _ ->
        false
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
            {:css_position_math, {:css_calculation, _} = calculation} ->
              CssMathValidator.resolve_length(calculation, remaining, :property)

            {:percent, ratio} when is_number(ratio) ->
              {:ok, remaining * ratio}

            {:end, offset} ->
              with {:ok, offset} <- resolve(offset, remaining, style),
                   do: {:ok, remaining - offset}

            {:css_position_math, source} when is_binary(source) ->
              source = if String.valid?(source), do: source, else: "invalid"

              context = Map.take(style, [:_intrinsic_basis])
              dependencies = CssMathValidator.length_dependencies(source, context)

              prepared =
                case dependencies do
                  {:ok, dependencies} ->
                    with {:ok, scales} <-
                           CssMathValidator.capture_unit_scales(dependencies.units, style),
                         do:
                           math_environment(Map.put(style, :_css_unit_scales, scales), remaining)

                  _ ->
                    {:ok, %{}}
                end

              with {:ok, environment} <- prepared do
                case CssMathValidator.evaluate(source, environment) do
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
