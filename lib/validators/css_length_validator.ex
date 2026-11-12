defmodule NativeElixirPdfUtilities.Validators.CssLengthValidator do
  @moduledoc "Shared computed lengths with selected metrics and deferred containing-width percentages."

  alias NativeElixirPdfUtilities.Validators.{
    CssMathValidator,
    CssPositionValidator,
    HtmlValidator
  }

  @type policy :: :length | :signed_length | :padding | :margin
  @type calculation ::
          {:css_length_math,
           {String.t() | CssMathValidator.length_calculation(), boolean(), policy()}}

  @doc "Conservatively identifies declarations that can leave legacy root-unit tokens in computed styles."
  @spec requires_root_units?([term()]) :: boolean()
  def requires_root_units?(declarations) do
    Enum.any?(declarations, fn declaration ->
      {_property, value} =
        NativeElixirPdfUtilities.Validators.CssValueValidator.declaration_pair(declaration)

      # Functions and escapes can supply units indirectly. This is only a fast
      # dispatch decision; the owning value grammar still validates every value.
      String.contains?(value, ["(", "\\"]) or Regex.match?(~r/rem/i, value)
    end)
  end

  @doc "Prepares a length using the shared numeric grammar and the consuming property's range."
  @spec prepare(String.t(), policy()) :: {:ok, term()} | :error | {:error, {atom(), map()}}
  def prepare(value, policy) do
    literal =
      if policy == :margin,
        do: HtmlValidator.parse_css_margin(value),
        else:
          HtmlValidator.parse_css_length(
            value,
            if(policy == :signed_length, do: :margin, else: :nonnegative)
          )

    case literal do
      :error ->
        with {:ok, dependencies} <- CssMathValidator.length_dependencies(value),
             false <- dependencies.percentage? and policy in [:length, :signed_length],
             false <-
               policy not in [:margin, :signed_length] and
                 match?({n, _} when n < 0, HtmlValidator.parse_css_number(String.trim(value))) do
          {:ok, {:css_length_math, {value, dependencies.percentage?, policy}}}
        else
          {:error, _} = error -> error
          _ -> :error
        end

      result ->
        result
    end
  end

  @doc "Resolves a validated length against real selected-font metrics and its percentage basis."
  @spec resolve(term(), number() | nil, map()) ::
          {:ok, term()} | :error | {:error, {atom(), map()}}
  def resolve(value, basis, style) do
    case value do
      {:css_length_math, {source, percentage?, policy}} ->
        if percentage? and basis == nil do
          case source do
            source when is_binary(source) ->
              with {:ok, calculation} <- CssMathValidator.compute_length(source, style),
                   do: {:ok, {:css_length_math, {calculation, percentage?, policy}}}

            _ ->
              {:ok, value}
          end
        else
          with {:ok, value} <-
                 CssPositionValidator.resolve({:css_position_math, source}, basis || 0.0, style),
               do: {:ok, if(policy in [:length, :padding], do: max(value, 0.0), else: value)}
        end

      {:rem, ratio} ->
        {:ok, ratio * style._root_font_size}

      _ ->
        {:ok, value}
    end
  end

  @doc "Resolves computed box edges, retaining percentages until the containing width is definite."
  @spec resolve_edges(map(), number() | nil) :: {:ok, map()} | :error | {:error, {atom(), map()}}
  def resolve_edges(style, basis \\ nil) do
    if plain_edges?(Map.get(style, :margin)) and plain_edges?(Map.get(style, :padding)) and
         plain_edges?(Map.get(style, :border_widths)),
       do: {:ok, style},
       else: resolve_computed_edges(style, basis)
  end

  defp plain_edges?(edges, allow_auto? \\ true, minimum \\ nil, maximum \\ nil) do
    case edges do
      nil ->
        true

      %{top: top, right: right, bottom: bottom, left: left}
      when map_size(edges) == 4 and (is_number(top) or (allow_auto? and top == :auto)) and
             (is_number(right) or (allow_auto? and right == :auto)) and
             (is_number(bottom) or (allow_auto? and bottom == :auto)) and
             (is_number(left) or (allow_auto? and left == :auto)) and
             (minimum == nil or
                (top >= minimum and right >= minimum and bottom >= minimum and left >= minimum)) and
             (maximum == nil or
                (top <= maximum and right <= maximum and bottom <= maximum and left <= maximum)) ->
        true

      _ ->
        false
    end
  end

  defp resolve_computed_edges(style, basis) do
    result =
      Enum.reduce_while([:margin, :padding, :border_widths], {:ok, style}, fn property,
                                                                              {:ok, result} ->
        values = Map.get(result, property, %{})

        case Enum.reduce_while(values, {:ok, values}, fn {edge, value}, {:ok, values} ->
               case value do
                 {:css_length_math, _} ->
                   case resolve(value, basis, Map.get(style, :calculation_context, style)) do
                     {:ok, resolved} -> {:cont, {:ok, Map.put(values, edge, resolved)}}
                     error -> {:halt, error}
                   end

                 _ ->
                   {:cont, {:ok, values}}
               end
             end) do
          {:ok, ^values} -> {:cont, {:ok, result}}
          {:ok, values} -> {:cont, {:ok, Map.put(result, property, values)}}
          error -> {:halt, error}
        end
      end)

    with {:ok, result} <- result do
      result =
        if match?({:css_length_math, _}, Map.get(result, :margin_after)),
          do: Map.put(result, :margin_after, result.margin.bottom),
          else: result

      deferred? =
        Enum.any?([:margin, :padding], fn property ->
          Enum.any?(Map.get(result, property, %{}), fn {_, value} ->
            match?({:css_length_math, _}, value)
          end)
        end)

      {:ok,
       if(deferred?,
         do: Map.put(result, :_calculated_edges, true),
         else: Map.delete(result, :_calculated_edges)
       )}
    end
  end

  @doc "Computes text lengths after selecting the element font, retaining parent line-height for lh cycles."
  @spec resolve_text(map()) :: {:ok, map()} | :error | {:error, {atom(), map()}}
  def resolve_text(style) do
    style =
      if Map.get(style, :_root_element, false),
        do:
          style
          |> Map.put(:_root_font_size, style.font_size)
          |> Map.put(:_root_font_face, Map.get(style, :font_face)),
        else: style

    multiplier = Map.get(style, :line_height_multiplier)

    preliminary =
      case multiplier do
        {:css_scalar_math, _} ->
          with {:ok, value} <- CssMathValidator.resolve_scalar(multiplier, style),
               do: {:ok, Map.put(style, :line_height_multiplier, value)}

        _ ->
          {:ok, style}
      end

    with {:ok, style} <- preliminary do
      result =
        case Map.get(style, :line_height) do
          {:css_length_math, _} = calculation ->
            parent_height = Map.fetch!(style, :_line_height_parent)
            context = Map.put(style, :line_height, parent_height)

            context =
              if Map.get(style, :_line_height_root, false),
                do: Map.put(context, :_root_line_height, parent_height),
                else: context

            with {:ok, height} <- resolve(calculation, style.font_size, context),
                 do:
                   {:ok,
                    style
                    |> Map.put(:line_height, height)
                    |> Map.drop([:_line_height_parent, :_line_height_root])}

          _ ->
            {:ok, style}
        end

      with {:ok, style} <- result do
        style =
          if Map.get(style, :_root_element, false),
            do: Map.put(style, :_root_line_height, computed_line_height(style)),
            else: style

        case Map.get(style, :letter_spacing) do
          {:css_length_math, _} = calculation ->
            with {:ok, spacing} <- resolve(calculation, 0.0, style),
                 do: {:ok, Map.put(style, :letter_spacing, spacing)}

          _ ->
            {:ok, style}
        end
      end
    end
  end

  @doc "Returns the computed CSS line height used by font-unit calculations, before native control restrictions."
  @spec computed_line_height(map()) :: term()
  def computed_line_height(style) do
    cond do
      Map.has_key?(style, :_css_line_height) ->
        style._css_line_height

      is_number(Map.get(style, :line_height_multiplier)) and is_number(Map.get(style, :font_size)) ->
        style.font_size * style.line_height_multiplier

      Map.get(style, :line_height_normal, false) and is_map(Map.get(style, :font_face)) and
          is_number(Map.get(style, :font_size)) ->
        HtmlValidator.normal_line_height(style)

      match?({:rem, _}, Map.get(style, :line_height)) ->
        elem(style.line_height, 1) * style._root_font_size

      true ->
        Map.get(style, :line_height)
    end
  end

  @doc "Checks a prepared edge through its owning grammar, including its percentage dependency."
  @spec valid_edge?(term(), :margin | :padding | :border_widths) :: boolean()
  def valid_edge?(value, property) do
    case value do
      {:css_length_math, {source, _, policy}} ->
        expected =
          case property do
            :margin -> :margin
            :padding -> :padding
            :border_widths -> :length
          end

        policy == expected and
          case source do
            {:css_calculation, _} ->
              case CssMathValidator.validate_prepared_length(source) do
                {:ok, dependencies} ->
                  dependencies.percentage? == elem(elem(value, 1), 1) and
                    not (dependencies.percentage? and policy == :length)

                _ ->
                  false
              end

            _ ->
              prepare(source, policy) == {:ok, value}
          end

      value when is_number(value) ->
        match?({:ok, _}, HtmlValidator.normalize_geometry_number(value)) and
          (property == :margin or value >= 0)

      _ ->
        false
    end
  end

  @doc "Checks computed box calculations before layout consumes their containing-width dependencies."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    maximum = NativeElixirPdfUtilities.Limits.get(:max_numeric_magnitude)

    plain? =
      plain_edges?(Map.get(style, :margin), false, -maximum, maximum) and
        plain_edges?(Map.get(style, :padding), false, 0, maximum) and
        plain_edges?(Map.get(style, :border_widths), false, 0, maximum)

    valid? =
      if plain? do
        Map.get(style, :_calculated_edges, false) == false
      else
        Map.get(style, :_calculated_edges) == true and
          Enum.all?([:margin, :padding, :border_widths], fn property ->
            case Map.get(style, property) do
              nil ->
                true

              %{top: _, right: _, bottom: _, left: _} = edges when map_size(edges) == 4 ->
                Enum.all?(edges, fn {_, value} -> valid_edge?(value, property) end)

              _ ->
                false
            end
          end)
      end

    if valid? and not Map.has_key?(style, :_percentage_edge_children),
      do: :ok,
      else:
        NativeElixirPdfUtilities.Diagnostics.error(
          :layout,
          :invalid_layout,
          "computed box edges require bounded numeric values or validated length calculations and their containing-width dependency flag; recompute styles",
          operation: :layout,
          module: __MODULE__,
          source: source
        )
  end
end
