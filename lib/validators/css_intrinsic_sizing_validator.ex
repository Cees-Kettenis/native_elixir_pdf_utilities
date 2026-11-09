defmodule NativeElixirPdfUtilities.Validators.CssIntrinsicSizingValidator do
  @moduledoc "Validated intrinsic sizing expressions shared by block and replaced layout."
  alias NativeElixirPdfUtilities.Validators.{
    CssMathValidator,
    CssPositionValidator,
    CssSizingValidator,
    HtmlValidator
  }

  @type size :: {:css_intrinsic_size, term()}

  @doc "Prepares intrinsic keywords, fit-content limits and bounded calc-size arithmetic."
  @spec prepare(String.t()) :: {:ok, size()} | :error | {:error, {atom(), map()}}
  def prepare(value) do
    HtmlValidator.with_render_budget(fn -> prepare_at_depth(String.trim(value), 0) end)
  end

  defp prepare_at_depth(value, depth) do
    HtmlValidator.check_render_resource(:max_css_calculation_depth, depth, :style)

    case HtmlValidator.parse_css_identifier(value) do
      {:ok, name, ""} ->
        case String.downcase(name, :ascii) do
          "min-content" -> {:ok, {:css_intrinsic_size, :min_content}}
          "max-content" -> {:ok, {:css_intrinsic_size, :max_content}}
          "fit-content" -> {:ok, {:css_intrinsic_size, :fit_content}}
          _ -> :error
        end

      {:ok, name, "(" <> rest} ->
        arguments =
          if String.ends_with?(rest, ")"),
            do: HtmlValidator.split_css_comma(binary_part(rest, 0, byte_size(rest) - 1), true),
            else: []

        case {String.downcase(name, :ascii), arguments} do
          {"fit-content", [limit]} ->
            parsed =
              case HtmlValidator.parse_css_length(limit, :nonnegative) do
                {:ok, {:rem, _}} -> CssSizingValidator.prepare_math(limit)
                :error -> CssSizingValidator.prepare_math(limit)
                result -> result
              end

            with {:ok, calculation} <- parsed,
                 do: {:ok, {:css_intrinsic_size, {:fit_content, calculation}}}

          {"calc-size", [basis, calculation]} ->
            prepared =
              case String.downcase(String.trim(basis), :ascii) do
                "any" ->
                  {:ok, :any}

                "auto" ->
                  {:ok, :auto}

                _ ->
                  case prepare_at_depth(String.trim(basis), depth + 1) do
                    :error -> CssSizingValidator.prepare_math("calc(" <> basis <> ")")
                    result -> result
                  end
              end

            with {:ok, basis} <- prepared,
                 {:ok, _} <-
                   CssMathValidator.length_dependencies("calc(" <> calculation <> ")", %{
                     _intrinsic_basis: if(basis == :any, do: nil, else: 1.0)
                   }),
                 do: {:ok, {:css_intrinsic_size, {:calculation, basis, calculation}}}

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  @doc "Captures font and viewport units once, retaining intrinsic and percentage bases for layout."
  @spec compute(size(), map()) :: {:ok, size()} | :error | {:error, {atom(), map()}}
  def compute(value, style) do
    case value do
      {:css_intrinsic_size, {:computed, _, _}} ->
        {:ok, value}

      _ ->
        units = referenced_units(value)

        if units == [] do
          {:ok, value}
        else
          with {:ok, scales} <- CssMathValidator.capture_unit_scales(units, style),
               do: {:ok, {:css_intrinsic_size, {:computed, value, scales}}}
        end
    end
  end

  defp referenced_units(value) do
    case value do
      {:css_intrinsic_size, {:fit_content, limit}} ->
        referenced_units(limit)

      {:css_intrinsic_size, {:calculation, basis, sum}} ->
        {:ok, dependencies} =
          CssMathValidator.length_dependencies(
            "calc(" <> sum <> ")",
            %{_intrinsic_basis: if(basis == :any, do: nil, else: 1.0)}
          )

        Enum.uniq(referenced_units(basis) ++ dependencies.units)

      {:css_size_math, {source, _}} when is_binary(source) ->
        {:ok, dependencies} = CssMathValidator.length_dependencies(source)
        dependencies.units

      _ ->
        []
    end
  end

  @doc "Checks intrinsic sizing metadata by reconstructing its owning source grammar."
  @spec valid?(term()) :: boolean()
  def valid?(value) do
    valid_at_depth?(value, 0)
  end

  defp valid_at_depth?(value, depth) do
    HtmlValidator.check_render_resource(:max_css_calculation_depth, depth, :layout)

    case value do
      {:css_intrinsic_size, {:computed, original, scales}} ->
        not match?({:css_intrinsic_size, {:computed, _, _}}, original) and
          valid_at_depth?(original, depth + 1) and CssMathValidator.valid_unit_scales?(scales) and
          Enum.sort(Map.keys(scales)) == Enum.sort(referenced_units(original))

      {:css_intrinsic_size, keyword} when keyword in [:min_content, :max_content, :fit_content] ->
        true

      {:css_intrinsic_size, {:fit_content, limit}} ->
        HtmlValidator.validate_prepared_css_size(limit, :size) == :ok and
          not match?({:css_intrinsic_size, _}, limit)

      {:css_intrinsic_size, {:calculation, basis, calculation}} ->
        valid_basis? =
          basis in [:auto, :any] or valid_at_depth?(basis, depth + 1) or
            CssSizingValidator.valid_calculation?(basis)

        valid_basis? and is_binary(calculation) and
          match?(
            {:ok, _},
            CssMathValidator.length_dependencies("calc(" <> calculation <> ")", %{
              _intrinsic_basis: if(basis == :any, do: nil, else: 1.0)
            })
          )

      _ ->
        false
    end
  end

  @doc "Resolves an intrinsic expression with measured min/max/auto sizes and a definite or indefinite axis."
  @spec resolve(size(), number() | nil, map(), map()) ::
          {:ok, number() | nil} | :error | {:error, {atom(), map()}}
  def resolve(value, available, sizes, style) do
    with {:ok, result} <- resolve_value(value, available, sizes, style),
         do: {:ok, if(is_number(result), do: max(result, 0.0))}
  end

  defp resolve_value({:css_intrinsic_size, value}, available, sizes, style) do
    case value do
      {:computed, original, scales} ->
        context = style |> Map.delete(:calculation_context) |> Map.put(:_css_unit_scales, scales)
        resolve_value(original, available, sizes, context)

      :min_content ->
        {:ok, Map.get(sizes, :min_content)}

      :max_content ->
        {:ok, Map.get(sizes, :max_content)}

      :fit_content ->
        fit_content(Map.get(sizes, :stretch, available), sizes)

      {:fit_content, limit} ->
        resolved =
          case limit do
            number when is_number(number) -> {:ok, number}
            {:percent, ratio} -> {:ok, if(is_number(available), do: ratio * available)}
            {:css_size_math, _} -> CssSizingValidator.resolve_math(limit, available, style)
          end

        with {:ok, limit} <- resolved, do: fit_content(limit, sizes)

      {:calculation, basis, calculation} ->
        result =
          case basis do
            :any ->
              {:ok, 0.0}

            :auto ->
              {:ok, Map.get(sizes, :auto)}

            {:css_intrinsic_size, _} ->
              resolve_value(basis, available, sizes, style)

            {:css_size_math, _} ->
              with {:ok, value} <-
                     CssSizingValidator.resolve_math(basis, available, style, :signed),
                   do: {:ok, value || Map.get(sizes, :auto)}
          end

        with {:ok, basis} <- result do
          if is_number(basis) do
            with {:ok, value} <-
                   CssPositionValidator.resolve(
                     {:css_position_math, "calc(" <> calculation <> ")"},
                     available || 0.0,
                     Map.put(style, :_intrinsic_basis, basis / 0.75)
                   ),
                 do: {:ok, value}
          else
            {:ok, nil}
          end
        end
    end
  end

  defp fit_content(limit, sizes) do
    minimum = Map.get(sizes, :min_content)
    maximum = Map.get(sizes, :max_content)

    {:ok,
     if(is_number(minimum) and is_number(maximum),
       do: max(minimum, min(maximum, limit || maximum))
     )}
  end
end
