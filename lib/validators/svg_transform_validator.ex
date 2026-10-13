defmodule NativeElixirPdfUtilities.Validators.SvgTransformValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  @identity {1.0, 0.0, 0.0, 1.0, 0.0, 0.0}
  @function ~r/([A-Za-z]+)[ \t\r\n]*\(([^()]*)\)/

  @doc false
  @spec parse(term()) :: {:ok, tuple()} | :error
  def parse(value) do
    if is_binary(value) do
      value = String.trim(value)
      functions = Regex.scan(@function, value)
      residue = Regex.replace(@function, value, "T")

      valid? =
        functions != [] and
          Regex.match?(~r/\AT(?:(?:[ \t\r\n]+,?[ \t\r\n]*|,[ \t\r\n]*)T)*\z/, residue) and
          length(functions) <= Limits.get(:max_svg_transform_operations)

      cond do
        value in ["none", ""] ->
          {:ok, @identity}

        not valid? ->
          :error

        true ->
          HtmlValidator.reserve_render_resource(:max_css_work, length(functions), :css)

          Enum.reduce_while(functions, {:ok, @identity}, fn [_, name, args], {:ok, ctm} ->
            args = String.trim(args)

            valid_arguments? =
              not String.starts_with?(args, ",") and not String.ends_with?(args, ",") and
                not Regex.match?(~r/,\s*,/, args)

            values =
              args
              |> String.split(~r/[\s,]+/, trim: true)
              |> Enum.map(fn token ->
                token = Regex.replace(~r/\.(?=[eE]|$)/, token, ".0")
                HtmlValidator.parse_css_number(token)
              end)

            result = if valid_arguments?, do: operation(name, values), else: :error

            case result do
              {:ok, matrix} ->
                combined = multiply(ctm, matrix)

                if Enum.all?(
                     Tuple.to_list(combined),
                     &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
                   ) do
                  {:cont, {:ok, combined}}
                else
                  {:halt, :error}
                end

              _ ->
                {:halt, :error}
            end
          end)
      end
    else
      :error
    end
  end

  @doc false
  @spec multiply(tuple(), tuple()) :: tuple()
  def multiply({a, b, c, d, e, f}, {g, h, i, j, k, l}) do
    {a * g + c * h, b * g + d * h, a * i + c * j, b * i + d * j, a * k + c * l + e,
     b * k + d * l + f}
  end

  @doc "Returns a bounded inverse of a canonical SVG transform, or rejects singular geometry."
  @spec inverse(term()) :: {:ok, tuple()} | :error
  def inverse(matrix) do
    case matrix do
      {a, b, c, d, e, f} ->
        if Enum.all?(
             Tuple.to_list(matrix),
             &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
           ) do
          determinant = a * d - b * c

          if determinant == 0 do
            :error
          else
            inverse =
              {d / determinant, -b / determinant, -c / determinant, a / determinant,
               (c * f - d * e) / determinant, (b * e - a * f) / determinant}

            if Enum.all?(
                 Tuple.to_list(inverse),
                 &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
               ), do: {:ok, inverse}, else: :error
          end
        else
          :error
        end

      _ ->
        :error
    end
  rescue
    ArithmeticError -> :error
  end

  defp operation(name, values) do
    case {name, values} do
      {"matrix", [{a, ""}, {b, ""}, {c, ""}, {d, ""}, {e, ""}, {f, ""}]} ->
        {:ok, {a, b, c, d, e, f}}

      {"translate", [{x, ""}]} ->
        {:ok, {1.0, 0.0, 0.0, 1.0, x, 0.0}}

      {"translate", [{x, ""}, {y, ""}]} ->
        {:ok, {1.0, 0.0, 0.0, 1.0, x, y}}

      {"scale", [{x, ""}]} ->
        {:ok, {x, 0.0, 0.0, x, 0.0, 0.0}}

      {"scale", [{x, ""}, {y, ""}]} ->
        {:ok, {x, 0.0, 0.0, y, 0.0, 0.0}}

      {"rotate", [{angle, unit} | centre]} when unit in ["", "deg", "rad", "grad", "turn"] ->
        angle = radians(angle, unit)

        rotation =
          {:math.cos(angle), :math.sin(angle), -:math.sin(angle), :math.cos(angle), 0.0, 0.0}

        case centre do
          [] ->
            {:ok, rotation}

          [{x, ""}, {y, ""}] ->
            {:ok,
             multiply(
               multiply({1.0, 0.0, 0.0, 1.0, x, y}, rotation),
               {1.0, 0.0, 0.0, 1.0, -x, -y}
             )}

          _ ->
            :error
        end

      {name, [{angle, unit}]}
      when name in ~w(skewX skewY) and unit in ["", "deg", "rad", "grad", "turn"] ->
        tangent = :math.tan(radians(angle, unit))

        {:ok,
         if(name == "skewX",
           do: {1.0, 0.0, tangent, 1.0, 0.0, 0.0},
           else: {1.0, tangent, 0.0, 1.0, 0.0, 0.0}
         )}

      _ ->
        :error
    end
  end

  defp radians(angle, unit) do
    case unit do
      "rad" -> angle
      "grad" -> angle * :math.pi() / 200
      "turn" -> angle * 2 * :math.pi()
      _ -> angle * :math.pi() / 180
    end
  end
end
