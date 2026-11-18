defmodule NativeElixirPdfUtilities.Validators.CssObjectViewBoxValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{
    CssLengthValidator,
    CssPositionValidator,
    HtmlValidator
  }

  @doc "Prepares the rectangular view box for a replaced element."
  @spec prepare(String.t()) :: {:ok, term()} | :error | {:error, term()}
  def prepare(source) do
    case HtmlValidator.parse_css_identifier(String.trim(source)) do
      {:ok, keyword, ""} ->
        if String.downcase(keyword, :ascii) == "none", do: {:ok, :none}, else: :error

      {:ok, name, "(" <> rest} ->
        with true <- String.ends_with?(rest, ")"),
             values =
               rest |> binary_part(0, byte_size(rest) - 1) |> HtmlValidator.split_css_tokens() do
          case {String.downcase(name, :ascii), values} do
            {"inset", values} ->
              with {:ok, offsets} <-
                     HtmlValidator.parse_css_box_lengths(
                       Enum.join(values, " "),
                       {:computed, :margin}
                     ),
                   false <- Enum.any?(offsets, fn {_, value} -> value == :auto end) do
                {:ok, {:inset, [offsets.top, offsets.right, offsets.bottom, offsets.left]}}
              else
                _ -> :error
              end

            {"rect", [_, _, _, _]} ->
              with {:ok, sides} <- lengths(values, [:margin, :margin, :margin, :margin]),
                   do: {:ok, {:rect, sides}}

            {"xywh", [_, _, _, _]} ->
              with {:ok, values} <- lengths(values, [:margin, :margin, :padding, :padding]),
                   false <- :auto in values,
                   do: {:ok, {:xywh, values}},
                   else: (error -> if(error == true, do: :error, else: error))

            _ ->
              :error
          end
        else
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp lengths(values, policies) do
    Enum.zip(values, policies)
    |> Enum.reduce_while({:ok, []}, fn {source, policy}, {:ok, acc} ->
      case CssLengthValidator.prepare(source, policy) do
        {:ok, value} -> {:cont, {:ok, [value | acc]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      error -> error
    end
  end

  @doc "Checks object-view-box supplied through a prepared style map."
  @spec valid_style?(map()) :: boolean()
  def valid_style?(style) do
    case Map.get(style, :object_view_box, :none) do
      :none ->
        true

      {kind, [a, b, c, d]} when kind in [:inset, :rect, :xywh] ->
        leading = [a, b]
        trailing = [c, d]
        policy = if kind == :xywh, do: :padding, else: :margin

        Enum.all?(leading, &valid_coordinate?(&1, :margin, kind == :rect)) and
          Enum.all?(trailing, &valid_coordinate?(&1, policy, kind == :rect))

      _ ->
        false
    end
  end

  defp valid_coordinate?(value, policy, auto?) do
    case value do
      :auto ->
        auto?

      {unit, number} when unit in [:percent, :rem] ->
        match?({:ok, _}, HtmlValidator.normalize_geometry_number(number)) and
          (policy == :margin or number >= 0)

      _ ->
        CssLengthValidator.valid_edge?(value, policy)
    end
  end

  @doc "Resolves the view box in oriented natural-image points."
  @spec resolve(term(), number(), number(), map()) :: {:ok, tuple()} | {:error, term()}
  def resolve(value, width, height, style) do
    {name, inputs, bases} =
      case value do
        {:inset, offsets} -> {:inset, offsets, [height, width, height, width]}
        {:rect, bounds} -> {:rect, bounds, [height, width, height, width]}
        {:xywh, bounds} -> {:xywh, bounds, [width, height, width, height]}
      end

    result =
      Enum.zip(inputs, bases)
      |> Enum.with_index()
      |> Enum.reduce_while({:ok, []}, fn {{value, basis}, index}, {:ok, values} ->
        value =
          case value do
            :auto -> if(index in [1, 2], do: basis, else: 0.0)
            {:rem, ratio} -> ratio * Map.get(style, :_root_font_size, 12)
            {:css_length_math, {calculation, _, _}} -> {:css_position_math, calculation}
            value -> value
          end

        case CssPositionValidator.resolve(value, basis, style) do
          {:ok, value} -> {:cont, {:ok, [value | values]}}
          error -> {:halt, error}
        end
      end)

    with {:ok, raw} <- result do
      [a, b, c, d] = Enum.reverse(raw)

      bounds =
        case name do
          :inset -> {d, a, max(width - b - d, 0), max(height - a - c, 0)}
          :rect -> {d, a, max(b - d, 0), max(c - a, 0)}
          :xywh -> {a, b, max(c, 0), max(d, 0)}
        end

      {:ok, bounds}
    end
  end
end
