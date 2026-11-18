defmodule NativeElixirPdfUtilities.Validators.CssBorderShapeValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{
    CssClipValidator,
    CssLengthValidator,
    CssLogicalValidator,
    HtmlValidator
  }

  @sides ~w(top right bottom left)
  @logical_sides ~w(block-start inline-start block-end inline-end)
  @corners ~w(top-left top-right bottom-right bottom-left)
  @logical_corners ~w(start-start start-end end-end end-start)
  @side_fields Map.new(@logical_sides, &{&1, String.to_atom(String.replace(&1, "-", "_"))})
  @clip_fields Map.new(@sides, &{&1, String.to_atom("border_" <> &1 <> "_clip")})
  @corner_fields Map.new(
                   @corners,
                   &{&1, String.to_atom("corner_" <> String.replace(&1, "-", "_") <> "_shape")}
                 )
  @limits Map.new(~w(sides corners) ++ @sides, &{&1, String.to_atom(&1)})
  @longhands ~w(border-shape border-limit) ++
               Enum.map(@sides ++ @logical_sides, &"border-#{&1}-clip") ++
               Enum.map(@corners ++ @logical_corners, &"corner-#{&1}-shape")
  @shorthands %{
    "border-clip" => Enum.map(@sides, &"border-#{&1}-clip"),
    "border-inline-clip" => ~w(border-inline-start-clip border-inline-end-clip),
    "border-block-clip" => ~w(border-block-start-clip border-block-end-clip),
    "corner-shape" => Enum.map(@corners, &"corner-#{&1}-shape")
  }
  @corner_params %{
    "round" => 1.0,
    "bevel" => 0.0,
    "scoop" => -1.0,
    "squircle" => 2.0,
    "square" => :square,
    "notch" => :notch
  }
  @border_boxes ~w(border-box half-border-box padding-box content-box margin-box)

  @doc false
  @spec longhands() :: [String.t()]
  def longhands, do: @longhands

  @doc false
  @spec shorthands() :: map()
  def shorthands, do: @shorthands

  @doc false
  @spec properties() :: [String.t()]
  def properties, do: @longhands ++ Map.keys(@shorthands)

  @doc false
  @spec prepare(map(), String.t(), String.t()) :: {:ok, map()} | :error | {:error, term()}
  def prepare(style, property, value) do
    tokens = HtmlValidator.split_css_tokens(value)

    case property do
      "border-shape" ->
        case tokens do
          ["none"] ->
            {:ok, %{border_shape: nil}}

          _ ->
            with {:ok, shapes} <- shapes(tokens, style), do: {:ok, %{border_shape: shapes}}
        end

      "border-limit" ->
        case tokens do
          ["all"] ->
            {:ok, %{border_limit: nil}}

          [kind] when kind in ["sides", "corners"] ->
            {:ok, %{border_limit: {Map.fetch!(@limits, kind), nil}}}

          [kind, value] when kind in ["sides", "corners", "top", "right", "bottom", "left"] ->
            with {:ok, value} <- length(value, style),
                 do: {:ok, %{border_limit: {Map.fetch!(@limits, kind), value}}}

          _ ->
            :error
        end

      "corner-shape" ->
        case corners(tokens) do
          {:ok, values} ->
            {:ok,
             Map.new(Enum.zip(@corners, values), fn {name, parameter} ->
               {corner_key(name), parameter}
             end)}

          :error ->
            :error
        end

      "corner-" <> _ ->
        name =
          property |> String.replace_prefix("corner-", "") |> String.replace_suffix("-shape", "")

        with [value] <- tokens,
             {:ok, parameter} <- corner(value),
             do: {:ok, %{corner_key(physical_corner(name, style)) => parameter}},
             else: (_ -> :error)

      "border-clip" ->
        with {:ok, list} <- clips(tokens, style),
             do: {:ok, Map.new(@sides, &{clip_key(&1), list})}

      "border-" <> _ ->
        side =
          property |> String.replace_prefix("border-", "") |> String.replace_suffix("-clip", "")

        with {:ok, list} <- clips(tokens, style) do
          sides =
            case side do
              side when side in ["block", "inline"] ->
                Enum.map(["-start", "-end"], &physical_side(side <> &1, style))

              _ ->
                [physical_side(side, style)]
            end

          {:ok, Map.new(sides, &{clip_key(&1), list})}
        end
    end
  end

  defp shapes(tokens, style) do
    grouped =
      Enum.chunk_while(
        tokens,
        [],
        fn token, acc ->
          case {token in @border_boxes, acc} do
            {true, [shape]} -> {:cont, {shape, token}, []}
            {true, []} -> {:cont, :invalid, []}
            {false, []} -> {:cont, [token]}
            {false, [shape]} -> {:cont, {shape, nil}, [token]}
          end
        end,
        fn
          [] -> {:cont, []}
          [shape] -> {:cont, {shape, nil}, []}
        end
      )

    refs = if length(grouped) == 1, do: [:half_border_box], else: [:border_box, :padding_box]

    case grouped do
      grouped when length(grouped) in 1..2 ->
        Enum.zip(grouped, refs)
        |> Enum.reduce_while({:ok, []}, fn
          {{source, box}, fallback}, {:ok, shapes} ->
            text = if box not in [nil, "half-border-box"], do: source <> " " <> box, else: source

            case CssClipValidator.prepare(style, text) do
              {:ok, %{css_clip_path: %{type: type} = shape}} when type != :box ->
                ref =
                  case box do
                    nil -> fallback
                    "half-border-box" -> :half_border_box
                    _ -> shape.reference
                  end

                {:cont, {:ok, [%{shape | reference: ref} | shapes]}}

              _ ->
                {:halt, :error}
            end

          _, _ ->
            {:halt, :error}
        end)
        |> case do
          {:ok, shapes} -> {:ok, Enum.reverse(shapes)}
          error -> error
        end

      _ ->
        :error
    end
  end

  defp corner_key(name), do: Map.fetch!(@corner_fields, name)
  defp clip_key(name), do: Map.fetch!(@clip_fields, name)

  defp physical_side(side, style) do
    if side in @logical_sides do
      axes = CssLogicalValidator.axes(style)
      side = Map.fetch!(@side_fields, side)
      Atom.to_string(Map.fetch!(axes, side))
    else
      side
    end
  end

  defp physical_corner(name, style) do
    if name in @logical_corners do
      [first, last] = String.split(name, "-")
      a = physical_side("block-" <> first, style)
      b = physical_side("inline-" <> last, style)
      if a in ["top", "bottom"], do: a <> "-" <> b, else: b <> "-" <> a
    else
      name
    end
  end

  defp corners(values) do
    values = Enum.map(values, &corner/1)

    if Enum.all?(values, &match?({:ok, _}, &1)) do
      case Enum.map(values, &elem(&1, 1)) do
        [a] -> {:ok, [a, a, a, a]}
        [a, b] -> {:ok, [a, b, a, b]}
        [a, b, c] -> {:ok, [a, b, c, b]}
        [a, b, c, d] -> {:ok, [a, b, c, d]}
        _ -> :error
      end
    else
      :error
    end
  end

  defp corner(value) do
    case Map.fetch(@corner_params, value) do
      {:ok, _} = result ->
        result

      :error ->
        case Regex.run(~r/\Asuperellipse\(\s*(.+?)\s*\)\z/i, value) do
          [_, "infinity"] ->
            {:ok, :square}

          [_, "-infinity"] ->
            {:ok, :notch}

          [_, value] ->
            case HtmlValidator.parse_css_number(value) do
              {number, ""} -> {:ok, number}
              _ -> :error
            end

          _ ->
            :error
        end
    end
  end

  defp clips(tokens, style) do
    case tokens do
      ["none"] ->
        {:ok, nil}

      [] ->
        :error

      tokens ->
        Enum.reduce_while(tokens, {:ok, []}, fn token, {:ok, result} ->
          parsed =
            case HtmlValidator.parse_css_number(token) do
              {number, "fr"} when number >= 0 -> {:ok, {:fraction, number}}
              _ -> length(token, style)
            end

          case parsed do
            {:ok, value} -> {:cont, {:ok, [value | result]}}
            error -> {:halt, error}
          end
        end)
        |> case do
          {:ok, result} -> {:ok, Enum.reverse(result)}
          error -> error
        end
    end
  end

  defp length(value, style) do
    case HtmlValidator.parse_css_number(value) do
      {n, "%"} when n >= 0 ->
        {:ok, {:percent, n / 100}}

      _ ->
        with {:ok, length} <- CssLengthValidator.prepare(value, :padding),
             do: CssLengthValidator.resolve(length, nil, style)
    end
  end
end
