defmodule NativeElixirPdfUtilities.Validators.CssColumnsValidator do
  @moduledoc "Owns multicolumn declarations and computed column geometry."
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    CssLengthValidator,
    CssMathValidator,
    CssValueValidator,
    HtmlValidator,
    WriterValidator
  }

  @doc "Prepares columns, column rules and their longhands through shared length and border grammars."
  @spec prepare(String.t(), String.t()) :: {:ok, map()} | :error | {:error, term()}
  @spec prepare(String.t(), String.t(), tuple()) :: {:ok, map()} | :error | {:error, term()}
  def prepare(property, value, color \\ {0, 0, 0}) do
    value = value |> String.trim() |> String.downcase(:ascii)

    case property do
      "columns" ->
        tokens = HtmlValidator.split_css_tokens(value)

        Enum.reduce_while(
          tokens,
          {:ok, %{column_count: :auto, column_width: :auto}, MapSet.new()},
          fn token, {:ok, fields, seen} ->
            result =
              if token == "auto" do
                {:ok, %{}}
              else
                case prepare("column-count", token) do
                  {:ok, fields} -> {:ok, fields}
                  {:error, _} = error -> error
                  _ -> prepare("column-width", token)
                end
              end

            case result do
              {:ok, next} ->
                keys = MapSet.new(Map.keys(next))

                if MapSet.disjoint?(seen, keys),
                  do: {:cont, {:ok, Map.merge(fields, next), MapSet.union(seen, keys)}},
                  else: {:halt, :error}

              error ->
                {:halt, error}
            end
          end
        )
        |> case do
          {:ok, fields, _} when length(tokens) in 1..2 -> {:ok, fields}
          {:error, _} = error -> error
          _ -> :error
        end

      "column-count" ->
        if value == "auto" do
          {:ok, %{column_count: :auto}}
        else
          with {:ok, count} <- CssMathValidator.prepare_scalar(value, :integer, 1),
               do: {:ok, %{column_count: count}}
        end

      "column-width" ->
        if value == "auto" do
          {:ok, %{column_width: :auto}}
        else
          with {:ok, width} <- CssLengthValidator.prepare(value, :length),
               do: {:ok, %{column_width: width}}
        end

      "column-fill" ->
        keyword(value, :column_fill, %{
          "auto" => :auto,
          "balance" => :balance,
          "balance-all" => :balance_all
        })

      "column-span" ->
        keyword(value, :column_span, %{"none" => :none, "all" => :all})

      rule
      when rule in ["column-rule", "column-rule-width", "column-rule-style", "column-rule-color"] ->
        border = String.replace_prefix(rule, "column-rule", "border-left")

        with {:ok, parsed} <-
               CssValueValidator.validate_resolved(
                 %{
                   color: color,
                   border_widths: %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0},
                   border_colors: %{top: nil, right: nil, bottom: nil, left: nil},
                   border_styles: %{top: :none, right: :none, bottom: :none, left: :none}
                 },
                 border,
                 value
               ) do
          fields = [
            {:column_rule_width, :border_widths},
            {:column_rule_style, :border_styles},
            {:column_rule_color, :border_colors}
          ]

          fields =
            if rule == "column-rule",
              do: fields,
              else:
                Enum.filter(fields, fn {column, _} ->
                  Atom.to_string(column) == String.replace(rule, "-", "_")
                end)

          {:ok,
           Enum.reduce(fields, %{}, fn {column, edge}, fields ->
             Map.put(fields, column, Map.fetch!(parsed, edge).left)
           end)}
        end

      _ ->
        :error
    end
  end

  defp keyword(value, field, values) do
    case Map.fetch(values, String.downcase(value, :ascii)) do
      {:ok, value} -> {:ok, %{field => value}}
      :error -> :error
    end
  end

  @doc "Checks the used dimensions of an internal column fragmentainer."
  @spec validate_fragment_geometry(term(), term()) :: :ok | {:error, term()}
  def validate_fragment_geometry(width, height) do
    if is_number(width) and width > 0 and is_number(height) and height > 0 do
      HtmlValidator.check_render_resource(:max_numeric_magnitude, max(width, height), :columns)
      :ok
    else
      Diagnostics.error(
        :layout,
        :invalid_layout,
        "column fragmentainers require positive finite dimensions",
        operation: :layout,
        module: __MODULE__
      )
    end
  end

  @doc "Computes selected-font units while preserving automatic column selection."
  @spec compute_style(map()) :: {:ok, map()} | :error | {:error, term()}
  def compute_style(style) do
    case style do
      style
      when not is_map_key(style, :column_width) and not is_map_key(style, :column_rule_width) ->
        {:ok, style}

      _ ->
        Enum.reduce_while([:column_width, :column_rule_width], {:ok, style}, fn key,
                                                                                {:ok, style} ->
          case Map.get(style, key) do
            nil ->
              {:cont, {:ok, style}}

            :auto ->
              {:cont, {:ok, style}}

            value ->
              case CssLengthValidator.resolve(value, nil, style) do
                {:ok, value} -> {:cont, {:ok, Map.put(style, key, value)}}
                error -> {:halt, error}
              end
          end
        end)
    end
  end

  @doc "Validates column settings and charges explicit counts to the shared entry budget."
  @spec validate_style(map(), String.t()) :: :ok | {:error, term()}
  def validate_style(style, source) do
    case style do
      style
      when not is_map_key(style, :column_count) and not is_map_key(style, :column_width) and
             not is_map_key(style, :column_fill) and not is_map_key(style, :column_span) and
             not is_map_key(style, :column_rule_width) and
             not is_map_key(style, :column_rule_style) and
             not is_map_key(style, :column_rule_color) ->
        :ok

      _ ->
        HtmlValidator.with_render_budget(fn ->
          count = Map.get(style, :column_count, :auto)
          width = Map.get(style, :column_width, :auto)

          valid? =
            (count == :auto or (is_integer(count) and count > 0)) and
              (width == :auto or (is_number(width) and width >= 0)) and
              Map.get(style, :column_fill, :balance) in [:auto, :balance, :balance_all] and
              Map.get(style, :column_span, :none) in [:none, :all] and
              CssLengthValidator.valid_edge?(Map.get(style, :column_rule_width, 0.0), :padding) and
              Map.get(style, :column_rule_style, :none) in [
                :none,
                :hidden,
                :solid,
                :dashed,
                :dotted,
                :double,
                :groove,
                :ridge,
                :inset,
                :outset
              ] and
              (Map.get(style, :column_rule_color, :current_color) == :current_color or
                 WriterValidator.valid_color?(
                   Map.get(style, :column_rule_color, Map.get(style, :color, {0, 0, 0}))
                 ))

          if valid? do
            if is_integer(count),
              do: HtmlValidator.reserve_render_resource(:max_entries, count, :columns)

            :ok
          else
            Diagnostics.error(
              :layout,
              :invalid_layout,
              "columns require a positive count or width, a valid fill/span policy and a valid column rule",
              operation: :layout,
              module: __MODULE__,
              source: source
            )
          end
        end)
    end
  end
end
