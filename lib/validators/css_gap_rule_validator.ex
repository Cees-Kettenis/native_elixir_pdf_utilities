defmodule NativeElixirPdfUtilities.Validators.CssGapRuleValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{
    CssColumnsValidator,
    CssLengthValidator,
    HtmlValidator
  }

  @arms ~w(width style color break visibility-items inset-start inset-end)
  @axis_properties for axis <- ~w(row column), key <- @arms, do: axis <> "-rule-" <> key
  @axis_fields Map.new(@axis_properties, fn property ->
                 key = String.replace(property, "-", "_")
                 {key, String.to_atom(key)}
               end)
  @new_axis_properties @axis_properties --
                         ~w(column-rule-width column-rule-style column-rule-color)
  @both_properties for key <- @arms, do: "rule-" <> key
  @shorthand_properties ~w(row-rule rule rule-inset row-rule-inset column-rule-inset) ++
                          @both_properties

  @doc false
  @spec properties() :: [String.t()]
  def properties, do: @new_axis_properties ++ @shorthand_properties ++ ~w(rule-overlap)

  @doc false
  @spec longhands() :: [String.t()]
  def longhands, do: @new_axis_properties ++ ~w(rule-overlap)

  @doc false
  @spec shorthands() :: map()
  def shorthands do
    Map.new(@both_properties, fn "rule-" <> arm = key ->
      {key, ["row-rule-" <> arm, "column-rule-" <> arm]}
    end)
    |> Map.merge(%{
      "row-rule" => ~w(row-rule-width row-rule-style row-rule-color),
      "rule" =>
        ~w(row-rule-width row-rule-style row-rule-color column-rule-width column-rule-style column-rule-color),
      "row-rule-inset" => ~w(row-rule-inset-start row-rule-inset-end),
      "column-rule-inset" => ~w(column-rule-inset-start column-rule-inset-end),
      "rule-inset" =>
        ~w(row-rule-inset-start row-rule-inset-end column-rule-inset-start column-rule-inset-end)
    })
  end

  @doc false
  @spec prepare(map(), String.t(), String.t()) :: {:ok, map()} | :error | {:error, term()}
  def prepare(style, property, value) do
    value = String.trim(value)

    case property do
      "rule-overlap" ->
        keyword(:rule_overlap, value, %{"row-over-column" => :row, "column-over-row" => :column})

      "rule" ->
        with {:ok, column} <- prepare_axis("column", "", value, style),
             {:ok, row} <- prepare_axis("row", "", value, style),
             do: {:ok, Map.merge(column, row)}

      "rule-inset" ->
        with {:ok, column} <- prepare_axis("column", "-inset", value, style),
             {:ok, row} <- prepare_axis("row", "-inset", value, style),
             do: {:ok, Map.merge(column, row)}

      "rule-" <> arm ->
        with {:ok, row} <- prepare_axis("row", "-" <> arm, value, style),
             {:ok, column} <- prepare_axis("column", "-" <> arm, value, style),
             do: {:ok, Map.merge(row, column)}

      axis ->
        [axis, tail] = String.split(axis, "-rule", parts: 2)
        prepare_axis(axis, tail, value, style)
    end
  end

  defp prepare_axis(axis, tail, value, style) do
    key = axis <> "_rule" <> String.replace(tail, "-", "_")

    case tail do
      "-break" ->
        keyword(Map.fetch!(@axis_fields, key), value, %{
          "none" => :none,
          "normal" => :normal,
          "intersection" => :intersection
        })

      "-visibility-items" ->
        keyword(Map.fetch!(@axis_fields, key), value, %{
          "normal" => :normal,
          "all" => :all,
          "around" => :around,
          "between" => :between
        })

      "-inset" ->
        case HtmlValidator.split_css_tokens(value) do
          [both] ->
            prepare_insets(axis, both, both, style)

          [start, end_] ->
            prepare_insets(axis, start, end_, style)

          _ ->
            :error
        end

      suffix when suffix in ["-inset-start", "-inset-end"] ->
        with {:ok, inset} <- inset(value, style),
             do: {:ok, %{Map.fetch!(@axis_fields, key) => inset}}

      _ ->
        with {:ok, properties} <-
               CssColumnsValidator.prepare(
                 "column-rule" <> tail,
                 value,
                 Map.get(style, :color, :current_color)
               ) do
          {:ok,
           if(axis == "column",
             do: properties,
             else:
               Map.new(properties, fn {field, value} ->
                 {"column_" <> name, value} = {Atom.to_string(field), value}
                 {Map.fetch!(@axis_fields, "row_" <> name), value}
               end)
           )}
        end
    end
  end

  defp prepare_insets(axis, start, finish, style) do
    with {:ok, start} <- inset(start, style), {:ok, finish} <- inset(finish, style) do
      {:ok,
       %{
         Map.fetch!(@axis_fields, axis <> "_rule_inset_start") => start,
         Map.fetch!(@axis_fields, axis <> "_rule_inset_end") => finish
       }}
    end
  end

  defp inset(value, style) do
    case HtmlValidator.parse_css_number(value) do
      {n, "%"} ->
        {:ok, {:percent, n / 100}}

      _ ->
        with {:ok, edge} <- CssLengthValidator.prepare(value, :margin),
             false <- edge == :auto,
             do: CssLengthValidator.resolve(edge, nil, style),
             else: (
               true -> :error
               error -> error
             )
    end
  end

  defp keyword(key, source, choices) do
    with {:ok, value} <- Map.fetch(choices, String.downcase(source, :ascii)),
         do: {:ok, %{key => value}}
  end
end
