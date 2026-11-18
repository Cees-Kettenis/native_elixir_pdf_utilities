defmodule NativeElixirPdfUtilities.Validators.CssAdvancedLayoutValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{CssLengthValidator, HtmlValidator}

  @properties ~w(flow-into flow-from region-fragment wrap-flow wrap-through shape-padding line-grid line-snap box-snap line-height-step block-step-size block-step-insert block-step-align block-step-round fit-tolerance)
  @fields Map.new(@properties, &{&1, String.to_atom(String.replace(&1, "-", "_"))})
  @reserved ~w(none auto initial inherit unset revert revert-layer default)
  @keywords %{
    "region-fragment" => ~w(auto break),
    "wrap-flow" => ~w(auto both start end minimum maximum clear),
    "wrap-through" => ~w(wrap none),
    "line-snap" => ~w(none baseline contain),
    "box-snap" => ~w(none block-start block-end center baseline last-baseline),
    "block-step-insert" => ~w(margin-box padding-box content-box),
    "block-step-align" => ~w(auto start center end),
    "block-step-round" => ~w(up down nearest)
  }

  @doc false
  @spec properties() :: [String.t()]
  def properties, do: @properties

  @doc false
  @spec prepare(String.t(), String.t(), map()) :: {:ok, map()} | :error | {:error, term()}
  def prepare(property, value, style) do
    value = String.trim(value)
    key = Map.fetch!(@fields, property)

    case property do
      "flow-into" ->
        case HtmlValidator.split_css_tokens(value) do
          ["none"] -> {:ok, %{flow_into: nil}}
          [name] -> flow_into(name, :element)
          [name, "element"] -> flow_into(name, :element)
          [name, "content"] -> flow_into(name, :content)
          _ -> :error
        end

      "flow-from" ->
        if value == "none", do: {:ok, %{flow_from: nil}}, else: identifier(value, :flow_from)

      "line-grid" ->
        case value do
          "match-parent" -> {:ok, %{line_grid: :match_parent}}
          "create" -> {:ok, %{line_grid: :create}}
          _ -> :error
        end

      property
      when property in ~w(line-height-step block-step-size shape-padding fit-tolerance) ->
        case {property, value} do
          {"block-step-size", "none"} ->
            {:ok, %{key => nil}}

          {"fit-tolerance", "normal"} ->
            {:ok, %{key => nil}}

          {"fit-tolerance", "infinite"} ->
            {:ok, %{key => :infinite}}

          {property, _} when property in ["fit-tolerance", "shape-padding"] ->
            with {:ok, size} <- length_percentage(value, style), do: {:ok, %{key => size}}

          _ ->
            with {:ok, size} <- CssLengthValidator.prepare(value, :length),
                 {:ok, size} <- CssLengthValidator.resolve(size, nil, style) do
              {:ok, %{key => size}}
            end
        end

      _ ->
        value = String.downcase(value, :ascii)

        if value in Map.fetch!(@keywords, property),
          do: {:ok, %{key => String.to_atom(String.replace(value, "-", "_"))}},
          else: :error
    end
  end

  defp length_percentage(value, style) do
    case HtmlValidator.parse_css_number(value) do
      {n, "%"} when n >= 0 ->
        {:ok, {:percent, n / 100}}

      _ ->
        with {:ok, size} <- CssLengthValidator.prepare(value, :padding),
             do: CssLengthValidator.resolve(size, nil, style)
    end
  end

  defp flow_into(name, mode) do
    with {:ok, %{flow_into: name}} <- identifier(name, :flow_into),
         do: {:ok, %{flow_into: {name, mode}}}
  end

  defp identifier(value, key) do
    case HtmlValidator.parse_css_identifier(value) do
      {:ok, name, ""} ->
        if String.downcase(name, :ascii) in @reserved, do: :error, else: {:ok, %{key => name}}

      _ ->
        :error
    end
  end
end
