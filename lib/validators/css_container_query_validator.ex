defmodule NativeElixirPdfUtilities.Validators.CssContainerQueryValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{
    CssLogicalValidator,
    CssSyntaxValidator,
    HtmlValidator,
    PrintMediaValidator
  }

  @types %{"normal" => :normal, "size" => :size, "inline-size" => :inline_size}
  @features ~w(width height inline-size block-size aspect-ratio orientation)
  @excluded_names ~w(none default initial inherit unset revert revert-layer)

  @doc "Parses container names, type and shorthand into canonical cascade fields."
  @spec declaration(String.t(), String.t()) :: {:ok, map()} | :error
  def declaration(property, value) do
    case {property, CssSyntaxValidator.split(value, "/")} do
      {"container-type", [type]} ->
        with {:ok, type} <- container_type(type), do: {:ok, %{container_type: type}}

      {"container-name", [names]} ->
        with {:ok, names} <- container_names(names), do: {:ok, %{container_name: names}}

      {"container", [names]} ->
        with {:ok, names} <- container_names(names),
             do: {:ok, %{container_name: names, container_type: :normal}}

      {"container", [names, type]} ->
        with {:ok, names} <- container_names(names),
             {:ok, type} <- container_type(type),
             do: {:ok, %{container_name: names, container_type: type}}

      _ ->
        :error
    end
  end

  defp container_type(source) do
    with {:ok, value, ""} <- HtmlValidator.parse_css_identifier(String.trim(source)) do
      Map.fetch(@types, String.downcase(value, :ascii))
    else
      _ -> :error
    end
  end

  defp container_names(source) do
    case HtmlValidator.split_css_tokens(source) do
      [single] ->
        case HtmlValidator.parse_css_identifier(single) do
          {:ok, value, ""} ->
            if String.downcase(value, :ascii) == "none", do: {:ok, []}, else: names([single])

          _ ->
            :error
        end

      [] ->
        :error

      tokens ->
        names(tokens)
    end
  end

  defp names(tokens) do
    parsed = Enum.map(tokens, &custom_name/1)
    if :error in parsed, do: :error, else: {:ok, Enum.uniq(parsed)}
  end

  defp custom_name(value) do
    with {:ok, name, ""} <- HtmlValidator.parse_css_identifier(value),
         false <- String.downcase(name, :ascii) in @excluded_names,
         do: name,
         else: (_ -> :error)
  end

  @doc "Prepares an ordered list of alternatives in an authored container condition."
  @spec prepare(String.t()) :: {:ok, [map()]} | :error
  def prepare(source) do
    alternatives =
      source
      |> CssSyntaxValidator.split(",")
      |> Enum.map(fn value ->
        case components(value) do
          [%{kind: :token, text: token} | rest] ->
            if String.downcase(token, :ascii) == "not",
              do: query(nil, components(value)),
              else: query(custom_name(token), rest)

          values ->
            query(nil, values)
        end
      end)

    if alternatives == [] or :error in alternatives, do: :error, else: {:ok, alternatives}
  end

  defp components(source) do
    source
    |> CssSyntaxValidator.components()
    |> Enum.reject(&(&1.kind == :comment or String.trim(&1.text) == ""))
  end

  defp query(name, values) do
    if name == :error or (name == nil and values == []) do
      :error
    else
      case if(values == [], do: :name, else: condition(values, 0)) do
        :error -> :error
        expression -> %{name: name, expression: expression, axes: axes(expression)}
      end
    end
  end

  defp condition(values, depth) do
    HtmlValidator.check_render_resource(:max_depth, depth, :css)

    case values do
      [%{kind: :token, text: operator}, term] ->
        case {String.downcase(operator, :ascii), term(term, depth)} do
          {"not", expression} when expression != :error -> {:not, expression}
          _ -> :error
        end

      [single] ->
        term(single, depth)

      [first, %{kind: :token, text: operator}, next | rest] ->
        operator = String.downcase(operator, :ascii)

        if operator in ["and", "or"] do
          with {:ok, terms} <- joined_terms([next | rest], operator, [first]),
               expressions <- Enum.map(terms, &term(&1, depth)),
               false <- :error in expressions do
            {if(operator == "and", do: :and, else: :or), expressions}
          else
            _ -> :error
          end
        else
          :error
        end

      _ ->
        :error
    end
  end

  defp joined_terms(terms, operator, acc) do
    case terms do
      [last] ->
        {:ok, Enum.reverse([last | acc])}

      [term, %{kind: :token, text: keyword} | rest] ->
        if String.downcase(keyword, :ascii) == operator,
          do: joined_terms(rest, operator, [term | acc]),
          else: :error

      _ ->
        :error
    end
  end

  defp term(value, depth) do
    case value do
      %{kind: :block, opener: "(", closed: true} ->
        content = body(value)

        case condition(components(content), depth + 1) do
          :error -> size_feature(content)
          nested -> nested
        end

      %{kind: :block, opener: opener, closed: true} ->
        case HtmlValidator.parse_css_identifier(opener) do
          {:ok, name, "("} ->
            case String.downcase(name, :ascii) do
              "style" -> style_condition(body(value), depth + 1)
              _ -> :unknown
            end

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp body(%{text: text, opener: opener, length: length}) do
    offset = byte_size(opener)
    text |> binary_part(offset, length - offset - 1) |> String.trim()
  end

  defp size_feature(source) do
    features =
      source
      |> String.downcase(:ascii)
      |> then(
        &Regex.scan(
          ~r/(?:\A|[^\w-])(?:min-|max-)?(inline-size|block-size|width|height|aspect-ratio|orientation)(?![\w-])/u,
          &1,
          capture: :all_but_first
        )
      )
      |> List.flatten()
      |> Enum.uniq()

    case features do
      [feature] when feature in @features -> {:size, feature, source}
      _ -> :unknown
    end
  end

  defp style_condition(source, depth) do
    HtmlValidator.check_render_resource(:max_depth, depth, :css)

    case CssSyntaxValidator.declarations(source) do
      [%{declaration: {"--" <> _ = property, value}, length: length}]
      when length == byte_size(source) ->
        {:style, property, tokens(value)}

      _ ->
        case HtmlValidator.parse_css_identifier(source) do
          {:ok, "--" <> _ = property, ""} -> {:style, property, :present}
          _ -> style_group(components(source), depth)
        end
    end
  end

  defp style_group(values, depth) do
    case values do
      [%{kind: :block, opener: "(", closed: true} = term] ->
        style_condition(body(term), depth + 1)

      [%{kind: :token, text: operator}, term] ->
        if String.downcase(operator, :ascii) == "not" and Map.get(term, :opener) == "(" and
             term.closed,
           do: {:not, style_condition(body(term), depth + 1)},
           else: :unknown

      [first, %{kind: :token, text: operator}, next | rest] ->
        operator = String.downcase(operator, :ascii)

        with true <- operator in ["and", "or"],
             {:ok, terms} <- joined_terms([next | rest], operator, [first]),
             true <- Enum.all?(terms, &match?(%{kind: :block, opener: "(", closed: true}, &1)) do
          {if(operator == "and", do: :and, else: :or),
           Enum.map(terms, &style_condition(body(&1), depth + 1))}
        else
          _ -> :unknown
        end

      _ ->
        :unknown
    end
  end

  defp tokens(value) do
    value
    |> CssSyntaxValidator.components()
    |> Enum.map(fn
      %{kind: :comment} -> " "
      %{text: text} -> if(String.trim(text) == "", do: " ", else: text)
    end)
    |> Enum.chunk_by(&(&1 == " "))
    |> Enum.map(fn
      [" " | _] -> " "
      chunk -> chunk
    end)
    |> IO.iodata_to_binary()
    |> String.trim()
  end

  defp axes(expression) do
    case expression do
      {:size, "width", _} ->
        [:width]

      {:size, "height", _} ->
        [:height]

      {:size, "inline-size", _} ->
        [:inline_size]

      {:size, "block-size", _} ->
        [:block_size]

      {:size, _, _} ->
        [:width, :height]

      {:not, term} ->
        axes(term)

      {operator, terms} when operator in [:and, :or] ->
        terms |> Enum.flat_map(&axes/1) |> Enum.uniq()

      _ ->
        []
    end
  end

  @doc "Chooses the nearest ancestor eligible for every feature in a condition."
  @spec eligible?(map(), map()) :: boolean()
  def eligible?(query, style) do
    (query.name == nil or query.name in Map.get(style, :container_name, [])) and
      Enum.all?(query.axes, &eligible_axis?(style, &1))
  end

  @doc "Checks the axis against the container type, writing mode and principal box."
  @spec eligible_axis?(map(), atom()) :: boolean()
  def eligible_axis?(style, axis) do
    type = Map.get(style, :container_type, :normal)

    type in [:size, :inline_size] and
      Map.get(style, :display) not in [
        :none,
        :contents,
        :inline,
        :table_row,
        :table_section,
        :table_cell,
        :table_column,
        :table_column_group
      ] and
      not Map.get(style, :_html_content_skipped, false) and
      (type == :size or axis == :inline_size or
         axis == CssLogicalValidator.axes(style).inline_size)
  end

  @doc "Evaluates a prepared condition with CSS three-valued Boolean logic."
  @spec evaluate(term(), map(), map() | nil, map()) :: boolean() | :unknown
  def evaluate(expression, style, geometry, viewport) do
    case expression do
      :name ->
        true

      {:size, _, source} ->
        if geometry do
          axes = CssLogicalValidator.axes(style)

          context =
            Map.merge(viewport, %{
              query_width: geometry.width * (1 / 0.75),
              query_height: geometry.height * (1 / 0.75),
              query_inline_size: Map.fetch!(geometry, axes.inline_size) * (1 / 0.75),
              query_block_size: Map.fetch!(geometry, axes.block_size) * (1 / 0.75),
              font_size: style.font_size / 0.75,
              root_font_size: Map.get(style, :_root_font_size, style.font_size) / 0.75
            })

          PrintMediaValidator.container_feature(source, context)
        else
          :unknown
        end

      {:style, property, expected} ->
        case Map.fetch(Map.get(style, :_custom_properties, %{}), property) do
          {:ok, value} when is_binary(value) -> expected == :present or tokens(value) == expected
          _ -> false
        end

      {:not, term} ->
        case evaluate(term, style, geometry, viewport) do
          :unknown -> :unknown
          value -> not value
        end

      {operator, terms} when operator in [:and, :or] ->
        values = Enum.map(terms, &evaluate(&1, style, geometry, viewport))
        decisive = operator == :or

        cond do
          decisive in values -> decisive
          :unknown in values -> :unknown
          true -> not decisive
        end

      :unknown ->
        :unknown
    end
  end
end
