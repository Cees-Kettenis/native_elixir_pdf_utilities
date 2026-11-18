defmodule NativeElixirPdfUtilities.Validators.CssAnchorValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{CssSyntaxValidator, HtmlValidator}

  @properties ~w(anchor-name anchor-scope position-anchor position-area position-try-order position-try-fallbacks position-visibility)
  @orders %{
    "normal" => :normal,
    "most-width" => :width,
    "most-height" => :height,
    "most-block-size" => :block_size,
    "most-inline-size" => :inline_size
  }
  @flips %{
    "flip-x" => :x,
    "flip-y" => :y,
    "flip-inline" => :inline,
    "flip-block" => :block,
    "flip-start" => :diagonal
  }
  @sides ~w(top right bottom left start end self-start self-end inside outside center)
  @axes ~w(width height block inline self-block self-inline)
  @area_sides ~w(top bottom left right x-start x-end y-start y-end block-start block-end inline-start inline-end self-block-start self-block-end self-inline-start self-inline-end start end self-start self-end center all)
  @try_properties ~w(position-anchor position-area top right bottom left inset inset-inline inset-block inset-inline-start inset-inline-end inset-block-start inset-block-end width height min-width max-width min-height max-height inline-size block-size min-inline-size max-inline-size min-block-size max-block-size margin margin-top margin-right margin-bottom margin-left margin-inline margin-block margin-inline-start margin-inline-end margin-block-start margin-block-end align-self justify-self place-self)

  @doc false
  @spec properties() :: [String.t()]
  def properties, do: @properties

  @doc false
  @spec name(String.t()) :: {:ok, String.t()} | :error
  def name(value) do
    case HtmlValidator.parse_css_identifier(value) do
      {:ok, "--" <> tail = name, ""} when tail != "" -> {:ok, name}
      _ -> :error
    end
  end

  @doc false
  @spec property(String.t(), String.t(), map()) :: {:ok, map()} | :error
  def property(property, source, style) do
    value = String.trim(source)
    tokens = HtmlValidator.split_css_tokens(value)

    case {property, value} do
      {"anchor-name", "none"} ->
        {:ok, %{anchor_name: []}}

      {"anchor-name", _} ->
        names(CssSyntaxValidator.split(value, ","), :anchor_name)

      {"anchor-scope", "none"} ->
        {:ok, %{anchor_scope: []}}

      {"anchor-scope", "all"} ->
        {:ok, %{anchor_scope: :all}}

      {"anchor-scope", _} ->
        names(CssSyntaxValidator.split(value, ","), :anchor_scope)

      {"position-anchor", "auto"} ->
        {:ok, %{position_anchor: :auto}}

      {"position-anchor", "none"} ->
        {:ok, %{position_anchor: :none}}

      {"position-anchor", _} ->
        with {:ok, name} <- name(value), do: {:ok, %{position_anchor: name}}

      {"position-area", _} ->
        with {:ok, area} <- area(tokens), do: {:ok, %{position_area: area}}

      {"position-try-order", _} ->
        with {:ok, order} <- Map.fetch(@orders, value), do: {:ok, %{position_try_order: order}}

      {"position-try-fallbacks", "none"} ->
        {:ok, %{position_try_fallbacks: []}}

      {"position-try-fallbacks", _} ->
        fallbacks =
          value
          |> CssSyntaxValidator.split(",")
          |> Enum.map(&fallback(HtmlValidator.split_css_tokens(&1)))

        if Enum.all?(fallbacks, &match?({:ok, _}, &1)),
          do: {:ok, %{position_try_fallbacks: Enum.map(fallbacks, &elem(&1, 1))}},
          else: :error

      {"position-try", _} ->
        {order, rest} =
          case tokens do
            [first | rest] -> {Map.get(@orders, first), rest}
            [] -> {nil, []}
          end

        with {:ok, fields} <-
               property(
                 "position-try-fallbacks",
                 if(order, do: Enum.join(rest, " "), else: value),
                 style
               ) do
          {:ok, Map.put(fields, :position_try_order, order || :normal)}
        end

      {"position-visibility", "always"} ->
        {:ok, %{position_visibility: []}}

      {"position-visibility", _} ->
        if tokens != [] and Enum.uniq(tokens) == tokens and
             Enum.all?(tokens, &(&1 in ~w(anchors-valid anchors-visible no-overflow))),
           do: {:ok, %{position_visibility: tokens}},
           else: :error
    end
  end

  defp names(values, key) do
    result = Enum.map(values, &(String.trim(&1) |> name()))

    if result != [] and Enum.all?(result, &match?({:ok, _}, &1)),
      do: {:ok, %{key => Enum.map(result, &elem(&1, 1))}},
      else: :error
  end

  defp area(tokens) do
    case tokens do
      ["none"] ->
        {:ok, nil}

      tokens ->
        if length(tokens) in 1..2 and
             Enum.all?(tokens, fn token ->
               case token do
                 "span-" <> side -> side in @area_sides
                 side -> side in @area_sides and side != "all"
               end
             end),
           do: {:ok, tokens},
           else: :error
    end
  end

  defp fallback(tokens) do
    case area(tokens) do
      {:ok, area} when is_list(area) ->
        {:ok, {:area, area}}

      _ ->
        {flips, names} = Enum.split_with(tokens, &Map.has_key?(@flips, &1))

        rule =
          case names do
            [] -> {:ok, nil}
            [name] -> name(name)
            _ -> :error
          end

        with true <- tokens != [] and length(flips) == length(Enum.uniq(flips)),
             {:ok, name} <- rule do
          {:ok, {:rule, name, Enum.map(flips, &Map.fetch!(@flips, &1))}}
        else
          _ -> :error
        end
    end
  end

  @doc false
  @spec function(String.t(), atom(), map()) :: {:ok, tuple()} | :error | :ordinary
  def function(source, property, style) do
    case Regex.run(~r/^(anchor|anchor-size)\((.*)\)$/is, String.trim(source)) do
      [_, kind, body] ->
        [arguments | fallback] = CssSyntaxValidator.split(body, ",")
        tokens = HtmlValidator.split_css_tokens(arguments)
        kind = String.downcase(kind, :ascii)
        side? = property in [:top, :right, :bottom, :left]

        with {:ok, name, keyword} <- arguments(tokens, kind),
             true <- kind != "anchor" or side?,
             {:ok, fallback} <- length_fallback(fallback, style) do
          {:ok, {:css_anchor, kind, name, keyword, fallback}}
        else
          _ -> :error
        end

      nil ->
        :ordinary
    end
  end

  defp arguments(tokens, kind) do
    {names, keywords} = Enum.split_with(tokens, &match?({:ok, _}, name(&1)))

    name =
      case names do
        [] -> {:ok, nil}
        [value] -> name(value)
        _ -> :error
      end

    {allowed, default} = if kind == "anchor", do: {@sides, nil}, else: {@axes, :property}

    with {:ok, name} <- name do
      case keywords do
        [] when default != nil ->
          {:ok, name, default}

        [keyword] ->
          keyword = String.downcase(keyword, :ascii)

          cond do
            keyword in allowed ->
              {:ok, name, keyword}

            kind == "anchor" ->
              case Float.parse(keyword) do
                {number, "%"} -> {:ok, name, {:percent, number / 100}}
                _ -> :error
              end

            true ->
              :error
          end

        _ ->
          :error
      end
    end
  end

  defp length_fallback(tokens, style) do
    case tokens do
      [] ->
        {:ok, :auto}

      [value] ->
        HtmlValidator.parse_css_size(String.trim(value), :offset, Map.get(style, :font_size))

      _ ->
        :error
    end
  end

  @doc false
  @spec try_declarations([tuple()]) :: [tuple()]
  def try_declarations(declarations) do
    Enum.filter(declarations, fn
      {property, _} -> property in @try_properties
      _ -> false
    end)
  end
end
