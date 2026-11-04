defmodule NativeElixirPdfUtilities.Validators.SvgViewportValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgStyleValidator}

  @doc false
  @spec prepare(term(), term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(attributes, style, host) do
    valid_host? =
      case host do
        %{width: width, height: height} ->
          match?({:ok, _}, HtmlValidator.normalize_geometry_number(width)) and width >= 0 and
            (is_nil(height) or
               (match?({:ok, _}, HtmlValidator.normalize_geometry_number(height)) and height >= 0)) and
            is_boolean(Map.get(host, :nested, false)) and
            (not Map.get(host, :nested, false) or is_number(height))

        _ ->
          false
      end

    if is_map(attributes) and is_map(style) and valid_host? do
      with {:ok, width} <- dimension(attributes, style, "width"),
           {:ok, height} <- dimension(attributes, style, "height"),
           {:ok, x} <- dimension(attributes, style, "x"),
           {:ok, y} <- dimension(attributes, style, "y"),
           {:ok, view_box} <- view_box(Map.get(attributes, "viewBox")),
           {:ok, aspect} <- aspect_ratio(Map.get(attributes, "preserveAspectRatio")),
           {:ok, font_size} <-
             HtmlValidator.normalize_geometry_number(Map.get(style, :font_size, 12.0)),
           {:ok, root_font_size} <-
             HtmlValidator.normalize_geometry_number(Map.get(style, :_root_font_size, 12.0)),
           true <- font_size >= 0 and root_font_size >= 0,
           overflow = Map.get(style, :overflow, :hidden),
           true <- overflow in [:hidden, :visible, :scroll, :auto] do
        {:ok,
         %{
           width: width,
           height: height,
           x: x,
           y: y,
           view_box: view_box,
           aspect: aspect,
           host: host,
           font_size: font_size,
           root_font_size: root_font_size,
           overflow: overflow
         }}
      else
        {:error, {_, _}} = error -> error
        _ -> invalid("SVG viewport requires nonnegative font sizes and supported overflow")
      end
    else
      invalid("SVG viewport requires attributes, computed style and nonnegative host dimensions")
    end
  end

  defp dimension(attributes, style, property) do
    key = String.to_existing_atom("svg_" <> property)

    value =
      Map.get(
        style,
        key,
        Map.get(attributes, property, if(property in ~w(x y), do: "0", else: "auto"))
      )

    result =
      case value do
        value when is_binary(value) ->
          SvgStyleValidator.parse(property, value)

        :auto when property in ~w(width height) ->
          {:ok, :auto}

        {unit, value} when unit in [:user, :points, :percent, :svg_em, :svg_rem] ->
          case HtmlValidator.normalize_geometry_number(value) do
            {:ok, number} when number >= 0 or property in ~w(x y) -> {:ok, {unit, number}}
            _ -> :error
          end

        _ ->
          :error
      end

    case result do
      {:ok, _} = result -> result
      _ -> invalid("SVG #{property} requires a valid geometry length")
    end
  end

  @doc false
  @spec view_box(term()) :: {:ok, nil | tuple()} | {:error, {atom(), map()}}
  def view_box(value) do
    result =
      case value do
        nil ->
          {:ok, nil}

        value when is_binary(value) ->
          # SVG comma-wsp permits one optional comma between numbers, never
          # empty entries, leading commas or adjacent comma separators.
          number = "[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?"
          separator = "(?:\\s+,?\\s*|,\\s*)"

          pattern =
            Regex.compile!(
              "\\A\\s*(#{number})#{separator}(#{number})#{separator}(#{number})#{separator}(#{number})\\s*\\z"
            )

          case Regex.run(pattern, value, capture: :all_but_first) do
            [x, y, width, height] ->
              values =
                Enum.map([x, y, width, height], fn token ->
                  # The shared CSS parser accepts decimal fractions with digits.
                  token = Regex.replace(~r/\.(?=[eE]|$)/, token, ".0")
                  HtmlValidator.parse_css_number(token)
                end)

              case values do
                [{x, ""}, {y, ""}, {width, ""}, {height, ""}] when width >= 0 and height >= 0 ->
                  {:ok, {x, y, width, height}}

                _ ->
                  :error
              end

            _ ->
              :error
          end

        _ ->
          :error
      end

    case result do
      {:ok, _} = result -> result
      _ -> invalid("viewBox requires four finite numbers and nonnegative width/height")
    end
  end

  @doc false
  @spec aspect_ratio(term()) :: {:ok, tuple()} | {:error, {atom(), map()}}
  def aspect_ratio(value) do
    parts = if is_binary(value), do: String.split(value), else: []

    parts =
      case parts do
        ["defer" | rest] -> rest
        parts -> parts
      end

    result =
      case {value, parts} do
        {nil, []} ->
          {:ok, {0.5, 0.5, :meet}}

        {_, ["none"]} ->
          {:ok, {0.0, 0.0, :none}}

        {_, ["none", mode]} when mode in ~w(meet slice) ->
          {:ok, {0.0, 0.0, :none}}

        {_, [align | mode]} when mode in [[], ["meet"], ["slice"]] ->
          case Regex.run(~r/\Ax(Min|Mid|Max)Y(Min|Mid|Max)\z/, align) do
            [_, x, y] ->
              positions = %{"Min" => 0.0, "Mid" => 0.5, "Max" => 1.0}
              {:ok, {positions[x], positions[y], if(mode == ["slice"], do: :slice, else: :meet)}}

            _ ->
              :error
          end

        _ ->
          :error
      end

    case result do
      {:ok, _} = result ->
        result

      _ ->
        invalid("preserveAspectRatio requires none or xMin/Mid/Max YMin/Mid/Max with meet/slice")
    end
  end

  @doc false
  @spec measured_geometry(map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def measured_geometry(measured) do
    numbers = [
      measured.width,
      measured.height,
      measured.x,
      measured.y | Tuple.to_list(measured.ctm)
    ]

    if Enum.all?(numbers, &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))) do
      {:ok, measured}
    else
      invalid(
        "SVG viewport transform exceeds the configured geometry range; reduce size or viewBox scale"
      )
    end
  end

  defp invalid(message) do
    Diagnostics.error(:layout, :invalid_document, message,
      module: __MODULE__,
      operation: :measure,
      source: "svg viewport"
    )
  end
end
