defmodule NativeElixirPdfUtilities.Validators.CssStackingValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Parses static isolation and layout/paint containment geometry."
  @spec prepare(String.t(), String.t()) :: {:ok, atom(), term()} | :error
  def prepare(property, source) do
    terms =
      Enum.map(HtmlValidator.split_css_tokens(source), fn token ->
        case HtmlValidator.parse_css_identifier(token) do
          {:ok, name, ""} -> String.downcase(name, :ascii)
          _ -> nil
        end
      end)

    case {property, terms} do
      {"isolation", ["auto"]} ->
        {:ok, :isolation, :auto}

      {"isolation", ["isolate"]} ->
        {:ok, :isolation, :isolate}

      {"contain", ["none"]} ->
        {:ok, :contain, []}

      {"contain", terms} ->
        if terms != [] and Enum.all?(terms, &(&1 in ["layout", "paint"])) and
             length(terms) == length(Enum.uniq(terms)),
           do: {:ok, :contain, Enum.map(Enum.sort(terms), &String.to_existing_atom/1)},
           else: :error

      _ ->
        :error
    end
  end

  @doc "Computes paint isolation, absolute and fixed containing-block ownership from one property policy."
  @spec geometry(map()) :: {boolean(), boolean(), boolean()}
  def geometry(style) do
    case style do
      %{position: :static, z_index: :auto} = style
      when not is_map_key(style, :css_transform) and not is_map_key(style, :contain) and
             not is_map_key(style, :isolation) and not is_map_key(style, :_stacking_item) ->
        {false, false, false}

      style
      when not is_map_key(style, :position) and
             not is_map_key(style, :css_transform) and
             not is_map_key(style, :contain) and
             not is_map_key(style, :isolation) and
             not is_map_key(style, :_stacking_item) ->
        {false, false, false}

      _ ->
        position = Map.get(style, :position, :static)

        fixed =
          Map.get(style, :css_transform, :none) != :none or Map.get(style, :contain, []) != []

        absolute = fixed or position != :static

        context =
          fixed or position in [:fixed, :sticky] or
            (is_integer(Map.get(style, :z_index)) and
               (position != :static or Map.get(style, :_stacking_item, false))) or
            Map.get(style, :isolation, :auto) == :isolate

        {context, absolute, fixed}
    end
  end

  @doc "Identifies a stacking context from validated geometry properties."
  @spec context?(map()) :: boolean()
  def context?(style), do: elem(geometry(style), 0)

  @doc "Identifies the absolute or fixed containing-block owner independently of paint isolation."
  @spec containing_block?(map(), :absolute | :fixed) :: boolean()
  def containing_block?(style, position),
    do: elem(geometry(style), if(position == :absolute, do: 1, else: 2))

  @doc "Returns actionable diagnostics for malformed computed box metadata."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    if valid_style?(style) do
      :ok
    else
      NativeElixirPdfUtilities.Diagnostics.error(
        :layout,
        :invalid_layout,
        "computed display, visibility, logical axes or stacking geometry is malformed; recompute styles before layout",
        module: __MODULE__,
        operation: :layout,
        source: source
      )
    end
  end

  @doc "Checks static computed isolation and containment geometry at the styled-tree boundary."
  @spec valid_style?(map()) :: boolean()
  def valid_style?(style) do
    case style do
      style
      when not is_map_key(style, :isolation) and not is_map_key(style, :contain) and
             not is_map_key(style, :_stacking_item) ->
        true

      _ ->
        Map.get(style, :isolation, :auto) in [:auto, :isolate] and
          Map.get(style, :contain, []) in [[], [:layout], [:paint], [:layout, :paint]] and
          is_boolean(Map.get(style, :_stacking_item, false))
    end
  end

  @doc "Checks the retained source order of an element participating in CSS stacking."
  @spec valid_source_order?(term()) :: boolean()
  def valid_source_order?(order) do
    is_list(order) and length(order) <= Limits.get(:max_html_depth) and
      Enum.all?(order, &(is_integer(&1) and &1 >= 0))
  end

  @doc "Checks bounded stacking-context ancestry retained when positioned descendants are hoisted."
  @spec valid_context_path?(term()) :: boolean()
  def valid_context_path?(path) do
    is_list(path) and length(path) <= Limits.get(:max_html_depth) and
      Enum.all?(path, fn entry ->
        case entry do
          {order, z} -> valid_source_order?(order) and is_integer(z)
          _ -> false
        end
      end)
  end

  @doc "Checks paint ancestry on already validated drawing boxes before ordering them."
  @spec validate_boxes([map()]) :: :ok | {:error, {atom(), map()}}
  def validate_boxes(boxes) do
    Enum.reduce_while(boxes, :ok, fn box, :ok ->
      case box do
        box
        when not is_map_key(box, :_stacking_path) and
               not is_map_key(box, :_stacking_background_order) and
               not is_map_key(box, :_stacking_root_background) ->
          case box do
            %{boxes: children} ->
              case validate_boxes(children) do
                :ok -> {:cont, :ok}
                error -> {:halt, error}
              end

            _ ->
              {:cont, :ok}
          end

        _ ->
          path = Map.get(box, :_stacking_path, [])
          background = Map.get(box, :_stacking_background_order)

          valid =
            valid_context_path?(path) and
              (background == nil or valid_source_order?(background)) and
              is_boolean(Map.get(box, :_stacking_root_background, false))

          result =
            cond do
              not valid ->
                Diagnostics.error(
                  :pagination,
                  :invalid_layout,
                  "CSS stacking requires bounded source-order paths and integer stack levels; recompute layout",
                  source: "CSS stacking",
                  module: __MODULE__
                )

              Map.has_key?(box, :boxes) ->
                validate_boxes(box.boxes)

              true ->
                :ok
            end

          case result do
            :ok -> {:cont, :ok}
            error -> {:halt, error}
          end
      end
    end)
  end
end
