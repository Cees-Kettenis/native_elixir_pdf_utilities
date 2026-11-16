defmodule NativeElixirPdfUtilities.Validators.CssFloatValidator do
  @moduledoc "Owns float/clear keywords and their computed blockification."
  alias NativeElixirPdfUtilities.Diagnostics

  @floats %{
    "none" => :none,
    "left" => :left,
    "right" => :right,
    "inline-start" => :inline_start,
    "inline-end" => :inline_end
  }
  @clears Map.put(@floats, "both", :both)

  @doc "Prepares a physical or logical float or clearance declaration."
  @spec prepare(String.t(), String.t()) :: {:ok, map()} | :error
  def prepare(property, value) do
    values =
      case property do
        "float" -> @floats
        "clear" -> @clears
        _ -> %{}
      end

    case Map.fetch(values, value |> String.trim() |> String.downcase(:ascii)) do
      {:ok, value} -> {:ok, %{if(property == "float", do: :float, else: :clear) => value}}
      :error -> :error
    end
  end

  @doc "Blockifies a floated principal box after its complete cascade."
  @spec compute_style(map()) :: {:ok, map()}
  def compute_style(style) do
    case style do
      style when not is_map_key(style, :float) ->
        {:ok, style}

      _ ->
        if Map.get(style, :float, :none) != :none and
             Map.get(style, :position, :static) not in [:absolute, :fixed] do
          display =
            case Map.get(style, :display) do
              :inline -> :block
              :inline_block -> :block
              :inline_flex -> :flex
              :inline_grid -> :grid
              :inline_table -> :table
              display -> display
            end

          {:ok, Map.put(style, :display, display)}
        else
          {:ok, style}
        end
    end
  end

  @doc "Establishes a flex/grid item's independent flow context and suppresses float and clear at its root."
  @spec prepare_item(map()) :: map()
  def prepare_item(style) do
    style = Map.put(style, :_flow_root, true)

    if Map.get(style, :float, :none) == :none and Map.get(style, :clear, :none) == :none,
      do: style,
      else: style |> Map.put(:float, :none) |> Map.put(:clear, :none)
  end

  @doc "Checks computed float keywords and forbids caller-supplied placement state."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    case style do
      style
      when not is_map_key(style, :float) and
             not is_map_key(style, :clear) and
             not is_map_key(style, :_float_placed) and
             not is_map_key(style, :_float_preceding_width) and
             not is_map_key(style, :_float_line_height) ->
        :ok

      _ ->
        if Map.get(style, :float, :none) in Map.values(@floats) and
             Map.get(style, :clear, :none) in Map.values(@clears) and
             not Enum.any?(
               [:_float_placed, :_float_preceding_width, :_float_line_height],
               &Map.has_key?(style, &1)
             ),
           do: :ok,
           else:
             Diagnostics.error(
               :layout,
               :invalid_layout,
               "float and clear require validated physical or inline edges; float placement belongs to the layout pass",
               operation: :layout,
               module: __MODULE__,
               source: source
             )
    end
  end
end
