defmodule NativeElixirPdfUtilities.HtmlToPdf.CssZoom do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @dimensions ~w(width height min_width min_height max_width max_height font_size line_height
    letter_spacing word_spacing row_gap column_gap column_width flex_basis text_indent
    border_radius outline_width outline_offset top right bottom left contain_intrinsic_width
    contain_intrinsic_height)a
  @edges ~w(margin padding border_widths border_radii offsets)a

  @doc "Projects computed lengths into the parent's static layout coordinate space."
  @spec project(map(), number()) :: map()
  def project(tree, scale) do
    tree
    |> Map.update!(:children, &Enum.map(&1, fn child -> project_node(child, scale) end))
    |> Map.update!(:document_root_sizing, fn
      nil -> nil
      style -> style |> project_style(scale) |> Map.delete(:_zoom_factor)
    end)
    |> Map.update!(:document_canvas, fn canvas ->
      Map.update!(canvas, :style, fn style ->
        style |> project_style(scale) |> Map.delete(:_zoom_factor)
      end)
    end)
  end

  defp project_node(node, inherited) do
    style = node.style
    scale = inherited * Map.get(style, :zoom, 1.0)
    HtmlValidator.check_render_resource(:max_numeric_magnitude, scale, :style)

    node = %{node | style: project_style(style, scale)}

    case node do
      %{children: children} -> %{node | children: Enum.map(children, &project_node(&1, scale))}
      _ -> node
    end
  end

  defp project_style(style, scale) do
    if is_map(style) and scale != 1 do
      style
      |> Map.put(:_zoom_factor, scale)
      |> scale_fields(@dimensions, scale)
      |> scale_fields(@edges, scale)
    else
      style
    end
  end

  defp scale_fields(style, fields, scale) do
    Enum.reduce(fields, style, fn field, style ->
      case Map.fetch(style, field) do
        {:ok, value} -> Map.put(style, field, length_value(value, scale))
        :error -> style
      end
    end)
  end

  defp length_value(value, scale) do
    case value do
      value when is_number(value) ->
        scaled = value * scale
        HtmlValidator.check_render_resource(:max_numeric_magnitude, abs(scaled), :style)
        scaled

      value when is_map(value) ->
        Map.new(value, fn {key, value} -> {key, length_value(value, scale)} end)

      {left, right} when is_number(left) and is_number(right) ->
        {length_value(left, scale), length_value(right, scale)}

      value ->
        value
    end
  end
end
