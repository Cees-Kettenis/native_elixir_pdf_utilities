defmodule NativeElixirPdfUtilities.HtmlToPdf.Visibility do
  @moduledoc "Preserves layout allocation while suppressing hidden artwork and interaction."
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  @key {__MODULE__, :hidden_artwork}

  @doc "Scopes hidden-artwork bookkeeping to one layout, including nested child-document layouts."
  @spec with_layout((-> term())) :: term()
  def with_layout(operation) do
    previous = Process.put(@key, false)

    try do
      case operation.() do
        {:ok, layout} = result ->
          if Process.get(@key),
            do: {:ok, %{layout | boxes: materialize(layout.boxes)}},
            else: result

        other ->
          other
      end
    after
      if previous == nil, do: Process.delete(@key), else: Process.put(@key, previous)
    end
  end

  @doc "Marks only artwork owned by the element; independently visible descendants keep their own paint."
  @spec mark(map(), map()) :: map()
  def mark(box, style) do
    case style do
      %{_force_hidden: true} ->
        Process.put(@key, true)
        Map.put(box, :_css_hidden_artwork, true)

      %{visibility: visibility} when visibility in [:hidden, :force_hidden, :collapse] ->
        Process.put(@key, true)
        Map.put(box, :_css_hidden_artwork, true)

      _ ->
        box
    end
  end

  @doc "Converts hidden artwork into nonpainting flow geometry before pagination and annotation creation."
  @spec materialize([map()]) :: [map()]
  def materialize(boxes) do
    Enum.map(boxes, fn box ->
      case box do
        %{_css_hidden_artwork: true} ->
          {top, bottom} = PageGeometry.box_vertical_bounds(box)

          box
          |> Map.drop([
            :_css_hidden_artwork,
            :boxes,
            :transform,
            :clip,
            :image,
            :responsive_style,
            :embedded_widgets,
            :text,
            :font,
            :font_face,
            :font_size,
            :native_glyph_runs,
            :_css_actual_text,
            :_math_actual_text,
            :form_control,
            :form_owner,
            :initial_option_id,
            :link_url,
            :link_destination,
            :link_rect,
            :_image_map_geometry,
            :shape,
            :select_arrow,
            :native_menu_clip,
            :native_listbox_clip
          ])
          |> Map.merge(%{
            type: :rect,
            y: bottom,
            height: top - bottom,
            fill_color: {0.0, 0.0, 0.0, 0.0},
            stroke_color: nil,
            stroke_width: 0.0,
            border_widths: %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0},
            border_colors: %{top: nil, right: nil, bottom: nil, left: nil},
            border_styles: %{top: :none, right: :none, bottom: :none, left: :none},
            border_radius: 0.0
          })

        %{boxes: children} ->
          %{box | boxes: materialize(children)}

        _ ->
          box
      end
    end)
  end
end
