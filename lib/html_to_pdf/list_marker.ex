defmodule NativeElixirPdfUtilities.HtmlToPdf.ListMarker do
  @moduledoc "Shared prepared marker content for block and inline list items."
  alias NativeElixirPdfUtilities.HtmlToPdf.CounterStyle

  @doc "Returns validated marker artwork and its inline gap using the existing counter and image state."
  @spec nodes(map(), map(), atom(), integer()) :: {[map()], [map()]}
  def nodes(style, marker_style, marker_type, index) do
    markers = Map.get(style, :_list_marker, [])

    markers =
      if markers == [] do
        text = CounterStyle.marker(Map.get(style, :list_marker_type, marker_type), index)
        if text == "", do: [], else: [%{type: :text, text: text, style: marker_style}]
      else
        markers
      end

    markers =
      case Map.get(style, :list_style_image) do
        nil ->
          markers

        image ->
          image_style =
            marker_style
            |> Map.merge(%{
              display: :image,
              image_display: :inline,
              image: image,
              width: nil,
              height: nil
            })

          [%{type: :element, tag: "img", style: image_style, children: []}]
      end

    gap =
      cond do
        markers == [] ->
          []

        Map.get(style, :list_style_image) != nil ->
          # Chromium's marker image margin is a fixed seven CSS pixels.
          [
            %{
              type: :element,
              tag: "span",
              children: [],
              style:
                marker_style |> Map.merge(%{display: :inline_block, width: 5.25, height: 0.0})
            }
          ]

        true ->
          [%{type: :text, text: " ", style: marker_style}]
      end

    {markers, gap}
  end
end
