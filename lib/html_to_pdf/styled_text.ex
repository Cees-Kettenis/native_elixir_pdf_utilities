defmodule NativeElixirPdfUtilities.HtmlToPdf.StyledText do
  @moduledoc false

  alias NativeElixirPdfUtilities.HtmlToPdf.Visibility

  @spec heading_anchor(map()) :: map() | nil
  def heading_anchor(block) do
    case block do
      %{type: :element, tag: <<"h", level>>, children: children}
      when level in ?1..?6 and is_list(children) ->
        title =
          children |> descendant_text(:bookmark) |> String.replace(~r/\s+/u, " ") |> String.trim()

        case title do
          "" -> nil
          title -> %{title: title, level: Map.get(block.style, :heading_level, level - ?0)}
        end

      _ ->
        nil
    end
  end

  # A display:contents heading contributes its children directly to the parent's
  # layout. Their painted text fragments share one semantic bookmark identity.
  @spec contents_children(map()) :: [map()]
  def contents_children(element) do
    case heading_anchor(element) do
      nil -> element.children
      heading -> mark_heading_text(element.children, Map.put(heading, :id, make_ref()))
    end
  end

  defp mark_heading_text(children, anchor) do
    Enum.map(children, fn child ->
      case child do
        %{type: :text, style: style} ->
          if Visibility.hidden?(style),
            do: child,
            else: %{child | style: Map.put(style, :_contents_heading, anchor)}

        %{style: style, children: [_ | _] = nested} when style.display != :none ->
          %{child | children: mark_heading_text(nested, anchor)}

        _ ->
          child
      end
    end)
  end

  @spec descendant_text([map()], :baseline | :bookmark) :: String.t()
  def descendant_text(children, purpose) do
    Enum.map_join(children, "", fn child ->
      case child do
        %{style: %{display: :none}} ->
          ""

        %{type: :text, text: text, style: style} when purpose == :bookmark ->
          if Visibility.hidden?(style), do: "", else: text

        %{type: :text, text: text} ->
          text

        %{type: :element, tag: "br"} ->
          " "

        %{children: nested} when is_list(nested) ->
          descendant_text(nested, purpose)

        _ ->
          ""
      end
    end)
  end
end
