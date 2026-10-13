defmodule NativeElixirPdfUtilities.Validators.SvgFormValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.Reader

  @doc false
  @spec prepare_source(term()) :: {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_source(source) do
    with {:ok, document} <- Reader.read(source),
         [page] <- document.pages,
         %{value: %{"Contents" => {:ref, content_ref}} = page_value} <-
           Map.get(document.objects, page.ref),
         %{value: content_dictionary, stream: stream} <-
           Map.get(document.objects, content_ref),
         [0, 0, width, height] <- page.media_box,
         true <- is_map(page.resources) and is_map(content_dictionary) and is_binary(stream),
         true <- is_number(width) and is_number(height) and width > 0 and height > 0,
         {:ok, links, targets} <- prepare_links(page_value, document.objects, width, height) do
      {:ok,
       %{
         links: links,
         targets: targets,
         width: width,
         height: height,
         stream: stream,
         content_dictionary: content_dictionary,
         resources: page.resources,
         group: Map.get(page_value, "Group"),
         objects: document.objects
       }}
    else
      {:error, {_reason, _diagnostic}} = error ->
        error

      _ ->
        Diagnostics.error(
          :svg,
          :invalid_pdf_input,
          "SVG vector conversion must produce one page with a bounded content stream and resources",
          operation: :vectorize,
          module: __MODULE__,
          source: "SVG vector PDF"
        )
    end
  end

  defp prepare_links(page, objects, width, height) do
    annots = Map.get(page, "Annots", [])
    targets = Map.get(page, "NativeSvgTargets", [])

    rect? = fn rect ->
      case rect do
        [l, b, r, t] ->
          Enum.all?(
            rect,
            &match?(
              {:ok, _},
              NativeElixirPdfUtilities.Validators.HtmlValidator.normalize_geometry_number(&1)
            )
          ) and l >= 0 and b >= 0 and r <= width and t <= height and r > l and t > b

        _ ->
          false
      end
    end

    if is_list(annots) and is_list(targets) do
      links =
        Enum.map(annots, fn ref ->
          case ref do
            {:ref, id} -> Map.get(objects, id, %{}) |> Map.get(:value)
            _ -> nil
          end
        end)

      if Enum.all?(links, fn item ->
           is_map(item) and Map.get(item, "Subtype") == {:name, "Link"} and
             match?({:string, value} when is_binary(value), Map.get(item, "NativeSvgHref")) and
             is_integer(Map.get(item, "NativeSvgAnchorIndex")) and
             item["NativeSvgAnchorIndex"] >= 0 and rect?.(item["Rect"])
         end) and
           Enum.all?(targets, fn item ->
             is_map(item) and match?({:string, value} when is_binary(value), Map.get(item, "Id")) and
               rect?.(item["Rect"])
           end) do
        {:ok,
         Enum.map(
           links,
           &%{
             source: elem(&1["NativeSvgHref"], 1),
             index: &1["NativeSvgAnchorIndex"],
             rect: &1["Rect"]
           }
         ), Map.new(targets, &{elem(&1["Id"], 1), &1["Rect"]})}
      else
        Diagnostics.error(
          :svg,
          :invalid_pdf_input,
          "SVG annotations require actual finite bounded link and target rectangles",
          source: "SVG vector annotations",
          module: __MODULE__,
          operation: :prepare
        )
      end
    else
      Diagnostics.error(:svg, :invalid_pdf_input, "SVG annotations require direct bounded arrays",
        source: "SVG vector annotations",
        module: __MODULE__,
        operation: :prepare
      )
    end
  end
end
