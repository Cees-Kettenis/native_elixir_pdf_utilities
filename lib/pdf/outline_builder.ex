defmodule NativeElixirPdfUtilities.Pdf.OutlineBuilder do
  @moduledoc false

  alias NativeElixirPdfUtilities.Pdf.InfoCodec
  alias NativeElixirPdfUtilities.Validators.OutlineValidator

  @typedoc "Generated indirect PDF object ready for serialization."
  @type generated_object :: {non_neg_integer(), non_neg_integer(), map()}

  @doc false
  @spec build(
          [OutlineValidator.item()],
          (pos_integer() -> {non_neg_integer(), non_neg_integer()}),
          non_neg_integer()
        ) ::
          %{
            required(:root_ref) => {non_neg_integer(), non_neg_integer()} | nil,
            required(:objects) => [generated_object()],
            required(:next_id) => non_neg_integer()
          }
  def build(items, page_resolver, first_id) do
    case items do
      [] ->
        %{root_ref: nil, objects: [], next_id: first_id}

      items ->
        root_id = first_id
        {allocated, next_id} = allocate_items(items, first_id + 1, [])
        root_ref = {root_id, 0}

        item_objects =
          build_siblings(allocated, root_ref, page_resolver, [], nil) |> Enum.reverse()

        first_ref = allocated |> List.first() |> item_ref()
        last_ref = allocated |> List.last() |> item_ref()

        root = %{
          "Type" => {:name, "Outlines"},
          "First" => {:ref, first_ref},
          "Last" => {:ref, last_ref},
          "Count" => visible_item_count(allocated)
        }

        %{
          root_ref: root_ref,
          objects: [{root_id, 0, root} | item_objects],
          next_id: next_id
        }
    end
  end

  defp allocate_items(items, next_id, allocated) do
    items
    |> Enum.reduce({allocated, next_id}, fn item, {allocated, next_id} ->
      {children, child_next_id} = allocate_items(item.children, next_id + 1, [])
      visible_count = 1 + if(item.open, do: visible_item_count(children), else: 0)

      allocated_item = %{
        item: item,
        id: next_id,
        children: children,
        visible_count: visible_count
      }

      {[allocated_item | allocated], child_next_id}
    end)
    |> then(fn {allocated, next_id} -> {Enum.reverse(allocated), next_id} end)
  end

  defp build_siblings(items, parent_ref, page_resolver, objects, previous) do
    case items do
      [] ->
        objects

      [allocated | rest] ->
        dictionary =
          %{
            "Title" => InfoCodec.encode_text(allocated.item.title),
            "Parent" => {:ref, parent_ref}
          }
          |> maybe_put_reference("Prev", previous)
          |> maybe_put_reference("Next", List.first(rest))
          |> maybe_put_children(allocated.children, allocated.item.open)
          |> maybe_put_destination(allocated.item, page_resolver)

        objects =
          build_siblings(
            allocated.children,
            {allocated.id, 0},
            page_resolver,
            [{allocated.id, 0, dictionary} | objects],
            nil
          )

        build_siblings(rest, parent_ref, page_resolver, objects, allocated)
    end
  end

  defp maybe_put_reference(dictionary, key, allocated) do
    case allocated do
      nil -> dictionary
      allocated -> Map.put(dictionary, key, {:ref, item_ref(allocated)})
    end
  end

  defp maybe_put_children(dictionary, children, open) do
    case children do
      [] ->
        dictionary

      children ->
        visible = visible_item_count(children)
        count = if open, do: visible, else: -visible

        dictionary
        |> Map.put("First", {:ref, children |> List.first() |> item_ref()})
        |> Map.put("Last", {:ref, children |> List.last() |> item_ref()})
        |> Map.put("Count", count)
    end
  end

  defp maybe_put_destination(dictionary, item, page_resolver) do
    case item.page do
      nil ->
        dictionary

      page ->
        page_ref = page_resolver.(page)

        Map.put(dictionary, "Dest", [
          {:ref, page_ref},
          {:name, view_name(item.view)} | view_operands(item.view)
        ])
    end
  end

  defp view_name(view) do
    case view do
      :fit -> "Fit"
      :fit_b -> "FitB"
      {:fit_h, _top} -> "FitH"
      {:fit_v, _left} -> "FitV"
      {:fit_bh, _top} -> "FitBH"
      {:fit_bv, _left} -> "FitBV"
      {:fit_r, _left, _bottom, _right, _top} -> "FitR"
      {:xyz, _left, _top, _zoom} -> "XYZ"
    end
  end

  defp view_operands(view) do
    case view do
      view when view in [:fit, :fit_b] -> []
      {_kind, value} -> [value]
      {:fit_r, left, bottom, right, top} -> [left, bottom, right, top]
      {:xyz, left, top, zoom} -> [left, top, zoom]
    end
  end

  defp visible_item_count(items) do
    Enum.reduce(items, 0, fn allocated, count -> count + allocated.visible_count end)
  end

  defp item_ref(allocated) do
    {allocated.id, 0}
  end
end
