defmodule NativeElixirPdfUtilities.Validators.AssemblyValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Pdf.ObjectMapping
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.LinkActionValidator
  alias NativeElixirPdfUtilities.Validators.MergeValidator
  alias NativeElixirPdfUtilities.Validators.OutlineValidator
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @doc false
  @spec validate_output_size(non_neg_integer()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_output_size(bytes) do
    limit = min(Limits.get(:max_output_bytes), Limits.get(:max_input_bytes))

    if bytes <= limit do
      :ok
    else
      Diagnostics.error(
        :assembly_write,
        :resource_limit_exceeded,
        "assembled PDF output requires #{bytes} bytes, exceeding min(max_output_bytes, max_input_bytes) (#{limit})",
        operation: :assemble,
        module: __MODULE__
      )
    end
  end

  @doc false
  @spec prepare(PdfValidator.context(), [pos_integer()], %{optional(pos_integer()) => integer()}) ::
          {:ok, MergeValidator.input_context()}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(context, page_numbers, rotations) do
    with {:ok, source} <- prepare_source(context, page_numbers) do
      prepare_selection(source, page_numbers, rotations)
    end
  end

  @doc false
  @spec prepare_source(PdfValidator.context()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec prepare_source(PdfValidator.context(), :all | Enumerable.t()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_source(context, selection \\ :all) do
    with {:ok, input} <- MergeValidator.prepare(context, selection) do
      object_values =
        Map.new(input.objects, fn object ->
          {{object.obj, object.gen}, Map.get(object, :value_override, object.value)}
        end)

      document = %{
        context.document
        | objects:
            Map.new(context.document.objects, fn {ref, object} ->
              {ref, %{object | value: Map.get(object_values, ref, object.value)}}
            end)
      }

      {:ok,
       %{
         input: input,
         document: document,
         object_values: object_values,
         objects: Map.new(input.objects, &{{&1.obj, &1.gen}, &1}),
         object_order:
           input.objects
           |> Enum.with_index()
           |> Map.new(fn {object, index} -> {{object.obj, object.gen}, index} end),
         pages:
           context.pages |> Enum.with_index(1) |> Map.new(fn {page, number} -> {number, page} end),
         page_tree_refs: Map.new(document.pages, &{&1.ref, true}),
         all_page_refs:
           input.objects
           |> Enum.filter(&page_object?(document, &1))
           |> Map.new(&{{&1.obj, &1.gen}, true})
       }}
    end
  end

  @doc false
  @spec prepare_selection(map(), [pos_integer()], map()) ::
          {:ok, MergeValidator.input_context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_selection(source, page_numbers, rotations \\ %{}) do
    selected_pages = Enum.map(page_numbers, &Map.fetch!(source.pages, &1))

    with {:ok, overrides} <-
           page_overrides(
             source.document,
             source.object_values,
             selected_pages,
             source.input.inherited,
             rotations,
             source.page_tree_refs
           ),
         {:ok, objects} <- reachable_objects(source, overrides, selected_pages) do
      selected_refs = Enum.map(selected_pages, & &1.ref)

      {:ok,
       %{
         source.input
         | objects: objects,
           pages: selected_refs,
           inherited: Map.take(source.input.inherited, selected_refs),
           outlines: OutlineValidator.remap_for_selection(source.input.outlines, page_numbers)
       }}
    end
  end

  defp page_overrides(
         document,
         object_values,
         selected_pages,
         inheritances,
         rotations,
         all_page_refs
       ) do
    selected_refs = Map.new(selected_pages, &{&1.ref, true})

    links = %{selected: selected_refs, pages: all_page_refs, actions: %{}, annotations: %{}}

    selected_pages
    |> Enum.with_index(1)
    |> Enum.reduce_while({:ok, %{}, links}, fn {page, output_number}, {:ok, overrides, links} ->
      inherited = Map.fetch!(inheritances, page.ref)
      dictionary = Map.fetch!(object_values, page.ref)

      with {:ok, annotations} <- PdfValidator.optional_entry(document, dictionary, "Annots", nil),
           {:ok, annotations, links} <- LinkActionValidator.select(document, annotations, links) do
        rotation =
          Integer.mod((inherited.rotate_value || 0) + Map.get(rotations, output_number, 0), 360)

        dictionary =
          dictionary
          |> Map.put("Type", {:name, "Page"})
          |> Map.put("Parent", :generated_parent)
          |> Map.put("MediaBox", inherited.mediabox_value)
          |> put_or_delete("CropBox", inherited.cropbox_value)
          |> put_or_delete("Resources", inherited.resources_value)
          |> put_or_delete(
            "Rotate",
            rotation_value(inherited.rotate_value, rotation, rotations, output_number)
          )
          |> put_or_delete("Annots", annotations)

        {:cont, {:ok, Map.put(overrides, page.ref, dictionary), links}}
      else
        {:error, _error} = page_error -> {:halt, page_error}
      end
    end)
    |> case do
      {:ok, overrides, links} ->
        actions = Map.reject(links.actions, fn {_ref, value} -> value == :omit end)
        {:ok, overrides |> Map.merge(links.annotations) |> Map.merge(actions)}

      error ->
        error
    end
  end

  defp rotation_value(original, rotation, rotations, output_number) do
    case is_nil(original) and not Map.has_key?(rotations, output_number) and rotation == 0 do
      true -> nil
      false -> rotation
    end
  end

  defp reachable_objects(source, overrides, selected_pages) do
    selected_refs = Map.new(selected_pages, &{&1.ref, true})

    with {:ok, bookmark_refs} <- bookmark_dependencies(source, selected_refs),
         {:ok, reachable} <-
           walk_references(
             Enum.map(selected_pages, & &1.ref) ++ ObjectMapping.references(source.input.layers),
             bookmark_refs,
             source.objects,
             overrides,
             selected_refs,
             source.all_page_refs
           ) do
      prepared =
        reachable
        |> Map.keys()
        |> Enum.sort_by(&Map.fetch!(source.object_order, &1))
        |> Enum.map(&Map.fetch!(source.objects, &1))
        |> Enum.map(fn object ->
          case Map.fetch(overrides, {object.obj, object.gen}) do
            {:ok, value} -> Map.put(object, :value_override, value)
            :error -> object
          end
        end)

      {:ok, prepared}
    end
  end

  # Check the original bookmark graph before applying filtered annotation actions:
  # they can share an indirect action whose next page has been removed.
  defp bookmark_dependencies(source, selected_refs) do
    case ObjectMapping.references(source.input.outlines) do
      [] ->
        {:ok, %{}}

      refs ->
        with {:ok, seen} <-
               walk_references(
                 refs,
                 selected_refs,
                 source.objects,
                 %{},
                 selected_refs,
                 source.all_page_refs
               ) do
          {:ok, Map.drop(seen, Map.keys(selected_refs))}
        end
    end
  end

  defp walk_references(references, seen, objects, overrides, selected, all_pages) do
    case references do
      [] ->
        {:ok, seen}

      [ref | rest] ->
        cond do
          Map.has_key?(seen, ref) ->
            walk_references(rest, seen, objects, overrides, selected, all_pages)

          Map.has_key?(all_pages, ref) and not Map.has_key?(selected, ref) ->
            error(
              :page_dependencies,
              :unsupported_pdf_feature,
              "a retained object references page #{elem(ref, 0)}, which is not selected"
            )

          true ->
            case Map.fetch(objects, ref) do
              {:ok, object} ->
                nested_references =
                  case Map.fetch(overrides, ref) do
                    {:ok, value} ->
                      NativeElixirPdfUtilities.Pdf.ObjectMapping.references(value)

                    :error ->
                      NativeElixirPdfUtilities.Pdf.ObjectMapping.references(
                        Map.get(object, :value_override, object.value)
                      )
                  end

                walk_references(
                  nested_references ++ rest,
                  Map.put(seen, ref, true),
                  objects,
                  overrides,
                  selected,
                  all_pages
                )

              :error ->
                error(
                  :page_dependencies,
                  :invalid_pdf_input,
                  "a retained object references missing indirect object #{elem(ref, 0)} #{elem(ref, 1)}"
                )
            end
        end
    end
  end

  defp page_object?(document, object) do
    case object do
      %{value: %{"Type" => type}} -> PdfValidator.named?(document, type, "Page")
      _ -> false
    end
  end

  defp put_or_delete(dictionary, key, value) do
    case value do
      nil -> Map.delete(dictionary, key)
      value -> Map.put(dictionary, key, value)
    end
  end

  defp error(stage, reason, message) do
    Diagnostics.error(stage, reason, message, operation: :assemble, module: __MODULE__)
  end
end
