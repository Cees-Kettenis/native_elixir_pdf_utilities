defmodule NativeElixirPdfUtilities.Validators.AssemblyValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.MergeValidator
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @doc false
  @spec prepare(PdfValidator.context(), [pos_integer()], %{optional(pos_integer()) => integer()}) ::
          {:ok, MergeValidator.input_context()}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(context, page_numbers, rotations) do
    with {:ok, input} <- MergeValidator.prepare(context),
         selected_pages = selected_pages(context.pages, page_numbers),
         {:ok, overrides} <-
           page_overrides(context.document, selected_pages, input.inherited, rotations),
         {:ok, objects} <- reachable_objects(input.objects, overrides, selected_pages) do
      selected_refs = Enum.map(selected_pages, & &1.ref)

      {:ok,
       %{
         input
         | objects: objects,
           pages: selected_refs,
           inherited: Map.take(input.inherited, selected_refs)
       }}
    end
  end

  defp selected_pages(pages, page_numbers) do
    indexed = pages |> Enum.with_index(1) |> Map.new(fn {page, number} -> {number, page} end)
    Enum.map(page_numbers, &Map.fetch!(indexed, &1))
  end

  defp page_overrides(document, selected_pages, inheritances, rotations) do
    selected_refs = Map.new(selected_pages, &{&1.ref, true})
    all_page_refs = Map.new(document.pages, &{&1.ref, true})

    selected_pages
    |> Enum.with_index(1)
    |> Enum.reduce_while({:ok, %{}}, fn {page, output_number}, {:ok, overrides} ->
      inherited = Map.fetch!(inheritances, page.ref)

      with {:ok, annotations} <-
             sanitized_annotations(
               document,
               Map.get(page.dictionary, "Annots"),
               selected_refs,
               all_page_refs
             ) do
        rotation =
          Integer.mod((inherited.rotate_value || 0) + Map.get(rotations, output_number, 0), 360)

        dictionary =
          page.dictionary
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

        {:cont, {:ok, Map.put(overrides, page.ref, dictionary)}}
      else
        {:error, _error} = page_error -> {:halt, page_error}
      end
    end)
  end

  defp rotation_value(original, rotation, rotations, output_number) do
    case is_nil(original) and not Map.has_key?(rotations, output_number) and rotation == 0 do
      true -> nil
      false -> rotation
    end
  end

  defp sanitized_annotations(document, annotations, selected_refs, all_page_refs) do
    case annotations do
      nil ->
        {:ok, nil}

      annotations ->
        case PdfValidator.resolve(document, annotations) do
          {:ok, annotations} when is_list(annotations) ->
            annotations
            |> Enum.reduce_while({:ok, []}, fn annotation, {:ok, kept} ->
              case keep_annotation?(document, annotation, selected_refs, all_page_refs) do
                {:ok, true} -> {:cont, {:ok, [annotation | kept]}}
                {:ok, false} -> {:cont, {:ok, kept}}
                {:error, _error} = annotation_error -> {:halt, annotation_error}
              end
            end)
            |> case do
              {:ok, []} -> {:ok, nil}
              {:ok, kept} -> {:ok, Enum.reverse(kept)}
              {:error, _error} = annotation_error -> annotation_error
            end

          _ ->
            error(:annotations, :invalid_pdf_input, "page Annots entry must resolve to an array")
        end
    end
  end

  defp keep_annotation?(document, annotation, selected_refs, all_page_refs) do
    case PdfValidator.resolve(document, annotation) do
      {:ok, %{"Subtype" => {:name, "Link"}} = dictionary} ->
        link_destination_status(document, dictionary, selected_refs, all_page_refs)

      {:ok, dictionary} when is_map(dictionary) ->
        {:ok, true}

      _ ->
        error(:annotations, :invalid_pdf_input, "page annotation must resolve to a dictionary")
    end
  end

  defp link_destination_status(document, dictionary, selected_refs, all_page_refs) do
    case Map.get(dictionary, "Dest") do
      nil ->
        action_destination_status(
          document,
          Map.get(dictionary, "A"),
          selected_refs,
          all_page_refs
        )

      destination ->
        destination_status(document, destination, selected_refs, all_page_refs)
    end
  end

  defp action_destination_status(document, action, selected_refs, all_page_refs) do
    case action do
      nil ->
        {:ok, true}

      action ->
        case PdfValidator.resolve(document, action) do
          {:ok, %{"S" => {:name, "GoTo"}} = action} ->
            destination_status(document, Map.get(action, "D"), selected_refs, all_page_refs)

          {:ok, action} when is_map(action) ->
            {:ok, true}

          _ ->
            error(:annotations, :invalid_pdf_input, "link action must resolve to a dictionary")
        end
    end
  end

  defp destination_status(document, destination, selected_refs, all_page_refs) do
    resolved =
      case destination do
        {:ref, _ref} -> PdfValidator.resolve(document, destination)
        destination -> {:ok, destination}
      end

    case resolved do
      {:ok, [{:ref, ref} | _rest]} ->
        cond do
          Map.has_key?(selected_refs, ref) ->
            {:ok, true}

          Map.has_key?(all_page_refs, ref) ->
            {:ok, false}

          true ->
            error(:annotations, :invalid_pdf_input, "link destination references an unknown page")
        end

      {:ok, {kind, _value}} when kind in [:name, :string, :hex] ->
        {:ok, false}

      _ ->
        error(:annotations, :invalid_pdf_input, "link destination is malformed")
    end
  end

  defp reachable_objects(objects, overrides, selected_pages) do
    object_by_ref = Map.new(objects, &{{&1.obj, &1.gen}, &1})
    selected_refs = Map.new(selected_pages, &{&1.ref, true})

    all_page_refs =
      objects
      |> Enum.filter(&page_object?/1)
      |> Map.new(&{{&1.obj, &1.gen}, true})

    with {:ok, reachable} <-
           walk_references(
             Enum.map(selected_pages, & &1.ref),
             %{},
             object_by_ref,
             overrides,
             selected_refs,
             all_page_refs
           ) do
      prepared =
        objects
        |> Enum.filter(&Map.has_key?(reachable, {&1.obj, &1.gen}))
        |> Enum.map(fn object ->
          case Map.fetch(overrides, {object.obj, object.gen}) do
            {:ok, value} -> Map.put(object, :value_override, value)
            :error -> object
          end
        end)

      {:ok, prepared}
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
                    {:ok, value} -> value_references(value)
                    :error -> token_references(object.tokens)
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

  defp value_references(value) do
    case value do
      {:ref, ref} ->
        [ref]

      values when is_list(values) ->
        Enum.flat_map(values, &value_references/1)

      dictionary when is_map(dictionary) ->
        dictionary |> Map.values() |> Enum.flat_map(&value_references/1)

      _ ->
        []
    end
  end

  defp token_references(tokens) do
    {references, _pending} =
      Enum.reduce(tokens, {[], []}, fn token, {references, pending} ->
        pending = Enum.take([token | pending], 3)

        case pending do
          [:R, {:int, generation}, {:int, object}] -> {[{object, generation} | references], []}
          _ -> {references, pending}
        end
      end)

    references
  end

  defp page_object?(object) do
    case object do
      %{value: %{"Type" => {:name, "Page"}}} -> true
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
