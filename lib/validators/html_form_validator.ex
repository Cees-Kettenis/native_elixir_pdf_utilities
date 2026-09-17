defmodule NativeElixirPdfUtilities.Validators.HtmlFormValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}

  @doc false
  @spec prepare([map()], keyword()) :: {:ok, [map()], [map()]} | {:error, {atom(), map()}}
  def prepare(pages, opts) do
    case Keyword.get(opts, :forms, :interactive) do
      :static ->
        {:ok, pages, []}

      :interactive ->
        controls =
          pages
          |> Enum.with_index(1)
          |> Enum.flat_map(fn {page, number} ->
            page.boxes
            |> Enum.filter(&is_map(Map.get(&1, :form_control)))
            |> Enum.uniq_by(& &1.form_control.id)
            |> Enum.sort_by(& &1.form_control.order)
            |> Enum.with_index(1)
            |> Enum.map(fn {box, index} ->
              control = box.form_control

              type =
                case control.tag do
                  "input" -> String.downcase(Map.get(control.attributes, "type", "text"))
                  "textarea" -> "text"
                  "select" -> "choice"
                end

              %{box: box, control: control, type: type, page: number, index: index}
            end)
          end)

        explicit =
          controls
          |> Enum.map(&Map.get(&1.control.attributes, "name"))
          |> Enum.reject(&(&1 in [nil, ""]))
          |> MapSet.new()

        cond do
          length(controls) * 2 > Limits.get(:max_appearance_widgets) ->
            error(:resource_limit_exceeded, "HTML controls exceed the appearance count limit")

          length(controls) > Limits.get(:max_pdf_form_fields) ->
            error(:resource_limit_exceeded, "HTML controls exceed max_pdf_form_fields")

          length(Enum.uniq_by(controls, & &1.control.id)) != length(controls) ->
            error(:unsupported_form, "a control cannot span pages or repeat in page furniture")

          true ->
            controls
            |> Enum.reduce_while({:ok, [], explicit}, fn item, {:ok, fields, names} ->
              attributes = item.control.attributes

              name =
                case Map.get(attributes, "name") do
                  name when is_binary(name) and name != "" ->
                    name

                  _ ->
                    base = "#{String.upcase(item.type)}_#{item.page}_#{item.index}"

                    Stream.iterate(0, &(&1 + 1))
                    |> Enum.find_value(fn suffix ->
                      candidate = if suffix == 0, do: base, else: "#{base}_#{suffix}"
                      if MapSet.member?(names, candidate), do: nil, else: candidate
                    end)
                end

              choices =
                if item.type == "choice" do
                  Enum.map(item.control.children, fn option ->
                    label = Enum.map_join(option.children, &Map.get(&1, :text, ""))

                    %{
                      value: Map.get(option.attributes, "value", label),
                      label: label,
                      selected: Map.has_key?(option.attributes, "selected")
                    }
                  end)
                else
                  []
                end

              value =
                case item.type do
                  "text" ->
                    if item.control.tag == "textarea" and item.control.children != [],
                      do: Enum.map_join(item.control.children, &Map.get(&1, :text, "")),
                      else: Map.get(attributes, "value", "")

                  "checkbox" ->
                    Map.has_key?(attributes, "checked")

                  "radio" ->
                    if Map.has_key?(attributes, "checked"),
                      do: Map.get(attributes, "value", "on"),
                      else: nil

                  "choice" ->
                    (Enum.find(choices, & &1.selected) || hd(choices)).value
                end

              box = item.box
              page = Enum.at(pages, item.page - 1)

              appearance_boxes =
                page.boxes
                |> Enum.filter(&(Map.get(&1, :form_owner) == item.control.id))
                |> Enum.map(fn box ->
                  box
                  |> Map.put(:x, box.x - item.box.x)
                  |> Map.put(:y, box.y - item.box.y)
                  |> Map.delete(:form_owner)
                end)

              field = %{
                name: name,
                type: String.to_existing_atom(item.type),
                value: value,
                choices: choices,
                export: Map.get(attributes, "value", "on"),
                page: item.page,
                rect: [box.x, box.y, box.x + box.width, box.y + box.height],
                flags:
                  if(Map.has_key?(attributes, "disabled"), do: 1, else: 0) +
                    if(item.control.tag == "textarea", do: 4096, else: 0),
                appearance: %{size: {box.width, box.height}, boxes: appearance_boxes}
              }

              cond do
                byte_size(name) > Limits.get(:max_pdf_form_text_bytes) ->
                  {:halt,
                   error(
                     :resource_limit_exceeded,
                     "HTML field name exceeds max_pdf_form_text_bytes"
                   )}

                String.contains?(name, [".", <<0>>]) ->
                  {:halt, error(:invalid_form, "HTML field names cannot contain a period or NUL")}

                Enum.any?(
                  fields,
                  &(&1.name == name and (&1.type != :radio or field.type != :radio))
                ) ->
                  {:halt,
                   error(
                     :invalid_form,
                     "duplicate HTML name #{inspect(name)}; only radio groups may share a name"
                   )}

                field.type == :radio and field.export in ["", "Off"] ->
                  {:halt, error(:invalid_form, "radio export values cannot be empty or Off")}

                field.type == :choice and
                    length(Enum.uniq_by(choices, & &1.value)) != length(choices) ->
                  {:halt, error(:invalid_form, "select options require unique export values")}

                field.type == :radio and
                    Enum.any?(
                      fields,
                      &(&1.name == name and
                            (&1.export == field.export or (&1.value != nil and value != nil)))
                    ) ->
                  {:halt,
                   error(
                     :invalid_form,
                     "radio groups require unique values and at most one checked option"
                   )}

                true ->
                  {:cont, {:ok, [field | fields], MapSet.put(names, name)}}
              end
            end)
            |> case do
              {:ok, fields, _} ->
                cleaned =
                  Enum.map(pages, fn page ->
                    %{
                      page
                      | boxes: Enum.reject(page.boxes, &(not is_nil(Map.get(&1, :form_owner))))
                    }
                  end)

                bytes =
                  Enum.reduce(fields, 0, fn field, total ->
                    Enum.reduce(field.appearance.boxes, total, fn box, total ->
                      total + byte_size(Map.get(box, :text, ""))
                    end)
                  end)

                if bytes <= Limits.get(:max_appearance_text_bytes),
                  do: {:ok, cleaned, Enum.reverse(fields)},
                  else:
                    error(
                      :resource_limit_exceeded,
                      "HTML control appearance text exceeds the aggregate limit"
                    )

              failure ->
                failure
            end
        end

      _ ->
        error(:invalid_options, "forms must be :interactive or :static")
    end
  end

  defp error(reason, message), do: Diagnostics.error(:forms, reason, message)
end
