defmodule NativeElixirPdfUtilities.Pdf.FormWriter do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, InfoCodec, ObjectMapping, Reader}
  alias NativeElixirPdfUtilities.Validators.FormValidator

  @doc false
  @spec create(map(), [map()]) :: {:ok, binary()} | {:error, {atom(), map()}}
  def create(context, controls) do
    pages =
      Enum.flat_map(controls, fn control ->
        if control.type in [:checkbox, :radio] do
          for state <- [false, true] do
            text =
              case {control.type, state} do
                {:checkbox, false} -> "☐"
                {:checkbox, true} -> "☒"
                {:radio, false} -> "○"
                {:radio, true} -> "●"
              end

            %{
              control.appearance
              | boxes:
                  Enum.map(control.appearance.boxes, fn box ->
                    if box.type == :text, do: %{box | text: text}, else: box
                  end)
            }
          end
        else
          [
            control.appearance,
            %{
              control.appearance
              | boxes: Enum.reject(control.appearance.boxes, &(&1.type == :text))
            }
          ]
        end
      end)

    with {:ok, objects, appearances, next} <-
           render_appearances(context, pages, 2 * length(controls) + 1) do
      {controls, _} =
        Enum.map_reduce(controls, appearances, fn control, appearances ->
          count = 2
          {own, rest} = Enum.split(appearances, count)
          {Map.put(control, :ap, own), rest}
        end)

      groups = Enum.chunk_by(controls, & &1.name)
      # Radio groups may be separated by other controls in the document.
      order = groups |> Enum.map(&hd(&1).name) |> Enum.uniq()
      groups = Enum.group_by(controls, & &1.name)

      {objects, roots, annots, next} =
        Enum.reduce(order, {objects, [], %{}, next}, fn name, {objects, roots, annots, id} ->
          controls = groups[name]
          first = hd(controls)

          flags =
            first.flags +
              case first.type do
                :radio -> 32768
                :choice -> 131_072
                _ -> 0
              end

          value =
            case first.type do
              :checkbox -> {:name, if(first.value, do: "Yes", else: "Off")}
              :radio -> {:name, Enum.find_value(controls, "Off", & &1.value)}
              _ -> InfoCodec.encode_text(first.value)
            end

          ft =
            case first.type do
              :text -> "Tx"
              :choice -> "Ch"
              _ -> "Btn"
            end

          kids =
            Enum.with_index(controls, id + 1)
            |> Enum.map(fn {_, widget_id} -> {:ref, {widget_id, 0}} end)

          dictionary = %{
            "FT" => {:name, ft},
            "T" => InfoCodec.encode_text(name),
            "Ff" => flags,
            "V" => value,
            "Kids" => kids
          }

          dictionary =
            if first.type == :choice,
              do:
                Map.put(
                  dictionary,
                  "Opt",
                  Enum.map(
                    first.choices,
                    &[InfoCodec.encode_text(&1.value), InfoCodec.encode_text(&1.label)]
                  )
                ),
              else: dictionary

          {widgets, annots} =
            Enum.with_index(controls, id + 1)
            |> Enum.map_reduce(annots, fn {control, widget_id}, annots ->
              page = Enum.at(context.pages, control.page - 1)

              widget = %{
                "Type" => {:name, "Annot"},
                "Subtype" => {:name, "Widget"},
                "Parent" => {:ref, {id, 0}},
                "P" => {:ref, page.ref},
                "Rect" => control.rect,
                "F" => 4
              }

              widget =
                case control.ap do
                  [normal, background] when control.type in [:text, :choice] ->
                    widget
                    |> Map.put("AP", %{"N" => normal})
                    |> Map.put("NEPUBackground", background)

                  [off, on] ->
                    export = if control.type == :checkbox, do: "Yes", else: control.export
                    state = if control.value in [false, nil], do: "Off", else: export

                    widget
                    |> Map.put("AP", %{"N" => %{"Off" => off, export => on}})
                    |> Map.put("AS", {:name, state})
                end

              {{widget_id, 0, {:value, widget}},
               Map.update(
                 annots,
                 page.ref,
                 [{:ref, {widget_id, 0}}],
                 &(&1 ++ [{:ref, {widget_id, 0}}])
               )}
            end)

          {objects ++ [{id, 0, {:value, dictionary}} | widgets], roots ++ [{:ref, {id, 0}}],
           annots, id + 1 + length(controls)}
        end)

      page_objects =
        Enum.flat_map(context.pages, fn page ->
          case Map.get(annots, page.ref) do
            nil ->
              []

            widgets ->
              {id, gen} = page.ref

              [
                {id, gen,
                 {:value,
                  Map.put(
                    page.dictionary,
                    "Annots",
                    Map.get(page.dictionary, "Annots", []) ++ widgets
                  )}}
              ]
          end
        end)

      form = %{
        "Fields" => roots,
        "NeedAppearances" => false,
        "DA" => {:string, "/NEPUFont 0 Tf 0 g"},
        "DR" => %{
          "Font" => %{
            "NEPUFont" => %{
              "Type" => {:name, "Font"},
              "Subtype" => {:name, "Type1"},
              "BaseFont" => {:name, "Helvetica"}
            }
          }
        }
      }

      {id, gen} = context.catalog_ref

      IncrementalWriter.write(
        context,
        objects ++
          page_objects ++
          [
            {next, 0, {:value, form}},
            {id, gen, {:value, Map.put(context.catalog, "AcroForm", {:ref, {next, 0}})}}
          ]
      )
    end
  end

  @doc false
  @spec fill(map(), map(), [map()]) :: {:ok, binary()} | {:error, {atom(), map()}}
  def fill(context, form, fields) do
    pages = Enum.flat_map(fields, & &1.appearances)

    with {:ok, objects, appearances, next} <- render_appearances(context, pages, 1) do
      backgrounds =
        fields
        |> Enum.reject(&(&1.type in [:checkbox, :radio]))
        |> Enum.flat_map(& &1.widgets)
        |> Enum.zip(appearances)
        |> Map.new(fn {widget, {:ref, ref}} ->
          {ref, Map.get(widget.dictionary, "NEPUBackground")}
        end)

      objects =
        Enum.map(objects, fn {id, gen, body} = entry ->
          case {Map.get(backgrounds, {id, gen}), body} do
            {nil, _} ->
              entry

            {background, {:stream, dictionary, data}} ->
              resources = Map.get(dictionary, "Resources", %{})
              xobjects = Map.put(Map.get(resources, "XObject", %{}), "NEPUBackground", background)

              dictionary =
                Map.put(dictionary, "Resources", Map.put(resources, "XObject", xobjects))

              {id, gen, {:stream, dictionary, "q /NEPUBackground Do Q\n" <> data}}
          end
        end)

      {patches, _} =
        Enum.reduce(fields, {%{}, appearances}, fn field, {patches, appearances} ->
          {own, remaining} = Enum.split(appearances, length(field.appearances))
          own = if own == [], do: List.duplicate(nil, length(field.widgets)), else: own

          value =
            case field.type do
              :text ->
                InfoCodec.encode_text(field.new_value)

              :choice ->
                if is_list(field.new_value),
                  do: Enum.map(field.new_value, &InfoCodec.encode_text/1),
                  else: InfoCodec.encode_text(field.new_value)

              :radio ->
                {:name, field.new_value}

              :checkbox ->
                {:name,
                 if(field.new_value,
                   do: Enum.find(hd(field.widgets).states, &(&1 != "Off")),
                   else: "Off"
                 )}
            end

          dictionary = field.dictionary |> Map.put("V", value) |> Map.delete("I")

          dictionary =
            if field.type == :choice and is_list(field.new_value) do
              indexes =
                field.choices
                |> Enum.with_index()
                |> Enum.filter(fn {choice, _} -> choice.value in field.new_value end)
                |> Enum.map(&elem(&1, 1))

              Map.put(dictionary, "I", indexes)
            else
              dictionary
            end

          patches = Map.put(patches, field.ref, dictionary)

          patches =
            Enum.zip(field.widgets, own)
            |> Enum.reduce(patches, fn {widget, appearance}, patches ->
              dictionary = Map.get(patches, widget.ref, widget.dictionary)

              dictionary =
                if field.type in [:checkbox, :radio] do
                  state =
                    case field.type do
                      :checkbox ->
                        if field.new_value,
                          do: Enum.find(widget.states, &(&1 != "Off")),
                          else: "Off"

                      :radio ->
                        if field.new_value in widget.states, do: field.new_value, else: "Off"
                    end

                  Map.put(dictionary, "AS", {:name, state})
                else
                  Map.put(dictionary, "AP", Map.put(widget.ap, "N", appearance))
                end

              Map.put(patches, widget.ref, dictionary)
            end)

          {patches, remaining}
        end)

      form_dictionary = Map.put(form.form, "NeedAppearances", false)
      {catalog_id, catalog_gen} = context.catalog_ref
      catalog = Map.put(context.catalog, "AcroForm", {:ref, {next, 0}})
      entries = Enum.map(patches, fn {{:ref, {id, gen}}, dict} -> {id, gen, {:value, dict}} end)

      IncrementalWriter.write(
        context,
        objects ++
          entries ++
          [{next, 0, {:value, form_dictionary}}, {catalog_id, catalog_gen, {:value, catalog}}]
      )
    end
  end

  @doc false
  @spec flatten(map(), map(), [map()], [map()]) :: {:ok, binary()} | {:error, {atom(), map()}}
  def flatten(context, form, fields, placements) do
    next = context.document.trailer["Size"]

    removed =
      MapSet.new(
        Enum.map(fields, & &1.ref) ++
          Enum.flat_map(fields, fn field -> Enum.map(field.widgets, & &1.ref) end)
      )

    {roots, tree_patches} = prune(form.roots, form.nodes, removed)

    {page_objects, next} =
      placements
      |> Enum.group_by(& &1.page.ref)
      |> Enum.sort_by(&elem(&1, 0))
      |> Enum.reduce({[], next}, fn {{page_id, page_gen}, placements}, {objects, id} ->
        first = hd(placements)

        {resources, invocations} =
          Enum.reduce(placements, {first.resources, []}, fn placement, {resources, commands} ->
            xobjects = resources["XObject"]

            name =
              Stream.iterate(1, &(&1 + 1))
              |> Enum.find_value(fn index ->
                name = "NEPUForm#{index}"
                if Map.has_key?(xobjects, name), do: nil, else: name
              end)

            resources =
              Map.put(resources, "XObject", Map.put(xobjects, name, {:ref, placement.stream}))

            matrix =
              Enum.map(placement.matrix, fn number ->
                {:ok, encoded} = InfoCodec.serialize_value(number)
                encoded
              end)
              |> Enum.intersperse(" ")

            {resources, [commands, "q ", matrix, " cm /", name, " Do Q\n"]}
          end)

        annots = Enum.reject(first.annots, &MapSet.member?(removed, &1))

        dictionary =
          first.page.dictionary
          |> Map.put("Resources", resources)
          |> Map.put("Annots", annots)
          |> Map.put("Contents", [{:ref, {id, 0}} | first.contents] ++ [{:ref, {id + 1, 0}}])

        entries = [
          {id, 0, {:stream, %{}, "q\n"}},
          {id + 1, 0, {:stream, %{}, IO.iodata_to_binary(["Q\n", invocations])}},
          {page_id, page_gen, {:value, dictionary}}
        ]

        {entries ++ objects, id + 2}
      end)

    catalog =
      if roots == [],
        do: Map.delete(context.catalog, "AcroForm"),
        else: Map.put(context.catalog, "AcroForm", Map.put(form.form, "Fields", roots))

    {catalog_id, catalog_gen} = context.catalog_ref

    with :ok <- FormValidator.capacity(context, next - context.document.trailer["Size"]) do
      IncrementalWriter.write(
        context,
        page_objects ++ tree_patches ++ [{catalog_id, catalog_gen, {:value, catalog}}]
      )
    end
  end

  @doc false
  @spec import_appearances(map(), pos_integer()) :: {[tuple()], [tuple()], pos_integer()}
  def import_appearances(source, next) do
    refs = Map.keys(source.document.objects) |> Enum.sort()
    mapping = refs |> Enum.with_index(next) |> Map.new()

    objects =
      Enum.map(refs, fn ref ->
        parsed = source.document.objects[ref]
        value = ObjectMapping.remap(parsed.value, mapping)

        body =
          if is_binary(parsed.stream),
            do: {:stream, Map.delete(value, "Length"), parsed.stream},
            else: {:value, value}

        {mapping[ref], elem(ref, 1), body}
      end)

    next = next + length(refs)

    {forms, next} =
      Enum.map_reduce(source.pages, next, fn page, id ->
        {:ok, contents} =
          NativeElixirPdfUtilities.Validators.PdfValidator.content_references(
            source.document,
            page.dictionary
          )

        data =
          Enum.map(contents, fn ref ->
            {:ok, bytes} = Reader.decoded_stream(source.document, ref)
            bytes
          end)
          |> Enum.join("\n")

        {:ok, bbox} =
          NativeElixirPdfUtilities.Validators.PdfValidator.resolve(
            source.document,
            page.media_box
          )

        dictionary = %{
          "Type" => {:name, "XObject"},
          "Subtype" => {:name, "Form"},
          "BBox" => bbox,
          "Resources" => ObjectMapping.remap(page.resources, mapping)
        }

        {{id, 0, {:stream, dictionary, data}}, id + 1}
      end)

    {objects ++ forms, Enum.map(forms, fn {id, _, _} -> {:ref, {id, 0}} end), next}
  end

  defp render_appearances(context, pages, extra_objects) do
    case pages do
      [] ->
        with :ok <- FormValidator.capacity(context, extra_objects) do
          {:ok, [], [], context.document.trailer["Size"]}
        end

      _ ->
        with {:ok, pdf} <- PdfWriter.render(pages, forms: :static),
             {:ok, source} <- Reader.read_validated(pdf),
             :ok <-
               FormValidator.capacity(
                 context,
                 map_size(source.document.objects) + length(pages) + extra_objects
               ) do
          {objects, appearances, next} =
            import_appearances(source, context.document.trailer["Size"])

          {:ok, objects, appearances, next}
        end
    end
  end

  defp prune(refs, nodes, removed) do
    Enum.reduce(refs, {[], []}, fn ref, {kept, patches} ->
      if MapSet.member?(removed, ref) do
        {kept, patches}
      else
        dictionary = Map.fetch!(nodes, ref)

        case Map.get(dictionary, "Kids") do
          kids when is_list(kids) and kids != [] ->
            {children, more} = prune(kids, nodes, removed)

            case children do
              [] ->
                {kept, patches ++ more}

              _ ->
                {:ref, {id, gen}} = ref

                {kept ++ [ref],
                 patches ++ more ++ [{id, gen, {:value, Map.put(dictionary, "Kids", children)}}]}
            end

          _ ->
            {kept ++ [ref], patches}
        end
      end
    end)
  end
end
