defmodule NativeElixirPdfUtilities.Validators.FormValidator do
  @moduledoc false
  import Bitwise
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Pdf.InfoCodec
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @inherited ~w(FT Ff V DV DA Q Opt MaxLen)
  @typep walk_state :: %{fields: [map()], nodes: map(), seen: map(), widgets: map()}

  @doc false
  @spec inspect_document(PdfValidator.context()) :: {:ok, map()} | {:error, {atom(), map()}}
  def inspect_document(context) do
    with {:ok, form} <-
           PdfValidator.dictionary(context.document, Map.get(context.catalog, "AcroForm", %{})),
         false <- Map.has_key?(form, "XFA"),
         {:ok, roots} <- PdfValidator.resolve(context.document, Map.get(form, "Fields", [])),
         true <- is_list(roots),
         {:ok, widgets} <- page_widgets(context),
         {:ok, state} <-
           walk(context.document, roots, Map.take(form, @inherited), "", 0, %{
             fields: [],
             nodes: %{},
             seen: %{},
             widgets: widgets
           }),
         fields <- Enum.reverse(state.fields),
         true <- length(Enum.uniq_by(fields, & &1.name)) == length(fields) do
      {:ok, %{form: form, roots: roots, fields: fields, nodes: state.nodes}}
    else
      true ->
        error(:unsupported_form, "XFA forms are unsupported; supply an AcroForm PDF")

      false ->
        error(:invalid_form, "AcroForm Fields must be an array with unique fully qualified names")

      {:error, _} = failure ->
        failure
    end
  end

  @doc false
  @spec options(term(), :fill | :flatten) :: {:ok, map()} | {:error, {atom(), map()}}
  def options(opts, operation) do
    allowed = if operation == :fill, do: [:flatten], else: [:fields]

    cond do
      not Keyword.keyword?(opts) ->
        error(:invalid_options, "form options must be a keyword list")

      Enum.any?(Keyword.keys(opts), &(&1 not in allowed)) or
          length(Keyword.keys(opts)) != length(Enum.uniq(Keyword.keys(opts))) ->
        error(:invalid_options, "unknown or repeated form option")

      operation == :fill and not is_boolean(Keyword.get(opts, :flatten, false)) ->
        error(:invalid_options, "flatten must be a boolean")

      operation == :flatten and
          not (Keyword.get(opts, :fields, :all) == :all or
                   (is_list(opts[:fields]) and Enum.all?(opts[:fields], &is_binary/1) and
                      length(Enum.uniq(opts[:fields])) == length(opts[:fields]))) ->
        error(:invalid_options, "fields must be :all or a list of unique field names")

      true ->
        {:ok, Map.new(opts)}
    end
  end

  @doc false
  @spec prepare_values(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_values(values) do
    if is_map(values) and not is_struct(values) and Enum.all?(Map.keys(values), &is_binary/1),
      do: {:ok, values},
      else:
        error(
          :invalid_form_value,
          "field values must be a map keyed by fully qualified string names"
        )
  end

  @doc false
  @spec prepare_write(map(), map(), map(), map(), :fill | :flatten) ::
          {:ok, [map()]} | {:error, {atom(), map()}}
  def prepare_write(context, form, values, opts, operation) do
    selected =
      case operation do
        :fill ->
          Map.keys(values)

        :flatten ->
          case Map.get(opts, :fields, :all) do
            :all -> Enum.map(form.fields, & &1.name)
            fields -> fields
          end
      end

    names = MapSet.new(Enum.map(form.fields, & &1.name))

    with :ok <- NativeElixirPdfUtilities.Validators.ModificationValidator.validate(context) do
      cond do
        Enum.any?(selected, &(not MapSet.member?(names, &1))) ->
          error(
            :unknown_form_field,
            "unknown field name; inspect Forms.fields/1 for available names"
          )

        true ->
          Enum.filter(form.fields, &(&1.name in selected))
          |> Enum.reduce_while({:ok, []}, fn field, {:ok, acc} ->
            value = Map.get(values, field.name, field.value)

            with :ok <- writable(field, operation),
                 {:ok, value} <-
                   if(operation == :flatten, do: {:ok, value}, else: field_value(field, value)),
                 {:ok, appearances} <- prepare_appearances(field, value, operation),
                 :ok <- button_appearances(context.document, field) do
              {:cont,
               {:ok, [Map.merge(field, %{new_value: value, appearances: appearances}) | acc]}}
            else
              {:error, {reason, diagnostic}} ->
                {:halt, {:error, {reason, Map.put(diagnostic, :source, field.name)}}}
            end
          end)
          |> case do
            {:ok, fields} -> {:ok, Enum.reverse(fields)}
            failure -> failure
          end
      end
    end
  end

  @doc false
  @spec capacity(map(), non_neg_integer()) :: :ok | {:error, {atom(), map()}}
  def capacity(context, count) do
    if context.document.trailer["Size"] + count <= Limits.get(:max_pdf_objects),
      do: :ok,
      else:
        error(:resource_limit_exceeded, "PDF object count cannot accommodate form appearances")
  end

  @doc false
  @spec prepare_flatten(map(), [map()]) :: {:ok, [map()]} | {:error, {atom(), map()}}
  def prepare_flatten(context, fields) do
    Enum.flat_map(fields, & &1.widgets)
    |> Enum.reduce_while({:ok, []}, fn widget, {:ok, acc} ->
      page = Enum.at(context.pages, widget.page - 1)

      with {:ok, resources} <- PdfValidator.dictionary(context.document, page.resources || %{}),
           {:ok, xobjects} <-
             PdfValidator.dictionary(context.document, Map.get(resources, "XObject", %{})),
           {:ok, contents} <- PdfValidator.content_references(context.document, page.dictionary),
           {:ok, annots} <-
             PdfValidator.resolve(context.document, Map.get(page.dictionary, "Annots", [])),
           {:ok, placement} <-
             flatten_placement(context.document, widget, %{
               widget: widget,
               page: page,
               resources: Map.put(resources, "XObject", xobjects),
               contents: contents,
               annots: annots
             }) do
        {:cont, {:ok, [placement | acc]}}
      else
        {:error, _} = failure -> {:halt, failure}
      end
    end)
    |> case do
      {:ok, placements} -> {:ok, Enum.reverse(placements)}
      failure -> failure
    end
  end

  # Hidden and NoView suppress screen artwork. Print does not affect this policy.
  defp flatten_placement(document, widget, placement) do
    if widget.hidden do
      {:ok, Map.put(placement, :stream, nil)}
    else
      normal = widget.ap["N"]

      normal =
        if widget.states == [],
          do: normal,
          else:
            widget.normal[
              case widget.dictionary["AS"] do
                {:name, name} -> name
                _ -> "Off"
              end
            ]

      with {:ok, stream} <- PdfValidator.validate_stream(document, normal),
           {:ok, bbox} <- PdfValidator.number_array(document, stream.dictionary["BBox"], 4),
           {:ok, matrix} <-
             PdfValidator.number_array(
               document,
               Map.get(stream.dictionary, "Matrix", [1, 0, 0, 1, 0, 0]),
               6
             ) do
        [a, b, c, d, e, f] = matrix
        [x0, y0, x1, y1] = bbox
        corners = for x <- [x0, x1], y <- [y0, y1], do: {a * x + c * y + e, b * x + d * y + f}
        xs = Enum.map(corners, &elem(&1, 0))
        ys = Enum.map(corners, &elem(&1, 1))
        min_x = Enum.min(xs)
        min_y = Enum.min(ys)
        width = Enum.max(xs) - min_x
        height = Enum.max(ys) - min_y
        [left, bottom, right, top] = widget.rect

        if width > 0 and height > 0 do
          sx = (right - left) / width
          sy = (top - bottom) / height

          {:ok,
           Map.merge(placement, %{
             stream: stream.ref,
             matrix: [sx, 0, 0, sy, left - sx * min_x, bottom - sy * min_y]
           })}
        else
          error(:unsupported_form, "appearance has a degenerate BBox or Matrix")
        end
      end
    end
  end

  defp page_widgets(context) do
    context.pages
    |> Enum.with_index(1)
    |> Enum.reduce_while({:ok, %{}}, fn {page, number}, {:ok, acc} ->
      with {:ok, annots} <-
             PdfValidator.resolve(context.document, Map.get(page.dictionary, "Annots", [])),
           true <- is_list(annots) do
        {:cont,
         {:ok,
          Enum.reduce(annots, acc, fn value, acc ->
            Map.update(acc, value, [number], &[number | &1])
          end)}}
      else
        false -> {:halt, error(:invalid_form, "page Annots must be an array")}
        {:error, _} = failure -> {:halt, failure}
      end
    end)
  end

  @spec walk(map(), list(), map(), String.t(), non_neg_integer(), walk_state()) ::
          {:ok, walk_state()} | {:error, {atom(), map()}}
  defp walk(document, refs, inherited, prefix, depth, state) do
    cond do
      depth > Limits.get(:max_pdf_form_depth) ->
        error(:resource_limit_exceeded, "field tree exceeds max_pdf_form_depth")

      true ->
        Enum.reduce_while(refs, {:ok, state}, fn ref, {:ok, state} ->
          cond do
            map_size(state.seen) >= Limits.get(:max_pdf_form_fields) ->
              {:halt, error(:resource_limit_exceeded, "field tree exceeds max_pdf_form_fields")}

            not match?({:ref, _}, ref) or Map.has_key?(state.seen, ref) ->
              {:halt,
               error(
                 :invalid_form,
                 "field tree requires unique indirect references without cycles"
               )}

            true ->
              with {:ok, dictionary} <- PdfValidator.dictionary(document, ref),
                   {:ok, partial} <- text(Map.get(dictionary, "T", {:string, ""})),
                   {:ok, kids} <- PdfValidator.resolve(document, Map.get(dictionary, "Kids", [])),
                   true <-
                     is_list(kids) and
                       length(kids) + 1 + map_size(state.seen) <=
                         Limits.get(:max_pdf_form_fields),
                   {:ok, child_dicts} <- resolve_children(document, kids) do
                name = Enum.reject([prefix, partial], &(&1 == "")) |> Enum.join(".")
                effective = Map.merge(inherited, Map.take(dictionary, @inherited))

                state = %{
                  state
                  | seen: Map.put(state.seen, ref, true),
                    nodes: Map.put(state.nodes, ref, Map.put(dictionary, "Kids", kids))
                }

                terminals =
                  Enum.all?(child_dicts, fn {_ref, child} ->
                    child["Subtype"] == {:name, "Widget"} and not Map.has_key?(child, "T")
                  end)

                result =
                  if kids != [] and not terminals do
                    walk(document, kids, effective, name, depth + 1, state)
                  else
                    widgets =
                      if dictionary["Subtype"] == {:name, "Widget"},
                        do: [{ref, dictionary}],
                        else: child_dicts

                    with true <-
                           length(Enum.uniq(kids)) == length(kids) and
                             Enum.all?(kids, &(not Map.has_key?(state.seen, &1))),
                         {:ok, field} <-
                           prepare_field(
                             document,
                             ref,
                             dictionary,
                             effective,
                             name,
                             widgets,
                             state.widgets
                           ) do
                      {:ok,
                       %{
                         state
                         | fields: [field | state.fields],
                           seen: Enum.reduce(kids, state.seen, &Map.put(&2, &1, true)),
                           nodes:
                             Enum.reduce(child_dicts, state.nodes, fn {ref, dict}, acc ->
                               Map.put(acc, ref, dict)
                             end)
                       }}
                    else
                      false ->
                        error(:invalid_form, "widgets must have unique references without cycles")

                      {:error, _} = failure ->
                        failure
                    end
                  end

                case result do
                  {:ok, state} -> {:cont, {:ok, state}}
                  failure -> {:halt, failure}
                end
              else
                false -> {:halt, error(:invalid_form, "field Kids must be an array")}
                {:error, _} = failure -> {:halt, failure}
              end
          end
        end)
    end
  end

  defp resolve_children(document, refs) do
    Enum.reduce_while(refs, {:ok, []}, fn ref, {:ok, children} ->
      case PdfValidator.dictionary(document, ref) do
        {:ok, dictionary} -> {:cont, {:ok, [{ref, dictionary} | children]}}
        failure -> {:halt, failure}
      end
    end)
    |> case do
      {:ok, children} -> {:ok, Enum.reverse(children)}
      failure -> failure
    end
  end

  defp prepare_field(document, ref, dictionary, effective, name, widget_dicts, page_widgets) do
    flags = Map.get(effective, "Ff", 0)

    with true <- name != "" and is_integer(flags) and flags >= 0,
         :ok <- field_metadata(name, effective),
         {:ok, choices} <- choices(document, Map.get(effective, "Opt", [])),
         {:ok, widgets} <- widgets(document, widget_dicts, page_widgets),
         type <-
           (case effective["FT"] do
              {:name, "Tx"} ->
                :text

              {:name, "Btn"} ->
                cond do
                  (flags &&& 65_536) != 0 -> :pushbutton
                  (flags &&& 32_768) != 0 -> :radio
                  true -> :checkbox
                end

              {:name, "Ch"} ->
                :choice

              {:name, "Sig"} ->
                :signature

              _ ->
                :unsupported
            end),
         {:ok, value} <- read_value(document, Map.get(effective, "V"), type) do
      value = if type == :checkbox, do: value not in [nil, "Off"], else: value

      {:ok,
       %{
         ref: ref,
         dictionary: dictionary,
         effective: effective,
         name: name,
         type: type,
         flags: flags,
         read_only: (flags &&& 1) != 0,
         value: value,
         choices: choices,
         widgets: widgets
       }}
    else
      false ->
        error(:invalid_form, "field requires a non-empty name and non-negative integer flags")

      {:error, _} = failure ->
        failure
    end
  end

  defp field_metadata(name, effective) do
    cond do
      byte_size(name) > Limits.get(:max_pdf_form_text_bytes) ->
        error(:resource_limit_exceeded, "field name exceeds max_pdf_form_text_bytes")

      Map.get(effective, "Q", 0) not in [0, 1, 2] ->
        error(:invalid_form, "field Q alignment must be 0, 1 or 2")

      true ->
        :ok
    end
  end

  defp widgets(document, dictionaries, page_widgets) do
    Enum.reduce_while(dictionaries, {:ok, []}, fn {ref, dictionary}, {:ok, acc} ->
      flags = Map.get(dictionary, "F", 0)

      with true <- is_integer(flags) and flags >= 0,
           true <- match?({:ref, _}, ref),
           [page] <- Map.get(page_widgets, ref, []),
           {:ok, rect} <- PdfValidator.number_array(document, dictionary["Rect"], 4),
           [left, bottom, right, top] <- rect,
           true <- right > left and top > bottom,
           {:ok, ap} <- PdfValidator.dictionary(document, Map.get(dictionary, "AP", %{})),
           {:ok, normal} <- PdfValidator.resolve(document, Map.get(ap, "N")),
           {:ok, mk} <- PdfValidator.dictionary(document, Map.get(dictionary, "MK", %{})) do
        states =
          case normal do
            normal when is_map(normal) ->
              if normal["Subtype"] == {:name, "Form"}, do: [], else: Map.keys(normal)

            _ ->
              []
          end

        widget = %{
          ref: ref,
          dictionary: dictionary,
          hidden: (flags &&& (2 ||| 32)) != 0,
          page: page,
          rect: rect,
          ap: ap,
          states: states,
          normal: normal,
          mk: mk
        }

        {:cont, {:ok, [widget | acc]}}
      else
        {:error, _} = failure ->
          {:halt, failure}

        _ ->
          {:halt,
           error(
             :invalid_form,
             "widget must have non-negative integer F flags and be an indirect annotation on one page with a positive Rect"
           )}
      end
    end)
    |> case do
      {:ok, widgets} -> {:ok, Enum.reverse(widgets)}
      failure -> failure
    end
  end

  defp choices(document, value) do
    with {:ok, values} <- PdfValidator.resolve(document, value), true <- is_list(values) do
      Enum.reduce_while(values, {:ok, []}, fn value, {:ok, choices} ->
        pair =
          case value do
            [export, display] -> {export, display}
            text -> {text, text}
          end

        with {:ok, export} <- text(elem(pair, 0)), {:ok, display} <- text(elem(pair, 1)) do
          {:cont, {:ok, choices ++ [%{value: export, label: display}]}}
        else
          failure -> {:halt, failure}
        end
      end)
    else
      false -> error(:invalid_form, "choice Opt must be an array")
      {:error, _} = failure -> failure
    end
  end

  defp read_value(document, value, type) do
    case {type, value} do
      {:signature, value} when is_nil(value) or is_map(value) ->
        {:ok, value}

      {:signature, {:ref, _} = value} ->
        # Signature inspection preserves the opaque value; modification is unsupported.
        {:ok, value}

      _ ->
        with {:ok, value} <- PdfValidator.resolve(document, value) do
          case {type, value} do
            {_, nil} ->
              {:ok, nil}

            {type, {:name, value}} when type in [:checkbox, :radio, :pushbutton, :unsupported] ->
              {:ok, value}

            {:choice, values} when is_list(values) ->
              Enum.reduce_while(values, {:ok, []}, fn value, {:ok, acc} ->
                with {:ok, value} <- PdfValidator.resolve(document, value),
                     {:ok, value} <- text(value) do
                  {:cont, {:ok, acc ++ [value]}}
                else
                  failure -> {:halt, failure}
                end
              end)

            {:unsupported, value} when is_map(value) ->
              {:ok, value}

            {type, value} when type in [:text, :choice, :pushbutton, :unsupported] ->
              text(value)

            _ ->
              error(:invalid_form, "field V value does not match its #{type} field type")
          end
        end
    end
  end

  defp text(value) do
    case value do
      {kind, bytes} when kind in [:string, :hex] ->
        case InfoCodec.decode_text(bytes) do
          {:ok, text} -> {:ok, text}
          :error -> error(:invalid_form, "form text encoding is invalid")
        end

      _ ->
        error(:invalid_form, "form text must be a PDF string")
    end
  end

  defp writable(field, operation) do
    cond do
      field.type not in [:text, :checkbox, :radio, :choice] ->
        error(:unsupported_form, "this field type is unsupported")

      operation == :fill and field.read_only ->
        error(:read_only_form_field, "read-only fields cannot be filled")

      field.type == :text and (field.flags &&& (8192 ||| 16_777_216 ||| 33_554_432)) != 0 ->
        error(:unsupported_form, "password, comb and rich-text fields are unsupported")

      field.type == :choice and (field.flags &&& 262_144) != 0 ->
        error(:unsupported_form, "editable combo boxes are unsupported")

      field.widgets == [] ->
        error(:unsupported_form, "field has no visible widgets")

      Map.has_key?(field.dictionary, "AA") ->
        error(:unsupported_form, "script-driven field actions are unsupported")

      Enum.any?(
        field.widgets,
        &(Map.has_key?(&1.dictionary, "AA") or Map.get(&1.mk, "R", 0) != 0)
      ) ->
        error(:unsupported_form, "widget actions and widget-specific rotation are unsupported")

      field.type in [:checkbox, :radio] and
          Enum.any?(field.widgets, &("Off" not in &1.states or length(&1.states) != 2)) ->
        error(:unsupported_form, "button widgets require Off and one named on-state appearance")

      field.type == :checkbox and
          length(Enum.uniq(Enum.map(field.widgets, &Enum.sort(&1.states)))) != 1 ->
        error(:unsupported_form, "checkbox widgets must share the same export state")

      true ->
        :ok
    end
  end

  defp button_appearances(document, field) do
    if field.type in [:checkbox, :radio] do
      Enum.flat_map(field.widgets, &Map.values(&1.normal))
      |> Enum.reduce_while(:ok, fn reference, :ok ->
        case PdfValidator.validate_stream(document, reference) do
          {:ok, _} -> {:cont, :ok}
          failure -> {:halt, failure}
        end
      end)
    else
      :ok
    end
  end

  defp field_value(field, value) do
    case field.type do
      :text ->
        value = if is_nil(value), do: "", else: value
        max_length = field.effective["MaxLen"]

        cond do
          not is_binary(value) or not String.valid?(value) ->
            error(:invalid_form_value, "text value must be valid UTF-8")

          byte_size(value) > Limits.get(:max_pdf_form_text_bytes) ->
            error(:resource_limit_exceeded, "text exceeds max_pdf_form_text_bytes")

          not is_nil(max_length) and (not is_integer(max_length) or max_length < 0) ->
            error(:invalid_form, "MaxLen must be a non-negative integer")

          is_integer(max_length) and String.length(value) > max_length ->
            error(:invalid_form_value, "text exceeds the field MaxLen")

          (field.flags &&& 4096) == 0 and String.contains?(value, ["\r", "\n"]) ->
            error(:invalid_form_value, "single-line text cannot contain line breaks")

          true ->
            {:ok, value}
        end

      :checkbox ->
        if is_boolean(value),
          do: {:ok, value},
          else: error(:invalid_form_value, "checkbox value must be a boolean")

      :radio ->
        states = Enum.flat_map(field.widgets, & &1.states) |> Enum.reject(&(&1 == "Off"))

        if is_binary(value) and value in states,
          do: {:ok, value},
          else: error(:invalid_form_value, "radio value must match a declared export state")

      :choice ->
        values = if is_list(value), do: value, else: [value]
        allowed = Enum.map(field.choices, & &1.value)

        cond do
          value == nil ->
            {:ok, if((field.flags &&& 2_097_152) != 0, do: [], else: "")}

          is_list(value) and (field.flags &&& 2_097_152) == 0 ->
            error(:invalid_form_value, "choice permits only one selection")

          length(values) != length(Enum.uniq(values)) or not Enum.all?(values, &(&1 in allowed)) ->
            error(:invalid_form_value, "choice values must match distinct declared export values")

          true ->
            {:ok, value}
        end
    end
  end

  defp prepare_appearances(field, value, operation) do
    if operation == :flatten or field.type in [:checkbox, :radio] do
      {:ok, []}
    else
      with {:ok, registry} <- Font.load_registry(system_font_discovery: false),
           {:ok, _, font} <- Font.resolve("DejaVu Sans", 400, :normal, registry) do
        field.widgets
        |> Enum.reduce_while({:ok, []}, fn widget, {:ok, pages} ->
          [left, bottom, right, top] = widget.rect
          width = right - left
          height = top - bottom

          text =
            case field.type do
              :text ->
                value

              :choice ->
                field.choices
                |> Enum.filter(&(&1.value in List.wrap(value)))
                |> Enum.map_join("\n", & &1.label)
            end

          lines = String.split(String.replace(text, "\r\n", "\n"), "\n")
          max_width = Enum.map(lines, &Font.text_width(&1, font, 1.0)) |> Enum.max(fn -> 0 end)

          size =
            min(
              12.0,
              min((height - 4) / max(length(lines) * 1.2, 1), (width - 4) / max(max_width, 0.001))
            )

          cond do
            size <= 0 ->
              {:halt, error(:unsupported_form, "widget is too small to draw an appearance")}

            not Enum.all?(lines, &Font.supports_text?(font, &1)) ->
              {:halt,
               error(
                 :unsupported_form,
                 "value contains glyphs unsupported by the bundled appearance font"
               )}

            true ->
              boxes =
                lines
                |> Enum.with_index()
                |> Enum.map(fn {line, index} ->
                  %{
                    type: :text,
                    text: line,
                    x:
                      case Map.get(field.effective, "Q", 0) do
                        0 -> 2.0
                        1 -> (width - Font.text_width(line, font, size)) / 2
                        2 -> width - 2.0 - Font.text_width(line, font, size)
                      end,
                    y: height - 2 - size - index * size * 1.2,
                    font_size: size,
                    font: Font.pdf_name(font),
                    font_face: font,
                    color: {0, 0, 0}
                  }
                end)

              {:cont, {:ok, [%{size: {width, height}, boxes: boxes} | pages]}}
          end
        end)
        |> case do
          {:ok, pages} -> {:ok, Enum.reverse(pages)}
          failure -> failure
        end
      else
        _ -> error(:unsupported_form, "bundled appearance font could not be loaded")
      end
    end
  end

  defp error(reason, message), do: Diagnostics.error(:forms, reason, message)
end
