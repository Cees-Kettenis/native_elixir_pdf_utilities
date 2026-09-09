defmodule NativeElixirPdfUtilities.Validators.StampValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.PdfValidator
  alias NativeElixirPdfUtilities.Validators.TransformValidator

  @positions [
    :top_left,
    :top_center,
    :top_right,
    :center_left,
    :center,
    :center_right,
    :bottom_left,
    :bottom_center,
    :bottom_right
  ]
  @text_option_keys [
    :pages,
    :position,
    :margin,
    :font,
    :fonts,
    :font_weight,
    :font_style,
    :size,
    :color,
    :opacity,
    :rotation,
    :system_font_discovery
  ]
  @page_number_option_keys @text_option_keys ++ [:format, :numbering]
  @overlay_option_keys [:pages, :overlay_pages, :fit, :opacity]

  @doc false
  @spec prepare_text(PdfValidator.context(), term(), term(), :text | :watermark) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_text(context, text, options, kind) do
    defaults =
      case kind do
        :text ->
          %{position: :center, margin: 24.0, size: 12.0, opacity: 1.0, rotation: 0.0}

        :watermark ->
          %{position: :center, margin: 24.0, size: :auto, opacity: 0.15, rotation: 45.0}
      end

    with {:ok, text} <- validate_text(text),
         {:ok, options} <- normalize_options(options, @text_option_keys),
         {:ok, settings} <- text_settings(options, defaults),
         {:ok, target_pages} <- selected_target_pages(context, Map.get(options, :pages, :all)),
         {:ok, font_face} <- selected_font(options, [text]),
         {:ok, appearances} <-
           text_appearances(
             target_pages,
             List.duplicate(text, length(target_pages)),
             font_face,
             settings
           ) do
      {:ok, %{target_pages: target_pages, appearances: appearances}}
    end
  end

  @doc false
  @spec prepare_page_numbers(PdfValidator.context(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_page_numbers(context, options) do
    defaults = %{position: :bottom_center, margin: 24.0, size: 9.0, opacity: 1.0, rotation: 0.0}

    with {:ok, options} <- normalize_options(options, @page_number_option_keys),
         {:ok, format} <-
           page_number_format(Map.get(options, :format, "Page {{page}} of {{pages}}")),
         {:ok, numbering} <- numbering_mode(Map.get(options, :numbering, :document)),
         {:ok, settings} <- text_settings(options, defaults),
         {:ok, target_pages} <- selected_target_pages(context, Map.get(options, :pages, :all)),
         texts = page_number_texts(target_pages, length(context.pages), format, numbering),
         :ok <- validate_total_text_bytes(texts),
         {:ok, font_face} <- selected_font(options, texts),
         {:ok, appearances} <- text_appearances(target_pages, texts, font_face, settings) do
      {:ok, %{target_pages: target_pages, appearances: appearances}}
    end
  end

  @doc false
  @spec prepare_overlay(PdfValidator.context(), PdfValidator.context(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_overlay(target_context, overlay_context, options) do
    with {:ok, options} <- normalize_options(options, @overlay_option_keys),
         {:ok, fit} <- fit_mode(Map.get(options, :fit, :exact)),
         {:ok, opacity} <- opacity(Map.get(options, :opacity, 1.0)),
         {:ok, target_pages} <-
           selected_target_pages(target_context, Map.get(options, :pages, :all)),
         {:ok, overlay_page_numbers} <-
           overlay_page_numbers(
             Map.get(options, :overlay_pages, {:repeat, 1}),
             length(overlay_context.pages),
             length(target_pages)
           ) do
      prepare_overlay_pages(
        target_context,
        target_pages,
        overlay_context,
        overlay_page_numbers,
        fit,
        opacity,
        List.duplicate(nil, length(target_pages))
      )
    end
  end

  @doc false
  @spec prepare_generated_overlay(PdfValidator.context(), PdfValidator.context(), map()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_generated_overlay(target_context, overlay_context, text_plan) do
    case text_plan do
      %{target_pages: target_pages, appearances: appearances}
      when is_list(target_pages) and is_list(appearances) and
             length(target_pages) == length(appearances) ->
        transforms =
          Enum.map(appearances, fn appearance ->
            %{rotation: appearance.rotation, pivot: appearance.pivot}
          end)

        prepare_overlay_pages(
          target_context,
          target_pages,
          overlay_context,
          page_numbers(length(appearances)),
          :exact,
          1.0,
          transforms
        )

      _ ->
        error(:validation, :invalid_pdf_input, "prepared text stamp is malformed")
    end
  end

  defp validate_text(text) do
    cond do
      not is_binary(text) or not String.valid?(text) or String.trim(text) == "" ->
        error(:stamp, :invalid_stamp, "stamp text must be a non-empty UTF-8 string")

      byte_size(text) > Limits.get(:max_stamp_text_bytes) ->
        error(:limits, :resource_limit_exceeded, "stamp text exceeds the byte limit")

      true ->
        {:ok, text}
    end
  end

  defp validate_total_text_bytes(texts) do
    case Enum.reduce(texts, 0, fn text, total -> total + byte_size(text) end) <=
           Limits.get(:max_stamp_text_bytes) do
      true -> :ok
      false -> error(:limits, :resource_limit_exceeded, "stamp text exceeds the byte limit")
    end
  end

  defp normalize_options(options, allowed_keys) do
    case options do
      options when is_list(options) ->
        case Keyword.keyword?(options) do
          true ->
            keys = Keyword.keys(options)

            cond do
              length(keys) != length(Enum.uniq(keys)) ->
                error(:options, :invalid_options, "stamp options must not repeat keys")

              Enum.any?(keys, &(&1 not in allowed_keys)) ->
                error(:options, :invalid_options, "stamp options contain an unsupported key")

              true ->
                {:ok, Map.new(options)}
            end

          false ->
            error(:options, :invalid_options, "stamp options must be a keyword list")
        end

      _ ->
        error(:options, :invalid_options, "stamp options must be a keyword list")
    end
  end

  defp text_settings(options, defaults) do
    with {:ok, position} <- position(Map.get(options, :position, defaults.position)),
         {:ok, margin} <- nonnegative_number(Map.get(options, :margin, defaults.margin), "margin"),
         {:ok, size} <- font_size(Map.get(options, :size, defaults.size)),
         {:ok, color} <- color(Map.get(options, :color, {0.25, 0.25, 0.25})),
         {:ok, opacity} <- opacity(Map.get(options, :opacity, defaults.opacity)),
         {:ok, rotation} <- number(Map.get(options, :rotation, defaults.rotation), "rotation") do
      {:ok,
       %{
         position: position,
         margin: margin,
         size: size,
         color: color,
         opacity: opacity,
         rotation: rotation
       }}
    end
  end

  defp position(value) do
    case value do
      value when value in @positions -> {:ok, value}
      {x, y} when is_number(x) and is_number(y) -> {:ok, {x * 1.0, y * 1.0}}
      _ -> error(:options, :invalid_options, "position must be a supported anchor or {x, y}")
    end
  end

  defp font_size(value) do
    case value do
      :auto -> {:ok, :auto}
      value when is_number(value) and value > 0 -> {:ok, value * 1.0}
      _ -> error(:options, :invalid_options, "font size must be :auto or a positive number")
    end
  end

  defp color(value) do
    case value do
      {red, green, blue} ->
        case Enum.all?([red, green, blue], &(is_number(&1) and &1 >= 0 and &1 <= 1)) do
          true -> {:ok, {red * 1.0, green * 1.0, blue * 1.0}}
          false -> invalid_color()
        end

      _ ->
        invalid_color()
    end
  end

  defp invalid_color do
    error(:options, :invalid_options, "color must be an RGB tuple with channels from 0 to 1")
  end

  defp opacity(value) do
    case is_number(value) and value >= 0 and value <= 1 do
      true -> {:ok, value * 1.0}
      false -> error(:options, :invalid_options, "opacity must be a number from 0 to 1")
    end
  end

  defp number(value, label) do
    case is_number(value) do
      true -> {:ok, value * 1.0}
      false -> error(:options, :invalid_options, "#{label} must be a number")
    end
  end

  defp nonnegative_number(value, label) do
    case is_number(value) and value >= 0 do
      true -> {:ok, value * 1.0}
      false -> error(:options, :invalid_options, "#{label} must be a non-negative number")
    end
  end

  defp selected_font(options, texts) do
    family = Map.get(options, :font, "DejaVu Sans")
    fonts = Map.get(options, :fonts, [])
    weight = Map.get(options, :font_weight, 400)
    style = Map.get(options, :font_style, :normal)
    discovery = Map.get(options, :system_font_discovery, false)

    case is_binary(family) and String.trim(family) != "" and is_boolean(discovery) and
           is_number(weight) and weight >= 100 and weight <= 900 and
           style in [:normal, :italic] do
      false ->
        error(:font, :invalid_options, "stamp font configuration is invalid")

      true ->
        with {:ok, registry} <-
               Font.load_registry(fonts: fonts, system_font_discovery: discovery),
             [font_face | _rest] <- Font.requested_faces(family, weight, style, registry),
             true <- Enum.all?(texts, &Font.supports_text?(font_face, &1)) do
          {:ok, font_face}
        else
          false ->
            error(
              :font,
              :unsupported_glyph,
              "selected stamp font cannot encode the requested text"
            )

          _ ->
            error(:font, :invalid_options, "selected stamp font is unavailable or invalid")
        end
    end
  end

  defp selected_target_pages(context, selection) do
    case context do
      %{document: document, pages: pages} when is_map(document) and is_list(pages) ->
        with {:ok, page_numbers} <- selected_page_numbers(selection, length(pages)) do
          indexed =
            pages |> Enum.with_index(1) |> Map.new(fn {page, number} -> {number, page} end)

          page_numbers
          |> Enum.reduce_while({:ok, []}, fn page_number, {:ok, prepared} ->
            page = Map.fetch!(indexed, page_number)

            case prepare_page(document, page, page_number) do
              {:ok, page} -> {:cont, {:ok, [page | prepared]}}
              {:error, _error} = page_error -> {:halt, page_error}
            end
          end)
          |> case do
            {:ok, prepared} -> {:ok, Enum.reverse(prepared)}
            {:error, _error} = page_error -> page_error
          end
        end

      _ ->
        error(:validation, :invalid_pdf_input, "shared PDF validation context is malformed")
    end
  end

  defp selected_page_numbers(selection, page_count) do
    case selection do
      :all ->
        case page_count do
          0 ->
            error(:page_selection, :invalid_page_selection, "stamping requires at least one page")

          page_count ->
            {:ok, page_numbers(page_count)}
        end

      selection ->
        TransformValidator.expand_page_selection(selection, page_count, false)
    end
  end

  defp prepare_page(document, page, page_number) do
    crop_value = page.crop_box || page.media_box

    with {:ok, [left, bottom, right, top] = crop_box} <-
           PdfValidator.number_array(document, crop_value, 4),
         true <- right > left and top > bottom,
         {:ok, rotation} <- resolved_rotation(document, page.rotate),
         {:ok, user_unit} <- resolved_user_unit(document, page.dictionary),
         {:ok, resources} <- resolved_resources(document, page.resources),
         {:ok, contents} <- content_references(page.dictionary) do
      {width, height} =
        case rotation in [90, 270] do
          true -> {(top - bottom) * user_unit, (right - left) * user_unit}
          false -> {(right - left) * user_unit, (top - bottom) * user_unit}
        end

      normalization = normalization_matrix(crop_box, rotation, user_unit)

      {:ok,
       %{
         page_number: page_number,
         ref: page.ref,
         dictionary: page.dictionary,
         resources: resources,
         contents: contents,
         crop_box: crop_box,
         width: width * 1.0,
         height: height * 1.0,
         normalization: normalization,
         inverse_normalization: inverse_matrix(normalization)
       }}
    else
      false -> page_error(page_number, "has an invalid effective page box")
      _error -> page_error(page_number, "has malformed stamping geometry or resources")
    end
  end

  defp resolved_rotation(document, value) do
    case PdfValidator.resolve(document, value || 0) do
      {:ok, rotation} when is_integer(rotation) and rem(rotation, 90) == 0 ->
        {:ok, Integer.mod(rotation, 360)}

      _ ->
        :error
    end
  end

  defp resolved_user_unit(document, dictionary) do
    case PdfValidator.resolve(document, Map.get(dictionary, "UserUnit", 1)) do
      {:ok, value} when is_number(value) and value > 0 -> {:ok, value * 1.0}
      _ -> :error
    end
  end

  defp resolved_resources(document, value) do
    case PdfValidator.resolve(document, value) do
      {:ok, nil} ->
        {:ok, %{}}

      {:ok, resources} when is_map(resources) ->
        with {:ok, resources} <- materialize_resource_category(document, resources, "XObject"),
             {:ok, resources} <- materialize_resource_category(document, resources, "ExtGState") do
          {:ok, resources}
        end

      _ ->
        :error
    end
  end

  defp materialize_resource_category(document, resources, category) do
    case Map.get(resources, category) do
      nil ->
        {:ok, resources}

      value ->
        case PdfValidator.dictionary(document, value) do
          {:ok, dictionary} -> {:ok, Map.put(resources, category, dictionary)}
          {:error, _error} -> :error
        end
    end
  end

  defp content_references(dictionary) do
    case Map.get(dictionary, "Contents") do
      nil ->
        {:ok, []}

      {:ref, _ref} = reference ->
        {:ok, [reference]}

      references when is_list(references) ->
        case Enum.all?(references, &match?({:ref, _ref}, &1)) do
          true -> {:ok, references}
          false -> :error
        end

      _ ->
        :error
    end
  end

  defp text_appearances(target_pages, texts, font_face, settings) do
    appearances =
      Enum.zip(target_pages, texts)
      |> Enum.map(fn {page, text} -> text_appearance(page, text, font_face, settings) end)

    {:ok, appearances}
  end

  defp text_appearance(page, text, font_face, settings) do
    size = resolved_stamp_size(settings.size, page, text, font_face)
    width = Font.text_width(text, font_face, size)
    {x, y, pivot} = text_position(settings.position, settings.margin, page, width, size)
    {red, green, blue} = settings.color

    box = %{
      type: :text,
      text: text,
      x: x,
      y: y,
      font_size: size,
      font: Font.pdf_name(font_face),
      font_face: font_face,
      color: {red, green, blue, settings.opacity}
    }

    %{
      page: %{size: {page.width, page.height}, boxes: [box]},
      rotation: settings.rotation,
      pivot: pivot
    }
  end

  defp resolved_stamp_size(size, page, text, font_face) do
    case size do
      :auto ->
        width_at_one = max(Font.text_width(text, font_face, 1.0), 0.001)
        min(min(page.width, page.height) * 0.15, page.width * 0.8 / width_at_one)

      size ->
        size
    end
  end

  defp text_position(position, margin, page, text_width, size) do
    {horizontal, vertical, anchor_x, anchor_y} =
      case position do
        :top_left -> {:left, :top, margin, margin}
        :top_center -> {:center, :top, page.width / 2, margin}
        :top_right -> {:right, :top, page.width - margin, margin}
        :center_left -> {:left, :center, margin, page.height / 2}
        :center -> {:center, :center, page.width / 2, page.height / 2}
        :center_right -> {:right, :center, page.width - margin, page.height / 2}
        :bottom_left -> {:left, :bottom, margin, page.height - margin}
        :bottom_center -> {:center, :bottom, page.width / 2, page.height - margin}
        :bottom_right -> {:right, :bottom, page.width - margin, page.height - margin}
        {x, y} -> {:left, :top, x, y}
      end

    x =
      case horizontal do
        :left -> anchor_x
        :center -> anchor_x - text_width / 2
        :right -> anchor_x - text_width
      end

    y =
      case vertical do
        :top -> page.height - anchor_y - size
        :center -> page.height - anchor_y - size / 2
        :bottom -> page.height - anchor_y
      end

    {x, y, {x + text_width / 2, y + size / 2}}
  end

  defp page_number_format(format) do
    case is_binary(format) and String.valid?(format) and format != "" and
           Regex.match?(~r/{{(?:page|pages)}}/, format) do
      true ->
        validate_text(format)

      false ->
        error(:options, :invalid_options, "page-number format must contain {{page}} or {{pages}}")
    end
  end

  defp numbering_mode(mode) do
    case mode in [:document, :selection] do
      true -> {:ok, mode}
      false -> error(:options, :invalid_options, "numbering must be :document or :selection")
    end
  end

  defp page_number_texts(target_pages, document_count, format, numbering) do
    target_pages
    |> Enum.with_index(1)
    |> Enum.map(fn {target, selection_number} ->
      {page, pages} =
        case numbering do
          :document -> {target.page_number, document_count}
          :selection -> {selection_number, length(target_pages)}
        end

      format
      |> String.replace("{{page}}", Integer.to_string(page))
      |> String.replace("{{pages}}", Integer.to_string(pages))
    end)
  end

  defp fit_mode(mode) do
    case mode in [:exact, :contain, :cover, :stretch] do
      true ->
        {:ok, mode}

      false ->
        error(:options, :invalid_options, "fit must be :exact, :contain, :cover, or :stretch")
    end
  end

  defp overlay_page_numbers(mapping, overlay_count, target_count) do
    case mapping do
      :match when overlay_count == target_count and target_count > 0 ->
        {:ok, page_numbers(target_count)}

      :match ->
        error(:overlay, :invalid_stamp, "matched overlay and target page counts must be equal")

      {:repeat, page} when is_integer(page) and page > 0 and page <= overlay_count ->
        {:ok, List.duplicate(page, target_count)}

      {:repeat, _page} ->
        error(:overlay, :page_out_of_bounds, "repeated overlay page is outside the overlay PDF")

      _ ->
        error(:options, :invalid_options, "overlay_pages must be :match or {:repeat, page}")
    end
  end

  defp prepare_overlay_pages(
         target_context,
         target_pages,
         overlay_context,
         overlay_page_numbers,
         fit,
         opacity,
         transforms
       ) do
    with {:ok, source_pages} <- source_pages(overlay_context, overlay_page_numbers),
         :ok <- validate_fit(target_pages, source_pages, fit),
         {:ok, copied_objects} <- copied_resource_objects(overlay_context, source_pages),
         :ok <-
           validate_object_capacity(
             target_context,
             copied_objects,
             source_pages,
             target_pages,
             opacity
           ) do
      placements =
        Enum.zip([target_pages, source_pages, transforms])
        |> Enum.map(fn {target, source, transform} ->
          %{target: target, source: source, transform: transform, fit: fit}
        end)

      {:ok,
       %{
         target_context: target_context,
         overlay_context: overlay_context,
         placements: placements,
         copied_objects: copied_objects,
         opacity: opacity
       }}
    end
  end

  defp source_pages(context, requested_numbers) do
    indexed =
      context.pages |> Enum.with_index(1) |> Map.new(fn {page, number} -> {number, page} end)

    requested_numbers
    |> Enum.uniq()
    |> Enum.reduce_while({:ok, %{}, 0}, fn page_number, {:ok, pages, decoded_bytes} ->
      page = Map.fetch!(indexed, page_number)

      with {:ok, prepared} <- prepare_page(context.document, page, page_number),
           {:ok, content} <- decode_page_contents(context.document, prepared.contents),
           decoded_bytes = decoded_bytes + byte_size(content),
           true <- decoded_bytes <= Limits.get(:max_stamp_decoded_content_bytes),
           {:ok, group} <- resolved_group(context.document, page.dictionary) do
        prepared = Map.merge(prepared, %{content: content, group: group})
        {:cont, {:ok, Map.put(pages, page_number, prepared), decoded_bytes}}
      else
        false ->
          {:halt,
           error(:limits, :resource_limit_exceeded, "decoded overlay content exceeds the limit")}

        {:error, _error} = page_error ->
          {:halt, page_error}
      end
    end)
    |> case do
      {:ok, pages, _decoded_bytes} -> {:ok, Enum.map(requested_numbers, &Map.fetch!(pages, &1))}
      {:error, _error} = page_error -> page_error
    end
  end

  defp decode_page_contents(document, references) do
    references
    |> Enum.reduce_while({:ok, []}, fn reference, {:ok, parts} ->
      case Reader.decoded_stream(document, reference) do
        {:ok, content} -> {:cont, {:ok, [content | parts]}}
        {:error, _error} = stream_error -> {:halt, stream_error}
      end
    end)
    |> case do
      {:ok, parts} ->
        {:ok, parts |> Enum.reverse() |> Enum.intersperse("\n") |> IO.iodata_to_binary()}

      {:error, _error} = stream_error ->
        stream_error
    end
  end

  defp resolved_group(document, dictionary) do
    case Map.get(dictionary, "Group") do
      nil ->
        {:ok, nil}

      group ->
        case PdfValidator.dictionary(document, group) do
          {:ok, group} ->
            {:ok, group}

          {:error, _error} ->
            error(:overlay, :invalid_pdf_input, "overlay page Group is malformed")
        end
    end
  end

  defp copied_resource_objects(context, source_pages) do
    page_refs = Map.new(context.pages, &{&1.ref, true})
    roots = Enum.flat_map(source_pages, &[&1.resources, &1.group])

    case collect_references(roots, context.document.objects, page_refs, %{}) do
      {:ok, references} ->
        objects =
          references
          |> Map.keys()
          |> Enum.map(fn reference ->
            {reference, Map.fetch!(context.document.objects, reference)}
          end)
          |> Enum.sort_by(fn {{object, generation}, _value} -> {object, generation} end)

        {:ok, objects}

      {:error, _error} = reference_error ->
        reference_error
    end
  end

  defp collect_references(values, objects, page_refs, seen) do
    references = Enum.flat_map(values, &value_references/1)

    Enum.reduce_while(references, {:ok, seen}, fn reference, {:ok, seen} ->
      cond do
        Map.has_key?(seen, reference) ->
          {:cont, {:ok, seen}}

        Map.has_key?(page_refs, reference) ->
          {:halt,
           error(:overlay, :unsupported_pdf_feature, "overlay resources refer back to a PDF page")}

        true ->
          case Map.fetch(objects, reference) do
            {:ok, object} ->
              value =
                case object do
                  %{stream: stream, value: value} when is_binary(stream) and is_map(value) ->
                    Map.delete(value, "Length")

                  %{value: value} ->
                    value
                end

              case collect_references([value], objects, page_refs, Map.put(seen, reference, true)) do
                {:ok, seen} -> {:cont, {:ok, seen}}
                {:error, _error} = reference_error -> {:halt, reference_error}
              end

            :error ->
              {:halt,
               error(:overlay, :invalid_pdf_input, "overlay resource refers to a missing object")}
          end
      end
    end)
  end

  defp value_references(value) do
    case value do
      {:ref, reference} ->
        [reference]

      values when is_list(values) ->
        Enum.flat_map(values, &value_references/1)

      dictionary when is_map(dictionary) ->
        dictionary |> Map.values() |> Enum.flat_map(&value_references/1)

      _ ->
        []
    end
  end

  defp validate_fit(target_pages, source_pages, fit) do
    case fit do
      :exact ->
        case Enum.zip(target_pages, source_pages)
             |> Enum.all?(fn {target, source} ->
               abs(target.width - source.width) < 0.01 and
                 abs(target.height - source.height) < 0.01
             end) do
          true ->
            :ok

          false ->
            error(
              :geometry,
              :invalid_stamp,
              "exact overlay pages must match target page dimensions"
            )
        end

      _ ->
        :ok
    end
  end

  defp validate_object_capacity(context, copied_objects, source_pages, target_pages, opacity) do
    size = context.document.trailer["Size"]
    unique_source_pages = Enum.uniq_by(source_pages, & &1.page_number)
    generated = length(copied_objects) + length(unique_source_pages) + length(target_pages)
    generated = if opacity < 1.0, do: generated + 1, else: generated

    case is_integer(size) and size + generated <= Limits.get(:max_pdf_objects) + 1 do
      true ->
        :ok

      false ->
        error(:limits, :resource_limit_exceeded, "PDF object count cannot accommodate the stamp")
    end
  end

  defp normalization_matrix([left, bottom, right, top], rotation, user_unit) do
    case rotation do
      0 -> [user_unit, 0.0, 0.0, user_unit, -left * user_unit, -bottom * user_unit]
      90 -> [0.0, -user_unit, user_unit, 0.0, -bottom * user_unit, right * user_unit]
      180 -> [-user_unit, 0.0, 0.0, -user_unit, right * user_unit, top * user_unit]
      270 -> [0.0, user_unit, -user_unit, 0.0, top * user_unit, -left * user_unit]
    end
  end

  defp inverse_matrix([a, b, c, d, e, f]) do
    determinant = a * d - b * c

    [
      d / determinant,
      -b / determinant,
      -c / determinant,
      a / determinant,
      (c * f - d * e) / determinant,
      (b * e - a * f) / determinant
    ]
  end

  defp page_numbers(count) do
    case count do
      0 -> []
      count -> Enum.to_list(1..count)
    end
  end

  defp page_error(page_number, message) do
    error(:geometry, :invalid_pdf_input, "page #{page_number} #{message}")
  end

  defp error(stage, reason, message) do
    Diagnostics.error(stage, reason, message, operation: :stamp, module: __MODULE__)
  end
end
