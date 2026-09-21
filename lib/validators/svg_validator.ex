defmodule NativeElixirPdfUtilities.Validators.SvgValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc false
  @spec validate(term(), term(), HtmlValidator.image_budget() | nil) ::
          {:ok, keyword()} | {:error, {atom(), map()}}
  def validate(svg, raster_options, image_budget) do
    max_svg_bytes = Limits.get(:max_svg_bytes)

    case {svg, raster_options} do
      {svg, raster_options} when is_binary(svg) and is_list(raster_options) ->
        cond do
          byte_size(svg) > max_svg_bytes ->
            Diagnostics.error(
              :limits,
              :resource_limit_exceeded,
              "SVG source exceeds the #{max_svg_bytes}-byte limit"
            )

          not String.valid?(svg) ->
            Diagnostics.error(:style, :invalid_document, "SVG source must be valid UTF-8",
              source: "SVG"
            )

          not Keyword.keyword?(raster_options) or
              Enum.any?(Keyword.keys(raster_options), &(&1 not in [:width, :height])) ->
            Diagnostics.error(
              :style,
              :invalid_document,
              "SVG options accept only width and height",
              source: "SVG"
            )

          true ->
            with {:ok, attributes} <- validate_structure(svg),
                 {:ok, {intrinsic_width, intrinsic_height}} <-
                   svg_intrinsic_dimensions(attributes),
                 {:ok, {width, height}} <-
                   svg_raster_dimensions(raster_options, intrinsic_width, intrinsic_height),
                 :ok <- validate_svg_raster_budget(width, height),
                 :ok <-
                   (case image_budget do
                      nil ->
                        :ok

                      image_budget ->
                        HtmlValidator.reserve_decoded_image(image_budget, width, height, 4)
                    end) do
              {:ok, [width: width, height: height]}
            end
        end

      _ ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "SVG rasterization requires valid SVG source and dimension options"
        )
    end
  rescue
    ArithmeticError ->
      Diagnostics.error(
        :style,
        :invalid_document,
        "SVG dimensions exceed the supported numeric range; reduce dimensions or aspect ratio",
        source: "SVG"
      )
  end

  @doc false
  @spec conversion_result(term()) :: {:ok, binary()} | {:error, {atom(), map()}}
  def conversion_result(result) do
    case result do
      {:ok, png} when is_binary(png) ->
        if byte_size(png) <= Limits.get(:max_svg_output_bytes) do
          {:ok, png}
        else
          Diagnostics.error(
            :limits,
            :resource_limit_exceeded,
            "SVG conversion output exceeds max_svg_output_bytes",
            source: "SVG"
          )
        end

      {:error, message} when is_binary(message) ->
        Diagnostics.error(:style, :invalid_document, "SVG conversion failed: #{message}",
          source: "SVG"
        )

      _ ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "SVG converter returned an unexpected result",
          source: "SVG"
        )
    end
  end

  defp validate_structure(svg) do
    # External entities are disabled. Reject both DTD events because a bare
    # DOCTYPE emits only endDTD in xmerl.
    state = %{depth: 0, nodes: 0, paths: 0, filters: 0, references: 0, root: nil, styles: []}

    result =
      :xmerl_sax_parser.stream(svg, [
        :skip_external_dtd,
        :disallow_entities,
        {:external_entities, :none},
        {:event_fun, &validate_event/3},
        {:event_state, state}
      ])

    case result do
      {:ok, %{root: attributes}, rest} when is_map(attributes) ->
        if String.trim(rest) == "",
          do: {:ok, attributes},
          else:
            Diagnostics.error(
              :style,
              :invalid_document,
              "SVG has trailing content after its root",
              source: "SVG"
            )

      {:svg_validation, _location, error, _tags, _state} ->
        error

      {:fatal_error, {_file, _entity, line}, message, _tags, _state} ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "SVG XML is malformed: #{to_string(message)}",
          source: "SVG",
          line: line
        )
    end
  end

  defp validate_event(event, {_file, _entity, line}, state) do
    case event do
      {:startDTD, _name, _public, _system} ->
        reject("SVG resource references are not authorized; remove DTD/entity declarations", line)

      :endDTD ->
        reject("SVG resource references are not authorized; remove DTD/entity declarations", line)

      {:startElement, uri, local, _name, attributes} ->
        name = to_string(local)

        attrs =
          attributes
          |> Enum.filter(fn {uri, _prefix, _key, _value} -> uri == [] end)
          |> Map.new(fn {_uri, _prefix, key, value} ->
            {to_string(key), to_string(value)}
          end)

        if state.root == nil and
             (name != "svg" or to_string(uri) not in ["", "http://www.w3.org/2000/svg"]) do
          reject("SVG document must have an svg root", line)
        end

        if name in ["image", "feImage"] or
             Enum.any?(attributes, fn {_uri, _prefix, key, value} ->
               to_string(key) == "href" and
                 not String.starts_with?(String.trim(to_string(value)), "#")
             end) do
          reject(
            "SVG resource references are not authorized; use self-contained shapes and internal #id references",
            line
          )
        end

        Enum.each(attributes, fn {_uri, _prefix, key, value} ->
          if to_string(key) in [
               "style",
               "fill",
               "stroke",
               "filter",
               "clip-path",
               "mask",
               "marker",
               "marker-start",
               "marker-mid",
               "marker-end",
               "cursor"
             ] do
            validate_css_references(to_string(value), line)
          end
        end)

        paths = byte_size(Map.get(attrs, "d", "")) + byte_size(Map.get(attrs, "points", ""))

        state = %{
          state
          | depth: state.depth + 1,
            nodes: state.nodes + 1,
            styles: if(name == "style", do: [[] | state.styles], else: state.styles),
            paths: state.paths + paths,
            filters: state.filters + if(String.starts_with?(name, "fe"), do: 1, else: 0),
            references:
              state.references +
                Enum.count(attributes, fn {_uri, _prefix, key, _value} ->
                  to_string(key) == "href"
                end),
            root:
              state.root || Map.new(attrs, fn {key, value} -> {String.downcase(key), value} end)
        }

        for {key, count} <- [
              {:max_svg_nodes, state.nodes},
              {:max_svg_depth, state.depth},
              {:max_svg_path_bytes, state.paths},
              {:max_svg_filter_primitives, state.filters},
              {:max_svg_references, state.references}
            ] do
          if count > Limits.get(key) do
            throw(
              {:svg_validation,
               Diagnostics.error(:limits, :resource_limit_exceeded, "SVG exceeds #{key}",
                 source: "SVG",
                 line: line
               )}
            )
          end
        end

        state

      {:endElement, _uri, local, _name} ->
        styles =
          if to_string(local) == "style" do
            [chunks | rest] = state.styles
            validate_css_references(chunks |> Enum.reverse() |> IO.iodata_to_binary(), line)
            rest
          else
            state.styles
          end

        %{state | depth: state.depth - 1, styles: styles}

      {:characters, characters} ->
        case state.styles do
          [] -> state
          [chunks | rest] -> %{state | styles: [[to_string(characters) | chunks] | rest]}
        end

      _ ->
        state
    end
  end

  defp validate_css_references(value, line) do
    if String.contains?(value, "\\") or
         Regex.match?(~r/@import\b|url\(\s*["']?(?!\s*["']?#)[^)]/iu, value) do
      reject(
        "SVG external CSS resources and escaped resource references are not authorized",
        line
      )
    end
  end

  defp reject(message, line) do
    throw(
      {:svg_validation,
       Diagnostics.error(:style, :invalid_document, message, source: "SVG", line: line)}
    )
  end

  defp svg_intrinsic_dimensions(values) do
    with {:ok, view_box} <- svg_view_box(Map.get(values, "viewbox")),
         {:ok, width} <- svg_intrinsic_length(Map.get(values, "width"), view_box, 0),
         {:ok, height} <- svg_intrinsic_length(Map.get(values, "height"), view_box, 1) do
      {:ok, {width, height}}
    else
      _ ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "SVG source must contain valid intrinsic dimensions or a viewBox"
        )
    end
  end

  defp svg_view_box(value) do
    case value do
      nil ->
        {:ok, nil}

      value when is_binary(value) ->
        parts = String.split(value, ~r/[\s,]+/u, trim: true)

        case Enum.map(parts, &parse_svg_number/1) do
          [{:ok, _min_x}, {:ok, _min_y}, {:ok, width}, {:ok, height}]
          when width > 0 and height > 0 ->
            {:ok, {width, height}}

          _ ->
            :error
        end
    end
  end

  defp svg_intrinsic_length(value, view_box, index) do
    case value do
      nil ->
        svg_view_box_length(view_box, index)

      value when is_binary(value) ->
        normalized = String.trim(value)

        case Regex.run(
               ~r/^([+]?(?:\d+(?:\.\d*)?|\.\d+)(?:e[+-]?\d+)?)(px|pt|pc|mm|cm|in|q)?$/iu,
               normalized
             ) do
          [_, number] ->
            case parse_svg_number(number) do
              {:ok, number} when number > 0 -> {:ok, number}
              _ -> :error
            end

          [_, number, unit] ->
            case parse_svg_number(number) do
              {:ok, number} when number > 0 ->
                {:ok, number * svg_pixels_per_unit(String.downcase(unit))}

              _ ->
                :error
            end

          _ ->
            case Regex.run(~r/^([+]?(?:\d+(?:\.\d*)?|\.\d+))%$/u, normalized) do
              [_, percentage] ->
                with {:ok, percentage} when percentage > 0 <- parse_svg_number(percentage),
                     {:ok, base} <- svg_view_box_length(view_box, index) do
                  {:ok, base * percentage / 100.0}
                else
                  _ -> :error
                end

              _ ->
                :error
            end
        end
    end
  end

  defp parse_svg_number(value) do
    case Float.parse(value) do
      {number, ""} -> {:ok, number}
      _ -> :error
    end
  end

  defp svg_view_box_length(view_box, index) do
    case {view_box, index} do
      {{width, _height}, 0} -> {:ok, width}
      {{_width, height}, 1} -> {:ok, height}
      {nil, _index} -> :error
    end
  end

  defp svg_pixels_per_unit(unit) do
    case unit do
      "px" -> 1.0
      "pt" -> 96.0 / 72.0
      "pc" -> 16.0
      "mm" -> 96.0 / 25.4
      "cm" -> 96.0 / 2.54
      "in" -> 96.0
      "q" -> 96.0 / 101.6
    end
  end

  defp svg_raster_dimensions(raster_options, intrinsic_width, intrinsic_height) do
    width = Keyword.get(raster_options, :width)
    height = Keyword.get(raster_options, :height)

    case {width, height} do
      {width, height}
      when is_integer(width) and width > 0 and is_integer(height) and height > 0 ->
        {:ok, {width, height}}

      {width, nil} when is_integer(width) and width > 0 ->
        {:ok, {width, max(round(width * intrinsic_height / intrinsic_width), 1)}}

      {nil, height} when is_integer(height) and height > 0 ->
        {:ok, {max(round(height * intrinsic_width / intrinsic_height), 1), height}}

      {nil, nil} ->
        {:ok, {max(round(intrinsic_width), 1), max(round(intrinsic_height), 1)}}

      _ ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "SVG raster dimensions must be positive integers"
        )
    end
  end

  defp validate_svg_raster_budget(width, height) do
    max_dimension = Limits.get(:max_svg_raster_dimension)
    max_pixels = Limits.get(:max_svg_raster_pixels)

    cond do
      width > max_dimension or height > max_dimension ->
        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "SVG raster dimensions #{width}x#{height} exceed the #{max_dimension}-pixel per-axis limit"
        )

      width * height > max_pixels ->
        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "SVG raster dimensions #{width}x#{height} exceed the #{max_pixels}-pixel limit"
        )

      width > 0x7FFFFFFF or height > 0x7FFFFFFF ->
        # PNG dimensions and the renderer's signed geometry are fixed format bounds.
        Diagnostics.error(
          :style,
          :invalid_document,
          "SVG raster dimensions exceed the PNG signed 31-bit dimension range",
          source: "SVG"
        )

      true ->
        :ok
    end
  end
end
