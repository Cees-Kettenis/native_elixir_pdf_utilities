defmodule NativeElixirPdfUtilities.Validators.SvgFontValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{SvgScene, SvgRasterizer}

  alias NativeElixirPdfUtilities.Validators.{
    ColrFontValidator,
    FontValidator,
    HtmlValidator,
    SvgValidator,
    SvgImageValidator,
    SvgReferenceValidator,
    SvgSceneValidator
  }

  @doc "Selects one static SVG glyph, preserving its document coordinate system, palette and actual vector bounds."
  @spec prepare(map(), non_neg_integer(), tuple()) ::
          {:ok, map() | nil | :empty} | {:error, {atom(), map()}}
  def prepare(font, glyph, current) do
    FontValidator.with_budget(fn ->
      try do
        with {:ok, _, directory} <- FontValidator.prepare_table_directory(font.data) do
          case Map.get(directory, "SVG ") do
            nil ->
              {:ok, nil}

            {offset, size} ->
              data = binary_part(font.data, offset, size)
              <<version::16, index::32, reserved::32>> = read(data, 0, 10)

              unless version == 0 and reserved == 0 and index >= 10,
                do:
                  invalid(
                    "SVG font header version, directory offset or reserved bits are invalid"
                  )

              <<count::16>> = read(data, index, 2)
              FontValidator.reserve(:max_font_color_work, count)

              records =
                for <<first::16, last::16, offset::32,
                      size::32 <- read(data, index + 2, count * 12)>>,
                    do: {first, last, offset, size}

              {_previous, selected} =
                Enum.reduce(records, {-1, nil}, fn {first, last, relative, size},
                                                   {previous, selected} ->
                  unless first > previous and first <= last and last < length(font.widths) and
                           relative >= 2 + count * 12 and size > 0,
                         do:
                           invalid(
                             "SVG glyph ranges overlap or document bytes alias the directory"
                           )

                  bytes = read(data, index + relative, size)
                  {last, if(glyph >= first and glyph <= last, do: bytes, else: selected)}
                end)

              if selected == nil do
                {:ok, nil}
              else
                source = decode(selected)

                with {:ok, summary} <- SvgValidator.validate_extraction(source),
                     {:ok, root} <- SvgSceneValidator.extraction_root(summary.document),
                     :ok <- SvgSceneValidator.validate_source_tree(root),
                     {:ok, palette} <- palette(font, directory),
                     properties =
                       palette
                       |> Tuple.to_list()
                       |> Enum.with_index()
                       |> Map.new(fn {{r, g, b, a}, i} ->
                         {"--color#{i}", "rgba(#{r * 255},#{g * 255},#{b * 255},#{a})"}
                       end),
                     {:ok, root} <- prepare_tree(root, properties, HtmlValidator.new_css_budget()),
                     {:ok, root, resources} <-
                       prepare_images(root, HtmlValidator.new_image_budget()) do
                  id = "glyph#{glyph}"

                  if Map.has_key?(summary.ids, id) do
                    selected_id = "native-selected-glyph"

                    nodes =
                      if root.attributes["id"] == id,
                        do: root.children,
                        else: [
                          element("defs", %{}, root.children),
                          element("use", %{"href" => "#" <> id}, [])
                        ]

                    {r, g, b, a} = current
                    color = "rgba(#{r * 255},#{g * 255},#{b * 255},#{a})"

                    attributes =
                      root.attributes
                      |> Map.delete("id")
                      |> Map.put_new("width", to_string(font.units_per_em))
                      |> Map.put_new("height", to_string(font.units_per_em))
                      |> Map.put("overflow", "visible")
                      |> Map.put("color", color)

                    source =
                      SvgScene.authored_source(%{
                        root
                        | attributes: attributes,
                          children: [element("g", %{"id" => selected_id}, nodes)]
                      })

                    context = %{mode: :inline, resources: resources}

                    with {:ok, bounds} <-
                           SvgRasterizer.measure_bounds(source, selected_id, context) do
                      if bounds == nil do
                        {:ok, :empty}
                      else
                        {left, top, right, bottom} = bounds
                        # Query bounds already include the root viewBox mapping.
                        # Retain that mapping in a nested em-square SVG while
                        # the outer canvas selects its complete unclipped ink.
                        nested =
                          SvgScene.authored_source(%{
                            root
                            | attributes: attributes,
                              children: [element("g", %{"id" => selected_id}, nodes)]
                          })

                        wrapped =
                          "<svg xmlns='http://www.w3.org/2000/svg' viewBox='#{left} #{top} #{right - left} #{bottom - top}' width='#{right - left}' height='#{bottom - top}'>#{nested}</svg>"

                        {:ok,
                         %{
                           source: wrapped,
                           bounds: {left, -bottom, right, -top},
                           context: context
                         }}
                      end
                    end
                  else
                    {:ok, nil}
                  end
                end
              end
          end
        end
      catch
        {:svg_font_error, error} -> error
      end
    end)
  end

  # OpenType SVG glyphs may embed PNG/JPEG bytes. Resource lookup never reaches
  # the filesystem or network: all hrefs become approved opaque descriptors.
  defp prepare_images(root, budget) do
    href_attribute = SvgReferenceValidator.href_attribute(root)
    href = Map.get(root.attributes, href_attribute, "")

    result =
      if root.tag in ~w(image feImage) and not String.starts_with?(href, "#") do
        case SvgImageValidator.data_uri(href, budget) do
          {:ok, bytes, encoding} when encoding in [:png, :jpeg] ->
            key = "svg-resource:" <> Base.encode16(:crypto.hash(:sha256, bytes), case: :lower)

            descriptor = %{
              data: bytes,
              media_type: if(encoding == :png, do: "image/png", else: "image/jpeg")
            }

            {:ok, %{root | attributes: Map.put(root.attributes, href_attribute, key)},
             %{key => descriptor}}

          {:error, _} = error ->
            error

          _ ->
            invalid_result(
              "SVG font images require embedded PNG or JPEG data; external resources are not permitted"
            )
        end
      else
        {:ok, root, %{}}
      end

    with {:ok, root, resources} <- result do
      Enum.reduce_while(root.children, {:ok, [], resources}, fn child,
                                                                {:ok, children, resources} ->
        result =
          if Map.get(child, :type) == :element,
            do: prepare_images(child, budget),
            else: {:ok, child, %{}}

        case result do
          {:ok, child, nested} -> {:cont, {:ok, [child | children], Map.merge(resources, nested)}}
          error -> {:halt, error}
        end
      end)
      |> case do
        {:ok, children, resources} -> {:ok, %{root | children: Enum.reverse(children)}, resources}
        error -> error
      end
    end
  end

  defp palette(font, directory) do
    case Map.get(directory, "CPAL") do
      nil ->
        {:ok, {}}

      {offset, size} ->
        ColrFontValidator.prepare_palette(
          binary_part(font.data, offset, size),
          Map.get(font, :palette, :normal)
        )
    end
  end

  defp prepare_tree(node, properties, budget) do
    attributes =
      Enum.reduce_while(node.attributes, {:ok, %{}}, fn {name, value}, {:ok, attributes} ->
        case HtmlValidator.resolve_css_variables(value, properties, %{}, budget) do
          {:ok, value} -> {:cont, {:ok, Map.put(attributes, name, value)}}
          :error -> {:halt, invalid_result("SVG glyph has an unresolved palette variable")}
          error -> {:halt, error}
        end
      end)

    children =
      Enum.reduce_while(node.children, {:ok, []}, fn child, {:ok, children} ->
        result =
          if Map.get(child, :type) == :element,
            do: prepare_tree(child, properties, budget),
            else: {:ok, child}

        case result do
          {:ok, child} -> {:cont, {:ok, [child | children]}}
          error -> {:halt, error}
        end
      end)

    with {:ok, attributes} <- attributes,
         {:ok, children} <- children,
         do: {:ok, %{node | attributes: attributes, children: Enum.reverse(children)}}
  end

  defp element(tag, attributes, children),
    do: %{type: :element, namespace: :svg, tag: tag, attributes: attributes, children: children}

  defp decode(data) do
    if match?(<<31, 139, 8, _::binary>>, data) do
      zlib = :zlib.open()

      try do
        :ok = :zlib.inflateInit(zlib, 31)
        inflate(zlib, data, 0, [])
      rescue
        ErlangError ->
          invalid("SVG font gzip stream is malformed, truncated or has an invalid checksum")
      after
        :zlib.close(zlib)
      end
    else
      FontValidator.check(:max_font_decoded_bytes, byte_size(data))
      data
    end
  end

  defp inflate(zlib, data, size, chunks) do
    {status, output} = :zlib.safeInflate(zlib, data)
    bytes = IO.iodata_length(output)
    size = size + bytes
    FontValidator.check(:max_font_decoded_bytes, size)
    FontValidator.reserve(:max_font_color_work, bytes + 1)

    if status == :finished do
      :ok = :zlib.inflateEnd(zlib)
      IO.iodata_to_binary(Enum.reverse([output | chunks]))
    else
      inflate(zlib, <<>>, size, [output | chunks])
    end
  end

  defp read(data, offset, size) do
    if offset < 0 or size < 0 or offset > byte_size(data) - size,
      do: invalid("SVG font table is truncated or references bytes outside its owner")

    binary_part(data, offset, size)
  end

  defp invalid(message), do: throw({:svg_font_error, invalid_result(message)})

  defp invalid_result(message),
    do:
      Diagnostics.error(:font, :invalid_font, message,
        module: __MODULE__,
        operation: :prepare_color_font,
        source: "SVG font"
      )
end
