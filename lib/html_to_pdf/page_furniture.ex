defmodule NativeElixirPdfUtilities.HtmlToPdf.PageFurniture do
  @moduledoc """
  Adds opt-in running headers and footers to already paginated layout boxes.

  Page furniture is rendered after body pagination so current and total page
  tokens are known. Templates use the normal HTML, style, layout, image, and
  font pipeline and are placed inside the page margin.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.HtmlToPdf.FontFallback
  alias NativeElixirPdfUtilities.HtmlToPdf.Layout
  alias NativeElixirPdfUtilities.HtmlToPdf.Style

  @type page :: NativeElixirPdfUtilities.HtmlToPdf.Pagination.page()
  @type layout_tree :: NativeElixirPdfUtilities.HtmlToPdf.Layout.layout_tree()
  @type render_option :: HtmlToPdf.render_option()
  @type error_reason ::
          :invalid_css
          | :invalid_document
          | :invalid_encoding
          | :invalid_html
          | :invalid_layout
          | :invalid_options
          | :unsupported_glyph
          | :unsupported_html
  @type detailed_error :: {error_reason(), Diagnostics.diagnostic()}

  @variant_keys [:default, :first, :odd, :even]
  @furniture_keys [:header, :footer]

  @doc """
  Adds configured running headers and footers to paginated pages.

  Omitted, `nil`, and `false` page furniture leave pages unchanged. Configured
  templates must fit inside the numeric margin in `layout_tree`.
  """
  @spec decorate([page()], layout_tree(), [render_option()]) ::
          {:ok, [page()]} | {:error, detailed_error()}
  def decorate(pages, layout_tree, opts) do
    case {pages, layout_tree, opts} do
      {pages, %{page_size: page_size, margin: margin}, opts}
      when is_list(pages) and is_list(opts) and is_number(margin) ->
        case {Keyword.keyword?(opts), valid_layout_context?(pages, page_size, margin)} do
          {true, true} ->
            with {:ok, furniture} <- normalize(Keyword.get(opts, :page_furniture)),
                 {:ok, decorated} <-
                   decorate_pages(pages, page_size, margin, furniture, opts) do
              {:ok, decorated}
            end

          {false, _valid_context} ->
            invalid_options("page furniture options require a keyword list")

          {_keyword_options, false} ->
            invalid_layout()
        end

      _ ->
        invalid_layout()
    end
  end

  defp normalize(furniture) do
    case furniture do
      value when value in [nil, false] ->
        {:ok, nil}

      furniture when is_list(furniture) ->
        case Keyword.keyword?(furniture) do
          true -> normalize_furniture(Map.new(furniture))
          false -> invalid_options("page_furniture must be a keyword list or map")
        end

      furniture when is_map(furniture) ->
        normalize_furniture(furniture)

      _ ->
        invalid_options("page_furniture must be a keyword list or map")
    end
  end

  defp valid_layout_context?(pages, page_size, margin) do
    valid_page_size?(page_size) and margin >= 0 and
      Enum.all?(pages, fn page ->
        case page do
          %{size: ^page_size, boxes: boxes} when is_list(boxes) -> true
          _ -> false
        end
      end)
  end

  defp valid_page_size?(page_size) do
    case page_size do
      {width, height} when is_number(width) and is_number(height) and width > 0 and height > 0 ->
        true

      _ ->
        false
    end
  end

  defp normalize_furniture(furniture) do
    case Enum.reject(Map.keys(furniture), &(&1 in @furniture_keys)) do
      [] ->
        Enum.reduce_while(@furniture_keys, {:ok, %{}}, fn position, {:ok, normalized} ->
          case normalize_variants(Map.get(furniture, position), position) do
            {:ok, variants} -> {:cont, {:ok, Map.put(normalized, position, variants)}}
            {:error, {_reason, _diagnostic}} = error -> {:halt, error}
          end
        end)

      unknown ->
        invalid_options(
          "page_furniture contains unsupported keys: #{inspect(Enum.sort(unknown))}"
        )
    end
  end

  defp normalize_variants(value, position) do
    case value do
      value when value in [nil, false] ->
        {:ok, %{}}

      template when is_binary(template) ->
        {:ok, %{default: template}}

      variants when is_list(variants) ->
        case Keyword.keyword?(variants) do
          true -> normalize_variant_map(Map.new(variants), position)
          false -> invalid_variant_options(position)
        end

      variants when is_map(variants) ->
        normalize_variant_map(variants, position)

      _ ->
        invalid_variant_options(position)
    end
  end

  defp normalize_variant_map(variants, position) do
    unknown = Enum.reject(Map.keys(variants), &(&1 in @variant_keys))

    cond do
      unknown != [] ->
        invalid_options(
          "#{position} page furniture contains unsupported keys: #{inspect(Enum.sort(unknown))}"
        )

      Enum.all?(variants, fn {_variant, template} ->
        is_binary(template) or template in [nil, false]
      end) ->
        {:ok, variants}

      true ->
        invalid_variant_options(position)
    end
  end

  defp invalid_variant_options(position) do
    invalid_options(
      "#{position} page furniture must be HTML or default/first/odd/even HTML variants"
    )
  end

  defp decorate_pages(pages, _page_size, _margin, nil, _opts) do
    {:ok, pages}
  end

  defp decorate_pages(pages, page_size, margin, furniture, opts) do
    total_pages = length(pages)

    pages
    |> Enum.with_index(1)
    |> Enum.reduce_while({:ok, []}, fn {page, page_number}, {:ok, decorated} ->
      with {:ok, header_boxes} <-
             render_position(
               :header,
               select_template(furniture.header, page_number),
               page_number,
               total_pages,
               page_size,
               margin,
               opts
             ),
           {:ok, footer_boxes} <-
             render_position(
               :footer,
               select_template(furniture.footer, page_number),
               page_number,
               total_pages,
               page_size,
               margin,
               opts
             ) do
        decorated_page = %{page | boxes: page.boxes ++ header_boxes ++ footer_boxes}
        {:cont, {:ok, decorated ++ [decorated_page]}}
      else
        {:error, {_reason, _diagnostic}} = error -> {:halt, error}
      end
    end)
  end

  defp select_template(variants, page_number) do
    cond do
      page_number == 1 and Map.has_key?(variants, :first) ->
        Map.get(variants, :first)

      rem(page_number, 2) == 1 and Map.has_key?(variants, :odd) ->
        Map.get(variants, :odd)

      rem(page_number, 2) == 0 and Map.has_key?(variants, :even) ->
        Map.get(variants, :even)

      true ->
        Map.get(variants, :default)
    end
  end

  defp render_position(position, template, page_number, total_pages, page_size, margin, opts) do
    case template do
      template when template in [nil, false] ->
        {:ok, []}

      template when is_binary(template) ->
        html =
          template
          |> String.replace("{{page}}", Integer.to_string(page_number))
          |> String.replace("{{pages}}", Integer.to_string(total_pages))
          |> wrap_fragment()

        furniture_opts =
          opts
          |> Keyword.put(:page_size, page_size)
          |> Keyword.put(:margin, margin)

        with {:ok, dom} <- HtmlParser.parse_detailed(html),
             {:ok, styled_tree} <- Style.compute_detailed(dom, furniture_opts),
             {:ok, styled_tree} <- FontFallback.resolve(styled_tree) do
          case apply(Layout, :layout, [styled_tree, furniture_opts]) do
            {:ok, furniture_layout} ->
              place(position, furniture_layout.boxes, page_size, margin)

            {:error, reason} ->
              Diagnostics.error(
                :layout,
                reason,
                "#{position} page furniture layout failed: #{reason}",
                operation: :decorate_pages,
                module: __MODULE__
              )
          end
        end
    end
  end

  defp place(position, boxes, {_page_width, page_height}, margin) do
    drawable_boxes = Enum.reject(boxes, &match?(%{type: :page_break}, &1))
    bounds = Enum.map(drawable_boxes, &box_bounds/1)

    case bounds do
      [] ->
        {:ok, []}

      bounds ->
        top = bounds |> Enum.map(&elem(&1, 0)) |> Enum.max()
        bottom = bounds |> Enum.map(&elem(&1, 1)) |> Enum.min()
        height = top - bottom

        case height <= margin + 0.0001 do
          true ->
            target_top = if position == :header, do: page_height, else: margin
            {:ok, shift_boxes(drawable_boxes, target_top - top)}

          false ->
            Diagnostics.error(
              :layout,
              :invalid_layout,
              "#{position} page furniture height #{format_number(height)}pt exceeds the #{format_number(margin)}pt page margin",
              operation: :decorate_pages,
              module: __MODULE__
            )
        end
    end
  end

  defp box_bounds(box) do
    case box do
      %{type: type, y: y, height: height}
      when type in [:rect, :image] and is_number(y) and is_number(height) ->
        {y + height, y}

      %{type: :text, y: y, font_size: font_size}
      when is_number(y) and is_number(font_size) ->
        {y + font_size, y}
    end
  end

  defp shift_boxes(boxes, delta_y) do
    Enum.map(boxes, fn %{y: y} = box ->
      Map.put(box, :y, y + delta_y)
    end)
  end

  defp wrap_fragment(html) do
    case Regex.match?(~r/\A\s*(?:<!doctype\b|<html\b)/iu, html) do
      true -> html
      false -> "<div>#{html}</div>"
    end
  end

  defp invalid_options(message) do
    Diagnostics.error(:options, :invalid_options, message,
      operation: :decorate_pages,
      module: __MODULE__
    )
  end

  defp invalid_layout do
    Diagnostics.error(
      :layout,
      :invalid_layout,
      "page furniture requires paginated pages and a layout tree with page size and margin",
      operation: :decorate_pages,
      module: __MODULE__
    )
  end

  defp format_number(number) do
    number
    |> Kernel.*(100)
    |> round()
    |> Kernel./(100)
    |> to_string()
  end
end
