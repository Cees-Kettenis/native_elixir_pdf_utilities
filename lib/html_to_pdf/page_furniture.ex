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
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.HtmlToPdf.Style
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

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

  @furniture_keys [:header, :footer]
  @variant_keys [:default, :first, :odd, :even]

  @doc false
  @spec normalize_option(term()) :: {:ok, map() | nil} | {:error, term()}
  def normalize_option(furniture) do
    case furniture do
      value when value in [nil, false] ->
        {:ok, nil}

      furniture when is_list(furniture) ->
        case Keyword.keyword?(furniture) do
          true -> normalize_furniture(Map.new(furniture))
          false -> {:error, :invalid_furniture_container}
        end

      furniture when is_map(furniture) ->
        normalize_furniture(furniture)

      _ ->
        {:error, :invalid_furniture_container}
    end
  end

  @doc """
  Adds configured running headers and footers to paginated pages.

  Omitted, `nil`, and `false` page furniture leave pages unchanged. Configured
  templates must fit inside the corresponding top or bottom margin in
  `layout_tree`.
  """
  @spec decorate([page()], layout_tree(), [render_option()]) ::
          {:ok, [page()]} | {:error, detailed_error()}
  def decorate(pages, layout_tree, opts) do
    margins =
      case layout_tree do
        layout_tree when is_map(layout_tree) ->
          layout_tree
          |> Map.get(:margins, Map.get(layout_tree, :margin, :missing))
          |> PageGeometry.normalize_margins()

        _ ->
          {:error, :invalid_margin}
      end

    furniture =
      case Keyword.keyword?(opts) do
        true -> opts |> Keyword.get(:page_furniture) |> normalize_option()
        false -> {:error, :invalid_furniture_container}
      end

    case HtmlValidator.validate_furniture_input(pages, layout_tree, opts, margins, furniture) do
      :ok ->
        {:ok, margins} = margins
        {:ok, furniture} = furniture
        decorate_pages(pages, layout_tree.page_size, margins, furniture, opts)

      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          Diagnostics.with_context(diagnostic,
            operation: :decorate_pages,
            module: __MODULE__
          )}}
    end
  end

  defp decorate_pages(pages, page_size, margins, furniture, opts) do
    case furniture do
      nil ->
        {:ok, pages}

      furniture ->
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
                   margins,
                   opts
                 ),
               {:ok, footer_boxes} <-
                 render_position(
                   :footer,
                   select_template(furniture.footer, page_number),
                   page_number,
                   total_pages,
                   page_size,
                   margins,
                   opts
                 ) do
            decorated_page = %{page | boxes: page.boxes ++ header_boxes ++ footer_boxes}
            {:cont, {:ok, decorated ++ [decorated_page]}}
          else
            {:error, {_reason, _diagnostic}} = error -> {:halt, error}
          end
        end)
    end
  end

  defp normalize_furniture(furniture) do
    case Enum.reject(Map.keys(furniture), &(&1 in @furniture_keys)) do
      [] ->
        Enum.reduce_while(@furniture_keys, {:ok, %{}}, fn position, {:ok, normalized} ->
          case normalize_variants(Map.get(furniture, position), position) do
            {:ok, variants} -> {:cont, {:ok, Map.put(normalized, position, variants)}}
            {:error, reason} -> {:halt, {:error, reason}}
          end
        end)

      unknown ->
        {:error, {:unknown_furniture_keys, Enum.sort(unknown)}}
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
          false -> {:error, {:invalid_variants, position}}
        end

      variants when is_map(variants) ->
        normalize_variant_map(variants, position)

      _ ->
        {:error, {:invalid_variants, position}}
    end
  end

  defp normalize_variant_map(variants, position) do
    unknown = Enum.reject(Map.keys(variants), &(&1 in @variant_keys))

    cond do
      unknown != [] ->
        {:error, {:unknown_variant_keys, position, Enum.sort(unknown)}}

      Enum.all?(variants, fn {_variant, template} ->
        is_binary(template) or template in [nil, false]
      end) ->
        {:ok, variants}

      true ->
        {:error, {:invalid_variants, position}}
    end
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

  defp render_position(position, template, page_number, total_pages, page_size, margins, opts) do
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
          |> Keyword.put(:margin, margins)

        with {:ok, dom} <- HtmlParser.parse_detailed(html),
             {:ok, styled_tree} <- Style.compute_detailed(dom, furniture_opts),
             {:ok, styled_tree} <- FontFallback.resolve(styled_tree) do
          case apply(Layout, :layout, [styled_tree, furniture_opts]) do
            {:ok, furniture_layout} ->
              place(position, furniture_layout.boxes, page_size, margins)

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

  defp place(position, boxes, {_page_width, page_height}, margins) do
    drawable_boxes = Enum.reject(boxes, &match?(%{type: :page_break}, &1))
    bounds = Enum.map(drawable_boxes, &PageGeometry.box_vertical_bounds/1)

    case bounds do
      [] ->
        {:ok, []}

      bounds ->
        top = bounds |> Enum.map(&elem(&1, 0)) |> Enum.max()
        bottom = bounds |> Enum.map(&elem(&1, 1)) |> Enum.min()
        height = top - bottom
        available_margin = if position == :header, do: margins.top, else: margins.bottom

        case HtmlValidator.validate_furniture_fit(position, height, available_margin) do
          :ok ->
            target_top = if position == :header, do: page_height, else: margins.bottom
            {:ok, shift_boxes(drawable_boxes, target_top - top)}

          {:error, {reason, diagnostic}} ->
            {:error,
             {reason,
              Diagnostics.with_context(diagnostic,
                operation: :decorate_pages,
                module: __MODULE__
              )}}
        end
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
end
