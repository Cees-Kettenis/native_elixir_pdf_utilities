defmodule NativeElixirPdfUtilities.Validators.EmbeddedViewportValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewport
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
  alias NativeElixirPdfUtilities.Validators.HtmlUrlValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.ResourceValidator

  @doc false
  @spec prepare_options(term(), ResourceContext.t()) ::
          {:ok, keyword()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_options(opts, context) do
    with :ok <- ResourceValidator.validate_context(context),
         true <- is_list(opts) and Keyword.keyword?(opts),
         {:ok, normalized} <- Font.normalize_options(opts),
         :ok <- HtmlValidator.validate_render_request("", opts, {:ok, normalized}),
         opts <-
           Keyword.take(normalized, [
             :forms,
             :assets,
             :asset_resolver,
             :stylesheets,
             :default_font,
             :fonts,
             :unsupported_glyphs
           ]) do
      {:ok,
       opts
       |> Keyword.put(:base_url, context.resolution_base)
       |> Keyword.put(:system_font_discovery, false)}
    else
      {:error, {_reason, _diagnostic}} = error ->
        error

      _ ->
        Diagnostics.error(
          :resource,
          :invalid_options,
          "child viewport requires valid font and render options",
          module: __MODULE__
        )
    end
  end

  @doc false
  @spec prepare_document_context(map(), ResourceContext.t()) ::
          {:ok, map(), HtmlUrlValidator.context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_document_context(document, parent) do
    fallback =
      case document.source do
        {:file, path} ->
          "file://" <> URI.encode(path, &(&1 == ?/ or URI.char_unreserved?(&1)))

        {:srcdoc, _html} ->
          base = parent.resolution_base

          case if(is_binary(base), do: URI.parse(base)) do
            %URI{scheme: scheme} when is_binary(scheme) ->
              base

            _ ->
              {:ok, context} =
                HtmlUrlValidator.document_context(%{type: :document, children: []},
                  base_url: base
                )

              context.fallback_base
          end
      end

    with {:ok, urls} <- HtmlUrlValidator.document_context_from_url(document.dom, fallback),
         {:ok, context} <-
           ResourceContext.rebase(document.context, {:document_url, urls.resolution_base}),
         do: {:ok, %{document | context: context}, urls}
  end

  @doc false
  @spec prepare_iframe_style(map()) :: map()
  def prepare_iframe_style(style) do
    # The HTML replaced-element default is 300 by 150 CSS pixels, a fixed
    # format default rather than a configurable resource ceiling.
    display =
      if style.display in [:inline, :inline_block, :inline_flex, :inline_grid],
        do: :inline_block,
        else: :block

    style
    |> Map.put(:display, display)
    |> Map.update(:width, 225.0, fn value -> if value in [nil, :auto], do: 225.0, else: value end)
    |> Map.update(:height, 112.5, fn value -> if value in [nil, :auto], do: 112.5, else: value end)
  end

  @doc false
  @spec prepare_paint(map(), EmbeddedViewport.t(), number(), number()) ::
          {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_paint(layout, surface, width, height) do
    overflow =
      Enum.find_value(surface.tree.children, :visible, fn node ->
        case node do
          %{tag: "body", style: style} ->
            case Map.get(style, :document_overflow, :visible) do
              :visible -> Map.get(style, :overflow, :visible)
              overflow -> overflow
            end

          _ ->
            nil
        end
      end)

    overflowing? =
      Enum.any?(layout.boxes, fn box ->
        case box.type do
          :page_break ->
            false

          _ ->
            geometry = if box.type == :image, do: Map.get(box, :clip, box), else: box
            {_top, bottom} = PageGeometry.box_vertical_bounds(Map.put(geometry, :type, box.type))
            right = Map.get(geometry, :x, 0) + Map.get(geometry, :width, 0)
            # CSS layout's 1/64-pixel precision is a fixed format allowance.
            right > width + 0.75 / 64 or bottom < -0.75 / 64
        end
      end)

    cond do
      surface.host == :iframe and width > 0 and height > 0 and overflow != :hidden and
          overflowing? ->
        Diagnostics.error(
          :layout,
          :not_implemented,
          "this iframe requires automatic scrollbars and a reduced client area, which are not implemented yet; author html/body overflow:hidden for a static clipped viewport or keep child content within its viewport",
          module: __MODULE__,
          operation: :layout_embedded_viewport,
          source: "iframe"
        )

      true ->
        page = %{size: layout.page_size, boxes: layout.boxes}

        case NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare([page],
               forms: surface.forms
             ) do
          {:ok, [page], []} ->
            {:ok, Enum.map(page.boxes, &Map.delete(&1, :form_control))}

          {:ok, _pages, _controls} ->
            Diagnostics.error(
              :layout,
              :unsupported_form,
              "interactive child controls require transformed widget geometry; use forms: :static for their complete static artwork",
              module: __MODULE__,
              operation: :layout_embedded_viewport
            )

          {:error, {_reason, _diagnostic}} = error ->
            error
        end
    end
  end

  @doc false
  @spec prepare_layout(term(), term(), term()) ::
          {:ok, keyword()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_layout(surface, width, height) do
    case surface do
      %{tree: _tree, context: context, forms: _forms, host: host}
      when host in [:surface, :iframe] and is_number(width) and width >= 0 and
             is_number(height) and height >= 0 ->
        with :ok <- ResourceValidator.validate_context(context) do
          HtmlValidator.check_render_resource(:max_css_numeric_magnitude, width, :layout)
          HtmlValidator.check_render_resource(:max_css_numeric_magnitude, height, :layout)
          # Zero-area surfaces still validate and account for their entire tree.
          size = {if(width == 0, do: 0.75, else: width), if(height == 0, do: 0.75, else: height)}
          opts = [page_size: "#{elem(size, 0)}pt #{elem(size, 1)}pt", margin: 0]

          {:ok, opts}
        end

      _ ->
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "child viewport requires an issued document and nonnegative finite dimensions",
          module: __MODULE__,
          operation: :layout_embedded_viewport
        )
    end
  end
end
