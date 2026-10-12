defmodule NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewport do
  @moduledoc "Native child document surfaces with isolated styles and shared render budgets."

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.AssetLoader
  alias NativeElixirPdfUtilities.HtmlToPdf.EmbeddedResource
  alias NativeElixirPdfUtilities.HtmlToPdf.FontFallback
  alias NativeElixirPdfUtilities.HtmlToPdf.Layout
  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
  alias NativeElixirPdfUtilities.HtmlToPdf.Style
  alias NativeElixirPdfUtilities.Validators.EmbeddedViewportValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @type t :: %{
          tree: Style.styled_tree(),
          context: ResourceContext.t(),
          forms: :static | :interactive,
          host: :surface | :iframe
        }

  @doc "Prepares authorized local or srcdoc HTML without a recursive public render or pagination."
  @spec prepare_html(term(), ResourceContext.t(), keyword()) ::
          {:ok, t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_html(source, context, opts \\ []) do
    with {:ok, document} <- EmbeddedResource.load_html(source, context, opts),
         {:ok, document, urls} <-
           EmbeddedViewportValidator.prepare_document_context(document, context) do
      ResourceContext.with_document(document.context, fn ->
        with {:ok, opts} <- EmbeddedViewportValidator.prepare_options(opts, document.context),
             {:ok, fonts} <- local_fonts(Keyword.fetch!(opts, :fonts), document.context, opts),
             opts <- Keyword.put(opts, :fonts, fonts),
             {:ok, tree} <-
               Style.compute_child_detailed(
                 document.dom,
                 opts,
                 context.image_budget,
                 urls.fallback_base
               ),
             {:ok, tree} <-
               FontFallback.resolve(tree, Keyword.get(opts, :unsupported_glyphs, :replace)) do
          {:ok,
           %{
             tree: tree,
             context: document.context,
             forms: Keyword.get(opts, :forms, :interactive),
             host: :surface
           }}
        end
      end)
    end
  end

  @doc "Prepares iframe HTML with the element's initial viewport behavior."
  @spec prepare_iframe(term(), ResourceContext.t(), keyword()) ::
          {:ok, t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_iframe(source, context, opts \\ []) do
    with {:ok, surface} <- prepare_html(source, context, opts),
         do: {:ok, %{surface | host: :iframe}}
  end

  @doc "Prepares literal UTF-8 text as a bounded native document, preserving line breaks."
  @spec prepare_text(term(), ResourceContext.t(), keyword()) ::
          {:ok, t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_text(text, context, opts \\ []) do
    with {:ok, text} <- HtmlValidator.validate_html_source(text) do
      prepare_html({:srcdoc, "<pre>" <> escape_html(text) <> "</pre>"}, context, opts)
    end
  end

  @doc "Prepares a supported local or inline image using the native image decoder and shared budget."
  @spec prepare_image(String.t(), ResourceContext.t(), keyword()) ::
          {:ok, t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_image(reference, context, opts \\ []) do
    with {:ok, reference} <- HtmlValidator.validate_html_source(reference) do
      prepare_html(
        {:srcdoc,
         "<body style='margin:0'><img src=\"" <>
           escape_html(reference) <> "\"></body>"},
        context,
        opts
      )
    end
  end

  @doc "Lays out all child content once in its viewport and returns one clipped drawing group."
  @spec layout(t(), number(), number()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def layout(surface, width, height) do
    with {:ok, opts} <- EmbeddedViewportValidator.prepare_layout(surface, width, height) do
      ResourceContext.with_document(surface.context, fn ->
        with {:ok, layout} <-
               Layout.layout(Map.put(surface.tree, :_css_pixel_viewport, true), opts),
             {:ok, boxes} <-
               EmbeddedViewportValidator.prepare_paint(layout, surface, width, height) do
          # Child flow, headings and canvas anchors remain inside the group. No child
          # pagination, page furniture, metadata or outlines enter the parent pipeline.
          boxes =
            Enum.reject(
              boxes,
              &(&1.type == :page_break or
                  (&1.type == :flow_marker and Map.get(&1, :html_targets, []) == []))
            )

          link_metadata =
            layout
            |> Map.take([:link_scope, :link_expected_targets, :link_document_targets])
            |> Map.update(:link_document_targets, [], fn targets ->
              Enum.map(
                targets,
                &Map.merge(&1, %{x: layout.margins.left, y: height - layout.margins.top})
              )
            end)

          link_metadata =
            if Map.has_key?(link_metadata, :link_scope),
              do:
                Map.put(link_metadata, :link_document_origin, %{
                  x: layout.margins.left,
                  y: height - layout.margins.top
                }),
              else: link_metadata

          {:ok,
           Map.merge(
             %{
               type: HtmlValidator.reserve_layout_box(:drawing_group),
               width: width,
               height: height,
               transform: {1, 0, 0, 1, 0, 0},
               clip: %{x: 0.0, y: 0.0, width: width, height: height},
               boxes: boxes
             },
             link_metadata
           )}
        else
          {:error, {_reason, _diagnostic}} = error ->
            error

          {:error, reason} ->
            Diagnostics.error(:layout, reason, "embedded document layout failed: #{reason}",
              operation: :layout_embedded_viewport,
              module: __MODULE__
            )
        end
      end)
    end
  end

  defp local_fonts(fonts, context, opts) do
    Enum.reduce_while(fonts, {:ok, []}, fn font, {:ok, acc} ->
      case font do
        %{path: paths} ->
          result =
            Enum.reduce_while(paths, {:ok, []}, fn path, {:ok, bytes} ->
              case AssetLoader.resolve_local(path, :font, context, opts) do
                {:ok, data} -> {:cont, {:ok, bytes ++ [data]}}
                {:error, _} = error -> {:halt, error}
              end
            end)

          case result do
            {:ok, bytes} ->
              {:cont, {:ok, acc ++ [font |> Map.delete(:path) |> Map.put(:data, bytes)]}}

            {:error, _} = error ->
              {:halt, error}
          end

        %{data: _} ->
          {:cont, {:ok, acc ++ [font]}}
      end
    end)
  end

  defp escape_html(text) do
    text
    |> String.replace("&", "&amp;")
    |> String.replace("<", "&lt;")
    |> String.replace(">", "&gt;")
    |> String.replace("\"", "&quot;")
  end
end
