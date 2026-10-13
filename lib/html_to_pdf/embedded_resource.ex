defmodule NativeElixirPdfUtilities.HtmlToPdf.EmbeddedResource do
  @moduledoc """
  Loads authorized child HTML into an isolated document descriptor.

  This is resource preparation, not viewport rendering. The normal HTML parser
  applies input policy and charges the existing cumulative HTML/node budgets.
  Document mappings and resolvers are deliberately not document sources.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.FileReader
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.ObjectValidator
  alias NativeElixirPdfUtilities.Validators.ResourceValidator

  @type document :: %{
          context: ResourceContext.t(),
          source: ResourceValidator.document_descriptor(),
          html: String.t(),
          dom: HtmlParser.dom_tree()
        }

  @doc "Prevalidates and loads a child local HTML file or inline srcdoc without resolving mappings."
  @spec load_html(term(), ResourceContext.t(), keyword()) ::
          {:ok, document()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def load_html(source, context, opts \\ []) do
    load_document(source, context, opts, :html)
  end

  @doc "Loads selected local object HTML after authorization and MIME validation."
  @spec load_object_html(term(), ResourceContext.t(), keyword()) ::
          {:ok, document()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def load_object_html(source, context, opts \\ []) do
    load_document(source, context, opts, :object)
  end

  @doc "Reads exact selected local bytes with an issued child scope and existing source limits."
  @spec load_file(term(), String.t(), ResourceContext.t(), keyword()) ::
          {:ok, %{context: ResourceContext.t(), source: {:file, String.t()}, bytes: binary()}}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def load_file(source, mime, context, opts \\ []) do
    with {:ok, mime} <- ObjectValidator.prepare_mime(mime),
         {:ok, descriptor, child} <- ObjectValidator.authorize_html(source, context, opts),
         {:ok, maximum} <-
           if(mime in ["image/png", "image/jpeg", "image/svg+xml"],
             do: HtmlValidator.image_source_read_limit(context.image_budget),
             else: {:ok, Limits.get(:max_html_source_bytes)}
           ),
         {:file, path} <- descriptor,
         {:ok, bytes} <- FileReader.read(path, maximum),
         :ok <- ObjectValidator.validate_content(bytes, descriptor, mime) do
      {:ok, %{context: child, source: descriptor, bytes: bytes}}
    else
      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          Diagnostics.with_context(diagnostic,
            operation: :load_embedded_resource,
            module: __MODULE__,
            source: if(is_binary(source), do: source)
          )}}

      {:error, reason} ->
        Diagnostics.error(
          :resource,
          if(reason in [:enoent, :enotdir], do: :resource_not_found, else: :invalid_document),
          "authorized embedded resource file could not be read: #{reason}",
          operation: :load_embedded_resource,
          module: __MODULE__,
          source: if(is_binary(source), do: source)
        )
    end
  end

  defp load_document(source, context, opts, kind) do
    with {:ok, descriptor, child} <-
           if(kind == :object,
             do: ObjectValidator.authorize_html(source, context, opts),
             else: ResourceValidator.authorize_document(source, context, opts)
           ),
         {:ok, html} <-
           (case descriptor do
              {:srcdoc, html} -> {:ok, html}
              {:file, path} -> FileReader.read(path, Limits.get(:max_html_source_bytes))
            end),
         :ok <-
           if(kind == :object, do: ObjectValidator.validate_content(html, descriptor), else: :ok),
         {:ok, dom} <- HtmlParser.parse_document_detailed(html) do
      {:ok, %{context: child, source: descriptor, html: html, dom: dom}}
    else
      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          Diagnostics.with_context(diagnostic,
            operation: :load_embedded_resource,
            module: __MODULE__,
            source: if(is_binary(source), do: source)
          )}}

      {:error, reason} ->
        Diagnostics.error(
          :resource,
          if(reason in [:enoent, :enotdir], do: :resource_not_found, else: :invalid_document),
          "authorized embedded HTML file could not be read: #{reason}",
          operation: :load_embedded_resource,
          module: __MODULE__,
          source: if(is_binary(source), do: source)
        )
    end
  end
end
