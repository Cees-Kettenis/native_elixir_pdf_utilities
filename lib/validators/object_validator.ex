defmodule NativeElixirPdfUtilities.Validators.ObjectValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.HtmlUrlValidator
  alias NativeElixirPdfUtilities.Validators.MimeValidator
  alias NativeElixirPdfUtilities.Validators.ResourceValidator
  alias NativeElixirPdfUtilities.Validators.SvgViewportValidator

  @document_mimes ["text/html", "text/plain"]
  @image_mimes ["image/png", "image/jpeg", "image/svg+xml"]

  @doc "Authorizes a selected local object/embed resource without inline document descriptors."
  @spec authorize_html(term(), NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.t(), term()) ::
          {:ok, ResourceValidator.document_descriptor(),
           NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.t()}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def authorize_html(source, context, opts) do
    case source do
      reference when is_binary(reference) ->
        ResourceValidator.authorize_document(reference, context, opts)

      _ ->
        Diagnostics.error(
          :resource,
          :invalid_document,
          "embedded resource requires a selected local file reference",
          operation: :load_embedded_resource,
          module: __MODULE__
        )
    end
  end

  @type selected ::
          :fallback
          | :empty
          | :blank
          | {:blank, String.t()}
          | {:document, String.t(), String.t()}
          | {:image, String.t(), String.t()}

  @doc "Selects object fallback, an empty embed, or a supported local resource before lookup."
  @spec select(map(), NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.t(), :object | :embed) ::
          {:ok, selected()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def select(attributes, context, host \\ :object) do
    attribute = if host == :embed, do: "src", else: "data"
    reference = attributes |> Map.get(attribute, "") |> HtmlValidator.trim_ascii_whitespace()
    type = attributes |> Map.get("type", "") |> HtmlValidator.trim_ascii_whitespace()
    inactive = if host == :embed, do: :empty, else: :fallback

    # Object/embed type is a MIME hint. Parameters do not change its essence.
    supplied = if type == "", do: nil, else: type |> String.split(";", parts: 2) |> hd()

    mime =
      if is_nil(supplied) do
        MimeValidator.prepare("", URI.parse(reference).path || "", nil)
      else
        MimeValidator.validate(HtmlValidator.trim_ascii_whitespace(supplied))
      end

    blank = HtmlUrlValidator.blank_document_url(reference)

    case {reference, mime, blank} do
      {"", _, _} when host == :embed ->
        {:ok, if(Map.has_key?(attributes, "src"), do: :blank, else: :empty)}

      {reference, _, {:ok, _url}} when host == :embed ->
        {:ok, {:blank, reference}}

      {"", _, _} ->
        {:ok, inactive}

      {reference, {:ok, mime}, _} when mime in @document_mimes ->
        {:ok, {:document, reference, mime}}

      {reference, {:ok, mime}, _} when mime in @image_mimes ->
        {:ok, {:image, reference, mime}}

      {reference, {:ok, "application/pdf"}, _} ->
        with {:ok, _path} <- ResourceValidator.authorize_reference(reference, context) do
          Diagnostics.error(
            :resource,
            :not_implemented,
            "#{host} PDF page rendering depends on the 1.5.0 PDF composition work; use local HTML, text or a supported still image",
            operation: :load_embedded_resource,
            module: __MODULE__,
            source: reference
          )
        end

      {reference, _, _} when host == :embed ->
        with {:ok, _path} <- ResourceValidator.authorize_reference(reference, context) do
          Diagnostics.error(
            :resource,
            :not_implemented,
            "embed MIME requires plugin rendering; use local HTML, text or a supported PNG, JPEG or static SVG",
            operation: :load_embedded_resource,
            module: __MODULE__,
            source: reference
          )
        end

      _ ->
        {:ok, inactive}
    end
  end

  @doc "Validates a selected dispatch plan and its host before any file work."
  @spec prepare_selection(term(), term()) ::
          {:ok, :document | :image | :blank, String.t() | nil, String.t() | nil}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_selection(selected, host) do
    case {selected, host} do
      {:blank, :embed} ->
        {:ok, :blank, nil, nil}

      {{:blank, url}, :embed} ->
        case HtmlUrlValidator.blank_document_url(url) do
          {:ok, _normalized} ->
            {:ok, :blank, url, nil}

          :not_blank ->
            Diagnostics.error(
              :resource,
              :invalid_document,
              "embed blank source requires an about:blank URL",
              operation: :load_embedded_resource,
              module: __MODULE__
            )
        end

      {{kind, reference, mime}, host}
      when kind in [:document, :image] and is_binary(reference) and host in [:object, :embed] ->
        if (kind == :document and mime in @document_mimes) or
             (kind == :image and mime in @image_mimes) do
          {:ok, kind, reference, mime}
        else
          Diagnostics.error(
            :resource,
            :invalid_document,
            "selected embedded resource kind and MIME must agree",
            operation: :load_embedded_resource,
            module: __MODULE__,
            source: reference
          )
        end

      _ ->
        Diagnostics.error(
          :resource,
          :invalid_document,
          "selected embedded resource requires a supported typed local source and object/embed host",
          operation: :load_embedded_resource,
          module: __MODULE__
        )
    end
  end

  @doc "Normalizes the supported selected-file MIME vocabulary before reads."
  @spec prepare_mime(term()) :: {:ok, String.t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_mime(value) do
    with {:ok, mime} <- MimeValidator.validate(value) do
      if mime in @document_mimes or mime in @image_mimes,
        do: {:ok, mime},
        else:
          Diagnostics.error(
            :resource,
            :invalid_document,
            "selected file MIME is outside the native document/image dispatch",
            module: __MODULE__
          )
    else
      {:error, {reason, diagnostic}} ->
        Diagnostics.error(:resource, reason, diagnostic.message,
          operation: :load_embedded_resource,
          module: __MODULE__
        )
    end
  end

  @doc "Selects a static SVG document's own aspect rule instead of outer bitmap fitting."
  @spec prepare_svg_fitting(term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_svg_fitting(root, image) do
    with true <- is_map(root),
         %{width: image_width, height: image_height} <- image,
         true <-
           is_number(image_width) and image_width > 0 and is_number(image_height) and
             image_height > 0,
         attributes <-
           root
           |> Map.put("viewBox", Map.get(root, "viewbox"))
           |> Map.put("preserveAspectRatio", Map.get(root, "preserveaspectratio")),
         {:ok, viewport} <-
           SvgViewportValidator.prepare(attributes, %{}, %{
             width: image_width,
             height: image_height
           }) do
      {horizontal, vertical, mode} = viewport.aspect

      case viewport.view_box do
        {_x, _y, width, height}
        when height > 0 and (mode == :none or width * image_height == height * image_width) ->
          {:ok,
           %{
             object_fit:
               case mode do
                 :none -> :fill
                 :meet -> :contain
                 :slice -> :cover
               end,
             object_position: {{:percent, horizontal}, {:percent, vertical}}
           }}

        _ ->
          Diagnostics.error(
            :resource,
            :not_implemented,
            "selected SVG documents require a viewBox with an intrinsic matching aspect ratio, or preserveAspectRatio=none; other roots require a dynamic SVG document viewport",
            module: __MODULE__,
            operation: :load_embedded_resource,
            source: "SVG"
          )
      end
    else
      {:error, {_reason, _diagnostic}} = error ->
        error

      _ ->
        Diagnostics.error(
          :resource,
          :invalid_document,
          "selected SVG fitting requires canonical root attributes and positive decoder dimensions",
          operation: :load_embedded_resource,
          module: __MODULE__
        )
    end
  end

  @doc "Checks selected resource content against file MIME evidence before decoding."
  @spec validate_content(binary(), {:file, String.t()}, String.t()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_content(bytes, descriptor, mime \\ "text/html") do
    {:file, path} = descriptor

    case MimeValidator.prepare(bytes, path, mime) do
      {:ok, ^mime} ->
        :ok

      {:error, {reason, diagnostic}} ->
        Diagnostics.error(:resource, reason, diagnostic.message,
          operation: :load_embedded_resource,
          module: __MODULE__,
          source: path
        )
    end
  end
end
