defmodule NativeElixirPdfUtilities.Validators.PdfFormValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Operation
  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @sources_key {__MODULE__, :prepared_sources}

  @doc "Collects reusable source preparations during one writer validation."
  @spec with_prepared_sources((-> result)) :: {result, %{binary() => map()}} when result: term()
  def with_prepared_sources(fun) do
    RenderCache.run(fn cache ->
      previous = Process.put(@sources_key, cache)

      try do
        result = PdfValidator.with_reader_budget(fun)
        sources = Map.new(for {source, {:ok, form}} <- Process.get(cache), do: {source, form})
        {result, sources}
      after
        if previous == nil,
          do: Process.delete(@sources_key),
          else: Process.put(@sources_key, previous)
      end
    end)
  end

  @doc "Prepares a genuine single-page PDF for ordinary PDF Form resource import."
  @spec prepare_source(term()) :: {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_source(source) do
    case Process.get(@sources_key) do
      nil -> prepare_uncached_source(source)
      cache -> RenderCache.fetch(cache, source, fn -> prepare_uncached_source(source) end)
    end
  end

  defp prepare_uncached_source(source) do
    # Enter the bounded operation before opening its process-local allowances.
    # Reader.read then reuses this worker for parsing and subsequent decoding.
    Operation.run(__MODULE__, :prepare_pdf_form, fn ->
      HtmlValidator.with_render_budget(fn ->
        PdfValidator.with_reader_budget(fn ->
          with {:ok, document} <- Reader.read(source),
               [page] <- document.pages,
               %{value: %{"Contents" => contents} = page_value} <-
                 Map.get(document.objects, page.ref),
               {:ok, dictionary, stream} <- prepare_content(contents, document),
               [0, 0, width, height] <- page.media_box,
               true <-
                 is_map(page.resources) and is_number(width) and is_number(height) and width > 0 and
                   height > 0 do
            {:ok,
             %{
               width: width,
               height: height,
               stream: stream,
               content_dictionary: dictionary,
               resources: page.resources,
               group: Map.get(page_value, "Group"),
               objects: document.objects
             }}
          else
            {:error, {_reason, _diagnostic}} = error ->
              error

            _ ->
              Diagnostics.error(
                :pdf,
                :invalid_pdf_input,
                "PDF Form import requires one origin-zero page with bounded dimensions, resources and content streams",
                module: __MODULE__,
                operation: :prepare_pdf_form,
                source: "PDF Form source"
              )
          end
        end)
      end)
    end)
  end

  defp prepare_content(contents, document) do
    case contents do
      {:ref, reference} ->
        case Map.get(document.objects, reference) do
          %{value: dictionary, stream: stream} when is_map(dictionary) and is_binary(stream) ->
            {:ok, dictionary, stream}

          _ ->
            :invalid
        end

      streams when is_list(streams) and streams != [] ->
        Enum.reduce_while(streams, {:ok, []}, fn reference, {:ok, data} ->
          # Decoders charge every occurrence. Reserve its joining separator
          # first, including for empty streams, before decoding or retaining it.
          if data != [], do: PdfValidator.charge_reader_budget(:max_pdf_reader_decoded_bytes, 1)

          case Reader.decoded_stream(document, reference) do
            {:ok, stream} -> {:cont, {:ok, [stream | data]}}
            error -> {:halt, error}
          end
        end)
        |> case do
          {:ok, data} -> {:ok, %{}, data |> Enum.reverse() |> Enum.join("\n")}
          error -> error
        end

      _ ->
        :invalid
    end
  end
end
