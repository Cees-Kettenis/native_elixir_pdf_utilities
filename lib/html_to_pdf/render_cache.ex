defmodule NativeElixirPdfUtilities.HtmlToPdf.RenderCache do
  @moduledoc false

  @doc false
  @spec run((reference() -> result)) :: result when result: term()
  def run(fun) do
    cache = make_ref()
    Process.put(cache, %{})

    try do
      fun.(cache)
    after
      Process.delete(cache)
    end
  end

  @doc false
  @spec fetch(reference(), term(), (-> result)) :: result when result: term()
  def fetch(cache, key, loader) do
    case Map.fetch(Process.get(cache), key) do
      {:ok, result} ->
        result

      :error ->
        result = loader.()
        Process.put(cache, Map.put(Process.get(cache), key, result))
        result
    end
  end
end
