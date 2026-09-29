defmodule NativeElixirPdfUtilities.HtmlToPdf.RenderCache do
  @moduledoc false

  @doc false
  @spec with_stylesheets((-> result)) :: result when result: term()
  def with_stylesheets(fun) do
    key = {__MODULE__, :stylesheets}

    case Process.get(key) do
      nil ->
        run(fn cache ->
          Process.put(key, cache)

          try do
            fun.()
          after
            Process.delete(key)
          end
        end)

      _cache ->
        fun.()
    end
  end

  @doc false
  @spec fetch_stylesheet(term(), (-> result), (result -> :ok | {:error, term()})) ::
          result | {:error, term()}
        when result: term()
  def fetch_stylesheet(key, loader, on_hit \\ fn _ -> :ok end) do
    case Process.get({__MODULE__, :stylesheets}) do
      nil -> loader.()
      cache -> fetch(cache, key, loader, on_hit)
    end
  end

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

  @doc false
  @spec fetch(reference(), term(), (-> result), (result -> :ok | {:error, term()})) ::
          result | {:error, term()}
        when result: term()
  def fetch(cache, key, loader, on_hit) do
    case Map.fetch(Process.get(cache), key) do
      {:ok, result} ->
        case on_hit.(result) do
          :ok -> result
          {:error, _reason} = error -> error
        end

      :error ->
        result = loader.()
        Process.put(cache, Map.put(Process.get(cache), key, result))
        result
    end
  end
end
