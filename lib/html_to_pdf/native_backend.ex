defmodule NativeElixirPdfUtilities.HtmlToPdf.NativeBackend do
  @moduledoc false

  @doc "Loads the private CSS color and Unicode line-break utility binding."
  @spec load_native() :: {:module, module()} | {:error, term()}
  def load_native do
    directory = :filename.join(:code.priv_dir(:native_elixir_pdf_utilities), ~c"native")
    :code.add_patha(directory)
    :code.ensure_loaded(NativeElixirPdfUtilities.HtmlToPdf.NativeBackend.NativeBytes)
  end
end
