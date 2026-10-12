defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes do
  @moduledoc false
  @on_load :load_native

  @doc false
  @spec load_native() :: :ok | {:error, term()}
  def load_native do
    path =
      :filename.join(
        :code.priv_dir(:native_elixir_pdf_utilities),
        ~c"native/libnative_svg_backend"
      )

    :erlang.load_nif(path, 0)
  end

  @doc false
  @spec render(binary(), pos_integer(), pos_integer(), [binary()], %{binary() => binary()}) ::
          {:ok, binary()} | {:error, binary()}
  def render(_source, _width, _height, _fonts, _resources), do: :erlang.nif_error(:nif_not_loaded)
end
