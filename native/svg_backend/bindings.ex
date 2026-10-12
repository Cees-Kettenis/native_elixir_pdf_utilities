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
  def render(source, width, height, fonts, resources),
    do: render(source, width, height, fonts, resources, false)

  @doc false
  @spec render(
          binary(),
          pos_integer(),
          pos_integer(),
          [binary()],
          %{binary() => binary()},
          boolean()
        ) :: {:ok, binary()} | {:error, binary()}
  def render(_source, _width, _height, _fonts, _resources, _authored_hints),
    do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec vectorize(binary(), [binary()], %{binary() => binary()}) ::
          {:ok, binary()} | {:error, binary()}
  def vectorize(_source, _fonts, _resources), do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec validate_shape_work(binary(), pos_integer()) :: {:ok, non_neg_integer()} | {:error, binary()}
  def validate_shape_work(_source, _ceiling), do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec validate_path(binary(), pos_integer(), float()) ::
          {:ok, non_neg_integer()} | {:error, binary()}
  def validate_path(_source, _max_segments, _max_coordinate),
    do: :erlang.nif_error(:nif_not_loaded)
end
