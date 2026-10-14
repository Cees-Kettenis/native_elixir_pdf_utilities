defmodule NativeElixirPdfUtilities.HtmlToPdf.NativeBackend.NativeBytes do
  @moduledoc false
  # This generated private BEAM is also an explicit Dialyzer input.
  @compile :debug_info
  @on_load :load_native

  @doc false
  @spec load_native() :: :ok | {:error, term()}
  def load_native do
    path =
      :filename.join(
        :code.priv_dir(:native_elixir_pdf_utilities),
        ~c"native/libnative_html_backend"
      )

    :erlang.load_nif(path, 0)
  end

  @doc false
  @spec unicode_line_breaks(binary()) :: [{non_neg_integer(), boolean()}]
  def unicode_line_breaks(_source), do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec css_color_math_components(binary()) :: [{non_neg_integer(), binary()}]
  def css_color_math_components(_source), do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec parse_css_color(binary(), boolean(), [{non_neg_integer(), non_neg_integer(), float()}]) ::
          {:ok, {float(), float(), float(), float()} | :currentcolor} | {:error, binary()}
  def parse_css_color(_source, _initial_input, _math), do: :erlang.nif_error(:nif_not_loaded)
end
