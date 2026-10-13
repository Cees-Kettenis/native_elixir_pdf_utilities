defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes do
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
        ~c"native/libnative_svg_backend"
      )

    :erlang.load_nif(path, 0)
  end

  @doc false
  @spec css_color_math_components(binary()) :: [{non_neg_integer(), binary()}]
  def css_color_math_components(_source), do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec parse_css_color(binary(), boolean(), [{non_neg_integer(), non_neg_integer(), float()}]) ::
          {:ok, {float(), float(), float(), float()} | :currentcolor} | {:error, binary()}
  def parse_css_color(_source, _initial_input, _math), do: :erlang.nif_error(:nif_not_loaded)

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
  @spec render_document(
          binary(),
          pos_integer(),
          pos_integer(),
          [binary()],
          %{binary() => binary()},
          tuple()
        ) ::
          {:ok, binary()} | {:error, binary()}
  def render_document(_source, _width, _height, _fonts, _resources, _matrix),
    do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec vectorize(binary(), [binary()], %{binary() => binary()}) ::
          {:ok, binary()} | {:error, binary()}
  def vectorize(_source, _fonts, _resources), do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec vectorize_slots(binary(), [binary()], [binary()]) :: {:ok, binary()} | {:error, binary()}
  def vectorize_slots(_source, _fonts, _slots), do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec validate_shape_work(
          binary(),
          {pos_integer(), pos_integer(), [binary()]}
          | {pos_integer(), pos_integer(), [binary()], map()}
          | {:prepare_text,
             {pos_integer(), pos_integer(), pos_integer(), pos_integer(), non_neg_integer(),
              pos_integer()}, [binary()]}
          | {:prepare_text, tuple(), [binary()], map()}
        ) ::
          {:ok, {non_neg_integer(), non_neg_integer()}}
          | {:ok,
             {non_neg_integer(), non_neg_integer(), binary(), non_neg_integer(),
              non_neg_integer(), non_neg_integer()}}
          | {:error, binary()}
  def validate_shape_work(_source, _ceilings), do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec validate_pattern_work(binary(), pos_integer(), pos_integer(), pos_integer(), [binary()]) ::
          {:ok, non_neg_integer()} | {:error, binary()}
  def validate_pattern_work(_source, _width, _height, _ceiling, _fonts),
    do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec validate_filter_work(
          binary(),
          pos_integer(),
          pos_integer(),
          {pos_integer(), pos_integer()},
          [binary()]
        ) :: {:ok, {non_neg_integer(), non_neg_integer()}} | {:error, binary()}
  def validate_filter_work(_source, _width, _height, _ceilings, _fonts),
    do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec vectorize_effects(binary(), [binary()], [binary()], {number(), number()}) ::
          {:ok, binary()} | {:error, binary()}
  def vectorize_effects(_source, _fonts, _slots, _scale), do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec vectorize_resources(
          binary(),
          [binary()],
          map(),
          [binary()],
          {pos_integer(), pos_integer()}
        ) :: {:ok, binary()} | {:error, binary()}
  def vectorize_resources(_source, _fonts, _resources, _slots, _dimensions),
    do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec validate_resource_surfaces(
          binary(),
          {pos_integer(), pos_integer()},
          {pos_integer(), pos_integer()},
          [binary()],
          map()
        ) :: {:ok, {non_neg_integer(), non_neg_integer()}} | {:error, binary()}
  def validate_resource_surfaces(_source, _dimensions, _ceilings, _fonts, _resources),
    do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec prepare_html_surface(
          binary(),
          pos_integer(),
          pos_integer(),
          {pos_integer(), pos_integer()},
          non_neg_integer()
        ) ::
          {:ok, {non_neg_integer(), non_neg_integer(), non_neg_integer(), [tuple()]}}
          | {:error, binary()}
  def prepare_html_surface(_source, _width, _height, _ceilings, _source_pixels),
    do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec render_html_surface(binary(), pos_integer(), pos_integer()) ::
          {:ok, binary()} | {:error, binary()}
  def render_html_surface(_source, _width, _height), do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec probe_html_slots(binary(), [binary()], [binary()], map()) ::
          {:ok, binary()} | {:error, binary()}
  def probe_html_slots(_source, _fonts, _slots, _resources),
    do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec vectorize_html_surfaces(
          binary(),
          [binary()],
          map(),
          map(),
          {pos_integer(), pos_integer()},
          {pos_integer(), pos_integer()}
        ) :: {:ok, binary()} | {:error, binary()}
  def vectorize_html_surfaces(_source, _fonts, _surfaces, _resources, _dimensions, _ceilings),
    do: :erlang.nif_error(:nif_not_loaded)

  @doc false
  @spec validate_path(binary(), pos_integer(), float()) ::
          {:ok, non_neg_integer()} | {:error, binary()}
  def validate_path(_source, _max_segments, _max_coordinate),
    do: :erlang.nif_error(:nif_not_loaded)
end
