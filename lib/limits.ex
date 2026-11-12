defmodule NativeElixirPdfUtilities.Limits do
  @moduledoc """
  Runtime resource limits used by PDF parsing, information access, text
  extraction, merging, page transforms, splitting, and HTML-to-PDF rendering.

  HTML/PDF link clipping uses the shared `:max_work` budget for contour
  subdivision and planar arrangement work. `:clip_curve_precision` is
  the reciprocal of the maximum curve-to-chord deviation in PDF points.
  The shared `:max_depth` setting bounds geometric intersections and drawing groups.

  Applications can override individual defaults in `config/config.exs` or
  `config/runtime.exs`:

      config :native_elixir_pdf_utilities,
        limits: [
          max_input_bytes: 75_000_000,
          max_raster_dimension: 16_284
        ]

  Configuration is validated when the library application starts. Values are
  application-wide, and changing them requires restarting the application.
  Document operations default to a five-minute deadline and a 1 GiB observed
  BEAM worker-memory allowance. Native allocations are outside that allowance;
  see the resource limits guide for enforcement boundaries.
  """

  @defaults %{
    max_operation_timeout_ms: 300_000,
    max_operation_memory_bytes: 1_073_741_824,
    operation_memory_check_interval_ms: 25,
    max_input_bytes: 50_000_000,
    max_output_bytes: 50_000_000,
    max_decoded_bytes: 100_000_000,
    max_entries: 1_000_000,
    max_depth: 1_000,
    max_work: 1_000_000_000,
    max_pages: 10_000,
    max_numeric_magnitude: 4_294_967_295,
    max_numeric_token_bytes: 1_024,
    max_cache_bytes: 100_000_000,
    max_cache_entries: 64,
    max_raster_dimension: 8_192,
    max_raster_pixels: 134_217_728,
    max_raster_work: 250_000_000_000,
    max_decompression_ratio: 100,
    svg_raster_scale: 2,
    clip_curve_precision: 1_024,
    font_color_sweep_segments: 2_048
  }

  @persistent_key {__MODULE__, :effective}

  @typedoc "A configurable resource-limit name."
  @type key ::
          :max_operation_timeout_ms
          | :max_operation_memory_bytes
          | :operation_memory_check_interval_ms
          | :max_input_bytes
          | :max_output_bytes
          | :max_decoded_bytes
          | :max_entries
          | :max_depth
          | :max_work
          | :max_pages
          | :max_numeric_magnitude
          | :max_numeric_token_bytes
          | :max_cache_bytes
          | :max_cache_entries
          | :max_raster_dimension
          | :max_raster_pixels
          | :max_raster_work
          | :max_decompression_ratio
          | :svg_raster_scale
          | :clip_curve_precision
          | :font_color_sweep_segments
  @typedoc "The complete validated resource-limit configuration."
  @type t :: %{required(key()) => pos_integer()}

  @doc "Returns the library's built-in resource limits."
  @spec defaults() :: t()
  def defaults do
    @defaults
  end

  @doc "Returns the effective resource limits loaded for this application instance."
  @spec effective() :: t()
  @compile {:inline, effective: 0}
  def effective do
    :persistent_term.get(@persistent_key, @defaults)
  end

  @doc false
  @spec keys() :: [key()]
  def keys do
    Map.keys(@defaults)
  end

  @doc false
  @spec get(key()) :: pos_integer()
  def get(key) do
    Map.fetch!(effective(), key)
  end

  @doc false
  @spec install(t()) :: :ok
  def install(limits) do
    :persistent_term.put(@persistent_key, limits)
  end
end
