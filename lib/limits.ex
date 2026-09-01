defmodule NativeElixirPdfUtilities.Limits do
  @moduledoc """
  Runtime resource limits used by PDF parsing, information access, text
  extraction, merging, page transforms, splitting, and HTML-to-PDF rendering.

  Applications can override individual defaults in `config/config.exs` or
  `config/runtime.exs`:

      config :native_elixir_pdf_utilities,
        limits: [
          max_svg_bytes: 10_000_000,
          max_svg_raster_dimension: 16_284
        ]

  Configuration is validated when the library application starts. Values are
  process-wide, and changing them requires restarting the application.
  """

  @defaults %{
    max_svg_bytes: 5_000_000,
    max_svg_raster_dimension: 8_192,
    max_svg_raster_pixels: 16_777_216,
    max_image_count: 1_000,
    max_image_source_bytes: 10_000_000,
    max_aggregate_image_source_bytes: 50_000_000,
    max_decoded_image_bytes: 40_000_000,
    max_aggregate_decoded_image_bytes: 80_000_000,
    max_background_image_tiles: 10_000,
    max_layout_cardinality: 1_000,
    max_pdf_input_bytes: 50_000_000,
    max_pdf_objects: 100_000,
    max_pdf_object_stream_entries: 10_000,
    max_pdf_pages: 10_000,
    max_pdf_page_tree_depth: 1_000,
    max_pdf_reference_chain_depth: 1_000,
    max_pdf_reference_resolution_work: 25_000,
    max_pdf_value_depth: 100,
    max_pdf_decoded_stream_bytes: 25_000_000,
    max_pdf_decompression_ratio: 100,
    max_pdf_xref_length_candidates: 1_000,
    max_pdf_xref_revisions: 1_000,
    max_pdf_info_value_bytes: 1_000_000,
    max_pdf_info_total_bytes: 5_000_000,
    max_merge_inputs: 100,
    max_aggregate_merge_input_bytes: 100_000_000,
    max_merged_objects: 100_000,
    max_merged_pages: 10_000,
    max_split_outputs: 1_000,
    max_split_object_writes: 1_000_000,
    max_aggregate_split_output_bytes: 100_000_000,
    max_text_decoded_content_bytes: 50_000_000,
    max_text_parsed_instructions: 100_000,
    max_text_stream_uses: 100_000,
    max_text_instruction_uses: 1_000_000,
    max_text_form_expansions: 10_000,
    max_text_spans: 25_000,
    max_cmap_bytes: 1_000_000,
    max_cmap_entries: 100_000,
    max_cid_width_entries: 65_536,
    max_form_xobject_depth: 20,
    max_font_cache_entries: 64,
    max_system_font_cache_entries: 64
  }

  @persistent_key {__MODULE__, :effective}

  @typedoc "A configurable resource-limit name."
  @type key ::
          :max_svg_bytes
          | :max_svg_raster_dimension
          | :max_svg_raster_pixels
          | :max_image_count
          | :max_image_source_bytes
          | :max_aggregate_image_source_bytes
          | :max_decoded_image_bytes
          | :max_aggregate_decoded_image_bytes
          | :max_background_image_tiles
          | :max_layout_cardinality
          | :max_pdf_input_bytes
          | :max_pdf_objects
          | :max_pdf_object_stream_entries
          | :max_pdf_pages
          | :max_pdf_page_tree_depth
          | :max_pdf_reference_chain_depth
          | :max_pdf_reference_resolution_work
          | :max_pdf_value_depth
          | :max_pdf_decoded_stream_bytes
          | :max_pdf_decompression_ratio
          | :max_pdf_xref_length_candidates
          | :max_pdf_xref_revisions
          | :max_pdf_info_value_bytes
          | :max_pdf_info_total_bytes
          | :max_merge_inputs
          | :max_aggregate_merge_input_bytes
          | :max_merged_objects
          | :max_merged_pages
          | :max_split_outputs
          | :max_split_object_writes
          | :max_aggregate_split_output_bytes
          | :max_text_decoded_content_bytes
          | :max_text_parsed_instructions
          | :max_text_stream_uses
          | :max_text_instruction_uses
          | :max_text_form_expansions
          | :max_text_spans
          | :max_cmap_bytes
          | :max_cmap_entries
          | :max_cid_width_entries
          | :max_form_xobject_depth
          | :max_font_cache_entries
          | :max_system_font_cache_entries

  @typedoc "The complete validated resource-limit configuration."
  @type t :: %{required(key()) => pos_integer()}

  @doc "Returns the library's built-in resource limits."
  @spec defaults() :: t()
  def defaults do
    @defaults
  end

  @doc "Returns the effective resource limits loaded for this application instance."
  @spec effective() :: t()
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
