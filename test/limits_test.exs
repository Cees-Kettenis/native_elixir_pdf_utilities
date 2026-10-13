defmodule NativeElixirPdfUtilities.LimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Application, as: LibraryApplication
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.LimitsValidator
  alias NativeElixirPdfUtilities.Validators.MergeValidator
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  setup do
    original_limits = Limits.effective()

    on_exit(fn ->
      Limits.install(original_limits)
    end)

    :ok
  end

  test "HTML protocol span bounds remain fixed when resource limits are raised" do
    Limits.install(Map.put(Limits.effective(), :max_layout_cardinality, 70_000))

    assert {:ok, 65_534} =
             HtmlValidator.validate_table_span_attribute(%{"rowspan" => "99999"}, "rowspan")

    assert {:ok, 1_000} =
             HtmlValidator.validate_table_span_attribute(%{"colspan" => "99999"}, "colspan")

    assert {:ok, 1_000} =
             HtmlValidator.validate_table_span_attribute(%{"span" => "99999"}, "span")

    Limits.install(Map.put(Limits.effective(), :max_layout_cardinality, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.validate_table_span_attribute(%{"rowspan" => "3"}, "rowspan")

    assert diagnostic.message =~ "2-item limit"

    assert {:error, {:resource_limit_exceeded, column_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<table><colgroup span=3></colgroup><tr><td>Column limit</td></tr></table>"
             )

    assert column_diagnostic.stage == :limits and
             column_diagnostic.reason == :resource_limit_exceeded

    assert column_diagnostic.operation == :render and column_diagnostic.message =~ "2-item limit"
    alias NativeElixirPdfUtilities.Validators.HtmlTableValidator
    cell = %{tag: "td", style: %{display: :table_cell, colspan: 3}, children: []}
    row = %{tag: "tr", style: %{display: :table_row}, children: [cell]}
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlTableValidator.form([row])
    assert diagnostic.source == "td"
    Limits.install(Map.put(Limits.effective(), :max_table_grid_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<table><tr><td>Work</td></tr></table>")

    assert diagnostic.message =~ "max_table_grid_work"
  end

  test "chosen image budget failures preserve the selected resource diagnostic" do
    png =
      Base.decode64!(
        "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADElEQVR4nGNg+A8AAQIBAM9Vq9EAAAAASUVORK5CYII="
      )

    Limits.install(Map.put(Limits.effective(), :max_image_source_bytes, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<img srcset='chosen 2x'>",
               assets: %{"chosen" => {:bytes, png}}
             )

    assert diagnostic.source == "chosen" and diagnostic.message =~ "image"
  end

  test "responsive candidate and print predicate complexity use centralized limits" do
    alias NativeElixirPdfUtilities.HtmlToPdf
    original = Limits.effective()
    Limits.install(Map.put(original, :max_responsive_image_candidates, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<img srcset='a 1x, b 2x, c 3x'>")

    assert diagnostic.stage == :html and diagnostic.message =~ "max_responsive_image_candidates"
    Limits.install(Map.put(original, :max_media_query_depth, 2))

    html =
      "<picture style='display:block'><source media='((((width:20px))))' srcset=a><img src=b></picture>"

    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.stage == :style and diagnostic.message =~ "max_media_query_depth"
    Limits.install(Map.put(original, :max_html_numeric_token_bytes, 3))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<img srcset='a 1234w'>")

    assert diagnostic.message =~ "max_html_numeric_token_bytes"
  end

  test "gauge numeric work and artwork use existing centralized resource budgets" do
    Limits.install(Map.put(Limits.effective(), :max_html_numeric_token_bytes, 3))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<meter value='1234'></meter>")

    assert diagnostic.stage == :html and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_html_numeric_token_bytes"
    Limits.install(Map.put(Limits.defaults(), :max_layout_boxes, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<progress value=.5></progress>")

    assert diagnostic.stage == :layout and diagnostic.message =~ "max_layout_boxes"
  end

  test "uses every built-in default when no overrides are configured" do
    assert {:ok, defaults} = LimitsValidator.validate([])
    assert defaults == Limits.defaults()
    assert Limits.effective() == Limits.defaults()
    assert defaults.max_svg_gradient_work == 100_000
    assert map_size(defaults) == 155
    assert defaults.max_frameset_tracks == 1_024
    assert defaults.max_frameset_cells == 65_536
  end

  test "accepts a partial override for every configurable resource limit" do
    Enum.each(Limits.defaults(), fn {key, default} ->
      configured =
        case key do
          :max_cid_width_entries -> default - 1
          _ -> default + 1
        end

      assert {:ok, limits} = LimitsValidator.validate([{key, configured}])
      assert limits[key] == configured
      assert map_size(limits) == map_size(Limits.defaults())
    end)
  end

  test "HTML integer parsing uses ASCII prefixes and bounds numeric work" do
    for {source, expected} <- [
          {"  +12 trailing", 12},
          {"\t-02.5", -2},
          {"0", 0},
          {"2147483648", 2_147_483_648}
        ] do
      assert HtmlValidator.parse_html_integer(source) == {:ok, expected}
    end

    for source <- [nil, "", "-", " + 2", "\u00A02", "٢"] do
      assert HtmlValidator.parse_html_integer(source) == :error
    end

    Limits.install(Map.put(Limits.effective(), :max_html_numeric_token_bytes, 3))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<ol start='1234'><li>A</li></ol>")

    assert diagnostic.stage == :html
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_html_numeric_token_bytes"
    assert diagnostic.source == "1234"
    assert diagnostic.operation == :render

    assert {:error, {:resource_limit_exceeded, _}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<ol><li value='1234'>A</li></ol>")
  end

  test "rejects malformed, unknown, repeated, and unsafe limit configuration" do
    assert {:error, "resource limits must be configured as a keyword list"} =
             LimitsValidator.validate(%{})

    assert {:error, unknown_message} = LimitsValidator.validate(max_typo_bytes: 1)
    assert unknown_message =~ ":max_typo_bytes"

    assert {:error, "resource limit keys must not be repeated"} =
             LimitsValidator.validate(max_svg_bytes: 1, max_svg_bytes: 2)

    for value <- [0, -1, :infinity, 9_223_372_036_854_775_808] do
      assert {:error, value_message} = LimitsValidator.validate(max_svg_bytes: value)
      assert value_message =~ ":max_svg_bytes"
    end
  end

  test "rejects inconsistent aggregate, object-stream, and CID limits" do
    assert {:error, source_message} =
             LimitsValidator.validate(
               max_image_source_bytes: 10,
               max_aggregate_image_source_bytes: 9
             )

    assert source_message =~ ":max_aggregate_image_source_bytes"

    assert {:error, decoded_message} =
             LimitsValidator.validate(
               max_decoded_image_bytes: 10,
               max_aggregate_decoded_image_bytes: 9
             )

    assert decoded_message =~ ":max_aggregate_decoded_image_bytes"

    assert {:error, object_message} =
             LimitsValidator.validate(
               max_pdf_objects: 10,
               max_pdf_object_stream_entries: 11
             )

    assert object_message =~ ":max_pdf_object_stream_entries"

    assert {:error, info_message} =
             LimitsValidator.validate(
               max_pdf_info_value_bytes: 10,
               max_pdf_info_total_bytes: 9
             )

    assert info_message =~ ":max_pdf_info_total_bytes"

    assert {:error, cid_message} = LimitsValidator.validate(max_cid_width_entries: 65_537)
    assert cid_message =~ "PDF CID range"
  end

  test "installed overrides are enforced by each major validation boundary" do
    configured = [
      max_svg_bytes: 10,
      max_layout_cardinality: 2,
      max_pdf_input_bytes: 8,
      max_merge_inputs: 1
    ]

    assert {:ok, limits} = LimitsValidator.validate(configured)
    assert :ok = Limits.install(limits)
    assert Limits.effective() == limits
    assert Limits.get(:max_svg_bytes) == 10

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             HtmlValidator.validate_svg_raster(String.duplicate("x", 11), [])

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             HtmlValidator.validate_layout_cardinality(:grid_tracks, 3)

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             PdfValidator.validate_input("%PDF-1.7\n")

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             MergeValidator.validate_inputs(["one", "two"])
  end

  test "application startup rejects invalid configuration before starting children" do
    original_config = Application.get_env(:native_elixir_pdf_utilities, :limits, [])

    on_exit(fn ->
      Application.put_env(:native_elixir_pdf_utilities, :limits, original_config)
    end)

    Application.put_env(:native_elixir_pdf_utilities, :limits, max_svg_bytes: 0)

    assert {:error, {:invalid_limits_configuration, message}} =
             LibraryApplication.start(:normal, [])

    assert message =~ ":max_svg_bytes"
  end
end
