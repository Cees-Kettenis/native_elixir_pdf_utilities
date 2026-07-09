defmodule NativeElixirPdfUtilities.HtmlToPdf.BrowserParityTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  @moduletag :browser_parity

  @fixtures_dir Path.expand("../fixtures/html_to_pdf/browser_parity", __DIR__)
  @real_fixtures_dir Path.expand("../fixtures/html_to_pdf", __DIR__)

  @fixture_thresholds [
    {"block_box_model.html", max_changed_ratio: 0.08, max_average_delta: 0.02},
    {"box_sizing_and_margins.html", max_changed_ratio: 0.12, max_average_delta: 0.03},
    {"css_cascade_selectors.html", max_changed_ratio: 0.12, max_average_delta: 0.03},
    {"css_remaining_supported_values.html", max_changed_ratio: 0.16, max_average_delta: 0.04},
    {"display_lists_and_inline_block.html", max_changed_ratio: 0.14, max_average_delta: 0.035},
    {"html_semantics_typography.html", max_changed_ratio: 0.16, max_average_delta: 0.04},
    {"images_data_uris.html", max_changed_ratio: 0.18, max_average_delta: 0.045},
    {"inline_text_flow.html", max_changed_ratio: 0.12, max_average_delta: 0.03},
    {"links_entities_and_protocols.html", max_changed_ratio: 0.16, max_average_delta: 0.04},
    {"text_style_variants.html", max_changed_ratio: 0.14, max_average_delta: 0.035},
    {"units_and_sizing.html", max_changed_ratio: 0.12, max_average_delta: 0.03},
    {"break_variants.html", max_changed_ratio: 0.14, max_average_delta: 0.035},
    {"flex_grid_alignment.html", max_changed_ratio: 0.12, max_average_delta: 0.03},
    {"flex_direction_and_justification.html", max_changed_ratio: 0.14, max_average_delta: 0.035},
    {"grid_tracks_and_placement.html", max_changed_ratio: 0.14, max_average_delta: 0.035},
    {"layout_compositions_remaining.html", max_changed_ratio: 0.18, max_average_delta: 0.045},
    {"table_collapsed_borders.html", max_changed_ratio: 0.10, max_average_delta: 0.025},
    {"table_pagination_headers.html", max_changed_ratio: 0.16, max_average_delta: 0.04},
    {"table_rowspan_tfoot.html", max_changed_ratio: 0.16, max_average_delta: 0.04},
    {"table_separate_borders.html", max_changed_ratio: 0.12, max_average_delta: 0.03},
    {"nested_table_grid_flex.html", max_changed_ratio: 0.14, max_average_delta: 0.035},
    {"nested_table_collapsed_borders.html", max_changed_ratio: 0.14, max_average_delta: 0.035},
    {"page_rules_landscape.html",
     render_opts: [margin: 0], max_changed_ratio: 0.08, max_average_delta: 0.02},
    {"pagination_breaks.html", max_changed_ratio: 0.12, max_average_delta: 0.03}
  ]

  @real_document_fixture_thresholds [
    {"purchase_order.html",
     render_opts: [page_size: :a4],
     chromium_page_size: :a4,
     max_changed_ratio: 0.20,
     max_average_delta: 0.05},
    {"material_requisition.html",
     render_opts: [page_size: :a4],
     chromium_page_size: :a4,
     max_changed_ratio: 0.20,
     max_average_delta: 0.05},
    {"stock_sticker.html",
     render_opts: [page_size: {4.92126, 1.49606}, margin: 0],
     chromium_page_size: {4.92126, 1.49606},
     max_changed_ratio: 0.20,
     max_average_delta: 0.05},
    {"trim_card.html",
     render_opts: [page_size: {11.6929, 8.2677}, margin: 0],
     chromium_page_size: {11.6929, 8.2677},
     max_changed_ratio: 0.20,
     max_average_delta: 0.05}
  ]

  test "has thresholds for every browser parity fixture" do
    configured_names =
      Enum.map(@fixture_thresholds, fn {fixture_name, _thresholds} -> fixture_name end)

    fixture_names =
      @fixtures_dir
      |> Path.join("*.html")
      |> Path.wildcard()
      |> Enum.map(&Path.basename/1)
      |> Enum.sort()

    assert Enum.sort(configured_names) == fixture_names
  end

  test "has thresholds for every real document parity fixture" do
    configured_names =
      Enum.map(@real_document_fixture_thresholds, fn {fixture_name, _thresholds} ->
        fixture_name
      end)

    assert Enum.sort(configured_names) == [
             "material_requisition.html",
             "purchase_order.html",
             "stock_sticker.html",
             "trim_card.html"
           ]
  end

  for {fixture_name, thresholds} <- @fixture_thresholds do
    @fixture_name fixture_name
    @thresholds thresholds

    test "matches Chromium for #{@fixture_name}" do
      fixture_path = Path.join(@fixtures_dir, @fixture_name)

      assert %{page_count: page_count, artifact_dir: artifact_dir} =
               PdfVisualCompare.assert_browser_match!(fixture_path, @thresholds)

      assert page_count >= 1
      assert File.dir?(artifact_dir)
    end
  end

  for {fixture_name, thresholds} <- @real_document_fixture_thresholds do
    @fixture_name fixture_name
    @thresholds thresholds

    test "matches Chromium for real document #{@fixture_name}" do
      fixture_path = Path.join(@real_fixtures_dir, @fixture_name)

      assert %{page_count: page_count, artifact_dir: artifact_dir} =
               PdfVisualCompare.assert_browser_match!(fixture_path, @thresholds)

      assert page_count >= 1
      assert File.dir?(artifact_dir)
    end
  end
end
