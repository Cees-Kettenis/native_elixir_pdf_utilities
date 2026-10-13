defmodule NativeElixirPdfUtilities.HtmlToPdf.TablePrintBaselineContextTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator

  @html "<table style='width:180pt;border-collapse:collapse'><tr><td style='padding:0;vertical-align:baseline;font-size:8pt;line-height:12pt'>Small</td><td style='padding:0;vertical-align:baseline;font-size:18pt;line-height:24pt'>Large</td></tr></table>"

  test "actual paginated flow and child viewport use different issued alignment geometry" do
    assert {:ok, dom} = HtmlParser.parse_detailed(@html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    [table] = styled.children
    [row] = table.children
    assert Enum.all?(row.children, &(&1.style.vertical_align == :baseline))

    assert {:ok, paged} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
    assert {:ok, surface} = Layout.layout_viewport(styled, 240.0, 160.0)
    small_paged = Enum.find(paged.boxes, &(Map.get(&1, :text) == "Small"))
    large_paged = Enum.find(paged.boxes, &(Map.get(&1, :text) == "Large"))
    small_surface = Enum.find(surface.boxes, &(Map.get(&1, :text) == "Small"))
    large_surface = Enum.find(surface.boxes, &(Map.get(&1, :text) == "Large"))
    assert small_paged.y > large_paged.y + 5.0
    assert_in_delta small_surface.y, large_surface.y, 0.000001
    assert HtmlTableValidator.baseline_policy(false) == :fragmented_start
  end

  test "measured block children use their actual unfragmented table geometry" do
    NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.run(
      [],
      NativeElixirPdfUtilities.Validators.HtmlValidator.new_image_budget(),
      fn context ->
        assert {:ok, dom} = HtmlParser.parse_detailed("<div>" <> @html <> "</div>")
        assert {:ok, %{children: [node]}} = Style.compute_detailed(dom)
        assert {:ok, measured} = Layout.measure_child(node, context, %{available_width: 180.0})
        small = Enum.find(measured.boxes, &(Map.get(&1, :text) == "Small"))
        large = Enum.find(measured.boxes, &(Map.get(&1, :text) == "Large"))
        assert small != nil and large != nil
        assert_in_delta small.y, large.y, 0.000001
      end
    )
  end

  test "fragmented print keeps baseline grid allocation while issuing start offsets" do
    assert {:ok, dom} = HtmlParser.parse_detailed(@html)
    assert {:ok, %{children: [table]}} = Style.compute_detailed(dom)
    assert {:ok, %{rows: rows}} = HtmlTableValidator.form(table.children, table.style)

    metrics = %{
      {0, 0} => %{
        intrinsic_height: 30.0,
        minimum_height: 30.0,
        baseline: 8.0,
        inset_bottom: 0.0,
        has_flow: true
      },
      {0, 1} => %{
        intrinsic_height: 24.0,
        minimum_height: 24.0,
        baseline: 20.0,
        inset_bottom: 0.0,
        has_flow: true
      }
    }

    assert {:ok, aligned} =
             HtmlTableValidator.prepare_baseline_geometry(rows, metrics, 0.0, :aligned)

    assert {:ok, fragmented} =
             HtmlTableValidator.prepare_baseline_geometry(rows, metrics, 0.0, :fragmented_start)

    assert aligned.heights == [42.0]
    assert fragmented.heights == aligned.heights
    assert aligned.offsets[{0, 0}] == 12.0
    assert fragmented.offsets == %{{0, 0} => 0.0, {0, 1} => 0.0}

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_baseline_geometry(rows, metrics, 0.0, :bad)

    assert diagnostic.stage == :layout
    assert diagnostic.module == HtmlTableValidator
  end

  test "layout context restores its parent after nested execution and programmer exceptions" do
    assert HtmlTableValidator.baseline_policy(false) == :fragmented_start

    assert HtmlTableValidator.with_layout_context(:surface, fn ->
             assert HtmlTableValidator.baseline_policy(false) == :aligned

             assert HtmlTableValidator.with_layout_context(:paginated, fn ->
                      assert HtmlTableValidator.baseline_policy(false) == :fragmented_start
                      HtmlTableValidator.baseline_policy(true)
                    end) == :aligned

             HtmlTableValidator.baseline_policy(false)
           end) == :aligned

    assert HtmlTableValidator.baseline_policy(false) == :fragmented_start

    assert_raise RuntimeError, "test callback", fn ->
      HtmlTableValidator.with_layout_context(:surface, fn -> raise "test callback" end)
    end

    assert HtmlTableValidator.baseline_policy(false) == :fragmented_start
  end

  test "invalid layout contexts and ancestry return shared actionable diagnostics" do
    for error <- [
          HtmlTableValidator.with_layout_context(:bad, fn -> :ok end),
          HtmlTableValidator.with_layout_context(:surface, :bad),
          HtmlTableValidator.baseline_policy(:bad)
        ] do
      assert {:error, {:invalid_layout, diagnostic}} = error
      assert diagnostic.stage == :layout
      assert diagnostic.reason == :invalid_layout
      assert diagnostic.module == HtmlTableValidator
      assert diagnostic.operation == :prepare_baseline_geometry
      assert diagnostic.message != ""
    end
  end
end
