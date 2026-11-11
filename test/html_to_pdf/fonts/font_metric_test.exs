defmodule NativeElixirPdfUtilities.HtmlToPdf.FontMetricTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.TestSupport.Paths

  alias NativeElixirPdfUtilities.Validators.{
    FontMetricValidator,
    FontRenderingValidator,
    FontValidator,
    HtmlValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "all supported font aspects use physical metrics or defined missing-glyph defaults" do
    data = File.read!(Paths.fixture("fonts/selected-variable.ttf"))
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Metrics", data: data}])
    assert {:ok, 0.5} = FontMetricValidator.aspect(font, "ex-height")
    assert {:ok, 0.7} = FontMetricValidator.aspect(font, "cap-height")
    assert {:ok, 0.6} = FontMetricValidator.aspect(font, "ch-width")

    for metric <- ~w(ic-width ic-height),
        do: assert({:ok, 1} = FontMetricValidator.aspect(font, metric))

    missing = %{font | cmap: %{}}
    assert {:ok, 0.5} = FontMetricValidator.aspect(missing, "ch-width")
    assert {:ok, nil} = FontMetricValidator.aspect(%{missing | data: <<>>}, "cap-height")
    assert {:ok, unchanged} = FontMetricValidator.prepare_reference(font, %{})
    assert unchanged == %{}

    assert {:ok, cleared} =
             FontMetricValidator.prepare_reference(%{missing | data: <<>>}, %{
               font_controls: %{:"size-adjust" => {"cap-height", :from_font}}
             })

    assert cleared.font_controls[:"size-adjust"] == :none
    selected = %{font | cmap: Map.put(font.cmap, ?水, font.cmap[?H])}
    assert {:ok, 0.7} = FontMetricValidator.aspect(selected, "ic-height")
    assert {:ok, 0.6} = FontMetricValidator.aspect(selected, "ic-width")
    no_metrics = Map.put(%{font | data: <<>>}, :id, "invalid-metrics")
    assert {:error, {_, _}} = FontMetricValidator.aspect(no_metrics, "cap-height")

    assert {:ok, _} =
             FontMetricValidator.prepare(%{missing | data: <<>>}, %{
               font_controls: %{:"size-adjust" => {"cap-height", 0.5}}
             })
  end

  test "metric descriptor failures, used-size limits and public paint metadata retain diagnostics" do
    assert {:ok, %{ascent_override: :normal}} =
             FontMetricValidator.prepare_descriptor("ascent-override", "normal")

    assert :error = FontMetricValidator.prepare_descriptor("size-adjust", :normal)
    assert :error = FontMetricValidator.prepare_descriptor("unknown", 10)
    assert :error = FontMetricValidator.prepare_descriptor("ascent-override", false)
    data = File.read!(Paths.fixture("fonts/selected-container-sans.ttf"))
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Metrics", data: data}])
    assert FontMetricValidator.layout_size(Map.put(font, :metric_scale, 0), 24) == 0
    assert FontMetricValidator.baseline_offset(font, 24) == 0
    assert FontMetricValidator.baseline_offset(Map.put(font, :baseline_shift, 0), 24) == 0

    for patch <- [
          %{metric_scale: -1},
          %{metric_scale: "1"},
          %{glyph_scale: -1},
          %{baseline_shift: :invalid},
          %{layout_metrics: %{}},
          %{layout_metrics: :invalid},
          %{layout_metrics: %{units_per_em: 0, ascent: 10, descent: -1, line_gap: 0}}
        ] do
      assert {:error, {:invalid_font, diagnostic}} =
               FontMetricValidator.validate_paint(Map.merge(font, patch))

      assert diagnostic.operation == :write_pdf
      assert diagnostic.message =~ "metric"
    end

    assert {:ok, synthetic} =
             FontRenderingValidator.prepare(font, %{
               font_controls: %{:"variant-position" => ["super"]}
             })

    assert {:ok, synthetic} = FontRenderingValidator.prepare_grapheme(synthetic, "H")

    assert {:ok, {_, top, _, bottom}} =
             FontValidator.prepare_text_ink_rectangle(%{
               text: "H",
               font_face: synthetic,
               font_size: 24,
               x: 0,
               y: 20
             })

    assert top > 20 and bottom > top
    limit = Limits.get(:max_pdf_numeric_magnitude)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               FontMetricValidator.used_size(%{metric_scale: limit}, 2)
             end)

    assert diagnostic.message =~ "max_pdf_numeric_magnitude"
  end
end
