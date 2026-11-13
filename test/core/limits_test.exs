defmodule NativeElixirPdfUtilities.LimitsTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

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
    Limits.install(Map.put(Limits.effective(), :max_entries, 70_000))

    assert {:ok, 65_534} =
             HtmlValidator.validate_table_span_attribute(%{"rowspan" => "99999"}, "rowspan")

    assert {:ok, 1_000} =
             HtmlValidator.validate_table_span_attribute(%{"colspan" => "99999"}, "colspan")

    assert {:ok, 1_000} =
             HtmlValidator.validate_table_span_attribute(%{"span" => "99999"}, "span")

    Limits.install(Map.put(Limits.effective(), :max_entries, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.validate_table_span_attribute(%{"rowspan" => "3"}, "rowspan")

    assert diagnostic.message =~ "2-item limit"

    assert {:error, {:resource_limit_exceeded, column_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<table><colgroup span=3></colgroup><tr><td>Column limit</td></tr></table>"
             )

    assert column_diagnostic.reason == :resource_limit_exceeded
    assert column_diagnostic.operation == :render and column_diagnostic.message =~ "max_entries"
    alias NativeElixirPdfUtilities.Validators.HtmlTableValidator
    cell = %{tag: "td", style: %{display: :table_cell, colspan: 3}, children: []}
    row = %{tag: "tr", style: %{display: :table_row}, children: [cell]}
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlTableValidator.form([row])
    assert diagnostic.source == "td"
    Limits.install(Map.put(Limits.effective(), :max_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<table><tr><td>Work</td></tr></table>")

    assert diagnostic.message =~ "max_work"
  end

  test "the shared source budget rejects the first source before later image preparation" do
    png =
      Base.decode64!(
        "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADElEQVR4nGNg+A8AAQIBAM9Vq9EAAAAASUVORK5CYII="
      )

    Limits.install(Map.put(Limits.effective(), :max_input_bytes, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<img srcset='chosen 2x'>",
               assets: %{"chosen" => {:bytes, png}}
             )

    assert diagnostic.stage == :html and diagnostic.operation == :render
    assert diagnostic.message =~ "HTML input exceeds max_input_bytes"
  end

  test "responsive candidate and print predicate complexity use centralized limits" do
    alias NativeElixirPdfUtilities.HtmlToPdf
    original = Limits.effective()
    Limits.install(Map.put(original, :max_entries, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<img srcset='a 1x, b 2x, c 3x'>")

    assert diagnostic.stage == :html and diagnostic.message =~ "max_entries"
    Limits.install(Map.put(original, :max_depth, 2))

    html =
      "<picture style='display:block'><source media='((((width:20px))))' srcset=a><img src=b></picture>"

    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.message =~ "max_depth"
    Limits.install(Map.put(original, :max_numeric_token_bytes, 3))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<img srcset='a 1234w'>")

    assert diagnostic.message =~ "max_numeric_token_bytes"
  end

  test "gauge numeric work and artwork use existing centralized resource budgets" do
    Limits.install(Map.put(Limits.effective(), :max_numeric_token_bytes, 3))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<meter value='1234'></meter>")

    assert diagnostic.stage == :html and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_numeric_token_bytes"
    Limits.install(Map.put(Limits.defaults(), :max_entries, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<progress value=.5></progress>")

    assert diagnostic.operation == :render and diagnostic.message =~ "max_entries"
  end

  test "uses every built-in default when no overrides are configured" do
    assert {:ok, defaults} = LimitsValidator.validate([])
    assert defaults == Limits.defaults()
    assert Limits.effective() == Limits.defaults()
    assert map_size(defaults) == 21
    assert defaults.max_input_bytes == 50_000_000
    assert defaults.max_output_bytes == 50_000_000
    assert defaults.max_decoded_bytes == 100_000_000
    assert defaults.max_entries == 1_000_000
    assert defaults.max_work == 1_000_000_000
    assert defaults.max_depth == 1_000
    assert defaults.max_cache_bytes == 100_000_000
    assert defaults.max_raster_pixels == 134_217_728
    assert defaults.max_raster_work == 250_000_000_000
  end

  test "the public key type exactly describes every accepted limit" do
    assert {:ok, types} = Code.Typespec.fetch_types(Limits)

    assert {:type, {:key, {:type, _, :union, members}, []}} =
             Enum.find(types, fn {_kind, {name, _definition, _arguments}} -> name == :key end)

    keys = Enum.map(members, fn {:atom, _, key} -> key end)
    assert MapSet.new(keys) == MapSet.new(Map.keys(Limits.defaults()))
    assert length(keys) == map_size(Limits.defaults())
  end

  test "accepts a partial override for every configurable resource limit" do
    Enum.each(Limits.defaults(), fn {key, default} ->
      configured = default + 1

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

    Limits.install(Map.put(Limits.effective(), :max_numeric_token_bytes, 3))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<ol start='1234'><li>A</li></ol>")

    assert diagnostic.stage == :html
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_numeric_token_bytes"
    assert diagnostic.source == "1234"
    assert diagnostic.operation == :render

    assert {:error, {:resource_limit_exceeded, _}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<ol><li value='1234'>A</li></ol>")
  end

  test "unknown settings are ignored with one actionable warning without logging their values" do
    log =
      capture_log(fn ->
        assert {:ok, limits} =
                 LimitsValidator.validate(
                   max_typo_bytes: "private-config-value",
                   max_typo_bytes: 0,
                   max_pdf_value_depth: :invalid,
                   max_input_bytes: 123
                 )

        assert limits == Map.put(Limits.defaults(), :max_input_bytes, 123)
      end)

    assert log =~
             "Ignoring unknown resource limit settings [:max_typo_bytes, :max_pdf_value_depth]"

    assert log =~ "have no effect"
    assert log =~ "Limits.defaults/0"
    refute log =~ "private-config-value"
  end

  test "unknown-only configuration keeps all defaults" do
    assert capture_log(fn ->
             assert {:ok, limits} = LimitsValidator.validate(max_typo_bytes: :invalid)
             assert limits == Limits.defaults()
           end) =~ ":max_typo_bytes"
  end

  test "rejects malformed, repeated recognized, and unsafe limit configuration" do
    assert {:error, "resource limits must be configured as a keyword list"} =
             LimitsValidator.validate(%{})

    assert {:error, "resource limit keys must not be repeated"} =
             LimitsValidator.validate(max_input_bytes: 1, max_input_bytes: 2)

    for value <- [0, -1, :infinity, 9_223_372_036_854_775_808] do
      assert {:error, value_message} = LimitsValidator.validate(max_input_bytes: value)
      assert value_message =~ ":max_input_bytes"
    end
  end

  test "ignored settings do not bypass recognized value and duplicate validation" do
    log =
      capture_log(fn ->
        assert {:error, message} = LimitsValidator.validate(max_typo_bytes: 1, max_input_bytes: 0)
        assert message =~ ":max_input_bytes"

        assert {:error, "resource limit keys must not be repeated"} =
                 LimitsValidator.validate(
                   max_typo_bytes: 1,
                   max_input_bytes: 1,
                   max_input_bytes: 2
                 )
      end)

    assert log =~ ":max_typo_bytes"
  end

  test "shared budgets require no paired per-feature settings or configurable format bounds" do
    for key <- [:max_input_bytes, :max_decoded_bytes, :max_entries] do
      assert {:ok, limits} = LimitsValidator.validate([{key, 2}])
      assert limits[key] == 2
    end

    # PDF CID bounds belong to the format and remain fixed independently of this collection allowance.
    assert {:ok, %{max_entries: 100_000}} = LimitsValidator.validate(max_entries: 100_000)

    assert capture_log(fn ->
             assert {:ok, limits} = LimitsValidator.validate(max_cid_width_entries: 65_537)
             assert limits == Limits.defaults()
           end) =~ ":max_cid_width_entries"
  end

  test "all receive timeout settings share the runtime's inclusive range" do
    for key <- [
          :max_operation_timeout_ms,
          :operation_memory_check_interval_ms,
          :max_operation_timeout_ms
        ] do
      for value <- [1, 4_294_967_295] do
        assert {:ok, limits} = LimitsValidator.validate([{key, value}])
        assert limits[key] == value
      end

      for value <- [4_294_967_296, 9_223_372_036_854_775_807] do
        assert {:error, message} = LimitsValidator.validate([{key, value}])
        assert message == "resource limit #{inspect(key)} exceeds the BEAM receive timeout range"
      end
    end
  end

  test "startup rejects unsupported receive timeouts without installing them" do
    original_config = Application.get_env(:native_elixir_pdf_utilities, :limits, [])
    original_limits = Limits.effective()

    on_exit(fn ->
      Application.put_env(:native_elixir_pdf_utilities, :limits, original_config)
    end)

    for key <- [
          :max_operation_timeout_ms,
          :operation_memory_check_interval_ms,
          :max_operation_timeout_ms
        ] do
      Application.put_env(:native_elixir_pdf_utilities, :limits, [{key, 4_294_967_296}])

      assert {:error, {:invalid_limits_configuration, message}} =
               LibraryApplication.start(:normal, [])

      assert message == "resource limit #{inspect(key)} exceeds the BEAM receive timeout range"
      assert Limits.effective() == original_limits
    end
  end

  test "installed overrides are enforced by each major validation boundary" do
    configured = [
      max_input_bytes: 8,
      max_entries: 1
    ]

    assert {:ok, limits} = LimitsValidator.validate(configured)
    assert :ok = Limits.install(limits)
    assert Limits.effective() == limits
    assert Limits.get(:max_input_bytes) == 8

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

    Application.put_env(:native_elixir_pdf_utilities, :limits, max_input_bytes: 0)

    assert {:error, {:invalid_limits_configuration, message}} =
             LibraryApplication.start(:normal, [])

    assert message =~ ":max_input_bytes"
  end

  test "application startup warns about removed settings and installs recognized overrides" do
    original_config = Application.get_env(:native_elixir_pdf_utilities, :limits, [])

    on_exit(fn ->
      Application.put_env(:native_elixir_pdf_utilities, :limits, original_config)
      {:ok, _} = Application.ensure_all_started(:native_elixir_pdf_utilities)
    end)

    log =
      capture_log(fn ->
        assert :ok = Application.stop(:native_elixir_pdf_utilities)

        Application.put_env(:native_elixir_pdf_utilities, :limits,
          max_pdf_value_depth: 100,
          max_input_bytes: 123
        )

        assert {:ok, [:native_elixir_pdf_utilities]} =
                 Application.ensure_all_started(:native_elixir_pdf_utilities)

        assert Limits.effective() == Map.put(Limits.defaults(), :max_input_bytes, 123)
      end)

    assert log =~ ":max_pdf_value_depth"
    assert log =~ "have no effect"
  end
end
