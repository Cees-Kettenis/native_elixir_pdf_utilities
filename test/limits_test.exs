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

  test "uses every built-in default when no overrides are configured" do
    assert {:ok, defaults} = LimitsValidator.validate([])
    assert defaults == Limits.defaults()
    assert Limits.effective() == Limits.defaults()
    assert map_size(defaults) == 49
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
