defmodule NativeElixirPdfUtilities.HtmlToPdf.NativeBackendUtilitiesTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{NativeBackend, FormValueBackend}
  alias NativeElixirPdfUtilities.HtmlToPdf.NativeBackend.NativeBytes
  alias NativeElixirPdfUtilities.Validators.NativeBackendValidator

  test "the private utility binding loads without SVG rendering exports" do
    assert :ok = NativeBackendValidator.native_load_result(NativeBackend.load_native())
    assert function_exported?(NativeBytes, :parse_css_color, 3)
    refute function_exported?(NativeBytes, :render, 5)
    refute function_exported?(NativeBytes, :vectorize, 2)
    refute function_exported?(NativeBytes, :prepare_html_surface, 5)
  end

  test "color helpers preserve actual authored channels and evaluated component boundaries" do
    assert :ok = NativeBackendValidator.native_load_result(NativeBackend.load_native())
    assert {:ok, {red, green, blue, alpha}} = NativeBytes.parse_css_color("#33669980", false, [])
    assert_in_delta red, 0.2, 0.000001
    assert_in_delta green, 0.4, 0.000001
    assert_in_delta blue, 0.6, 0.000001
    assert_in_delta alpha, 128 / 255, 0.000001
    assert {:ok, :currentcolor} = NativeBytes.parse_css_color("currentColor", false, [])
    assert {:error, "invalid CSS color"} = NativeBytes.parse_css_color("rgb(broken)", false, [])

    source = "rgb(calc(100 + 20) 0 0)"
    assert [{offset, expression}] = NativeBytes.css_color_math_components(source)
    assert String.trim(expression) == "calc(100 + 20)"

    assert {:ok, {red, +0.0, +0.0, 1.0}} =
             NativeBytes.parse_css_color(source, false, [{offset, 0, 120.0}])

    assert_in_delta red, 120 / 255, 0.000001
  end

  test "Unicode line breaks retain UTF8 byte boundaries and mandatory newlines" do
    assert :ok = NativeBackendValidator.native_load_result(NativeBackend.load_native())
    assert [{3, false}, {7, true}, {8, true}] = NativeBytes.unicode_line_breaks("甲乙\nB")
  end

  test "disposable URL parsing permits unused zero match work and enforces used zero compile work" do
    url = "https://example.test/"
    payload = <<2, byte_size(url)::unsigned-big-32, url::binary>>

    request = %{
      payload: payload,
      ceilings: ["100000", "0", "8388608", "1000", Integer.to_string(byte_size(payload)), "2000"],
      timeout: 2500
    }

    assert {:ok, <<3, compile::unsigned-big-64, 0::unsigned-big-64, allocated::unsigned-big-64>>} =
             FormValueBackend.run(request)

    assert compile > 0 and allocated > 0
    assert {:exit, 74} = FormValueBackend.run(%{request | ceilings: ["0" | tl(request.ceilings)]})
  end

  test "unavailable independent helpers return actionable shared diagnostics" do
    assert {:error, {:native_backend_unavailable, diagnostic}} =
             NativeBackendValidator.native_load_result({:error, :nofile})

    assert diagnostic.stage == :layout
    assert diagnostic.module == NativeBackendValidator
    assert diagnostic.operation == :load_native
    assert diagnostic.source == "HTML utilities"
    assert diagnostic.message =~ "native/html_backend"
    assert diagnostic.message =~ "nofile"
  end
end
