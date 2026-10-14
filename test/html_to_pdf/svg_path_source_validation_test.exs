Code.require_file("../support/svg_converted_image.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPathSourceValidationTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.Validators.SvgSceneValidator
  alias NativeElixirPdfUtilities.TestSupport.SvgConvertedImage

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "original malformed path inputs fail source admission instead of parser recovery" do
    for d <- ["L1 2", "L 1 2", "M1 2 L3", "M 0 0 A 10 10 0 2 0 20 20", "M1e999 0"] do
      assert {:error, {:invalid_document, diagnostic}} =
               SvgSceneValidator.validate_node(%{
                 tag: "path",
                 attributes: %{"d" => d},
                 children: []
               })

      assert diagnostic.reason == :invalid_document
      assert diagnostic.stage == :layout and diagnostic.operation == :prepare
      assert diagnostic.module == SvgSceneValidator and diagnostic.source == "path d"
      assert diagnostic.message =~ "path d" and diagnostic.message =~ "complete"

      assert {:error, {:invalid_document, public_diagnostic}} =
               HtmlToPdf.render("<svg width=20 height=20><path d='#{d}' /></svg>")

      assert public_diagnostic.module == SvgSceneValidator
      assert public_diagnostic.operation == :prepare and public_diagnostic.source == "path d"
    end
  end

  test "original empty and trailing-dot paths retain public converter artwork behavior" do
    for d <- [
          "",
          "M0 0 L10 10 H15 V5 Z",
          "m1 1 c1 2 3 4 5 6 s2 3 4 5 q1 2 3 4 t5 6 a2 3 10 0 1 4 5 z",
          "M0. 0. L10. 10."
        ] do
      original = "<svg width=40 height=40><path d='#{d}' stroke=red fill=none /></svg>"

      assert :ok =
               SvgSceneValidator.validate_node(%{
                 tag: "path",
                 attributes: %{"d" => d},
                 children: []
               })

      assert {:ok, pdf} = HtmlToPdf.render(original)
      assert pdf =~ "/Subtype /Image"
      image = SvgConvertedImage.render(original)

      if d == "",
        do: assert(SvgConvertedImage.coverage(image) == 0),
        else: assert(SvgConvertedImage.coverage(image) > 0)
    end
  end

  test "commands accept implicit repetitions with their actual parameter arities" do
    for d <- [
          "M0 0 10 10 20 20",
          "m0 0-1-2+3+4",
          "M0 0L1 2 3 4",
          "M0 0H1 2 3v4 5",
          "M0 0C1 2 3 4 5 6 7 8 9 10 11 12",
          "M0 0s1 2 3 4 5 6 7 8",
          "M0 0Q1 2 3 4 5 6 7 8t9 10 11 12",
          "M0 0Z L1 2zM3 4zz",
          "M0 0a1 2 0 01 3 4 5 6 0 10 7 8"
        ] do
      assert :ok =
               SvgSceneValidator.validate_node(%{
                 tag: "path",
                 attributes: %{"d" => d},
                 children: []
               })
    end

    for d <- [
          "M",
          "m1",
          "Z",
          "H1",
          "M0 0L1",
          "M0 0C1 2 3 4 5",
          "M0 0S1 2 3",
          "M0 0Q1 2 3",
          "M0 0T1",
          "M0 0A1 2 3 0 1 4",
          "M0 0Z1 2",
          "M0 0R1 2",
          "M0 0L1 2 3"
        ] do
      assert {:error, {:invalid_document, _}} =
               SvgSceneValidator.validate_node(%{
                 tag: "path",
                 attributes: %{"d" => d},
                 children: []
               })
    end
  end

  test "SVG number spelling and compact arc flags are lexed without coordinate reconstruction" do
    for d <- [
          "M.5-.25L1e-2+2E+1",
          "M0.6.5",
          "M1.e2 0.",
          "M+0-0L.0+.0",
          "M0 0A10 10 0 0110 20",
          "M0 0a-10 -20 -30 10-5-6",
          "M0 0A1 2 0,0,1,3,4",
          "M0 0 A1 2 0 00.5.6",
          "M0,0\tL1,2\n3,4\rZ",
          "M0\f0",
          "none",
          "NoNe",
          "   "
        ] do
      assert :ok =
               SvgSceneValidator.validate_node(%{
                 tag: "path",
                 attributes: %{"d" => d},
                 children: []
               })
    end

    for d <- [
          "M,0 0",
          "M0,,0",
          "M0 0,",
          "M0 0,L1 2",
          "M0;0",
          "M0\u00a00",
          "M. 0",
          "M1e 0",
          "M1e+ 0",
          "M1e--2 0",
          "MNaN 0",
          "MInfinity 0",
          "M0 0L1px 2",
          "M0 0A1 2 0 0210 20",
          "M0 0A1 2 0 +1 0 3 4",
          "M0 0A1 2 0 .0 1 3 4",
          "M0 0A1 2 0 1e0 0 3 4",
          "M0 0A1 2 0 0 1 3"
        ] do
      assert {:error, {:invalid_document, _}} =
               SvgSceneValidator.validate_node(%{
                 tag: "path",
                 attributes: %{"d" => d},
                 children: []
               })
    end
  end

  test "path source syntax uses existing numeric and path byte ceilings without new work counters" do
    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 10))

    assert :ok =
             SvgSceneValidator.validate_node(%{
               tag: "path",
               attributes: %{"d" => "M10 -10"},
               children: []
             })

    assert {:error, {:invalid_document, _}} =
             SvgSceneValidator.validate_node(%{
               tag: "path",
               attributes: %{"d" => "M10.1 0"},
               children: []
             })

    Limits.install(Map.put(Limits.effective(), :max_svg_path_bytes, 3))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgSceneValidator.validate_node(%{
               tag: "path",
               attributes: %{"d" => "M0 0"},
               children: []
             })

    assert diagnostic.message =~ "max_svg_path_bytes"

    assert :ok =
             SvgSceneValidator.validate_node(%{
               tag: "g",
               attributes: %{"d" => "L1"},
               children: []
             })

    assert {:error, {:invalid_document, %{source: "path d"}}} =
             SvgSceneValidator.validate_node(%{
               tag: "path",
               attributes: %{"d" => nil},
               children: []
             })
  end
end
