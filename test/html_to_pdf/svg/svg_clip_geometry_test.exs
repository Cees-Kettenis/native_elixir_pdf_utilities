alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgClipGeometryTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{PdfWriter}

  setup do
    limits = Limits.effective()

    %{limits: limits}
  end

  test "writer retains quadrilateral navigation and rejects malformed hit regions" do
    region = %{
      type: :link_region,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      link_url: "https://example.test/clip",
      link_quad_points: [0, 20, 20, 0, 0, 0, 20, 0]
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [region]}])
    assert pdf =~ "/QuadPoints [0 20 0 20 0 0 20 0]"

    rectangle = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      fill_color: {0, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0
    }

    for quad <- [[], [1, 2], region.link_quad_points] do
      assert {:error, {:invalid_pdf_input, _}} =
               PdfWriter.render([
                 %{size: {100, 100}, boxes: [Map.put(rectangle, :link_quad_points, quad)]}
               ])
    end

    fractional = %{
      type: :link_region,
      x: 11.3525,
      y: 44.9843,
      width: 64.29499999999999,
      height: 0.36660000000000537,
      link_url: "https://example.test/fractional",
      link_quad_points: [11.4396, 45.3509, 75.5604, 45.3509, 11.3525, 44.9843, 75.6475, 44.9843]
    }

    assert {:ok, _} = PdfWriter.render([%{size: {100, 100}, boxes: [fractional]}])

    for quad <- [
          [0, 0],
          [0, 20, 30, 20, 0, 0, 20, 0],
          [0, 0, 20, 20, 0, 20, 20, 0],
          List.duplicate(0, 8)
        ] do
      assert {:error, {_, %{stage: :pdf, reason: _, message: message}}} =
               PdfWriter.render([
                 %{size: {100, 100}, boxes: [Map.put(region, :link_quad_points, quad)]}
               ])

      assert message != ""
    end
  end

  test "page bounds and nested foreign Form projection retain triangle hit geometry" do
    triangle = %{
      type: :link_region,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      link_url: "https://example.test/triangle",
      link_quad_points: [10, 20, 10, 20, 0, 0, 20, 0]
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {10, 10}, boxes: [triangle]}])
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    [link] =
      for {_, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects do
        link
      end

    assert link["Rect"] == [0, 0, 10, 10]
    assert Enum.all?(link["QuadPoints"], &(&1 >= 0 and &1 <= 10))

    paint = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      fill_color: {1, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0,
      shape: {:triangle, :up}
    }

    group = %{
      type: :drawing_group,
      width: 20,
      height: 20,
      transform: {1, 0, 0, 1, 0, 0},
      boxes: [paint, triangle]
    }

    _placement = %{
      width: 20,
      height: 20,
      transform: {1, 0, 0, 1, 0, 0},
      group: group,
      clip: nil,
      clip_geometry: nil
    }
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgClipGeometryGlobalStateTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{PdfWriter}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test(
    "translated clipped drawing groups retain quad hit regions and resource failures remain recoverable",
    %{limits: limits}
  ) do
    region = %{
      type: :link_region,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      link_url: "https://example.test/group",
      link_quad_points: [0, 20, 20, 20, 0, 0, 20, 0]
    }

    group = %{
      type: :drawing_group,
      width: 20,
      height: 20,
      transform: {2, 0, 0, 2, 10, 10},
      clip: %{x: 5, y: 5, width: 10, height: 10},
      boxes: [region]
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [group]}])
    assert pdf =~ "/QuadPoints [20 40 40 40 20 20 40 20]"
    Limits.install(Map.put(limits, :max_work, 10))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             PdfWriter.render([%{size: {100, 100}, boxes: [group]}])

    assert message =~ "max_work"
  end
end
