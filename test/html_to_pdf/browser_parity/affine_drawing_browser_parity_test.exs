alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.AffineDrawingBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PdfWriter}
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :affine_drawing

  test "rotated sheared reflected groups preserve nested clip paint and link polygons" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/drawing_group_affine.html"),
        native_renderer: &__MODULE__.render_native_groups/2,
        artifact_dir: "tmp/browser_parity/drawing_group_affine",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    pdf = File.read!("tmp/browser_parity/drawing_group_affine/native.pdf")
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

    links =
      for %{value: %{"Subtype" => {:name, "Link"}} = value} <-
            Map.values(context.document.objects),
          do: value

    assert length(links) == 4
    assert Enum.all?(links, &(length(&1["QuadPoints"]) >= 8))
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    for label <- ["Rotated", "Sheared", "Reflected", "Sibling"], do: assert(text =~ label)
  end

  @doc "Renders the direct writer fixture using genuine affine point groups and the exact caller-selected font."
  @spec render_native_groups(String.t(), keyword()) :: {:ok, binary()} | {:error, term()}
  def render_native_groups(_html, opts) do
    with {:ok, registry} <- Font.load_registry(opts),
         {:ok, _, font} <- Font.resolve("DejaVu Sans", 400, :normal, registry) do
      text = %{
        type: :text,
        text: "",
        x: 3,
        y: 8,
        width: 65,
        line_height: 14,
        font: Font.pdf_name(font),
        font_face: font,
        font_size: 12,
        color: {0, 0, 0},
        snap_to_css_pixel_grid: false
      }

      canvas = %{
        type: :rect,
        x: -5,
        y: -5,
        width: 75,
        height: 60,
        fill_color: nil,
        stroke_color: nil,
        stroke_width: 0,
        border_radius: 0
      }

      stripe = %{
        canvas
        | x: 3,
          y: 20,
          width: 48,
          height: 8,
          fill_color: {40 / 255, 120 / 255, 200 / 255}
      }

      panel = %{
        type: :drawing_group,
        width: 60,
        height: 45,
        transform: {1, 0, 0, 1, 0, 0},
        clip: %{x: 0, y: 0, width: 60, height: 45},
        boxes: []
      }

      rotated = %{
        panel
        | transform: {0, 1, -1, 0, 90, 15},
          boxes: [
            %{canvas | fill_color: {238 / 255, 244 / 255, 248 / 255}},
            Map.put(%{text | text: "Rotated N"}, :link_url, "https://example.com/rotation"),
            stripe
          ]
      }

      image = %{
        type: :image,
        x: 0,
        y: 0,
        width: 60,
        height: 40,
        image: %{
          format: :png,
          data: <<255, 0, 0>>,
          width_px: 1,
          height_px: 1,
          color_space: :device_rgb,
          bits_per_component: 8
        },
        link_url: "https://example.com/image"
      }

      nested = %{
        panel
        | width: 24,
          height: 28,
          transform: {0, 1, -1, 0, 50, 16},
          clip: %{x: 0, y: 0, width: 24, height: 28},
          boxes: [image]
      }

      sheared = %{
        panel
        | transform: {1, 0.25, 0.5, 1, 105, 20},
          boxes: [
            %{canvas | fill_color: {247 / 255, 239 / 255, 230 / 255}},
            Map.put(%{text | text: "Sheared N"}, :link_url, "https://example.com/shear"),
            nested
          ]
      }

      reflected = %{
        panel
        | transform: {-1, 0, 0, 1, 105, 110},
          boxes: [
            %{canvas | fill_color: {234 / 255, 240 / 255, 220 / 255}},
            Map.put(%{text | text: "Reflected N"}, :link_url, "https://example.com/reflection"),
            %{stripe | fill_color: {86 / 255, 119 / 255, 66 / 255}}
          ]
      }

      sibling = %{text | text: "Sibling N", x: 125, y: 125}
      PdfWriter.render([%{size: {240, 180}, boxes: [rotated, sheared, reflected, sibling]}], opts)
    end
  end
end
