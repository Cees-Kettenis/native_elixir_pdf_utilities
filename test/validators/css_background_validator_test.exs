defmodule NativeElixirPdfUtilities.Validators.CssBackgroundValidatorTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Validators.{CssBackgroundValidator, CssImageValueValidator}

  @style %{color: {0, 0, 0}, font_size: 12.0, _root_font_size: 12.0}

  test "prepared layer lists reject malformed positions and sizes and retain keyword sizing" do
    for fields <- [
          %{background_positions: [:bad]},
          %{background_sizes: [:bad]},
          %{background_images: [:bad]}
        ] do
      assert {:error, {:invalid_layout, %{source: "div", stage: :layout}}} =
               CssBackgroundValidator.validate_style(fields, "div")
    end

    assert :ok =
             CssBackgroundValidator.validate_style(%{background_sizes: [:cover, :contain]}, "div")

    assert :error = CssBackgroundValidator.prepare(@style, "other", "none")
    assert :error = CssBackgroundValidator.prepare(@style, "background-attachment", "none")

    for {source, expected} <- [{"repeat no-repeat", :repeat_x}, {"no-repeat repeat", :repeat_y}] do
      assert {:ok, %{background_repeat: ^expected}} =
               CssBackgroundValidator.prepare(@style, "background-repeat", source)
    end

    assert {:ok, %{background_sizes: [:cover, :contain]}} =
             CssBackgroundValidator.compute_style(
               Map.put(@style, :background_sizes, [:cover, :contain])
             )

    for source <- ["url('javascript:run()') red", "url('javascript:run()') red, blue"] do
      assert {:error, {:unsupported_html, %{source: "background-image"}}} =
               CssBackgroundValidator.prepare(@style, "background", source)
    end
  end

  test "shorthand retains independent layer boxes and resets every owned longhand" do
    assert {:ok, style} =
             CssBackgroundValidator.prepare(
               @style,
               "background",
               "linear-gradient(red,blue) left top / 20pt 30pt no-repeat content-box padding-box fixed, url(example) center / cover space border-box lime"
             )

    assert Enum.map(style.background_images, & &1.kind) == [:gradient, :url]
    assert style.background_sizes == [{20.0, 30.0}, :cover]
    assert style.background_origins == [:content_box, :border_box]
    assert style.background_clips == [:padding_box, :border_box]
    assert style.background_attachments == [:fixed, :scroll]
    assert style.background_repeats == [:no_repeat, {:space, :space}]
    assert style.background_color == {0, 1, 0}
    assert {:ok, reset} = CssBackgroundValidator.prepare(style, "background", "red")
    assert reset.background_color == {1, 0, 0}
    refute Map.has_key?(reset, :background_images)
    refute Map.has_key?(reset, :background_origins)
    assert reset.background_repeat == :repeat
    assert reset.background_size == {:auto, :auto}
  end

  test "layer longhands preserve list lengths for painting-time cycling" do
    assert {:ok, style} =
             CssBackgroundValidator.prepare(
               @style,
               "background-position",
               "left top, right bottom"
             )

    assert style.background_positions == [
             {{:percent, 0.0}, {:percent, 0.0}},
             {{:percent, 1.0}, {:percent, 1.0}}
           ]

    assert {:ok, style} = CssBackgroundValidator.prepare(style, "background-position", "center")
    refute Map.has_key?(style, :background_positions)
    assert style.background_position == {{:percent, 0.5}, {:percent, 0.5}}

    assert {:ok, style} =
             CssBackgroundValidator.prepare(style, "background-repeat", "round no-repeat, space")

    assert style.background_repeats == [{:round, :no_repeat}, {:space, :space}]

    for {property, value} <- [
          {"background", "red,blue"},
          {"background", "transparent,url(example)"},
          {"background", "url(a) url(b)"},
          {"background", "left /"},
          {"background", "red / cover"},
          {"background-origin", "margin-box"},
          {"background-clip", "none"},
          {"background-repeat", "repeat-x no-repeat"},
          {"background-size", "20pt 30pt 40pt"}
        ] do
      assert CssBackgroundValidator.prepare(@style, property, value) in [
               :error,
               {:error, :invalid_document}
             ],
             "#{property}: #{value}"
    end
  end

  test "CSS image sets reuse print density selection and skip unsupported MIME alternatives" do
    assert {:ok, set} =
             CssImageValueValidator.prepare(
               "image-set('one' 1x, url(two) 192dpi type('image/png'), url(three) 3x type('image/unknown'))",
               @style
             )

    assert %{kind: :url, source: "one", density: 1.0} = CssImageValueValidator.select(set, 1)
    assert %{kind: :url, source: "two", density: 2.0} = CssImageValueValidator.select(set, 1.5)
    assert %{kind: :url, source: "two", density: 2.0} = CssImageValueValidator.select(set, 4)
    assert {:ok, %{kind: :solid}} = CssImageValueValidator.prepare("image(ltr red)", @style)

    assert {:ok, %{kind: :fallback, direction: :rtl}} =
             CssImageValueValidator.prepare("image(rtl url(missing), red)", @style)

    for value <- [
          "image-set()",
          "image-set(url(a) 0x)",
          "image-set(url(a) 1x 2x)",
          "image-set(url(a),)",
          "image(blue,red,green)",
          "unknown(url(a))"
        ] do
      assert :error = CssImageValueValidator.prepare(value, @style), value
    end
  end
end
