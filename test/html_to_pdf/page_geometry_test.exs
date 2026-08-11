defmodule NativeElixirPdfUtilities.HtmlToPdf.PageGeometryTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry

  test "normalizes every named page size and orientation form" do
    expected_sizes = %{
      a5: {419.53, 595.28},
      a4: {595.28, 841.89},
      a3: {841.89, 1190.55},
      b5: {498.90, 708.66},
      b4: {708.66, 1000.63},
      jis_b5: {515.91, 728.50},
      jis_b4: {728.50, 1031.81},
      letter: {612.0, 792.0},
      legal: {612.0, 1008.0},
      ledger: {792.0, 1224.0}
    }

    Enum.each(expected_sizes, fn {name, expected} ->
      assert PageGeometry.normalize_page_size(name) == {:ok, expected}

      css_name = name |> Atom.to_string() |> String.replace("_", "-")
      assert PageGeometry.normalize_page_size(css_name) == {:ok, expected}

      {width, height} = expected

      assert PageGeometry.normalize_page_size({name, :portrait}) ==
               {:ok, {min(width, height), max(width, height)}}

      assert PageGeometry.normalize_page_size({:landscape, name}) ==
               {:ok, {max(width, height), min(width, height)}}
    end)

    assert PageGeometry.normalize_page_size(:"jis-b5") == {:ok, expected_sizes.jis_b5}
    assert PageGeometry.normalize_page_size(:"jis-b4") == {:ok, expected_sizes.jis_b4}

    assert PageGeometry.normalize_page_size({:"jis-b5", :landscape}) ==
             {:ok, {728.50, 515.91}}

    assert PageGeometry.normalize_page_size({:portrait, :"jis-b4"}) ==
             {:ok, expected_sizes.jis_b4}

    assert PageGeometry.normalize_page_size("landscape") == {:ok, {841.89, 595.28}}
    assert PageGeometry.normalize_page_size("portrait a4") == {:ok, expected_sizes.a4}
    assert PageGeometry.normalize_page_size("a5 landscape") == {:ok, {595.28, 419.53}}
  end

  test "normalizes custom renderer tuples and CSS absolute page lengths" do
    assert PageGeometry.normalize_page_size({8.5, 11}) == {:ok, {612.0, 792.0}}
    assert PageGeometry.normalize_page_size({200, 100}) == {:ok, {200.0, 100.0}}
    assert PageGeometry.normalize_page_size("8.5in 11in") == {:ok, {612.0, 792.0}}
    assert PageGeometry.normalize_page_size("10pt 10pt") == {:ok, {10.0, 10.0}}

    assert {:ok, {width, height}} =
             PageGeometry.normalize_page_size("1cm 4q")

    assert_in_delta width, 72.0 / 2.54, 0.0001
    assert_in_delta height, 72.0 / 25.4, 0.0001

    assert PageGeometry.normalize_page_size("1pc 1px") == {:ok, {12.0, 0.75}}

    for invalid <- [
          :unknown,
          {:unknown, :portrait},
          {:portrait, :unknown},
          {0, 100},
          "auto",
          "10pt",
          "0 10pt",
          "10em 20em",
          "10pt 20pt 30pt",
          nil
        ] do
      assert PageGeometry.normalize_page_size(invalid) == {:error, :invalid_page_size}
    end
  end

  test "normalizes renderer margin shorthand and side maps" do
    assert PageGeometry.normalize_margins(12) ==
             {:ok, %{top: 12.0, right: 12.0, bottom: 12.0, left: 12.0}}

    assert PageGeometry.normalize_margins("0") ==
             {:ok, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}}

    assert PageGeometry.normalize_margins("1pt 2pt") ==
             {:ok, %{top: 1.0, right: 2.0, bottom: 1.0, left: 2.0}}

    assert PageGeometry.normalize_margins("1pt 2pt 3pt") ==
             {:ok, %{top: 1.0, right: 2.0, bottom: 3.0, left: 2.0}}

    assert PageGeometry.normalize_margins("1pt 2pt 3pt 4pt") ==
             {:ok, %{top: 1.0, right: 2.0, bottom: 3.0, left: 4.0}}

    assert PageGeometry.normalize_margins(%{top: ".5in", left: "+1e2px"}) ==
             {:ok, %{top: 36.0, right: 0.0, bottom: 0.0, left: 75.0}}

    assert {:ok, margins} =
             PageGeometry.normalize_margins(%{
               top: "1mm",
               right: "1cm",
               bottom: "1q",
               left: "1pc"
             })

    assert_in_delta margins.top, 72.0 / 25.4, 0.0001
    assert_in_delta margins.right, 72.0 / 2.54, 0.0001
    assert_in_delta margins.bottom, 72.0 / 101.6, 0.0001
    assert_in_delta margins.left, 12.0, 0.0001

    for invalid <- [
          -1,
          "-1pt",
          "auto",
          "1pt 2pt 3pt 4pt 5pt",
          %{top: -1},
          %{unknown: 1},
          [:top],
          nil
        ] do
      assert PageGeometry.normalize_margins(invalid) == {:error, :invalid_margin}
    end
  end

  test "compacts and cascades resolved margin defaults" do
    uniform = %{top: 2.0, right: 2.0, bottom: 2.0, left: 2.0}
    asymmetric = %{top: 1.0, right: 2.0, bottom: 3.0, left: 4.0}

    assert PageGeometry.compact_margins(uniform) == 2.0
    assert PageGeometry.compact_margins(asymmetric) == asymmetric
    assert PageGeometry.merge_margin_defaults(asymmetric, "5pt") == "5pt"

    assert PageGeometry.merge_margin_defaults(nil, %{left: "4pt"}) == %{left: "4pt"}

    assert PageGeometry.merge_margin_defaults(%{top: 1.0}, %{left: 4.0}) ==
             %{top: 1.0, left: 4.0}

    assert PageGeometry.merge_margin_defaults("2pt", %{left: 4.0}) ==
             %{top: 2.0, right: 2.0, bottom: 2.0, left: 4.0}

    assert PageGeometry.merge_margin_defaults(:invalid, %{left: 4.0}) == %{left: 4.0}
  end

  test "converts only directly resolvable CSS geometry options" do
    assert PageGeometry.css_margin_option("0") == 0.0
    assert PageGeometry.css_margin_option("10PX") == "10px"

    assert PageGeometry.css_margin_option("1pt 2pt 3pt 4pt") ==
             %{top: 1.0, right: 2.0, bottom: 3.0, left: 4.0}

    for no_op <- ["", "auto", "-1pt", "1pt auto", "1pt 2pt 3pt 4pt 5pt"] do
      assert PageGeometry.css_margin_option(no_op) == nil
    end

    expected = [
      {"A4", :a4},
      {"A4 portrait", :a4},
      {"portrait A4", :a4},
      {"A4 landscape", {841.89, 595.28}},
      {"landscape A4", {841.89, 595.28}},
      {"letter", :letter},
      {"letter portrait", :letter},
      {"portrait letter", :letter},
      {"letter landscape", {792.0, 612.0}},
      {"landscape letter", {792.0, 612.0}},
      {"landscape", "landscape"},
      {"A5", "a5"},
      {"B4 portrait", "b4 portrait"},
      {"portrait JIS-B5", "portrait jis-b5"},
      {"8.5in 11in", "8.5in 11in"}
    ]

    Enum.each(expected, fn {css, option} ->
      assert PageGeometry.css_page_size_option(css) == option
    end)

    for no_op <- ["", "auto", "10pt", "10em 20em", "A4 sideways", "A4 portrait extra"] do
      assert PageGeometry.css_page_size_option(no_op) == nil
    end
  end

  test "validates the printable area using all four margins" do
    assert PageGeometry.valid_printable_area?(
             {100, 80},
             %{top: 10, right: 20, bottom: 30, left: 40}
           )

    refute PageGeometry.valid_printable_area?(
             {100, 80},
             %{top: 40, right: 20, bottom: 40, left: 40}
           )

    refute PageGeometry.valid_printable_area?(
             {100, 80},
             %{top: 10, right: 50, bottom: 10, left: 50}
           )

    refute PageGeometry.valid_printable_area?(
             {100, 80},
             %{top: -1, right: 0, bottom: 0, left: 0}
           )

    refute PageGeometry.valid_printable_area?(:a4, %{})
  end

  test "returns shared vertical bounds for drawable layout boxes" do
    assert PageGeometry.box_vertical_bounds(%{type: :rect, y: 10, height: 20}) == {30, 10}
    assert PageGeometry.box_vertical_bounds(%{type: :image, y: 5, height: 8}) == {13, 5}

    assert PageGeometry.box_vertical_bounds(%{
             type: :text,
             y: 10,
             font_size: 8,
             line_height: 20
           }) == {18, -2}

    assert PageGeometry.box_vertical_bounds(%{type: :text, y: 10, font_size: 8}) == {18, 10}
    assert PageGeometry.box_vertical_bounds(%{type: :text, y: 10, line_height: 20}) == {30, 10}
    assert PageGeometry.box_vertical_bounds(%{type: :page_break, y: 7}) == {7, 7}
    assert PageGeometry.box_vertical_bounds(%{type: :metadata}) == {0.0, 0.0}
  end
end
