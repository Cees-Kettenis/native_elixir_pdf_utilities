alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.FontTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.MathFontValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.MathLayout
  alias NativeElixirPdfUtilities.Validators.MathLayoutValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  describe "configured font isolation and internal family identity" do
    test "independent text image defaults resolve the permitted serif bytes" do
      assert {:ok, registry} = Font.load_registry(system_font_discovery: true)
      assert {:ok, ["serif"], selected} = Font.resolve("serif", 400, :normal, registry)
      assert {:ok, [face]} = FontValidator.prepare_svg_image_fonts([], [])
      assert face == Map.take(selected, [:data, :family, :weight, :style])
      assert {:ok, family} = FontValidator.prepare_font_family_name(face.data)
      assert is_binary(family) and family != ""

      Limits.install(%{Limits.effective() | max_font_source_bytes: 0, max_font_count: 0})
      assert {:ok, []} = FontValidator.prepare_svg_image_fonts([])
      assert {:ok, []} = FontValidator.prepare_svg_image_fonts([], system_font_discovery: false)

      for opts <- [nil, [system_font_discovery: :enabled]] do
        assert {:error, {:invalid_options, diagnostic}} =
                 FontValidator.prepare_svg_image_fonts([], opts)

        assert diagnostic.stage == :options
        assert diagnostic.message != ""
      end

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               FontValidator.prepare_svg_image_fonts([], system_font_discovery: true)

      assert diagnostic.stage == :font
    end

    test "explicit image font bytes stay configured-only even when discovery is permitted" do
      bytes = File.read!("priv/fonts/dejavu/DejaVuSansMono.ttf")
      configs = [%{family: "Actual Caller Alias", data: bytes}]
      assert {:ok, [face]} = FontValidator.prepare_svg_image_fonts(configs, [])
      assert face.data == bytes
      assert face.family == "Actual Caller Alias"
    end

    test "an incomplete installed default font package returns load diagnostics" do
      application = :native_elixir_pdf_utilities
      original = application |> :code.lib_dir() |> List.to_string() |> Path.join("ebin")
      root = Path.join(System.tmp_dir!(), "font-package-#{System.unique_integer([:positive])}")
      package = Path.join(root, "native_elixir_pdf_utilities-0.0.0")
      ebin = Path.join(package, "ebin")
      File.mkdir_p!(ebin)

      try do
        assert true == :code.replace_path(application, String.to_charlist(ebin))
        assert Application.app_dir(application) == package

        assert {:error, {:unsupported_font, diagnostic}} =
                 FontValidator.prepare_svg_image_fonts([], system_font_discovery: true)

        assert diagnostic.operation == :prepare_svg_image_fonts

        directory = Path.join(package, "priv/fonts/dejavu")
        File.mkdir_p!(directory)
        malformed = valid_tables() |> Map.put("GSUB", <<0>>) |> ttf_fixture()
        File.write!(Path.join(directory, "DejaVuSans.ttf"), malformed)

        assert {:error, {:invalid_document, diagnostic}} =
                 FontValidator.prepare_svg_image_fonts([], system_font_discovery: true)

        assert diagnostic.stage == :font
        assert diagnostic.message =~ "GSUB"
      after
        :code.replace_path(application, String.to_charlist(original))
        File.rm_rf!(root)
      end
    end

    test "configured-only loading uses the exact caller byte snapshot and reads no default fonts" do
      data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
      assert {:ok, [face]} = Font.load_configured_faces([%{family: "Alias", data: data}])
      assert face.data == data
      assert face.family == "Alias"
      assert face.source == :configured
      assert {:ok, "DejaVu Sans"} = FontValidator.prepare_font_family_name(face.data)

      Limits.install(%{Limits.effective() | max_font_source_bytes: 0, max_font_count: 0})
      assert {:ok, []} = Font.load_configured_faces([])
    end

    test "configured-only paths use the canonical loader and invalid configurations return diagnostics" do
      assert {:ok, [face]} =
               Font.load_configured_faces([
                 {"OnlyCaller", "priv/fonts/dejavu/DejaVuSansMono.ttf"}
               ])

      assert face.family == "OnlyCaller"
      assert {:ok, "DejaVu Sans Mono"} = FontValidator.prepare_font_family_name(face.data)

      for configs <- [
            nil,
            [%{}],
            [%{family: "Broken", data: <<1, 2>>}],
            [%{family: "Missing", path: "/nonexistent/explicit-font.ttf"}]
          ] do
        assert {:error, {reason, diagnostic}} = Font.load_configured_faces(configs)
        assert is_atom(reason)
        assert diagnostic.reason == reason
        assert is_atom(diagnostic.stage)
        assert is_binary(diagnostic.message)
      end
    end

    test "name selection follows typographic and English family records, without CSS aliases" do
      unicode = fn name -> :unicode.characters_to_binary(name, :utf8, {:utf16, :big}) end

      for {records, expected} <- [
            {[
               {3, 1, 0x0409, 1, unicode.("Legacy")},
               {3, 1, 0x0411, 16, unicode.("Localized")},
               {3, 1, 0x0409, 16, unicode.("Typographic")}
             ], "Typographic"},
            {[{0, 4, 0, 1, unicode.("Unicode")}, {1, 0, 0, 1, "MacFamily"}], "MacFamily"},
            {[
               {0, 4, 0, 1, unicode.("Unicode")},
               {3, 1, 0x0409, 1, unicode.("English")},
               {1, 0, 0, 1, "MacFamily"}
             ], "English"},
            {[{7, 9, 0, 16, "unsupported"}, {1, 0, 0, 1, <<"Mono", 0xCA, "Family">>}],
             "Mono\u00A0Family"},
            {[{1, 0, 0, 1, <<0x80, "Family">>}], "ÄFamily"},
            {[{1, 0, 0, 1, <<0x11, 0xDB, 0xAD, 0xB0, 0xDE>>}], "⌘€≠∞ﬁ"},
            {[{1, 0, 0, 16, <<0xF0>>}, {0, 4, 0, 1, unicode.("Valid")}], "\uF8FF"},
            {[{3, 1, 0x0409, 16, <<0xD800::16>>}, {0, 4, 0, 1, unicode.("Valid")}], "Valid"}
          ] do
        data = valid_tables() |> Map.put("name", family_name_table(records)) |> ttf_fixture()
        assert {:ok, ^expected} = FontValidator.prepare_font_family_name(data)
      end
    end

    test "closed byte-face admission rejects malformed and extra metadata without loading defaults" do
      bytes = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
      face = %{family: "Actual", data: bytes, weight: 400, style: :normal}

      for faces <- [[nil], [Map.put(face, :extra, true)], []] do
        assert {:error, {:invalid_font, diagnostic}} = FontValidator.prepare_byte_faces(faces)
        assert diagnostic.stage == :font
        assert diagnostic.operation == :prepare_byte_faces
      end

      assert {:error, {:unsupported_font, diagnostic}} =
               Font.load_byte_faces([%{face | data: <<1>>}])

      assert diagnostic.operation == :load_byte_faces
    end

    test "name lookup preserves malformed substitution diagnostics" do
      data =
        valid_tables()
        |> Map.put("name", family_name_table([{1, 0, 0, 1, "Family"}]))
        |> Map.put("GSUB", <<0>>)
        |> ttf_fixture()

      assert {:error, {:invalid_document, diagnostic}} =
               FontValidator.prepare_font_family_name(data)

      assert diagnostic.stage == :font
      assert diagnostic.message =~ "GSUB"

      assert {:error, {:invalid_document, _}} =
               Font.load_configured_faces([%{family: "Malformed", data: data}])
    end

    test "missing, truncated and unsupported name tables fail through the font diagnostic contract" do
      for data <- [
            nil,
            <<>>,
            ttf_fixture(valid_tables()),
            ttf_fixture(Map.put(valid_tables(), "name", <<0::16, 1::16, 18::16>>)),
            ttf_fixture(Map.put(valid_tables(), "name", <<2::16, 0::16, 6::16>>)),
            ttf_fixture(
              Map.put(valid_tables(), "name", family_name_table([{3, 1, 0x0409, 1, <<>>}]))
            )
          ] do
        assert {:error, {:invalid_font, diagnostic}} =
                 FontValidator.prepare_font_family_name(data)

        assert diagnostic.stage == :font
        assert diagnostic.operation == :prepare_font_family_name
        assert diagnostic.message =~ "family name"
      end

      data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
      Limits.install(%{Limits.effective() | max_font_math_work: 0})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               FontValidator.prepare_font_family_name(data)

      assert diagnostic.stage == :font
      assert diagnostic.message =~ "font"
    end
  end

  test "design outline measurements use the selected bytefont, including true empty glyphs" do
    assert {:ok, [face]} =
             Font.load_configured_faces([
               %{family: "DesignBounds", data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")}
             ])

    box = %{text: "A", font_face: face, font_size: 12, x: 30, y: 45}
    assert {:ok, {top, bottom}} = FontValidator.prepare_text_ink_bounds(box)

    assert {:ok, {left, rectangle_bottom, right, rectangle_top}} =
             FontValidator.prepare_text_ink_rectangle(box)

    assert left < right and bottom < top
    assert rectangle_bottom == bottom and rectangle_top == top
    assert {:ok, nil} = FontValidator.prepare_text_ink_rectangle(%{box | text: " "})
    assert {:ok, nil} = FontValidator.prepare_text_ink_bounds(%{box | text: " "})

    assert {:ok, {_, _, spaced_right, _}} =
             FontValidator.prepare_text_ink_rectangle(%{box | text: "A A"})

    assert spaced_right > right

    limit = Limits.get(:max_pdf_numeric_magnitude)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FontValidator.prepare_text_ink_rectangle(%{box | x: limit})

    assert diagnostic.operation == :measure_child
    assert diagnostic.source == "A"
    assert diagnostic.message =~ "max_pdf_numeric_magnitude"

    for invalid <- [Map.put(box, :glyph_ids, [0]), %{box | text: <<255>>}, %{box | x: "invalid"}] do
      assert {:error, {reason, diagnostic}} = FontValidator.prepare_text_ink_rectangle(invalid)
      assert reason in [:invalid_document, :invalid_encoding]
      assert diagnostic.reason == reason
      assert diagnostic.stage == :font
      assert diagnostic.message != ""
    end

    for invalid <- [
          %{},
          %{box | font_face: Map.delete(face, :widths)},
          %{box | font_face: %{face | widths: []}},
          Map.put(box, :glyph_ids, [face.cmap[?A] | :invalid])
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               FontValidator.prepare_text_ink_rectangle(invalid)

      assert diagnostic.stage == :font
      assert diagnostic.operation == :measure_child
    end

    assert {:error, {:invalid_document, diagnostic}} = FontValidator.prepare_glyph_bounds(nil)
    assert diagnostic.operation == :glyph_bounds

    assert {:error, {_, diagnostic}} =
             FontValidator.prepare_text_ink_rectangle(%{box | font_face: %{face | data: <<0>>}})

    assert diagnostic.operation == :glyph_bounds
  end

  test "CSS advances and design widths keep separate cache plans and font byte identities" do
    bytes = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    mono = File.read!("priv/fonts/dejavu/DejaVuSansMono.ttf")

    assert {:ok, [first, second]} =
             Font.load_configured_faces([
               %{family: "SharedAlias", data: bytes},
               %{family: "SharedAlias", data: mono}
             ])

    text = "AVffi"
    first_width = Font.css_text_width(text, first, 9)
    second_width = Font.css_text_width(text, second, 9)
    design_width = Font.text_width(text, first, 9)
    assert first_width != second_width

    Font.with_measurement_cache(fn ->
      assert Font.css_glyph_advances("", first, 9) == []
      assert Font.css_text_width(text, first, 9) == first_width
      assert is_list(Font.css_glyph_advances(text, first, 9))
      assert Font.css_text_width(text, first, 9) == first_width
      assert Font.text_width(text, first, 9) == design_width
      assert Font.css_text_width(text, second, 9) == second_width
      assert Font.css_text_width(text, first, 9) == first_width
    end)
  end

  defp family_name_table(records) do
    {headers, bytes, _} =
      Enum.reduce(records, {<<>>, <<>>, 0}, fn {platform, encoding, language, id, text},
                                               {headers, bytes, offset} ->
        header =
          <<platform::16, encoding::16, language::16, id::16, byte_size(text)::16, offset::16>>

        {headers <> header, bytes <> text, offset + byte_size(text)}
      end)

    <<0::16, length(records)::16, 6 + length(records) * 12::16, headers::binary, bytes::binary>>
  end

  defp pair_position_table(subtable, type) do
    scripts = <<1::16, "latn", 8::16, 4::16, 0::16, 0::16, 0xFFFF::16, 1::16, 0::16>>
    features = <<1::16, "kern", 8::16, 0::16, 1::16, 0::16>>
    lookups = <<1::16, 4::16, type::16, 0::16, 1::16, 8::16, subtable::binary>>

    <<1::16, 0::16, 10::16, 10 + byte_size(scripts)::16,
      10 + byte_size(scripts) + byte_size(features)::16, scripts::binary, features::binary,
      lookups::binary>>
  end

  describe "selected mathematical glyph measurement" do
    setup do
      {:ok, registry} = Font.load_registry(system_font_discovery: false)
      {:ok, _, font} = Font.resolve("math", 400, :normal, registry)
      {:ok, font: font}
    end

    test "math token pairs use selected-font GPOS without changing ordinary text measurement", %{
      font: font
    } do
      pairs = [
        {font.cmap[?w], font.cmap[?o]},
        {font.cmap[?r], font.cmap[?d]},
        {font.cmap[?e], font.cmap[?x]}
      ]

      assert {:ok, result} = FontValidator.prepare_math_kerning(font, pairs, "latn")
      assert Map.values(result) |> Enum.sort() == [-10, -10, -5]
      assert Font.text_width("word", font, 20) == 43.64

      FontValidator.with_budget(fn ->
        assert {:ok, ^result} = FontValidator.prepare_math_kerning(font, pairs, "latn")
        assert {:ok, ^result} = FontValidator.prepare_math_kerning(font, pairs, "latn")

        assert {:error, {:invalid_document, diagnostic}} =
                 FontValidator.prepare_math_kerning(%{font | data: <<>>}, pairs, "latn")

        assert diagnostic.source =~ "GPOS"
      end)

      assert {:error, {:invalid_document, diagnostic}} =
               FontValidator.prepare_math_kerning(%{}, [], "latn")

      assert diagnostic.message =~ "embedded"

      for pairs <- [nil, [nil], [{0, 65_535}]] do
        assert {:error, {:invalid_document, diagnostic}} =
                 FontValidator.prepare_math_kerning(font, pairs, "latn")

        assert diagnostic.source =~ "GPOS"
      end

      without_gpos = %{font | data: ttf_fixture(valid_tables()), kerning: %{{2, 3} => -17}}

      assert {:ok, %{{2, 3} => -17}} =
               FontValidator.prepare_math_kerning(without_gpos, [{2, 3}], "latn")

      assert {:error, {:invalid_document, _}} =
               FontValidator.prepare_math_kerning(without_gpos, nil, "latn")
    end

    test "Core rows carry final corrections across upright, slanted and nested boundaries", %{
      font: font
    } do
      style = %{
        display: :block,
        font_size: 20,
        font_face: font,
        color: {0, 0, 0},
        line_height: 24,
        text_transform: :math_auto
      }

      tokens =
        Enum.map(["x", "y", "1"], fn text ->
          %{
            type: :element,
            namespace: :mathml,
            tag: if(text == "1", do: "mn", else: "mi"),
            attributes: %{},
            style: style,
            children: [%{type: :text, text: text, style: style}]
          }
        end)

      [x, y, one] = tokens

      assert {:ok, spaced} =
               MathLayout.measure(%{x | children: [%{type: :text, text: " x ", style: style}]})

      [spaced_glyph] = spaced.glyphs
      assert spaced_glyph.glyph_id == font.cmap[?x]
      [x_text] = x.children

      inline = %{
        x
        | style: Map.put(style, :display, :inline_block),
          children: [
            x_text,
            %{type: :element, tag: "span", style: %{display: :inline}, children: y.children}
          ]
      }

      assert {:ok, mixed} = MathLayout.measure(inline)
      assert Enum.map(mixed.glyphs, & &1.glyph_id) == [font.cmap[0x1D465], font.cmap[0x1D466]]

      nowrap = Map.put(style, :white_space, :nowrap)

      split_fi = %{
        x
        | style: Map.put(nowrap, :display, :inline_block),
          children: [
            %{type: :text, text: "f", style: nowrap},
            %{
              type: :element,
              tag: "span",
              style: %{display: :inline},
              children: [%{type: :text, text: "i", style: nowrap}]
            }
          ]
      }

      assert {:ok, split_measure} = MathLayout.measure(split_fi)

      assert Enum.map(split_measure.glyphs, & &1.glyph_id) == [
               font.cmap[0x1D453],
               font.cmap[0x1D456]
             ]

      assert split_measure.actual_text == "fi"

      source =
        "<math><mi style='display:inline-block;white-space:nowrap'>f<span style='white-space:nowrap'>i</span></mi></math>"

      assert {:ok, dom} = NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse_detailed(source)

      assert {:ok, %{children: [atom]}} =
               NativeElixirPdfUtilities.HtmlToPdf.Style.compute_detailed(dom)

      assert Enum.map(atom.style._math_box.glyphs, & &1.glyph_id) == [
               font.cmap[0x1D453],
               font.cmap[0x1D456]
             ]

      assert {:ok, pdf} = HtmlToPdf.render(source)
      assert {:ok, "fi"} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert {:ok, xm} = MathLayout.measure(x)
      assert {:ok, ym} = MathLayout.measure(y)
      assert xm.italic_correction > 0
      assert ym.italic_correction > 0
      row = %{x | tag: "mrow", children: tokens}
      assert {:ok, measured} = MathLayout.measure(row)
      [_, yg, ng] = measured.glyphs
      assert_in_delta yg.x, xm.width, 0.000001
      assert_in_delta ng.x, xm.width + ym.width + ym.italic_correction, 0.000001
      assert measured.italic_correction == 0
      slanted = %{row | children: [x, y]}
      assert {:ok, slanted_measure} = MathLayout.measure(slanted)
      assert slanted_measure.italic_correction == ym.italic_correction
      assert_in_delta slanted_measure.width, xm.width + ym.width + ym.italic_correction, 0.000001
      assert {:ok, nested} = MathLayout.measure(%{row | children: [slanted, one]})
      [_, _, number] = nested.glyphs

      assert_in_delta number.x,
                      slanted_measure.width + slanted_measure.italic_correction,
                      0.000001

      assert nested.actual_text == "xy1"
      assert {:ok, number_measure} = MathLayout.measure(one)
      assert nested.width == number.x + number_measure.width
    end

    test "math styles validate depths, resets, malformed hints and selected scaling", %{
      font: font
    } do
      parent = %{size: 20.0, depth: 1, style: :normal, font: font}
      style = %{font_size: :math, font_face: font, _math_context: parent, _root_font_size: 12.0}

      MathLayout.with_context(fn ->
        assert {:ok, scaled} = MathLayoutValidator.compute_style(Map.put(style, :math_depth, 2))
        assert_in_delta scaled.font_size, 20 * 55 / 70, 0.000001

        assert {:ok, deeper} =
                 MathLayoutValidator.compute_style(
                   %{style | _math_context: %{parent | depth: 3}}
                   |> Map.put(:math_depth, 4)
                 )

        assert_in_delta deeper.font_size, 14.2, 0.000001

        assert {:error, {:invalid_document, _}} =
                 MathLayoutValidator.compute_style(Map.put(style, :math_depth, 2500))

        assert {:error, {:invalid_document, _}} =
                 MathLayoutValidator.compute_style(
                   %{
                     style
                     | _math_context: %{
                         parent
                         | depth: 0,
                           font: %{
                             font
                             | data:
                                 ttf_fixture(
                                   Map.put(
                                     valid_tables(),
                                     "MATH",
                                     math_table(
                                       <<0::signed-16, 60::signed-16,
                                         binary_part(default_constants(), 4, 210)::binary>>
                                     )
                                   )
                                 )
                           }
                       }
                   }
                   |> Map.put(:math_depth, 1)
                 )
      end)

      for {property, value} <- [
            {"math-depth", "add(99999999999999)"},
            {"math-depth", "99999999999999"},
            {"math-depth", "invalid"},
            {"math-style", "invalid"}
          ] do
        assert :error = MathLayoutValidator.parse_property(property, value, style)
      end

      assert {:ok, %{math_shift: :normal}} =
               MathLayoutValidator.parse_property("math-shift", "normal", style)

      assert {:ok, %{display: :inline}} =
               MathLayoutValidator.parse_property("display", "inline math", style)

      for display <- ["math", "block math"],
          do:
            assert(
              {:ok, %{display: :block}} =
                MathLayoutValidator.parse_property("display", display, style)
            )

      assert {:ok, :math} = MathLayoutValidator.parse_font_size("math", style)
      assert :error = MathLayoutValidator.parse_font_size("math", %{font_size: 20})
      assert {:ok, 20.0} = MathLayoutValidator.parse_font_size("inherit", style)
      assert :error = MathLayoutValidator.parse_font_size("1000000%", %{font_size: 1_000_000.0})

      assert MathLayoutValidator.style_hints(%{
               namespace: :mathml,
               tag: "mi",
               attributes: %{"displaystyle" => "TRUE"}
             }) == [{"math-style", "normal"}]

      assert MathLayoutValidator.math_auto_text("h") == "ℎ"
      assert MathLayoutValidator.math_auto_text("exp") == "exp"
      assert {:ok, x_height} = FontValidator.prepare_x_height(font)
      assert x_height > 0

      assert {:error, {:unsupported_glyph, _}} =
               FontValidator.prepare_x_height(%{font | cmap: %{}})

      assert {:error, {:invalid_document, _}} = FontValidator.prepare_x_height(%{})

      assert {:error, {:invalid_document, _}} =
               FontValidator.prepare_x_height(%{font | data: <<>>})

      assert {:ok, size} =
               MathLayoutValidator.parse_font_size("2ex", %{
                 style
                 | _math_context: %{parent | font: font}
               })

      assert_in_delta size, x_height * 40.0, 0.00001

      assert :error =
               MathLayoutValidator.parse_font_size("-2ex", %{
                 style
                 | _math_context: %{parent | font: font}
               })

      assert :error =
               MathLayoutValidator.parse_font_size("1000000000ex", %{
                 style
                 | _math_context: %{parent | font: font}
               })

      assert [{"font-size", "2ex"}] =
               MathLayoutValidator.style_hints(%{
                 namespace: :mathml,
                 tag: "mi",
                 attributes: %{"mathsize" => "2ex"}
               })
    end

    test "math decorations, flow extents, ligatures and transport are measured explicitly", %{
      font: font
    } do
      style = %{
        display: :block,
        font_size: 20,
        font_face: font,
        color: {0, 0, 0},
        line_height: 24,
        white_space: :normal
      }

      token = %{
        type: :element,
        namespace: :mathml,
        tag: "mtext",
        attributes: %{},
        style: style,
        children: [%{type: :text, text: "office", style: style}]
      }

      assert {:ok, ligature} = MathLayout.measure(token)
      assert ligature.actual_text == "office"

      # Pinned STIX has no Latin liga feature; its presentation-form cmap is not shaping authority.
      assert font.latin_ligatures == %{}
      assert length(ligature.glyphs) == 6
      assert Enum.map(ligature.glyphs, & &1.glyph_id) == Enum.map(~c"office", &font.cmap[&1])
      changed = %{font | cmap: Map.put(font.cmap, ?o, 65_535)}
      bad_style = %{style | font_face: changed}

      assert {:error, {:unsupported_glyph, _}} =
               MathLayout.measure(%{
                 token
                 | style: bad_style,
                   children: [%{type: :text, text: "office", style: bad_style}]
               })

      decorated_style =
        Map.merge(style, %{
          background_color: {1, 1, 0},
          width: 40.0,
          height: 25.0,
          padding: %{top: 1.0, right: 2.0, bottom: 3.0, left: 4.0}
        })

      decorated = %{token | style: decorated_style}
      assert {:ok, measured} = MathLayout.measure(%{token | tag: "mrow", children: [decorated]})
      assert measured.width == 46.0
      assert Enum.any?(measured.drawing_items, &(&1.kind == :background))

      assert :ok =
               MathLayoutValidator.validate_flow_geometry(
                 [%{style: %{_math_box: measured}}],
                 100,
                 100
               )

      for broken <- [
            Map.delete(measured, :actual_text),
            %{measured | drawing_items: measured.drawing_items ++ [%{kind: :invalid}]}
          ] do
        assert {:error, {:invalid_document, diagnostic}} =
                 MathLayoutValidator.validate_flow_geometry(
                   [%{style: %{_math_box: broken}}],
                   100,
                   100
                 )

        assert diagnostic.message =~ "drawing items"
      end

      assert {:error, {:unsupported_html, _}} =
               MathLayout.measure(%{token | style: %{style | display: :grid}})

      assert {:error, {:unsupported_html, _}} =
               MathLayout.measure(%{token | style: Map.put(style, :background_image, %{})})

      <<_::32, count::16, _::48, records::binary-size(count * 16), _::binary>> = font.data

      [{offset, _length}] =
        for <<tag::binary-size(4), _::32, offset::32, length::32 <- records>>, tag == "GPOS",
          do: {offset, length}

      bad_pair_font = %{font | data: replace_bytes(font.data, offset, <<0::16>>)}
      bad_pair_style = %{style | font_face: bad_pair_font}

      assert {:error, {:invalid_document, diagnostic}} =
               MathLayout.measure(%{
                 token
                 | style: bad_pair_style,
                   children: [%{type: :text, text: "word", style: bad_pair_style}]
               })

      assert diagnostic.source =~ "GPOS"

      assert {:error, {:invalid_document, _}} =
               MathLayout.measure(%{
                 token
                 | tag: "mspace",
                   children: [],
                   style: Map.put(style, :width, -1)
               })

      assert {:error, {_, diagnostic}} =
               MathLayout.measure(%{
                 token
                 | children: [
                     %{
                       type: :element,
                       tag: "span",
                       style: %{display: :inline, padding: nil},
                       children: []
                     }
                   ]
               })

      assert diagnostic.message != ""

      hidden = %{type: :element, tag: "span", style: %{display: :none}, children: []}
      assert {:ok, empty} = MathLayout.measure(%{token | children: [hidden]})
      assert empty.width == 0 and empty.ascent == 0 and empty.descent == 0

      mixed = %{
        type: :element,
        tag: "span",
        style: %{display: :inline},
        children: [%{type: :text, text: "😀", style: style}]
      }

      assert {:error, {:unsupported_glyph, _}} = MathLayout.measure(%{token | children: [mixed]})
      space = %{token | tag: "mspace", children: [%{type: :text, text: " \n", style: style}]}
      assert {:ok, _} = MathLayout.measure(space)

      assert {:error, {:unsupported_html, _}} =
               MathLayout.measure(%{
                 space
                 | children: [%{type: :text, text: "visible", style: style}]
               })

      assert {:error, {:unsupported_html, _}} = MathLayout.measure(%{space | children: [mixed]})

      assert {:ok, ex_space} =
               MathLayout.measure(%{space | children: [], attributes: %{"width" => "2ex"}})

      assert {:ok, x_height} = FontValidator.prepare_x_height(font)
      assert_in_delta ex_space.width, x_height * 40, 0.00001

      assert {:error, {:unsupported_glyph, _}} =
               MathLayout.measure(%{
                 space
                 | children: [],
                   attributes: %{"width" => "2ex"},
                   style: %{style | font_face: %{font | cmap: %{}}}
               })

      assert {:ok, negative_depth} =
               MathLayout.measure(%{
                 space
                 | children: [],
                   attributes: %{"height" => "3pt", "depth" => "-2pt"}
               })

      assert negative_depth.ascent == 3 and negative_depth.descent == -2
    end

    test "GPOS validates explicit and class pairs, extensions, scripts, offsets and work" do
      explicit =
        <<1::16, 18::16, 4::16, 0::16, 1::16, 12::16, 1::16, 3::16, -20::signed-16, 1::16, 1::16,
          2::16>>

      classes =
        <<2::16, 24::16, 4::16, 0::16, 30::16, 38::16, 2::16, 2::16, 0::16, 0::16, 0::16,
          -20::signed-16, 1::16, 1::16, 2::16, 1::16, 2::16, 1::16, 1::16, 1::16, 3::16, 1::16,
          1::16>>

      for {subtable, type} <- [
            {explicit, 2},
            {classes, 2},
            {<<1::16, 2::16, 8::32, explicit::binary>>, 9}
          ] do
        table = pair_position_table(subtable, type)

        assert {:ok, %{{2, 3} => -20}} =
                 MathFontValidator.prepare_pair_adjustments(table, 5, [{2, 3}], "latn")

        assert {:ok, %{}} = MathFontValidator.prepare_pair_adjustments(table, 5, [{4, 3}], "latn")
        assert {:ok, %{}} = MathFontValidator.prepare_pair_adjustments(table, 5, [{2, 3}], "grek")

        for size <- 0..(byte_size(table) - 1) do
          assert {:error, {:invalid_document, diagnostic}} =
                   MathFontValidator.prepare_pair_adjustments(
                     binary_part(table, 0, size),
                     5,
                     [{2, 3}],
                     "latn"
                   )

          assert diagnostic.stage == :font
          assert diagnostic.message =~ "GPOS"
        end
      end

      ranged =
        <<2::16, 24::16, 4::16, 0::16, 30::16, 40::16, 2::16, 2::16, 0::16, 0::16, 0::16,
          -20::signed-16, 1::16, 1::16, 2::16, 2::16, 1::16, 2::16, 2::16, 1::16, 2::16, 1::16,
          3::16, 3::16, 1::16>>

      assert {:ok, %{{2, 3} => -20}} =
               MathFontValidator.prepare_pair_adjustments(
                 pair_position_table(ranged, 2),
                 5,
                 [{2, 3}],
                 "latn"
               )

      for {data, count, pairs, script} <- [
            {nil, 5, [], "latn"},
            {<<>>, 0, [], "latn"},
            {pair_position_table(explicit, 2), 5, [nil], "latn"},
            {pair_position_table(explicit, 2), 5, [{2, 5}], "latn"},
            {pair_position_table(explicit, 2), 5, [], "arab"}
          ] do
        assert {:error, {:invalid_document, _}} =
                 MathFontValidator.prepare_pair_adjustments(data, count, pairs, script)
      end

      for bad <- [
            <<0::16, binary_part(explicit, 2, byte_size(explicit) - 2)::binary>>,
            <<1::16, 18::16, 1::16, binary_part(explicit, 6, byte_size(explicit) - 6)::binary>>
          ] do
        assert {:error, {:invalid_document, _}} =
                 MathFontValidator.prepare_pair_adjustments(
                   pair_position_table(bad, 2),
                   5,
                   [{2, 3}],
                   "latn"
                 )
      end

      table = pair_position_table(explicit, 2)
      <<major::16, _minor::16, scripts::16, features::16, lookups::16, body::binary>> = table

      version_one =
        <<major::16, 1::16, scripts + 4::16, features + 4::16, lookups + 4::16, 0::32,
          body::binary>>

      assert {:ok, %{{2, 3} => -20}} =
               MathFontValidator.prepare_pair_adjustments(version_one, 5, [{2, 3}], "latn")

      assert {:error, {:invalid_document, _}} =
               MathFontValidator.prepare_pair_adjustments(
                 replace_bytes(version_one, 10, <<1::32>>),
                 5,
                 [{2, 3}],
                 "latn"
               )

      assert {:ok, %{}} =
               MathFontValidator.prepare_pair_adjustments(
                 replace_bytes(table, scripts + 8, <<0::16>>),
                 5,
                 [{2, 3}],
                 "latn"
               )

      assert {:ok, %{}} =
               MathFontValidator.prepare_pair_adjustments(
                 replace_bytes(table, features + 2, "dist"),
                 5,
                 [{2, 3}],
                 "latn"
               )

      for changed <- [
            replace_bytes(classes, 30, <<0::16>>),
            replace_bytes(classes, 12, <<0::16>>)
          ] do
        assert {:error, {:invalid_document, _}} =
                 MathFontValidator.prepare_pair_adjustments(
                   pair_position_table(changed, 2),
                   5,
                   [{2, 3}],
                   "latn"
                 )
      end

      overlapping =
        <<2::16, 24::16, 4::16, 0::16, 30::16, 46::16, 2::16, 2::16, 0::16, 0::16, 0::16,
          -20::signed-16, 1::16, 1::16, 2::16, 2::16, 2::16, 2::16, 2::16, 1::16, 2::16, 2::16,
          1::16, 1::16, 3::16, 1::16, 1::16>>

      assert {:error, {:invalid_document, diagnostic}} =
               MathFontValidator.prepare_pair_adjustments(
                 pair_position_table(overlapping, 2),
                 5,
                 [{2, 3}],
                 "latn"
               )

      assert diagnostic.message =~ "class ranges"

      Limits.install(%{Limits.effective() | max_font_math_work: 1})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               MathFontValidator.prepare_pair_adjustments(
                 pair_position_table(classes, 2),
                 5,
                 [{2, 3}],
                 "latn"
               )

      assert diagnostic.message =~ "max_font_math_work"
    end

    test "canonical math preparation rejects malformed callers and preserves spaces and extents",
         %{font: font} do
      style = %{
        display: :block,
        font_size: 20,
        font_face: font,
        color: {0, 0, 0},
        line_height: 24,
        white_space: :normal
      }

      token = %{
        type: :element,
        namespace: :mathml,
        tag: "mn",
        attributes: %{},
        style: style,
        children: [%{type: :text, text: "12", style: style}]
      }

      assert {:ok, prepared} = MathLayoutValidator.prepare_element(token, %{})
      assert prepared.kind == :token
      assert {:ok, measured} = MathLayout.measure(token)
      assert measured.actual_text == "12"
      assert measured.ink_ascent == measured.ascent

      assert Enum.map(
               measured.drawing_items,
               &Map.drop(&1, [:kind, :logical_group, :logical_text])
             ) == measured.glyphs

      assert_in_delta measured.width, 19.8, 0.0001

      for node <- [
            nil,
            %{},
            %{token | style: %{}},
            %{token | children: [nil]},
            %{
              token
              | children: [%{type: :element, namespace: :mathml, style: %{display: :inline}}]
            }
          ] do
        assert {:error, {_, diagnostic}} = MathLayout.measure(node)
        assert diagnostic.message != ""
      end

      assert {:error, {:invalid_document, _}} = MathLayout.measure(token, nil)

      for {attrs, width, ascent, descent} <- [
            {%{}, 0.0, 0.0, 0.0},
            {%{"width" => "2em", "height" => "3pt", "depth" => "2pt"}, 40.0, 3.0, 2.0},
            {%{"width" => "bad", "height" => "20%", "depth" => "-4pt"}, 0.0, 0.0, 0.0}
          ] do
        space = %{token | tag: "mspace", attributes: attrs, children: []}
        assert {:ok, measured} = MathLayout.measure(space)
        assert {measured.width, measured.ascent, measured.descent} == {width, ascent, descent}
      end

      assert {:error, {:unsupported_html, _}} =
               MathLayout.measure(%{
                 token
                 | tag: "mspace",
                   style: Map.put(style, :width, :auto),
                   children: []
               })

      assert {:ok, hidden} = MathLayout.measure(%{token | style: %{style | display: :none}})
      assert hidden.glyphs == []
      assert hidden.width == 0

      row = %{
        token
        | tag: "mrow",
          children: [
            %{type: :text, text: "\n", style: style},
            token,
            %{type: :text, text: " ", style: style}
          ]
      }

      assert {:ok, row} = MathLayout.measure(row)
      assert row.actual_text == "12"

      assert {:error, {:invalid_document, _}} =
               MathLayoutValidator.validate_flow_geometry([%{style: %{_math_box: %{}}}], 100, 100)
    end

    test "italic mappings preserve original Unicode and selected outline metrics", %{font: font} do
      FontValidator.with_budget(fn ->
        {:ok, bounds} = Font.glyph_bounds(font)
        {:ok, metrics} = Font.math_metrics(font)

        for {original, transformed} <- [
              {"A", "𝐴"},
              {"h", "ℎ"},
              {"ı", "𝚤"},
              {"ȷ", "𝚥"},
              {"α", "𝛼"},
              {"ϴ", "𝛳"},
              {"∂", "𝜕"},
              {"ϖ", "𝜛"},
              {"∞", "∞"}
            ] do
          assert {:ok, measured} =
                   MathLayout.glyph(original, font, 20, text_transform: :math_auto)

          [paint] = measured.glyphs
          glyph = Map.fetch!(font.cmap, hd(String.to_charlist(transformed)))
          assert paint.glyph_id == glyph
          assert paint.text == original
          assert paint.font_face == font

          assert_in_delta paint.width,
                          Enum.at(font.widths, glyph) / font.units_per_em * 20,
                          1.0e-10

          {_, bottom, _, top} = elem(bounds, glyph)
          assert_in_delta measured.ascent, top * 20 / font.units_per_em, 1.0e-10
          assert_in_delta measured.descent, -bottom * 20 / font.units_per_em, 1.0e-10
          expected = Map.get(metrics.glyph_info.italics_corrections, glyph, %{value: 0})

          assert_in_delta measured.italic_correction,
                          expected.value * 20 / font.units_per_em,
                          1.0e-10
        end

        assert {:ok, measured} = MathLayout.glyph("x", font, 0)
        assert measured.width == 0
        assert measured.ascent == 0
        assert measured.descent == 0
        assert measured.top_accent_attachment == 0
        assert {:ok, normal} = MathLayout.glyph("x", font, 20)
        assert hd(normal.glyphs).glyph_id == font.cmap[?x]
      end)
    end

    test "script forms apply ordered lookups and retain their own bounds and advances", %{
      font: font
    } do
      FontValidator.with_budget(fn ->
        {:ok, metrics} = Font.math_metrics(font)
        [lookup] = metrics.substitutions["ssty"]
        [script, scriptscript] = lookup[font.cmap[0x1D465]]

        for {level, expected} <- [{0, font.cmap[0x1D465]}, {1, script}, {2, scriptscript}] do
          assert {:ok, measured} =
                   MathLayout.glyph("x", font, 10,
                     text_transform: :math_auto,
                     script_feature_level: level
                   )

          [paint] = measured.glyphs
          assert paint.glyph_id == expected
          assert paint.text == "x"

          assert_in_delta measured.width,
                          Enum.at(font.widths, expected) * 10 / font.units_per_em,
                          1.0e-10
        end

        {unicode, original} =
          Enum.find(font.cmap, fn {_unicode, id} -> id > 0 and not Map.has_key?(lookup, id) end)

        assert {:ok, unchanged} =
                 MathLayout.glyph(<<unicode::utf8>>, font, 10, script_feature_level: 2)

        assert hd(unchanged.glyphs).glyph_id == original
      end)
    end

    test "single script substitutions apply at both feature levels", %{font: font} do
      <<_::32, count::16, _::48, records::binary-size(count * 16), _::binary>> = font.data

      tables =
        Map.new(
          for <<tag::binary-size(4), _::32, offset::32, length::32 <- records>>,
            do: {tag, binary_part(font.data, offset, length)}
        )

      base = font.cmap[0x1D465]
      {:ok, metrics} = Font.math_metrics(font)
      [script, scriptscript] = hd(metrics.substitutions["ssty"])[base]
      single = <<2::16, 8::16, 1::16, script::16, 1::16, 1::16, base::16>>
      lookup = <<1::16, 0::16, 1::16, 8::16, single::binary>>
      changed = %{font | data: tables |> Map.put("GSUB", gsub_table([lookup])) |> ttf_fixture()}

      for level <- [1, 2] do
        assert {:ok, measured} = MathLayout.glyph("𝑥", changed, 10, script_feature_level: level)
        assert hd(measured.glyphs).glyph_id == script
      end

      second = <<2::16, 8::16, 1::16, scriptscript::16, 1::16, 1::16, script::16>>

      changed = %{
        font
        | data:
            tables
            |> Map.put(
              "GSUB",
              gsub_table(
                [lookup, <<1::16, 0::16, 1::16, 8::16, second::binary>>],
                [{"ssty", [0, 1]}]
              )
            )
            |> ttf_fixture()
      }

      assert {:ok, measured} = MathLayout.glyph("𝑥", changed, 10, script_feature_level: 2)
      assert hd(measured.glyphs).glyph_id == scriptscript
      alternate = <<1::16, 8::16, 1::16, 14::16, 1::16, 1::16, base::16, 1::16, script::16>>

      changed = %{
        font
        | data:
            tables
            |> Map.put(
              "GSUB",
              gsub_table([
                <<3::16, 0::16, 1::16, 8::16, alternate::binary>>
              ])
            )
            |> ttf_fixture()
      }

      assert {:ok, measured} = MathLayout.glyph("𝑥", changed, 10, script_feature_level: 2)
      assert hd(measured.glyphs).glyph_id == base
    end

    test "advance caching retains source identity and shared layout budgets", %{font: font} do
      assert Font.glyph_widths(font, 2) == font.widths |> Enum.take(3) |> List.to_tuple()

      MathLayout.with_context(fn ->
        assert {:ok, glyph} = MathLayout.glyph("𝑥", font, 20)
        changed = %{font | widths: List.replace_at(font.widths, font.cmap[0x1D465], 777)}
        assert {:ok, changed_glyph} = MathLayout.glyph("𝑥", changed, 20)
        assert_in_delta changed_glyph.width, 777 * 20 / font.units_per_em, 1.0e-10
        refute changed_glyph.width == glyph.width
      end)

      Limits.install(%{Limits.effective() | max_layout_text_work: 1})
      assert {:error, {:resource_limit_exceeded, diagnostic}} = MathLayout.glyph("𝑥", font, 20)
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :measure_math
      assert diagnostic.message =~ "max_layout_text_work"
    end

    test "row baselines and italic correction follow the pinned placement algorithm", %{
      font: font
    } do
      FontValidator.with_budget(fn ->
        {:ok, variable} = MathLayout.glyph("x", font, 20, text_transform: :math_auto)
        {:ok, number} = MathLayout.glyph("1", font, 20)
        slanted = %{variable | italic_correction: 3.0}
        upright = %{number | italic_correction: 0.0}
        assert {:ok, row} = MathLayout.row([slanted, slanted, upright, slanted])
        assert Enum.map(row.glyphs, & &1.text) == ["x", "x", "1", "x"]
        assert Enum.map(row.glyphs, & &1.y) == [0.0, 0.0, 0.0, 0.0]

        assert Enum.map(row.glyphs, & &1.x) == [
                 0.0,
                 slanted.width,
                 slanted.width * 2 + 3,
                 slanted.width * 2 + upright.width + 3
               ]

        assert row.width == slanted.width * 3 + upright.width + 6
        assert row.italic_correction == 3
        assert row.ascent == max(slanted.ascent, upright.ascent)
        assert row.descent == max(slanted.descent, upright.descent)
        assert {:ok, empty} = MathLayout.row([])

        assert empty == %{
                 width: 0.0,
                 ascent: 0.0,
                 descent: 0.0,
                 italic_correction: 0.0,
                 glyphs: []
               }

        assert {:ok, nested} = MathLayout.row([row])
        assert nested.glyphs == row.glyphs
        assert nested.width == row.width + 3
        assert {:ok, negative} = MathLayout.row([%{upright | ascent: -2.0, descent: 4.0}])
        assert negative.ascent == -2
        assert negative.descent == 4
      end)
    end

    test "invalid glyph requests and geometry return actionable diagnostics", %{font: font} do
      for {text, face, size, opts} <- [
            {nil, font, 10, []},
            {<<255>>, font, 10, []},
            {"", font, 10, []},
            {"xy", font, 10, []},
            {String.duplicate("x", 100_000), font, 10, []},
            {"x", font, -1, []},
            {"x", font, "10", []},
            {"x", font, 10, nil},
            {"x", font, 10, [unknown: true]},
            {"x", font, 10, [text_transform: :italic]},
            {"x", font, 10, [script_feature_level: 3]},
            {"x", %{type: :built_in}, 10, []},
            {"x", %{font | units_per_em: 0}, 10, []}
          ] do
        assert {:error, {:invalid_document, diagnostic}} =
                 MathLayout.glyph(text, face, size, opts)

        assert diagnostic.stage == :layout
        assert diagnostic.operation == :measure_math
        assert diagnostic.message =~ "one Unicode scalar"
      end

      assert {:error, {:invalid_document, diagnostic}} =
               MathLayout.glyph("x", %{font | units_per_em: font.units_per_em + 1}, 10)

      assert diagnostic.message =~ "units disagree"

      for changed <- [
            %{font | cmap: %{}},
            %{font | cmap: %{?x => "invalid"}},
            %{font | cmap: %{?x => 65_535}},
            %{font | widths: []},
            %{font | widths: List.replace_at(font.widths, font.cmap[?x], -1)}
          ] do
        assert {:error, {:unsupported_glyph, diagnostic}} = MathLayout.glyph("x", changed, 10)
        assert diagnostic.source == "x"
        assert diagnostic.message =~ "U+78"
      end

      assert {:ok, glyph} = MathLayout.glyph("x", font, 10)

      for children <- [
            nil,
            [nil],
            [%{}],
            [%{glyph | width: "invalid"}],
            [%{glyph | glyphs: [nil]}],
            [%{glyph | glyphs: [%{hd(glyph.glyphs) | glyph_id: 0}]}]
          ] do
        assert {:error, {:invalid_document, %{operation: :measure_math}}} =
                 MathLayout.row(children)
      end

      Limits.install(%{Limits.effective() | max_font_math_work: 1})

      assert {:error, {:resource_limit_exceeded, %{operation: :measure_math}}} =
               MathLayout.glyph("x", font, 10)

      assert {:error, {:resource_limit_exceeded, %{operation: :measure_math}}} =
               MathLayout.row([glyph, glyph])
    end

    test "device adjustments use CSS pixels and reject unresolved variation indices" do
      assert {:ok, nil} = MathLayoutValidator.prepare_value(nil, 9)
      assert {:ok, {30, +0.0}} = MathLayoutValidator.prepare_value(%{value: 30, device: nil}, 9)
      device = %{start_size: 10, end_size: 12, deltas: [-2, -1, 1]}

      for {size, delta} <- [{6, 0.0}, {7.5, -1.5}, {8.25, -0.75}, {9, 0.75}, {9.75, 0.0}] do
        assert {:ok, {30, ^delta}} =
                 MathLayoutValidator.prepare_value(%{value: 30, device: device}, size)
      end

      for record <- [
            %{},
            %{value: 30, device: %{variation_index: {0, 1}}},
            %{value: 30, device: %{device | deltas: []}},
            %{value: 30, device: %{device | start_size: 15}}
          ] do
        assert {:error, {:invalid_document, %{operation: :measure_math}}} =
                 MathLayoutValidator.prepare_value(record, 9)
      end
    end
  end

  test "loaded caller fonts with variation-dependent kern heights return recoverable Math diagnostics" do
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, original} = Font.resolve("math", 400, :normal, registry)
    font = math_variation_font(original, original.cmap[?1], :kern)

    style = %{
      display: :block,
      font_size: 20,
      font_face: font,
      color: {0, 0, 0},
      line_height: 24,
      math_style: :compact
    }

    token = fn text ->
      %{
        type: :element,
        namespace: :mathml,
        tag: "mn",
        attributes: %{},
        style: style,
        children: [%{type: :text, text: text, style: style}]
      }
    end

    for tag <- ~w(msubsup mmultiscripts) do
      node = %{
        type: :element,
        namespace: :mathml,
        tag: tag,
        attributes: %{},
        style: style,
        children: Enum.map(~w(1 2 3), token)
      }

      assert {:error, {_, diagnostic}} = MathLayout.measure(node)
      assert diagnostic.stage == :layout and diagnostic.message =~ "variation adjustments"
      assert diagnostic.source =~ "kern"
    end
  end

  test "loaded caller ligature correction variations fail before any Math glyph is emitted" do
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, original} = Font.resolve("DejaVu Sans", 400, :normal, registry)
    font = math_variation_font(original, original.cmap[0xFB03], :italic)
    assert Font.shape_ligatures("ffi", font) == "ﬃ"

    style = %{
      display: :block,
      font_size: 20,
      font_face: font,
      color: {0, 0, 0},
      line_height: 24,
      math_style: :compact
    }

    node = %{
      type: :element,
      namespace: :mathml,
      tag: "mtext",
      attributes: %{},
      style: style,
      children: [%{type: :text, text: "ffi", style: style}]
    }

    assert {:error, {_, diagnostic}} = MathLayout.measure(node)
    assert diagnostic.stage == :layout and diagnostic.message =~ "variation adjustments"
  end

  describe "mathematical stretch geometry" do
    setup do
      {:ok, registry} = Font.load_registry(system_font_discovery: false)
      {:ok, _, font} = Font.resolve("math", 400, :normal, registry)
      {:ok, font: font}
    end

    test "natural size, first sufficient variant, and last-variant fallback", %{font: font} do
      assert {:ok, normal} = MathLayout.glyph("(", font, 12)
      assert {:ok, same} = MathLayout.stretch("(", font, 12, :vertical, 0)
      assert same == normal
      assert {:ok, same} = MathLayout.stretch("(", font, 12, :vertical, 11.184)
      assert same == normal

      for {target, id} <- [{11.19, 1064}, {11.2, 1301}, {14.244, 1301}, {14.25, 1302}] do
        assert {:ok, variant} = MathLayout.stretch("(", font, 12, :vertical, target)
        assert [glyph] = variant.glyphs
        assert glyph.glyph_id == id
        assert glyph.text == "("
      end

      # STIX's right-arrow head has a zero connector at a required join.
      assert {:ok, arrow} = MathLayout.stretch("→", font, 12, :horizontal, 60)
      assert {:ok, original_arrow} = MathLayout.glyph("→", font, 12)
      assert arrow == original_arrow
      assert {:ok, zero} = MathLayout.stretch("(", font, 0, :vertical, 10)
      assert hd(zero.glyphs).glyph_id == 1312
      assert zero.width == 0
      assert zero.ascent == 0
    end

    test "vertical assemblies join bottom to top with exact target and source Unicode", %{
      font: font
    } do
      assert {:ok, measured} = MathLayout.stretch("(", font, 12, :vertical, 80)
      assert_in_delta measured.width, 5.808, 1.0e-10
      assert_in_delta measured.ascent, 80, 1.0e-10
      assert measured.descent == 0
      assert measured.top_accent_attachment == nil
      assert Enum.map(measured.glyphs, & &1.glyph_id) == [4862, 4861, 4861, 4861, 4861, 4860]

      for {glyph, baseline} <-
            Enum.zip(measured.glyphs, [0, 13.1584, 26.0528, 38.9472, 51.8416, 64.724]) do
        assert_in_delta glyph.y, baseline, 1.0e-10
        assert glyph.x == 0
        assert glyph.text == "("
        assert glyph.font_face == font
      end

      assert {:ok, radical} = MathLayout.stretch("√", font, 12, :vertical, 80)

      assert Enum.map(radical.glyphs, & &1.glyph_id) ==
               [1661] ++ List.duplicate(1664, 8) ++ [1662]

      assert_in_delta hd(radical.glyphs).y, 0.06, 1.0e-10
      assert_in_delta radical.ascent, 80, 1.0e-10
      assert {:ok, integral} = MathLayout.stretch("∫", font, 12, :vertical, 80)
      assert Enum.map(integral.glyphs, & &1.glyph_id) == [4836, 4815, 4815, 4815, 4835]
      assert integral.italic_correction > 0
    end

    test "horizontal assemblies repeat every extender equally and share one baseline", %{
      font: font
    } do
      assert {:ok, brace} = MathLayout.stretch("⏞", font, 12, :horizontal, 60)
      assert Enum.map(brace.glyphs, & &1.glyph_id) == [2106, 2073, 2073, 2107, 2073, 2073, 2108]
      assert Enum.all?(brace.glyphs, &(&1.y == 0 and &1.text == "⏞"))
      assert_in_delta brace.width, 60, 1.0e-10

      for {glyph, offset} <-
            Enum.zip(brace.glyphs, [0, 9.762, 17.052, 24.342, 33.936, 41.226, 48.516]) do
        assert_in_delta glyph.x, offset, 1.0e-10
      end
    end

    test "zero, one, and repeated pieces retain unpainted extenders in orthogonal metrics", %{
      font: font
    } do
      parts = [{4862, 0, 100, 300, 0}, {4861, 100, 100, 200, 1}, {4860, 100, 0, 300, 0}]
      changed = math_construction_font(font, :vertical, [], parts)
      changed = %{changed | widths: List.replace_at(changed.widths, 4861, 2000)}
      assert {:ok, small} = MathLayout.stretch("A", changed, 1000, :vertical, 550)
      assert Enum.map(small.glyphs, & &1.glyph_id) == [4862, 4860]
      assert small.width == 2000
      assert small.ascent == 550

      assert {:ok, minimum} = MathLayout.stretch("A", changed, 1000, :vertical, 1)
      assert minimum.ascent == 500
      assert length(minimum.glyphs) == 2

      all_extenders = math_construction_font(font, :vertical, [], [{4861, 100, 100, 200, 1}])
      assert {:ok, single} = MathLayout.stretch("A", all_extenders, 1000, :vertical, 10)
      assert single.ascent == 200
      assert length(single.glyphs) == 1
      assert {:ok, repeated} = MathLayout.stretch("A", all_extenders, 1000, :vertical, 201)
      assert repeated.ascent == 300
      assert length(repeated.glyphs) == 2

      # A device correction is independent of design-unit scaling.
      device = <<10::16, 12::16, 1::16, 0xB400::16>>
      adjusted = math_construction_font(font, :horizontal, [], parts, italic: {25, device})
      assert {:ok, painted} = MathLayout.stretch("A", adjusted, 9, :horizontal, 10)
      assert_in_delta painted.italic_correction, 0.975, 1.0e-10
      assert painted.ascent != 0
      assert painted.descent != 0
    end

    test "unusable assemblies fall back without inventing connector lengths", %{font: font} do
      for parts <- [
            [],
            [{4862, 0, 100, 300, 0}],
            [{4861, 10, 10, 200, 1}],
            [{4861, 20, 20, 20, 1}],
            [{4862, 0, 100, 300, 0}, {4861, 100, 100, 200, 1}, {4860, 0, 0, 300, 0}]
          ] do
        changed = math_construction_font(font, :vertical, [{1301, 100}], parts)
        assert {:ok, fallback} = MathLayout.stretch("A", changed, 12, :vertical, 80)
        assert hd(fallback.glyphs).glyph_id == 1301
      end

      changed = math_construction_font(font, :vertical, [], [])
      assert {:ok, fallback} = MathLayout.stretch("A", changed, 12, :vertical, 80)
      assert hd(fallback.glyphs).glyph_id == 1

      missing = math_construction_font(font, :vertical, [], [{0, 100, 100, 200, 1}])

      assert {:error, {:unsupported_glyph, %{source: "A"}}} =
               MathLayout.stretch("A", missing, 12, :vertical, 80)

      variable =
        math_construction_font(font, :vertical, [], [{4861, 100, 100, 200, 1}],
          italic: {10, <<0::16, 1::16, 0x8000::16>>}
        )

      assert {:error, {:invalid_document, diagnostic}} =
               MathLayout.stretch("A", variable, 12, :vertical, 80)

      assert diagnostic.message =~ "variation coordinates"
    end

    test "requests and expansions fail through validators before allocating huge lists", %{
      font: font
    } do
      for {axis, target} <- [{nil, 1}, {:vertical, -1}, {:horizontal, "1"}, {:vertical, 1.0e20}] do
        assert {:error, {:invalid_document, diagnostic}} =
                 MathLayout.stretch("(", font, 12, axis, target)

        assert diagnostic.operation == :measure_math
        assert diagnostic.stage == :layout
        assert diagnostic.message =~ "bounded nonnegative target"
      end

      assert {:error, {:invalid_document, _}} = MathLayout.glyph("(", font, 1.0e20)

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               MathLayout.stretch("(", font, 1.0e-308, :vertical, 1_000_000_000)

      assert diagnostic.stage == :font
      assert diagnostic.operation == :measure_math
      assert diagnostic.message =~ "max_font_math_work"

      assert {:error, {:unsupported_glyph, diagnostic}} =
               MathLayout.stretch("x", font, 12, :vertical, 0)

      assert diagnostic.source == "x"
      assert diagnostic.message =~ "no vertical stretch construction"

      limits = Limits.effective()
      Limits.install(%{limits | max_layout_boxes: 3})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               MathLayout.stretch("(", font, 12, :vertical, 80)

      assert diagnostic.message =~ "max_layout_boxes"
      assert diagnostic.operation == :measure_math

      Limits.install(%{limits | max_layout_boxes: 100})

      assert {:error, {:resource_limit_exceeded, _}} =
               MathLayout.stretch("(", font, 0.000001, :vertical, 100_000)

      Limits.install(%{limits | max_layout_boxes: 13})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               MathLayout.with_context(fn ->
                 assert {:ok, _} = MathLayout.stretch("(", font, 12, :vertical, 80)
                 MathLayout.stretch("(", font, 12, :vertical, 80)
               end)

      assert diagnostic.message =~ "max_layout_boxes"
    end
  end

  describe "OpenType MATH data" do
    test "all fixed constants retain their signed values and record order" do
      records = for i <- 1..51, into: <<>>, do: <<-i::signed-16, 0::16>>

      constants =
        <<80::signed-16, 60::signed-16, 1200::16, 1800::16, records::binary, 65::signed-16>>

      assert {:ok, m} = MathFontValidator.prepare(math_table(constants), 8)
      assert map_size(m.constants) == 56
      assert m.constants.script_percent_scale_down == 80
      assert m.constants.script_script_percent_scale_down == 60
      assert m.constants.delimited_sub_formula_min_height == 1200
      assert m.constants.display_operator_min_height == 1800
      assert m.constants.axis_height == %{value: -2, device: nil}
      assert m.constants.fraction_rule_thickness.value == -35
      assert m.constants.radical_kern_after_degree.value == -51
      assert m.constants.radical_degree_bottom_raise_percent == 65
      assert m.glyph_info.italics_corrections == %{}
      assert m.glyph_info.top_accent_attachments == %{}
      assert m.glyph_info.extended_shapes == MapSet.new()
      assert m.glyph_info.kerning == %{}
      assert m.variants.vertical == %{}
      assert m.variants.horizontal == %{}
    end

    test "glyph corrections, range coverage, corner kerning, and assemblies use their own bases" do
      data = math_table(default_constants(), glyph_info(), variants())
      assert {:ok, m} = MathFontValidator.prepare(data, 8)

      assert m.glyph_info.italics_corrections[1] == %{
               value: -12,
               device: %{start_size: 10, end_size: 12, deltas: [-2, -1, 1]}
             }

      assert m.glyph_info.top_accent_attachments[2] == %{value: 333, device: nil}
      assert m.glyph_info.extended_shapes == MapSet.new([1, 2])
      kern = m.glyph_info.kerning[3]
      assert kern.top_left == nil
      assert kern.bottom_left == kern.top_right
      assert Enum.map(kern.top_right.heights, & &1.value) == [-5, 5]
      assert Enum.map(kern.top_right.values, & &1.value) == [-2, 0, 3]
      assert m.variants.min_connector_overlap == 20
      assert m.variants.horizontal[2] == %{variants: [%{glyph: 7, advance: 400}], assembly: nil}
      vertical = m.variants.vertical[1]
      assert vertical.variants == [%{glyph: 4, advance: 500}, %{glyph: 5, advance: 1000}]
      assert vertical.assembly.italics_correction == %{value: 15, device: nil}

      assert vertical.assembly.parts == [
               %{
                 glyph: 6,
                 start_connector: 0,
                 end_connector: 100,
                 full_advance: 300,
                 extender?: false
               },
               %{
                 glyph: 7,
                 start_connector: 100,
                 end_connector: 100,
                 full_advance: 200,
                 extender?: true
               }
             ]
    end

    test "all packed device formats and variation indices retain their adjustments" do
      for {format, bytes, expected} <- [
            {1, <<0b10110100::16>>, [0, 0, 0]},
            {2, <<0xF270::16>>, [-1, 2, 7]},
            {3, <<0x807F::16, 0xFD00::16>>, [-128, 127, -3]}
          ] do
        constants = default_constants() |> replace_bytes(10, <<214::16>>)
        constants = constants <> <<10::16, 12::16, format::16, bytes::binary>>
        assert {:ok, m} = MathFontValidator.prepare(math_table(constants), 8)
        assert m.constants.math_leading.device.deltas == expected
      end

      constants =
        replace_bytes(default_constants(), 10, <<214::16>>) <> <<5::16, 8::16, 0x8000::16>>

      assert {:ok, m} = MathFontValidator.prepare(math_table(constants), 8)
      assert m.constants.math_leading.device == %{variation_index: {5, 8}}
    end

    test "empty and single-range coverage and zero-height kerning are valid" do
      info = <<8::16, 0::16, 0::16, 0::16, 8::16, 0::16, 0::32, 2::16, 0::16>>
      assert {:ok, _} = MathFontValidator.prepare(math_table(default_constants(), info), 8)

      # A zero-height kern still has one value applying at every height.
      info =
        <<0::16, 0::16, 0::16, 8::16, 12::16, 1::16, 18::16, 0::16, 0::16, 0::16, 1::16, 1::16,
          1::16, 0::16, -4::signed-16, 0::16>>

      assert {:ok, m} = MathFontValidator.prepare(math_table(default_constants(), info), 8)

      assert m.glyph_info.kerning[1].top_right == %{
               heights: [],
               values: [%{value: -4, device: nil}]
             }
    end

    test "every truncated prefix and malformed scalar returns an actionable diagnostic" do
      data = math_table(default_constants(), glyph_info(), variants())

      for size <- 0..(byte_size(data) - 1) do
        assert {:error, {:invalid_document, diagnostic}} =
                 MathFontValidator.prepare(binary_part(data, 0, size), 8)

        assert diagnostic.stage == :font
        assert diagnostic.reason == :invalid_document
        assert diagnostic.operation == :math_metrics
        assert diagnostic.module == MathFontValidator
        assert diagnostic.source == "selected font MATH table"
        assert diagnostic.message =~ "OpenType MATH"
      end

      for invalid <- [nil, 1, %{}, []],
          do: assert({:error, {:invalid_document, _}} = MathFontValidator.prepare(invalid, 8))

      for count <- [nil, 0, -1, 65_537],
          do: assert({:error, {:invalid_document, _}} = MathFontValidator.prepare(data, count))

      for malformed <- [
            replace_bytes(data, 0, <<2::16>>),
            replace_bytes(data, 2, <<1::16>>),
            replace_bytes(data, 4, <<0::16>>),
            replace_bytes(data, 6, <<65_535::16>>)
          ] do
        assert {:error, {:invalid_document, _}} = MathFontValidator.prepare(malformed, 8)
      end
    end

    test "coverage validates format, ordering, glyph bounds, and consecutive range indices" do
      for coverage <- [
            <<3::16, 0::16>>,
            <<1::16, 2::16, 2::16, 1::16>>,
            <<1::16, 2::16, 1::16, 1::16>>,
            <<1::16, 1::16, 8::16>>,
            <<2::16, 1::16, 2::16, 1::16, 0::16>>,
            <<2::16, 1::16, 1::16, 8::16, 0::16>>,
            <<2::16, 1::16, 1::16, 2::16, 1::16>>,
            <<2::16, 2::16, 1::16, 2::16, 0::16, 2::16, 3::16, 2::16>>
          ] do
        info = <<0::16, 0::16, 8::16, 0::16, coverage::binary>>

        assert {:error, {:invalid_document, _}} =
                 MathFontValidator.prepare(math_table(default_constants(), info), 8)
      end
    end

    test "subtable counts, offsets, glyphs, and sort order are validated" do
      info = glyph_info()
      variants = variants()

      for invalid_info <- [
            replace_bytes(info, 10, <<2::16>>),
            replace_bytes(info, 56, <<2::16>>),
            replace_bytes(info, 78, <<-6::signed-16>>),
            replace_bytes(info, 26, <<9::16>>),
            replace_bytes(info, 22, <<15::16, 10::16>>)
          ] do
        assert {:error, {:invalid_document, _}} =
                 MathFontValidator.prepare(math_table(default_constants(), invalid_info), 8)
      end

      for invalid_variants <- [
            replace_bytes(variants, 6, <<2::16>>),
            replace_bytes(variants, 10, <<0::16>>),
            replace_bytes(variants, 30, <<8::16>>),
            replace_bytes(variants, 36, <<499::16>>),
            replace_bytes(variants, 42, <<0::16>>),
            replace_bytes(variants, 44, <<8::16>>),
            replace_bytes(variants, 52, <<2::16>>)
          ] do
        assert {:error, {:invalid_document, _}} =
                 MathFontValidator.prepare(
                   math_table(default_constants(), info, invalid_variants),
                   8
                 )
      end
    end

    test "decoded byte and expanded entry work uses the cumulative shared limit" do
      data = math_table()
      Limits.install(%{Limits.effective() | max_font_math_work: byte_size(data)})
      assert {:ok, _} = MathFontValidator.prepare(data, 8)

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               FontValidator.with_budget(fn ->
                 {:ok, _} = MathFontValidator.prepare(data, 8)
                 MathFontValidator.prepare(data, 8)
               end)

      assert diagnostic.stage == :font
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.message =~ "max_font_math_work"

      Limits.install(%{Limits.effective() | max_font_math_work: 241})
      assert {:error, {:resource_limit_exceeded, _}} = MathFontValidator.prepare(data, 8)
    end

    test "bundled math metrics reflect STIX data and ordinary font loading stays lazy" do
      assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
      assert {:ok, _, font} = Font.resolve("math", 400, :normal, registry)
      assert {:ok, m} = Font.math_metrics(font)
      assert m.units_per_em == 1000
      assert m.constants.axis_height.value == 258
      assert m.constants.script_percent_scale_down == 70
      assert map_size(m.glyph_info.italics_corrections) == 889
      assert map_size(m.variants.vertical) == 118
      assert m.variants.vertical[font.cmap[?√]].assembly != nil
      assert Map.keys(m.substitutions) |> Enum.sort() == ["dtls", "flac", "ssty"]
      assert [script_forms] = m.substitutions["ssty"]
      assert [script, scriptscript] = script_forms[font.cmap[0x1D465]]
      assert script != scriptscript
      assert script != font.cmap[0x1D465]
      assert {:ok, bounds} = Font.glyph_bounds(font)
      assert tuple_size(bounds) == length(font.widths)
      {xmin, ymin, xmax, ymax} = elem(bounds, font.cmap[?√])
      assert xmin < xmax
      assert ymin < ymax
      Limits.install(%{Limits.effective() | max_font_math_work: 1})
      assert {:ok, _} = Font.load_registry(system_font_discovery: false)
      assert {:error, {:resource_limit_exceeded, _}} = Font.math_metrics(font)
    end

    test "direct selected-font metrics enforce font source limits before parsing" do
      assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
      assert {:ok, _, font} = Font.resolve("math", 400, :normal, registry)
      Limits.install(%{Limits.effective() | max_font_source_bytes: 1})

      assert {:error, {:resource_limit_exceeded, %{stage: :font, message: message}}} =
               Font.math_metrics(font)

      assert message =~ "max_font_source_bytes"
    end
  end

  describe "math font substitutions" do
    test "single, alternate, and extension lookups retain application order" do
      delta = <<1::16, 6::16, 1::16, 1::16, 1::16, 1::16>>
      explicit = <<2::16, 10::16, 2::16, 3::16, 4::16, 1::16, 2::16, 1::16, 2::16>>
      alternate = <<1::16, 8::16, 1::16, 14::16, 1::16, 1::16, 1::16, 2::16, 5::16, 6::16>>
      extended = <<1::16, 1::16, 8::32, delta::binary>>

      for {type, subtable, expected} <- [
            {1, delta, %{1 => [2]}},
            {1, explicit, %{1 => [3], 2 => [4]}},
            {3, alternate, %{1 => [5, 6]}},
            {7, extended, %{1 => [2]}}
          ] do
        data = gsub_table([<<type::16, 0::16, 1::16, 8::16, subtable::binary>>])
        assert {:ok, %{"ssty" => [^expected]}} = MathFontValidator.prepare_substitutions(data, 8)
        kind = if type == 3, do: :alternate, else: :single
        assert {:ok, features} = MathFontValidator.prepare_substitution_features(data, 8)
        assert features.substitutions == %{"ssty" => [expected]}
        assert features.substitution_kinds == %{"ssty" => [kind]}
      end

      lookups = [
        <<1::16, 1::16, 1::16, 8::16, delta::binary>>,
        <<1::16, 0::16, 1::16, 8::16, explicit::binary>>
      ]

      data = gsub_table(lookups, [{"dtls", [0]}, {"flac", [1]}, {"ssty", [1, 0, 1]}])

      mixed =
        <<7::16, 0::16, 2::16, 10::16, 10 + byte_size(extended)::16, extended::binary, 1::16,
          3::16, 8::32, alternate::binary>>

      assert {:error, {:invalid_document, diagnostic}} =
               MathFontValidator.prepare_substitution_features(gsub_table([mixed]), 8)

      assert diagnostic.message =~ "different lookup types"
      assert {:ok, substitutions} = MathFontValidator.prepare_substitutions(data, 8)
      assert substitutions["dtls"] == [%{1 => [2]}]
      assert substitutions["flac"] == [%{1 => [3], 2 => [4]}]
      assert substitutions["ssty"] == [%{1 => [2]}, %{1 => [3], 2 => [4]}]

      # Overlapping subtable coverage uses the first matching subtable.
      lookup = <<1::16, 0::16, 2::16, 10::16, 22::16, delta::binary, explicit::binary>>

      assert {:ok, %{"ssty" => [%{1 => [2], 2 => [4]}]}} =
               MathFontValidator.prepare_substitutions(gsub_table([lookup]), 8)

      # Signed deltas are modulo 65,536, not clamped.
      negative = <<1::16, 6::16, -1::signed-16, 1::16, 1::16, 1::16>>
      lookup = <<1::16, 0::16, 1::16, 8::16, negative::binary>>

      assert {:ok, %{"ssty" => [%{1 => [0]}]}} =
               MathFontValidator.prepare_substitutions(gsub_table([lookup]), 8)
    end

    test "only selected default math features are decoded" do
      unrelated = <<5::16, 0::16, 1::16, 8::16, 0::16>>

      assert {:ok, %{}} =
               MathFontValidator.prepare_substitutions(
                 gsub_table([unrelated], [{"ccmp", [0]}]),
                 8
               )

      assert {:ok, %{}} =
               MathFontValidator.prepare_substitutions(
                 gsub_table([unrelated], [{"ssty", [0]}], script: "latn"),
                 8
               )

      assert {:ok, %{}} =
               MathFontValidator.prepare_substitutions(
                 gsub_table([unrelated], [{"ssty", [0]}], default: false),
                 8
               )

      assert {:ok, %{"ssty" => []}} =
               MathFontValidator.prepare_substitutions(gsub_table([], [{"ssty", []}]), 8)

      data = gsub_table([], [{"ssty", []}])
      # The required feature participates even when the optional list is empty.
      data = replace_bytes(data, 24, <<0::16, 0::16>>)
      assert {:ok, %{"ssty" => []}} = MathFontValidator.prepare_substitutions(data, 8)

      # GSUB 1.1 without feature variations has the same static behavior.
      <<_::binary-size(4), scripts::16, features::16, lookups::16, rest::binary>> = data

      data =
        <<1::16, 1::16, scripts + 4::16, features + 4::16, lookups + 4::16, 0::32, rest::binary>>

      assert {:ok, %{"ssty" => []}} = MathFontValidator.prepare_substitutions(data, 8)

      assert {:error, {:invalid_document, _}} =
               MathFontValidator.prepare_substitutions(replace_bytes(data, 10, <<1::32>>), 8)
    end

    test "malformed GSUB offsets, counts, glyphs, filtering, and contexts are diagnosed" do
      alternate = <<1::16, 8::16, 1::16, 14::16, 1::16, 1::16, 1::16, 2::16, 5::16, 6::16>>
      lookup = <<3::16, 0::16, 1::16, 8::16, alternate::binary>>
      data = gsub_table([lookup])

      for size <- 0..(byte_size(data) - 1) do
        assert {:error, {:invalid_document, diagnostic}} =
                 MathFontValidator.prepare_substitutions(binary_part(data, 0, size), 8)

        assert diagnostic.stage == :font
        assert diagnostic.reason == :invalid_document
        assert diagnostic.operation == :math_metrics
        assert diagnostic.source == "selected font GSUB table"
        assert diagnostic.message =~ "OpenType GSUB"
      end

      for invalid <- [nil, [], %{}],
          do:
            assert(
              {:error, {:invalid_document, _}} =
                MathFontValidator.prepare_substitutions(invalid, 8)
            )

      for count <- [nil, 0, 65_537],
          do:
            assert(
              {:error, {:invalid_document, _}} =
                MathFontValidator.prepare_substitutions(data, count)
            )

      # Absolute offsets refer to the single-feature fixture's actual records.
      for {offset, bytes} <- [
            {0, <<2::16>>},
            {2, <<2::16>>},
            {4, <<0::16>>},
            {16, <<0::16>>},
            {18, <<1::16>>},
            {22, <<1::16>>},
            {24, <<2::16>>},
            {28, <<2::16>>},
            {40, <<2::16>>},
            {44, <<0::16>>},
            {46, <<5::16>>},
            {48, <<2::16>>},
            {50, <<2::16>>},
            {52, <<0::16>>},
            {54, <<2::16>>},
            {56, <<0::16>>},
            {58, <<2::16>>},
            {60, <<0::16>>},
            {62, <<0::16>>},
            {70, <<0::16>>},
            {72, <<8::16>>}
          ] do
        assert {:error, {:invalid_document, _}} =
                 MathFontValidator.prepare_substitutions(replace_bytes(data, offset, bytes), 8),
               "offset #{offset}"
      end

      for {type, subtable} <- [
            {1, <<3::16, 6::16, 0::16, 1::16, 0::16>>},
            {1, <<2::16, 8::16, 1::16, 8::16, 1::16, 1::16, 1::16>>},
            {1, <<2::16, 8::16, 2::16, 0::16, 1::16, 1::16, 1::16>>},
            {7, <<2::16, 1::16, 8::32>>},
            {7, <<1::16, 7::16, 8::32>>},
            {7, <<1::16, 1::16, 0::32>>}
          ] do
        lookup = <<type::16, 0::16, 1::16, 8::16, subtable::binary>>

        assert {:error, {:invalid_document, _}} =
                 MathFontValidator.prepare_substitutions(gsub_table([lookup]), 8)
      end

      for {features, opts} <- [
            {[{"ssty", [0]}, {"flac", [0]}], []},
            {[{"ssty", [0]}], [script: "math", script_offset: 0]},
            {[{"ssty", [0]}], [feature_offset: 0]}
          ] do
        assert {:error, {:invalid_document, _}} =
                 MathFontValidator.prepare_substitutions(gsub_table([lookup], features, opts), 8)
      end
    end

    test "selected substitutions participate in font bytes, memoization, and cumulative work" do
      lookup = <<1::16, 0::16, 1::16, 8::16, 1::16, 6::16, 0::16, 1::16, 1::16, 1::16>>
      gsub = gsub_table([lookup])

      data =
        valid_tables() |> Map.put("MATH", math_table()) |> Map.put("GSUB", gsub) |> ttf_fixture()

      assert {:ok, registry} = Font.load_registry(fonts: [%{family: "Math fixture", data: data}])
      assert {:ok, _, font} = Font.resolve("Math fixture", 400, :normal, registry)
      assert {:ok, metrics} = Font.math_metrics(font)
      assert metrics.substitutions == %{"ssty" => [%{1 => [1]}]}

      changed = %{
        font
        | data:
            valid_tables()
            |> Map.put("MATH", math_table())
            |> Map.put("GSUB", replace_bytes(gsub, 60, <<-1::signed-16>>))
            |> ttf_fixture()
      }

      FontValidator.with_budget(fn ->
        assert Font.math_metrics(font) == {:ok, metrics}
        assert Font.math_metrics(font) == {:ok, metrics}
        assert {:ok, %{substitutions: %{"ssty" => [%{1 => [0]}]}}} = Font.math_metrics(changed)
        assert Font.math_metrics(font) == {:ok, metrics}
      end)

      invalid = %{
        font
        | data:
            valid_tables()
            |> Map.put("MATH", math_table())
            |> Map.put("GSUB", <<>>)
            |> ttf_fixture()
      }

      assert {:error, {:invalid_document, %{source: "selected font GSUB table"}}} =
               Font.math_metrics(invalid)

      Limits.install(%{Limits.effective() | max_font_math_work: 1})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               MathFontValidator.prepare_substitutions(gsub, 2)

      assert diagnostic.stage == :font
      assert diagnostic.message =~ "max_font_math_work"

      Limits.install(%{Limits.effective() | max_font_math_work: 68})
      assert {:ok, _} = MathFontValidator.prepare_substitutions(gsub, 2)

      assert {:error, {:resource_limit_exceeded, _}} =
               FontValidator.with_budget(fn ->
                 assert {:ok, _} = MathFontValidator.prepare_substitutions(gsub, 2)
                 MathFontValidator.prepare_substitutions(gsub, 2)
               end)
    end
  end

  test "format-12 mappings prefer full Unicode coverage and encode supplementary text" do
    full = cmap_format12_table([{?A, ?A, 1}, {0x1D465, 0x1D466, 2}, {0x10FFFF, 0x10FFFF, 4}])
    bmp = cmap_format4_table(?A, ?A, 1, 0)
    full_offset = 20 + byte_size(bmp)

    for {platform, encoding} <- [{3, 10}, {0, 4}, {0, 6}] do
      cmap =
        <<0::16, 2::16, 3::16, 1::16, 20::32, platform::16, encoding::16, full_offset::32,
          bmp::binary, full::binary>>

      data =
        valid_tables()
        |> Map.put("cmap", cmap)
        |> Map.put("maxp", <<0::32, 5::16>>)
        |> ttf_fixture()

      assert {:ok, registry} = Font.load_registry(fonts: [%{family: "Full Unicode", data: data}])
      assert {:ok, _, font} = Font.resolve("Full Unicode", 400, :normal, registry)
      assert font.cmap == %{?A => 1, 0x1D465 => 2, 0x1D466 => 3, 0x10FFFF => 4}
      assert Font.supports_text?(font, "A𝑥𝑦")
      assert Font.text_width("A𝑥𝑦", font, 10) == 18.0
      encoding = Font.pdf_encoding(["A𝑥𝑦"], font)
      assert encoding.cid_to_unicode == %{1 => ?A, 2 => 0x1D465, 3 => 0x1D466}
    end
  end

  test "format-12 missing glyphs and noncharacters are not format-4 sentinels" do
    full = cmap_format12_table([{?A, ?B, 0}, {0xFFFF, 0xFFFF, 2}])
    cmap = <<0::16, 1::16, 3::16, 10::16, 12::32, full::binary>>

    data =
      valid_tables()
      |> Map.put("cmap", cmap)
      |> Map.put("maxp", <<0::32, 3::16>>)
      |> ttf_fixture()

    assert {:ok, metrics} = FontValidator.prepare_ttf(data)
    assert metrics.cmap == %{?B => 1, 0xFFFF => 2}

    empty = cmap_format12_table([])
    bmp = cmap_format4_table(?A, ?A, 1, 0)
    offset = 20 + byte_size(empty)

    fallback =
      <<0::16, 2::16, 3::16, 10::16, 20::32, 3::16, 1::16, offset::32, empty::binary,
        bmp::binary>>

    assert {:ok, metrics} =
             FontValidator.prepare_ttf(
               valid_tables()
               |> Map.put("cmap", fallback)
               |> ttf_fixture()
             )

    assert metrics.cmap == %{?A => 66}
  end

  test "format-12 groups reject invalid ranges, offsets, and glyph indices" do
    malformed = [
      cmap_format12_table([{70, 65, 1}]),
      cmap_format12_table([{65, 70, 1}, {70, 75, 1}]),
      cmap_format12_table([{70, 75, 1}, {65, 69, 1}]),
      cmap_format12_table([{0x110000, 0x110000, 1}]),
      cmap_format12_table([{0xD800, 0xD800, 1}]),
      cmap_format12_table([{0xD7FF, 0xE000, 1}]),
      cmap_format12_table([{65, 66, 0xFFFFFFFF}]),
      cmap_format12_table([{65, 66, 65_535}]),
      <<12::16, 0::16, 28::32, 0::32, 1::32>>,
      <<12::16, 0::16, 16::32, 0::32, 1::32, 65::32, 65::32, 1::32>>,
      <<12::16, 1::16, 16::32, 0::32, 0::32>>,
      <<12::16, 0::16, 16::32, 1::32, 0::32>>
    ]

    for full <- malformed do
      cmap = <<0::16, 1::16, 3::16, 10::16, 12::32, full::binary>>
      assert {:ok, []} = FontValidator.prepare_cmap(cmap)

      assert :error =
               FontValidator.prepare_ttf(valid_tables() |> Map.put("cmap", cmap) |> ttf_fixture())
    end

    full = cmap_format12_table([{65, 66, 1}])
    cmap = <<0::16, 1::16, 3::16, 10::16, 12::32, full::binary>>
    assert {:ok, [_]} = FontValidator.prepare_cmap(cmap)

    assert :error =
             FontValidator.prepare_ttf(valid_tables() |> Map.put("cmap", cmap) |> ttf_fixture())

    for count <- [0, -1, 65_537, :bad] do
      assert :error = FontValidator.prepare_cmap(cmap, count)
    end
  end

  test "format-12 preparation shares the existing cumulative work budget" do
    full = cmap_format12_table([{0x1D465, 0x1D467, 1}])
    cmap = <<0::16, 2::16, 0::16, 4::16, 20::32, 3::16, 10::16, 20::32, full::binary>>
    Limits.install(%{Limits.effective() | max_font_cmap_work: 6})
    assert {:ok, [[{0x1D465, 0x1D467, 0, {:sequential, 1}}]]} = FontValidator.prepare_cmap(cmap)

    for budget <- [1, 2, 5] do
      Limits.install(%{Limits.effective() | max_font_cmap_work: budget})
      assert {:error, {:resource_limit_exceeded, diagnostic}} = FontValidator.prepare_cmap(cmap)
      assert diagnostic.stage == :limits
      assert diagnostic.message =~ "character-map work"
    end

    Limits.install(%{Limits.effective() | max_font_cmap_work: 3})
    data = valid_tables() |> Map.put("cmap", cmap) |> ttf_fixture()

    assert {:error, {:resource_limit_exceeded, %{module: Font, operation: :load_registry}}} =
             HtmlToPdf.render("<p>𝑥</p>", fonts: [%{family: "Bounded Unicode", data: data}])
  end

  test "bundled STIX Two Math is pinned, lazy, and accepts caller overrides" do
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    refute Enum.any?(registry.embedded, &(&1.family == "STIX Two Math"))

    for family <- ["STIX Two Math", "math"] do
      assert {:ok, [^family], font} = Font.resolve(family, 700, :italic, registry)

      assert {font.family, font.weight, font.style, font.source} ==
               {"STIX Two Math", 400, :normal, :bundled}

      assert Font.supports_text?(font, "𝑥𝑦∑√")

      assert Base.encode16(:crypto.hash(:sha256, font.data), case: :lower) ==
               "49b22767985171bce579307b21d98f4d35012fa39e316acc4ffcab8644dfb69b"
    end

    for family <- ["STIX Two Math", "math"] do
      data = ttf_fixture(valid_tables())

      assert {:ok, override} =
               Font.load_registry(
                 fonts: [%{family: family, data: data}],
                 system_font_discovery: false
               )

      assert {:ok, _, font} = Font.resolve("math", 400, :normal, override)
      assert font.family == family
      assert font.source == :configured
      assert font.data == data
    end
  end

  test "initial control sizing uses font averages and falls back for missing and legacy metrics" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
    os2 = fn average -> <<0::16, average::signed-16, 0::size(26 * 8)>> end
    hhea = replace_bytes(hhea_table(), 10, <<900::16>>)

    cases = [
      {"No Average", valid_tables(), 42.0},
      # The selected head bounds span 1000 units; hhea's maximum advance is 900.
      # Input sizing uses the rounded typeface span, preserving both font metrics.
      {"Metric Average", valid_tables() |> Map.put("OS/2", os2.(300)) |> Map.put("hhea", hhea),
       27.75},
      {"Wide Average", valid_tables() |> Map.put("OS/2", os2.(2000)), 42.0},
      {"Courier", valid_tables() |> Map.put("OS/2", os2.(300)), 42.0}
    ]

    for {family, tables, width} <- cases do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<input size=7 value=A style='font-family:#{family};font-size:10pt'>"
               )

      font = %{family: family, data: ttf_fixture(tables), weight: 400, style: :normal}

      assert {:ok, %{children: [input]}} =
               Style.compute_detailed(dom, fonts: [font], system_font_discovery: false)

      assert input.style.width == width
      assert input.style.font_family == family
      assert input.style._form_control.initial_state.value == "A"
    end
  end

  test "text metrics use selected font tables and cache exact source content" do
    assert :error = FontValidator.prepare_text_metrics(nil)
    assert :error = FontValidator.prepare_text_metrics(%{type: :built_in})
    assert :error = FontValidator.prepare_text_metrics(%{type: :embedded, id: "bad", data: <<>>})
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)
    assert {:ok, metrics} = FontValidator.prepare_text_metrics(font)
    assert metrics.units_per_em == 2048
    assert metrics.underline == %{position: -40, thickness: 90}
    assert metrics.strike.position > 0
    assert metrics.strike.thickness > 0
    assert metrics.average_width > 0 and metrics.max_width >= metrics.average_width

    for hhea <- [nil, <<>>] do
      tables =
        if is_nil(hhea),
          do: Map.delete(valid_tables(), "hhea"),
          else: Map.put(valid_tables(), "hhea", hhea)

      assert {:ok, %{max_width: nil}} =
               FontValidator.prepare_text_metrics(%{font | data: ttf_fixture(tables)})
    end

    post = <<0::64, -200::signed-16, 120::signed-16>>
    os2 = <<0::208, 80::signed-16, 350::signed-16>>
    data = valid_tables() |> Map.put("post", post) |> Map.put("OS/2", os2) |> ttf_fixture()
    other = %{font | data: data}

    FontValidator.with_budget(fn ->
      assert FontValidator.prepare_text_metrics(font) == {:ok, metrics}
      assert {:ok, changed} = FontValidator.prepare_text_metrics(other)
      assert changed.underline.position == -200
      assert changed.strike.position == 350
      assert FontValidator.prepare_text_metrics(font) == {:ok, metrics}
    end)

    for tables <- [
          valid_tables(),
          Map.put(valid_tables(), "post", <<>>),
          Map.put(valid_tables(), "post", <<0::64, 0::16, -1::signed-16>>),
          Map.put(valid_tables(), "OS/2", <<>>),
          Map.put(valid_tables(), "OS/2", <<0::208, -1::signed-16, 0::16>>)
        ] do
      assert {:ok, %{underline: nil, strike: nil}} =
               FontValidator.prepare_text_metrics(%{font | data: ttf_fixture(tables)})
    end
  end

  test "radicals resolve caller font bytes and device values and diagnose missing font data" do
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, source} = Font.resolve("math", 400, :normal, registry)
    tables = font_tables(source.data)

    math = tables["MATH"]
    <<_::32, constants_offset::16, _::binary>> = math
    # These byte offsets are fixed OpenType MathConstants format invariants.
    constants =
      default_constants()
      |> replace_bytes(188, <<100::signed-16>>)
      |> replace_bytes(196, <<40::signed-16>>)
      |> replace_bytes(200, <<10::signed-16>>)
      |> replace_bytes(204, <<-50::signed-16>>)
      |> replace_bytes(208, <<-400::signed-16>>)

    changed = replace_bytes(math, constants_offset, constants)

    for {data, expected} <- [
          {changed, {:ok, 0.8}},
          {math_table(
             replace_bytes(constants, 198, <<214::16>>) <> <<27::16, 27::16, 1::16, 0x4000::16>>
           ), {:ok, 1.55}},
          {math_table(replace_bytes(constants, 198, <<214::16>>) <> <<1::16, 2::16, 0x8000::16>>),
           :variation},
          {nil, :missing}
        ] do
      caller_tables = if data, do: Map.put(tables, "MATH", data), else: Map.delete(tables, "MATH")
      font = %{source | data: ttf_fixture(caller_tables)}

      style = %{
        display: :block,
        font_face: font,
        font_size: 20,
        line_height: 24,
        color: {0, 0, 1},
        math_style: :compact
      }

      space = %{
        type: :element,
        namespace: :mathml,
        tag: "mspace",
        style: style,
        children: [],
        attributes: %{"width" => "3pt", "height" => "5pt"}
      }

      node = %{
        type: :element,
        namespace: :mathml,
        tag: "msqrt",
        style: style,
        children: [space],
        attributes: %{}
      }

      case expected do
        {:ok, thickness} ->
          assert {:ok, prepared} = MathLayoutValidator.prepare_element(node, %{})
          assert_in_delta prepared.rule_thickness, thickness, 1.0e-10
          assert prepared.gap == 2.0 and prepared.extra_ascender == 0.2
          assert prepared.kern_before == 0.0 and prepared.kern_after == -8.0

          if data == changed do
            assert {:ok, measured} = MathLayout.measure(node)
            assert hd(measured.glyphs).font_face.data == font.data
            assert measured.ascent == 8.0

            assert {:error, {:unsupported_glyph, diagnostic}} =
                     MathLayout.measure(%{
                       node
                       | style: %{
                           style
                           | font_face: %{font | cmap: Map.delete(font.cmap, 0x221A)}
                         }
                     })

            assert diagnostic.source == "√" and diagnostic.operation == :measure_math
          else
            # The selected-font radical fallback uses the actual base glyph
            # when a valid caller face has no stretch construction. Preserve
            # its actual GID, outlines, advance and overbar are painted.
            assert {:ok, natural} = MathLayout.glyph("√", font, style.font_size)
            assert {:ok, metrics} = Font.math_metrics(font)
            [natural_glyph] = natural.glyphs
            refute Map.has_key?(metrics.variants.vertical, natural_glyph.glyph_id)

            assert {:error, {:unsupported_glyph, diagnostic}} =
                     MathLayout.stretch("√", font, style.font_size, :vertical, 30)

            assert diagnostic.source == "√" and diagnostic.operation == :measure_math
            assert diagnostic.message =~ "no vertical stretch construction"
            assert {:ok, measured} = MathLayout.measure(node)
            assert [surd] = measured.glyphs
            assert surd.font_face.data == font.data
            assert surd.glyph_id == natural_glyph.glyph_id
            assert surd.font_size == natural_glyph.font_size
            assert surd.width == natural_glyph.width
            assert surd.height == natural_glyph.height
            assert_in_delta measured.width, natural.width + 3.0, 1.0e-10
            assert [rule] = Enum.filter(measured.drawing_items, &(&1.kind == :native))
            assert rule.box.width == 3.0
            assert_in_delta rule.box.height, thickness, 1.0e-10

            assert {:ok, {left, bottom, right, top}} =
                     NativeElixirPdfUtilities.Validators.MathPaintValidator.prepare_geometry(
                       measured
                     )

            assert right > left and top > bottom

            artifact = Path.expand("tmp/math_radical_caller_fallback")
            File.mkdir_p!(artifact)
            font_path = Path.join(artifact, "caller.ttf")
            File.write!(font_path, font.data)
            File.write!(Path.join(artifact, "original-node.etf"), :erlang.term_to_binary(node))

            html =
              "<math style='font-family:Caller Radical;font-size:20pt;math-style:compact;color:blue'><msqrt><mspace width='3pt' height='5pt'/></msqrt></math>"

            File.write!(Path.join(artifact, "original-source.html"), html)

            assert {:ok, pdf} =
                     HtmlToPdf.render(html,
                       fonts: [%{family: "Caller Radical", path: font_path}],
                       system_font_discovery: false,
                       page_size: {120, 80},
                       margin: 10
                     )

            File.write!(Path.join(artifact, "native.pdf"), pdf)

            File.write!(
              Path.join(artifact, "selected-glyph.exs"),
              inspect(
                %{
                  gid: surd.glyph_id,
                  advance: surd.width,
                  height: surd.height,
                  font_sha256: Base.encode16(:crypto.hash(:sha256, font.data), case: :lower),
                  paint: {left, bottom, right, top}
                },
                pretty: true
              )
            )
          end

        _ ->
          assert {:error, {_, diagnostic}} = MathLayout.measure(node)
          assert diagnostic.stage == if(expected == :variation, do: :layout, else: :font)
          assert diagnostic.operation == :measure_math
          assert diagnostic.message =~ if(expected == :variation, do: "variation", else: "MATH")
      end
    end
  end

  test "script and limit placement use caller MATH bytes and device values with public failures" do
    # MathConstants offsets are fixed OpenType record positions, not limits.
    constants =
      default_constants()
      |> replace_bytes(24, <<300::signed-16>>)
      |> replace_bytes(36, <<400::signed-16>>)
      |> replace_bytes(40, <<200::signed-16>>)
      |> replace_bytes(60, <<50::signed-16>>)
      |> replace_bytes(64, <<100::signed-16>>)
      |> replace_bytes(68, <<700::signed-16>>)

    for {records, expected} <- [
          {constants, {:ok, 8.0}},
          {replace_bytes(constants, 38, <<214::16>>) <> <<27::16, 27::16, 1::16, 0x4000::16>>,
           {:ok, 8.75}},
          {replace_bytes(constants, 38, <<214::16>>) <> <<1::16, 2::16, 0x8000::16>>, :variation},
          {nil, :missing}
        ] do
      tables =
        if records, do: Map.put(valid_tables(), "MATH", math_table(records)), else: valid_tables()

      data = ttf_fixture(tables)
      assert {:ok, registry} = Font.load_registry(fonts: [%{family: "STIX Two Math", data: data}])
      assert {:ok, _, font} = Font.resolve("math", 400, :normal, registry)

      style = %{
        display: :block,
        font_face: font,
        font_size: 20,
        line_height: 24,
        color: {0, 0, 1},
        math_style: :normal,
        math_shift: :normal
      }

      space = %{
        type: :element,
        namespace: :mathml,
        tag: "mspace",
        style: style,
        children: [],
        attributes: %{"width" => "3pt", "height" => "1pt"}
      }

      node = %{
        type: :element,
        namespace: :mathml,
        tag: "msup",
        style: style,
        children: [space, space],
        attributes: %{}
      }

      case expected do
        {:ok, shift} ->
          assert {:ok, prepared} = MathLayoutValidator.prepare_element(node, %{})
          assert prepared.constants.superscript_shift_up == shift
          assert prepared.constants.space_after_script == 1.0
          assert {:ok, measured} = MathLayout.measure(node)
          assert measured.width == 7.0 and measured.ascent == 1 + shift

          assert {:ok, compact} =
                   MathLayout.measure(%{node | style: %{style | math_shift: :compact}})

          assert compact.ascent == 5.0
          assert {:ok, limit} = MathLayout.measure(%{node | tag: "mover"})
          assert limit.ink_ascent >= limit.ascent

          assert {:ok, public} =
                   HtmlToPdf.render(
                     "<math style='font-size:20pt'><msup><mspace width='3pt' height='1pt'/><mspace width='3pt' height='1pt'/></msup></math>",
                     fonts: [%{family: "STIX Two Math", data: data}],
                     system_font_discovery: false
                   )

          assert is_binary(public)

        _ ->
          assert {:error, {_, diagnostic}} = MathLayout.measure(node)
          assert diagnostic.operation == :measure_math
          assert diagnostic.message =~ if(expected == :missing, do: "MATH", else: "variation")

          assert {:error, {_, public_diagnostic}} =
                   HtmlToPdf.render(
                     "<math><msup><mspace width='3pt'/><mspace width='3pt'/></msup></math>",
                     fonts: [%{family: "STIX Two Math", data: data}],
                     system_font_discovery: false
                   )

          assert public_diagnostic.stage in [:font, :layout]
          assert public_diagnostic.source != nil and public_diagnostic.reason != nil
      end
    end
  end

  test "script corner kern thresholds and devices use the selected caller glyph size" do
    tables = ttf_font_path!() |> File.read!() |> font_tables()
    cmap = cmap_format12_table([{?A, ?A, 3}])
    tables = Map.put(tables, "cmap", <<0::16, 1::16, 3::16, 10::16, 12::32, cmap::binary>>)

    for {shift, adjustment, expected} <- [
          {99, :none, :lower},
          {100, :none, :upper},
          {101, :none, :upper},
          {100, :kern_device, :lower},
          {100, :height_device, :lower},
          {100, :reordered_height_device, :height_error},
          {100, :variation, :error}
        ] do
      device =
        case adjustment do
          :none -> <<>>
          :variation -> <<1::16, 2::16, 0x8000::16>>
          _ -> <<27::16, 27::16, 1::16, 0x4000::16>>
        end

      height_device = if adjustment == :height_device, do: 14, else: 0
      value_device = if adjustment in [:kern_device, :variation], do: 14, else: 0

      # Offsets follow the fixed OpenType MathKern record layout.
      kern =
        if adjustment == :reordered_height_device do
          <<2::16, 100::signed-16, 22::16, 110::signed-16, 0::16, 10::signed-16, 0::16,
            -30::signed-16, 0::16, 40::signed-16, 0::16, device::binary>>
        else
          <<1::16, 100::signed-16, height_device::16, 10::signed-16, 0::16, -30::signed-16,
            value_device::16, device::binary>>
        end

      info =
        <<0::16, 0::16, 0::16, 8::16, 12::16, 1::16, 18::16, 18::16, 18::16, 18::16, 1::16, 1::16,
          3::16, kern::binary>>

      constants = replace_bytes(default_constants(), 36, <<shift::signed-16>>)
      data = tables |> Map.put("MATH", math_table(constants, info)) |> ttf_fixture()

      {:ok, registry} =
        Font.load_registry(
          fonts: [%{family: "STIX Two Math", data: data}],
          system_font_discovery: false
        )

      {:ok, _, font} = Font.resolve("math", 400, :normal, registry)

      style = %{
        display: :block,
        font_face: font,
        font_size: 20,
        line_height: 24,
        color: {0, 0, 0},
        math_style: :normal,
        math_shift: :normal
      }

      token = fn size ->
        token_style = %{style | font_size: size}

        %{
          type: :element,
          namespace: :mathml,
          tag: "mn",
          style: token_style,
          attributes: %{},
          children: [%{type: :text, text: "A", style: token_style}]
        }
      end

      node = %{
        type: :element,
        namespace: :mathml,
        tag: "msup",
        style: style,
        attributes: %{},
        children: [token.(20), token.(10)]
      }

      case expected do
        :error ->
          assert {:error, {:invalid_document, diagnostic}} = MathLayout.measure(node)
          assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
          assert diagnostic.message =~ "variation"
          assert diagnostic.source =~ "glyph 3"

        :height_error ->
          assert {:error, {:invalid_document, diagnostic}} = MathLayout.measure(node)
          assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
          assert diagnostic.message =~ "height ranges"
          assert diagnostic.source =~ "glyph 3"

        _ ->
          assert {:ok, natural} = MathLayout.measure(token.(20))
          assert {:ok, measured} = MathLayout.measure(node)
          assert [base, script] = measured.glyphs
          assert base.glyph_id == 3 and script.glyph_id == 3
          assert script.font_size == 10 and script.font_face.data == data

          delta =
            if expected == :lower,
              do: (10 * 20 + 10 * 10) / font.units_per_em,
              else: (-30 * 20 + 10 * 10) / font.units_per_em

          assert_in_delta script.x, natural.width + delta, 1.0e-10
          assert measured.actual_text == "AA"
      end
    end
  end

  test "fractions resolve caller MATH constants and diagnose unsupported variation values" do
    constants = replace_bytes(default_constants(), 12, <<250::signed-16>>)
    constants = replace_bytes(constants, 144, <<40::signed-16>>)

    for {records, expected} <- [
          {constants, {:ok, 5.0, 0.8}},
          {replace_bytes(constants, 14, <<214::16>>) <> <<5::16, 8::16, 0x8000::16>>, :variation},
          {nil, :missing}
        ] do
      tables =
        if records, do: Map.put(valid_tables(), "MATH", math_table(records)), else: valid_tables()

      data = ttf_fixture(tables)
      assert {:ok, registry} = Font.load_registry(fonts: [%{family: "STIX Two Math", data: data}])
      assert {:ok, _, font} = Font.resolve("math", 400, :normal, registry)

      style = %{
        display: :block,
        font_face: font,
        font_size: 20,
        line_height: 24,
        color: {0, 0, 0},
        math_style: :compact
      }

      space = %{
        type: :element,
        namespace: :mathml,
        tag: "mspace",
        style: style,
        children: [],
        attributes: %{"width" => "3pt"}
      }

      node = %{
        type: :element,
        namespace: :mathml,
        tag: "mfrac",
        style: style,
        children: [space, space],
        attributes: %{}
      }

      result = MathLayoutValidator.prepare_element(node, %{})

      case expected do
        {:ok, axis, thickness} ->
          assert {:ok, prepared} = result
          assert prepared.axis == axis
          assert prepared.rule_thickness == thickness
          assert {:ok, measured} = MathLayout.measure(node)
          assert measured.ascent >= axis

          assert {:error, {_, diagnostic}} =
                   MathLayoutValidator.prepare_element(
                     %{node | attributes: %{"linethickness" => "1ex"}},
                     %{}
                   )

          assert diagnostic.stage == :font and diagnostic.operation == :measure_math

        failure ->
          assert {:error, {_, diagnostic}} = result
          assert diagnostic.operation == :measure_math
          assert diagnostic.stage in [:font, :layout]

          assert {:error, {_, public_diagnostic}} =
                   NativeElixirPdfUtilities.HtmlToPdf.render(
                     "<math><mfrac><mspace width='3pt'/><mspace width='3pt'/></mfrac></math>",
                     fonts: [%{family: "STIX Two Math", data: data}],
                     system_font_discovery: false
                   )

          assert public_diagnostic.operation ==
                   if(failure == :missing, do: :math_metrics, else: :measure_math)

          assert public_diagnostic.stage in [:font, :layout]

          assert public_diagnostic.message =~
                   if(failure == :variation, do: "variation", else: "MATH")

          if failure == :variation,
            do: assert(diagnostic.message =~ "variation"),
            else: assert(diagnostic.message =~ "MATH")
      end
    end
  end

  test "math metrics use caller font bytes and refresh reused identifiers" do
    records = <<0::size(204 * 8)>>

    constants =
      <<75::signed-16, 55::signed-16, 1000::16, 1500::16, records::binary, 60::signed-16>>

    math = <<1::16, 0::16, 10::16, 224::16, 232::16, constants::binary, 0::64, 0::80>>
    data = valid_tables() |> Map.put("MATH", math) |> ttf_fixture()
    assert {:ok, registry} = Font.load_registry(fonts: [%{family: "STIX Two Math", data: data}])
    assert {:ok, _, font} = Font.resolve("math", 400, :normal, registry)
    assert {:ok, metrics} = Font.math_metrics(font)
    assert metrics.units_per_em == 1000
    assert metrics.constants.script_percent_scale_down == 75

    changed_constants =
      <<50::signed-16, 55::signed-16, 1000::16, 1500::16, records::binary, 60::signed-16>>

    changed_math =
      <<1::16, 0::16, 10::16, 224::16, 232::16, changed_constants::binary, 0::64, 0::80>>

    changed = %{font | data: valid_tables() |> Map.put("MATH", changed_math) |> ttf_fixture()}

    FontValidator.with_budget(fn ->
      assert Font.math_metrics(font) == {:ok, metrics}
      assert Font.math_metrics(font) == {:ok, metrics}
      assert {:ok, other} = Font.math_metrics(changed)
      assert other.constants.script_percent_scale_down == 50
      assert Font.math_metrics(font) == {:ok, metrics}
    end)

    for invalid <- [
          nil,
          %{type: :built_in},
          %{font | data: <<>>},
          %{font | data: valid_tables() |> ttf_fixture()},
          %{font | data: valid_tables() |> Map.put("MATH", <<>>) |> ttf_fixture()}
        ] do
      assert {:error, {:invalid_document, diagnostic}} = Font.math_metrics(invalid)
      assert diagnostic.stage == :font
      assert diagnostic.reason == :invalid_document
      assert diagnostic.operation == :math_metrics
      assert diagnostic.message =~ "MATH"
    end
  end

  test "missing optional decoration metrics use the defined automatic thickness" do
    data = valid_tables() |> ttf_fixture()

    assert {:ok, dom} =
             NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse(
               "<u style='text-decoration-thickness:from-font'>A</u>"
             )

    assert {:ok, %{children: [element]}} =
             NativeElixirPdfUtilities.HtmlToPdf.Style.compute(dom,
               fonts: [%{family: "Metrics fixture", data: data}],
               default_font: "Metrics fixture",
               system_font_discovery: false
             )

    assert hd(element.style._text_decorations).thickness >= 0.75

    assert {:error, {:invalid_document, %{stage: :font, message: message}}} =
             HtmlValidator.resolve_text_decorations(%{
               font_face: %{type: :built_in, family: "Helvetica"},
               text_decoration_line: [:underline]
             })

    assert message =~ "selected-font metrics"

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.resolve_text_decorations(%{
               element.style
               | color: :invalid,
                 text_decoration_line: [:underline]
             })
  end

  test "TrueType preparation owns binary input validation" do
    assert :error = FontValidator.prepare_ttf(nil)
    assert :error = FontValidator.prepare_ttf(123)
    assert :error = FontValidator.prepare_ttf(<<0, 1, 2>>)

    data =
      File.read!(
        Path.join(:code.priv_dir(:native_elixir_pdf_utilities), "fonts/dejavu/DejaVuSans.ttf")
      )

    assert {:ok, metrics} = FontValidator.prepare_ttf(data)
    assert metrics.units_per_em == 2048
    assert metrics.cmap[?A] > 0
    assert metrics.widths != []
  end

  test "bundled monospace faces load on demand with deterministic metrics" do
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    refute Enum.any?(registry.embedded, &(&1.family == "DejaVu Sans Mono"))

    for {weight, style} <- [{400, :normal}, {700, :normal}, {400, :italic}, {700, :italic}] do
      assert {:ok, _, font} = Font.resolve("DejaVu Sans Mono", weight, style, registry)
      assert font.family == "DejaVu Sans Mono"
      assert font.source == :bundled
      assert font.weight == weight
      assert font.style == style
      assert Font.text_width("iii", font, 10) == Font.text_width("WWW", font, 10)
    end

    assert {:ok, _, mono} = Font.resolve("monospace", 400, :normal, registry)
    assert mono.family == "DejaVu Sans Mono"
  end

  test "high glyph indexes use constant-time widths during repeated text measurement" do
    font = %{
      type: :embedded,
      units_per_em: 1000,
      cmap: %{?A => 65_534, ?B => 65_535},
      widths: List.duplicate(600, 65_535),
      default_width: 700
    }

    text = String.duplicate("A", 2000)
    {:reductions, before_work} = Process.info(self(), :reductions)
    assert Font.text_width(text, font, 10) == 12_000.0
    {:reductions, after_work} = Process.info(self(), :reductions)
    # A linked-list lookup per character needs over 100 million reductions here.
    assert after_work - before_work < 1_000_000
    assert Font.text_width("B", font, 10) == 7.0
  end

  test "short low-index text avoids scanning the entire font and high-index scans share a budget" do
    font = %{
      type: :embedded,
      units_per_em: 1000,
      cmap: %{?A => 1, ?B => 65_534},
      widths: List.duplicate(600, 65_535),
      default_width: 700
    }

    {:reductions, before_work} = Process.info(self(), :reductions)
    for _ <- 1..1000, do: assert(Font.text_width("A", font, 10) == 6.0)
    {:reductions, after_work} = Process.info(self(), :reductions)
    assert after_work - before_work < 200_000

    limits = Limits.effective()
    Limits.install(%{limits | max_layout_text_work: 70_000})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.Validators.HtmlValidator.with_render_budget(fn ->
               assert Font.text_width("B", font, 10) == 6.0
               Font.text_width("B", font, 10)
             end)

    assert diagnostic.stage == :layout
    assert diagnostic.message =~ "max_layout_text_work"
    assert Font.text_width("", font, 10) == 0.0
  end

  test "font scopes restore process settings and measurement caches still charge work" do
    {:garbage_collection, before_flags} = Process.info(self(), :garbage_collection)
    before_floor = Keyword.fetch!(before_flags, :min_bin_vheap_size)
    assert FontValidator.source_bytes() == 0

    FontValidator.with_budget(fn ->
      assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
      assert {:ok, _, selected} = Font.resolve("DejaVu Sans", 400, :normal, registry)
      source_bytes = FontValidator.source_bytes()
      assert source_bytes > 0
      {:garbage_collection, loaded_flags} = Process.info(self(), :garbage_collection)
      loaded_floor = Keyword.fetch!(loaded_flags, :min_bin_vheap_size)
      assert loaded_floor >= before_floor
      assert loaded_floor * :erlang.system_info(:wordsize) >= source_bytes

      assert :ok =
               FontValidator.with_budget(fn ->
                 Font.with_measurement_cache(fn ->
                   assert Font.text_width("A", selected, 12) > 0

                   Font.with_measurement_cache(fn ->
                     {:garbage_collection, nested} = Process.info(self(), :garbage_collection)
                     assert Keyword.fetch!(nested, :min_bin_vheap_size) == loaded_floor
                   end)
                 end)

                 :ok
               end)

      {:garbage_collection, nested_done} = Process.info(self(), :garbage_collection)
      assert Keyword.fetch!(nested_done, :min_bin_vheap_size) == loaded_floor
    end)

    {:garbage_collection, restored} = Process.info(self(), :garbage_collection)
    assert Keyword.fetch!(restored, :min_bin_vheap_size) == before_floor

    assert_raise RuntimeError, "font scope failure", fn ->
      FontValidator.with_budget(fn ->
        assert {:ok, _registry} = Font.load_registry(system_font_discovery: false)
        raise "font scope failure"
      end)
    end

    {:garbage_collection, restored} = Process.info(self(), :garbage_collection)
    assert Keyword.fetch!(restored, :min_bin_vheap_size) == before_floor

    font = %{
      type: :embedded,
      id: "measurement-fixture",
      units_per_em: 1000,
      cmap: %{?A => 1, ?B => 2},
      widths: [0, 600, 700],
      default_width: 600
    }

    Limits.install(%{Limits.effective() | max_layout_text_work: 3})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               Font.with_measurement_cache(fn ->
                 assert Font.text_width("A", font, 10) == 6.0
                 assert Font.text_width("A", font, 10) == 6.0
               end)
             end)

    assert diagnostic.stage == :layout
    assert diagnostic.message =~ "max_layout_text_work"

    Limits.install(%{
      Limits.effective()
      | max_layout_text_work: 20,
        max_layout_measurement_cache_bytes: 1,
        max_layout_measurement_cache_entries: 1
    })

    assert Font.with_measurement_cache(fn ->
             Font.with_measurement_cache(fn ->
               assert Font.text_width("A", font, 10) == 6.0
               assert Font.text_width("B", font, 10) == 7.0
               assert Font.text_width("B", font, 10) == 7.0
             end)
           end)
  end

  test "high glyph measurements reuse bounded width prefixes without repeated font scans" do
    font = %{
      type: :embedded,
      id: "high-glyph-prefix",
      units_per_em: 1000,
      cmap: %{?A => 1, ?B => 65_534, ?C => 65_535},
      widths: List.duplicate(600, 65_535),
      default_width: 700
    }

    Limits.install(%{Limits.effective() | max_layout_text_work: 70_000})

    assert HtmlValidator.with_render_budget(fn ->
             Font.with_measurement_cache(fn ->
               assert Font.text_width("A", font, 10) == 6.0
               for _ <- 1..1000, do: assert(Font.text_width("B", font, 10) == 6.0)
               assert Font.text_width("B", font, 10) == 6.0
             end)
           end)
  end

  test "measurement caches track changed font contents despite a reused identifier" do
    font = %{
      type: :embedded,
      id: "reused-identifier",
      units_per_em: 1000,
      cmap: %{?A => 1, ?B => 2},
      widths: [0, 600, 700],
      default_width: 800
    }

    assert Font.with_measurement_cache(fn ->
             assert Font.text_width("A", font, 10) == 6.0
             assert Font.text_width("A", %{font | widths: [0, 900, 700]}, 10) == 9.0
             assert Font.text_width("A", %{font | cmap: %{?A => 2}}, 10) == 7.0
             assert Font.text_width("A", font, 10) == 6.0
             assert Font.text_width("B", font, 10) == 7.0
           end)

    Limits.install(%{Limits.effective() | max_layout_measurement_cache_entries: 1})

    assert Font.with_measurement_cache(fn ->
             assert Font.text_width("A", font, 10) == 6.0
             assert Font.text_width("A", %{font | widths: [0, 900, 700]}, 10) == 9.0
           end)

    Limits.install(%{Limits.defaults() | max_layout_text_work: 100_000})
    high = %{font | cmap: %{?A => 65_535}, widths: [0, 600, 700]}

    assert HtmlValidator.with_render_budget(fn ->
             Font.with_measurement_cache(fn ->
               assert Font.text_width("A", high, 10) == 8.0
               assert Font.text_width("A", high, 10) == 8.0
               assert Font.text_width("AA", high, 10) == 16.0
             end)
           end)
  end

  test "validates bounded horizontal kerning and measures pair advances" do
    subtable = <<0::16, 20::16, 1::16, 1::16, 0::48, 1::16, 2::16, -100::signed-16>>
    kern = <<0::16, 1::16, subtable::binary>>
    assert {:ok, %{{1, 2} => -100}} = FontValidator.prepare_kerning(kern, 3)

    assert {:ok, %{{1, 2} => -200}} =
             FontValidator.prepare_kerning(
               <<0::16, 2::16, subtable::binary, subtable::binary>>,
               3
             )

    override = <<0::16, 20::16, 9::16, 1::16, 0::48, 1::16, 2::16, -20::signed-16>>

    assert {:ok, %{{1, 2} => -20}} =
             FontValidator.prepare_kerning(
               <<0::16, 2::16, subtable::binary, override::binary>>,
               3
             )

    font = %{
      type: :embedded,
      units_per_em: 1000,
      cmap: %{?A => 1, ?V => 2},
      widths: [0, 600, 600],
      default_width: 600,
      kerning: %{{1, 2} => -100}
    }

    assert Font.text_width("AV", font, 10) == 11.0
    assert Font.text_width("VA", font, 10) == 12.0

    unsorted =
      <<0::16, 1::16, 0::16, 26::16, 1::16, 2::16, 0::48, 2::16, 1::16, -20::signed-16, 1::16,
        2::16, -20::signed-16>>

    assert :error = FontValidator.prepare_kerning(unsorted, 3)
    assert :error = FontValidator.prepare_kerning(kern, 2)
    assert :error = FontValidator.prepare_kerning(binary_part(kern, 0, byte_size(kern) - 1), 3)
    assert :error = FontValidator.prepare_kerning(kern <> <<0>>, 3)
    assert :error = FontValidator.prepare_kerning(<<0::16, 1::16, 0::16, 6::16, 1::16>>, 3)
    assert {:ok, %{}} = FontValidator.prepare_kerning(<<0x10000::32>>, 3)
    unsupported = <<0::16, 6::16, 1::8, 1::8>>
    assert {:ok, %{}} = FontValidator.prepare_kerning(<<0::16, 1::16, unsupported::binary>>, 3)

    Limits.install(%{Limits.effective() | max_font_kerning_pairs: 1})

    for data <- [kern, <<0::16, 2::16>>] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               FontValidator.prepare_kerning(data, 3)

      assert diagnostic.stage == :limits
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :load_registry
      assert diagnostic.source == "kern"
      assert diagnostic.message =~ "kerning"
    end

    Limits.install(%{Limits.effective() | max_font_kerning_pairs: 2})

    assert {:error, {:resource_limit_exceeded, _}} =
             FontValidator.prepare_kerning(
               <<0::16, 2::16, subtable::binary, unsupported::binary>>,
               3
             )
  end

  test "load_registry loads explicit TTF fonts and resolves fallback families" do
    font_path = ttf_font_path!()

    assert {:ok, registry} =
             Font.load_registry(
               fonts: [%{family: "Fixture Sans", path: font_path, weight: 400, style: :normal}]
             )

    assert {:ok, ["Missing", "Fixture Sans", "Helvetica"], font} =
             Font.resolve("Missing, 'Fixture Sans', Helvetica", 400, :normal, registry)

    assert font.type == :embedded
    assert font.family == "Fixture Sans"
    assert is_binary(font.data)
    assert Font.pdf_name(font) == "Embedded-" <> font.id
  end

  test "load_registry accepts OpenType files that use the shared TrueType table path" do
    open_type_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-true-type-outline.otf")
    File.cp!(ttf_font_path!(), open_type_path)

    assert {:ok, registry} =
             Font.load_registry(fonts: [%{family: "OpenType Fixture", path: open_type_path}])

    assert {:ok, _families, font} =
             Font.resolve("OpenType Fixture", 400, :normal, registry)

    assert font.type == :embedded
  after
    File.rm(Path.join(System.tmp_dir!(), "native-elixir-pdf-true-type-outline.otf"))
  end

  test "load_registry tries font path candidates in order" do
    missing_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-missing-fallback.ttf")

    assert {:ok, registry} =
             Font.load_registry(
               fonts: [%{family: "Fallback Fixture", path: [missing_path, ttf_font_path!()]}]
             )

    assert {:ok, _families, font} =
             Font.resolve("Fallback Fixture", 400, :normal, registry)

    assert font.type == :embedded

    assert Font.load_registry(fonts: [%{family: "Bad", path: []}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", path: [ttf_font_path!(), 123]}]) == :error
  end

  test "load_registry tries in-memory font candidates in order" do
    valid_font = File.read!(ttf_font_path!())

    assert {:ok, registry} =
             Font.load_registry(
               fonts: [%{family: "Data Fallback Fixture", data: ["not a font", valid_font]}]
             )

    assert {:ok, _families, font} =
             Font.resolve("Data Fallback Fixture", 400, :normal, registry)

    assert font.type == :embedded
    assert font.data == valid_font

    assert {:ok, _registry} =
             Font.load_registry(fonts: [%{family: "Single Data Fixture", data: valid_font}])

    assert Font.load_registry(fonts: [%{family: "Bad", data: []}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", data: [valid_font, 123]}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", data: 123}]) == :error
  end

  test "load_registry accepts supported config shapes and normalizes metadata" do
    font_path = ttf_font_path!()

    assert {:ok, registry} =
             Font.load_registry(
               fonts: [
                 [family: "Fixture Keyword", path: font_path, weight: 400.0, style: "normal"],
                 %{
                   "family" => "Fixture String Keys",
                   "path" => font_path,
                   "weight" => "bold",
                   "style" => "italic"
                 },
                 %{
                   family: "Fixture Atom Italic",
                   path: font_path,
                   weight: "normal",
                   style: :italic
                 },
                 %{family: "!!!", path: font_path, weight: "500", style: :normal}
               ]
             )

    assert {:ok, _families, keyword_font} =
             Font.resolve("Fixture Keyword", 400, :normal, registry)

    assert keyword_font.weight == 400.0
    assert keyword_font.style == :normal

    assert {:ok, _families, string_key_font} =
             Font.resolve("Fixture String Keys", 700, :italic, registry)

    assert string_key_font.weight == 700
    assert string_key_font.style == :italic

    assert {:ok, _families, atom_italic_font} =
             Font.resolve("Fixture Atom Italic", 400, :italic, registry)

    assert atom_italic_font.weight == 400
    assert atom_italic_font.style == :italic

    assert {:ok, _families, safe_name_font} = Font.resolve("!!!", 500, :normal, registry)
    assert safe_name_font.pdf_name =~ "EmbeddedFont-"
  end

  test "load_registry rejects ambiguous and missing font sources" do
    font_data = File.read!(ttf_font_path!())

    assert Font.load_registry(
             fonts: [%{family: "Ambiguous", path: ttf_font_path!(), data: font_data}]
           ) == :error

    assert Font.load_registry(fonts: [%{family: "Missing source"}]) == :error
  end

  test "resolve discovers installed generic families without rewriting the CSS family" do
    assert {:ok, registry} = Font.load_registry([])

    for family <- ["sans-serif", "serif", "monospace", "system-ui"] do
      assert {:ok, [^family], font} = Font.resolve(family, 400, :normal, registry)
      assert font.type == :embedded
      assert font.source == :system
    end

    assert {:ok, ["Helvetica"], font} = Font.resolve("Helvetica", 400, :normal, registry)
    assert font.type == :embedded

    assert {:ok, ["sans-serif"], emphasized} =
             Font.resolve("sans-serif", 700, :italic, registry)

    assert emphasized.type == :embedded
    assert emphasized.source == :system
    assert emphasized.weight >= 600
  end

  test "load_registry includes bundled fallback faces and discovers installed fonts on demand" do
    assert {:ok, registry} = Font.load_registry([])

    assert Enum.map(registry.fallback, &{&1.family, &1.weight, &1.style}) == [
             {"DejaVu Sans", 400, :normal},
             {"DejaVu Sans", 700, :normal},
             {"DejaVu Sans", 400, :italic},
             {"DejaVu Sans", 700, :italic}
           ]

    assert {:ok, ["DejaVu Sans"], bundled} =
             Font.resolve("DejaVu Sans", 400, :normal, registry)

    assert bundled.type == :embedded
    assert bundled.source == :bundled
    assert is_binary(bundled.data)

    assert Enum.all?(registry.embedded, &(&1.family == "DejaVu Sans"))
    assert {:ok, ["sans-serif"], system_font} = Font.resolve("sans-serif", 400, :normal, registry)
    assert system_font.type == :embedded
    assert system_font.source == :system

    assert {:ok, disabled_registry} = Font.load_registry(system_font_discovery: false)

    assert {:ok, ["sans-serif"], disabled_fallback} =
             Font.resolve("sans-serif", 400, :normal, disabled_registry)

    assert disabled_fallback.family == "DejaVu Sans"
    assert disabled_fallback.source == :bundled

    assert {:ok, ["A Font That Is Not Installed 123"], missing_fallback} =
             Font.resolve("A Font That Is Not Installed 123", 400, :normal, registry)

    assert missing_fallback.family == "DejaVu Sans"
    assert missing_fallback.source == :bundled
  end

  test "fallback_faces prefers configured families and matches weight and style" do
    font_path = ttf_font_path!()

    assert {:ok, registry} =
             Font.load_registry(
               fonts: [
                 %{family: "Provided Sans", path: font_path, weight: 400, style: :normal},
                 %{family: "Provided Sans", path: font_path, weight: 700, style: :italic}
               ]
             )

    assert [provided, provided_normal, bundled | remaining_bundled] =
             Font.fallback_faces(registry, 700, :italic)

    assert {provided.family, provided.weight, provided.style} == {"Provided Sans", 700, :italic}

    assert {provided_normal.family, provided_normal.weight, provided_normal.style} ==
             {"Provided Sans", 400, :normal}

    assert {bundled.family, bundled.weight, bundled.style} == {"DejaVu Sans", 700, :italic}

    assert Enum.map(remaining_bundled, &{&1.family, &1.weight, &1.style}) == [
             {"DejaVu Sans", 400, :italic},
             {"DejaVu Sans", 700, :normal},
             {"DejaVu Sans", 400, :normal}
           ]

    assert Font.fallback_faces(%{embedded: []}, 400, :normal) == []
  end

  test "supports_text validates built-in and embedded glyph coverage" do
    built_in = %{type: :built_in, family: "Helvetica", pdf_name: "Helvetica"}

    assert Font.supports_text?(built_in, "ASCII ~")
    refute Font.supports_text?(built_in, "ASCII\n")
    refute Font.supports_text?(built_in, "café")
    refute Font.supports_text?(built_in, <<255>>)

    assert {:ok, registry} = Font.load_registry([])
    assert {:ok, _families, embedded} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    assert Font.supports_text?(embedded, "café © α €")
    refute Font.supports_text?(embedded, "漢")
    refute Font.supports_text?(embedded, <<255>>)
    refute Font.supports_text?(%{type: :unknown}, "ASCII")
  end

  test "text_width and embedded text encoding use TTF cmap and hmtx data" do
    font_path = ttf_font_path!()

    assert {:ok, registry} = Font.load_registry(fonts: [{"Fixture Sans", font_path}])
    assert {:ok, _families, font} = Font.resolve("Fixture Sans", 400, :normal, registry)

    assert Font.text_width("iiii", font, 12.0) < Font.text_width("WWWW", font, 12.0)
    assert Font.encode_embedded_text("é", font) =~ ~r/^[0-9A-F]{4}$/u
    assert Font.unicode_mappings(["é", <<0>>], font) |> Map.values() == [?é]

    shared_glyph = Map.fetch!(font.cmap, ?A)
    shared_font = %{font | cmap: Map.put(font.cmap, ?B, shared_glyph)}
    encoding = Font.pdf_encoding(["AB"], shared_font)

    assert encoding.codepoint_to_cid == %{?A => 1, ?B => 2}
    assert encoding.cid_to_gid == %{1 => shared_glyph, 2 => shared_glyph}
    assert encoding.cid_to_unicode == %{1 => ?A, 2 => ?B}
    assert Font.encode_embedded_text("AB", encoding) == "00010002"
  end

  test "nonzero letter spacing preserves individual glyphs in measurement and encoding" do
    assert {:ok, registry} = Font.load_registry([])
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    without_ligatures = %{
      font
      | cmap: Map.drop(font.cmap, [0xFB00, 0xFB01, 0xFB02, 0xFB03, 0xFB04])
    }

    for spacing <- [4, -1] do
      assert Font.shape_ligatures("ffi file", font, spacing) == "ffi file"

      assert Font.text_width("ffi file", font, 20, spacing) ==
               Font.text_width("ffi file", without_ligatures, 20)

      encoding = Font.pdf_encoding([{"ffi", spacing}], font)
      assert Map.has_key?(encoding.codepoint_to_cid, ?f)
      assert Map.has_key?(encoding.codepoint_to_cid, ?i)
      refute Map.has_key?(encoding.codepoint_to_cid, 0xFB03)
    end

    mixed = Font.pdf_encoding([{"ffi", 4}, "ffi"], font)
    assert Map.has_key?(mixed.codepoint_to_cid, ?f)
    assert Map.has_key?(mixed.codepoint_to_cid, 0xFB03)
  end

  test "encoding accepts original ligature text with either a font or a prepared encoding" do
    assert {:ok, registry} = Font.load_registry([])
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    for text <- ["ffi", "office file", "fluff afflict", "ﬃ"], spacing <- [0, 4] do
      encoding = Font.pdf_encoding([{text, spacing}], font)

      expected =
        text
        |> Font.shape_ligatures(font, spacing)
        |> String.to_charlist()
        |> Enum.map_join(fn codepoint ->
          Base.encode16(<<Map.fetch!(encoding.codepoint_to_cid, codepoint)::16>>)
        end)

      assert Font.encode_embedded_text(text, font, spacing) == expected
      assert Font.encode_embedded_text(text, encoding, spacing) == expected
    end

    encoding = Font.pdf_encoding(["ffi"], font)
    assert Font.encode_embedded_text("ffi", font) == "0001"
    assert Font.encode_embedded_text("ffi", encoding) == "0001"

    mixed = Font.pdf_encoding(["ffi", {"ffi", 4}], font)
    assert Font.encode_embedded_text("ffi", mixed, 4) == "000200020003"
  end

  test "embedded fonts shape Latin ligatures before measuring and encoding" do
    assert {:ok, registry} = Font.load_registry([])
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 700, :normal, registry)

    assert Font.shape_ligatures("Office file flow", font) == "Oﬃce ﬁle ﬂow"
    assert Font.text_width("Officer", font, 9) == Font.text_width("Oﬃcer", font, 9)

    encoding = Font.pdf_encoding(["Officer"], font)
    assert Map.has_key?(encoding.codepoint_to_cid, 0xFB03)
    refute Map.has_key?(encoding.codepoint_to_cid, ?f)

    without_ligatures = %{
      font
      | cmap: Map.drop(font.cmap, [0xFB00, 0xFB01, 0xFB02, 0xFB03, 0xFB04])
    }

    assert Font.shape_ligatures("Office file", without_ligatures) == "Office file"
  end

  test "ligature shaping preserves replacement order and unavailable glyphs" do
    font = %{
      cmap: %{0xFB00 => 1, 0xFB01 => 2, 0xFB02 => 3, 0xFB03 => 4, 0xFB04 => 5},
      latin_ligatures: %{
        "ff" => 0xFB00,
        "fi" => 0xFB01,
        "fl" => 0xFB02,
        "ffi" => 0xFB03,
        "ffl" => 0xFB04
      }
    }

    original = "é ffi ffl ff fi fl 中文"

    assert Font.shape_ligatures(original, font) == "é ﬃ ﬄ ﬀ ﬁ ﬂ 中文"

    partial = %{font | cmap: %{0xFB00 => 1, 0xFB01 => 2, 0xFB02 => 0}}
    assert Font.shape_ligatures(original, partial) == "é ﬀi ﬀl ﬀ ﬁ fl 中文"
    assert Font.shape_ligatures(original, %{font | cmap: %{}}) == original

    for spacing <- [-1, 1] do
      assert Font.shape_ligatures(original, font, spacing) == original
    end
  end

  test "Latin GSUB rejection preserves diagnostics and extension/filtering contracts" do
    assert {:error, {:invalid_document, diagnostic}} =
             MathFontValidator.prepare_substitution_features(<<1::16, 0::16>>, 8, :latin)

    assert diagnostic.stage == :font and diagnostic.operation == :font_ligatures
    assert diagnostic.source == "selected font GSUB table"

    filtered = <<4::16, 2::16, 1::16, 8::16, 0::16>>

    assert {:ok, %{latin_policy: :requires_full_shaping}} =
             MathFontValidator.prepare_substitution_features(
               gsub_table([filtered], [{"liga", [0]}], script: "latn"),
               8,
               :latin
             )

    extension =
      <<7::16, 0::16, 1::16, 8::16, 1::16, 1::16, 8::32, 1::16, 6::16, 1::16, 1::16, 1::16,
        1::16>>

    assert {:ok, %{latin_policy: :requires_full_shaping}} =
             MathFontValidator.prepare_substitution_features(
               gsub_table([extension], [{"liga", [0]}], script: "latn"),
               8,
               :latin
             )
  end

  test "malformed selected-font GSUB fails recoverably at the shared ligature authority" do
    data = ttf_fixture(Map.put(valid_tables(), "GSUB", <<1::16, 0::16>>))

    assert {:error, {:invalid_document, diagnostic}} =
             Font.load_registry(fonts: [%{family: "Malformed GSUB", data: data}])

    assert diagnostic.stage == :font and diagnostic.reason == :invalid_document
    assert diagnostic.operation == :font_ligatures
    assert diagnostic.source == "selected font GSUB table"
    assert diagnostic.message =~ "truncated table"
  end

  test "selected Mono cmap ligatures stay inactive without a Latin liga feature" do
    mono_path =
      Path.join(:code.priv_dir(:native_elixir_pdf_utilities), "fonts/dejavu/DejaVuSansMono.ttf")

    assert {:ok, registry} =
             Font.load_registry(
               fonts: [%{family: "Selected Mono", path: mono_path}],
               system_font_discovery: false
             )

    assert {:ok, _, mono} = Font.resolve("Selected Mono", 400, :normal, registry)
    assert Map.fetch!(mono.cmap, 0xFB01) > 0
    assert mono.latin_ligatures == %{}
    assert Font.shape_ligatures("No file chosen", mono) == "No file chosen"
    encoding = Font.pdf_encoding(["file"], mono)
    refute Map.has_key?(encoding.codepoint_to_cid, 0xFB01)
    assert Font.text_width("file", mono, 10.5) == 4 * Font.text_width("f", mono, 10.5)
    assert String.length(Font.encode_embedded_text("file", encoding)) == 16

    assert {:ok, _, sans} = Font.resolve("DejaVu Sans", 400, :normal, registry)
    assert sans.latin_ligatures["fi"] == 0xFB01
    assert Font.shape_ligatures("file", sans) == "ﬁle"
    refute Font.shape_ligatures("file", Map.delete(sans, :latin_ligatures)) == "ﬁle"
  end

  test "load_registry skips an empty preferred cmap subtable" do
    empty_format4 = cmap_format4_table(0xFFFF, 0xFFFF, 1, 0)
    populated_format4 = cmap_format4_table(?A, ?A, 1, 0)
    populated_offset = 20 + byte_size(empty_format4)

    cmap =
      <<0::16, 2::16, 3::16, 10::16, 20::32, 3::16, 1::16, populated_offset::32,
        empty_format4::binary, populated_format4::binary>>

    font_data = valid_tables() |> Map.put("cmap", cmap) |> ttf_fixture()

    assert {:ok, registry} =
             Font.load_registry(fonts: [%{family: "Fallback Cmap Sans", data: font_data}])

    assert {:ok, ["Fallback Cmap Sans"], font} =
             Font.resolve("Fallback Cmap Sans", 400, :normal, registry)

    assert Map.has_key?(font.cmap, ?A)
  end

  test "rejects overlapping font segments and reads outside the declared subtable" do
    overlapping =
      <<4::16, 32::16, 0::16, 4::16, 0::16, 0::16, 0::16, 100::16, 110::16, 0::16, 65::16, 70::16,
        1::16, 1::16, 0::16, 0::16>>

    short_length =
      <<4::16, 16::16, binary_part(cmap_format4_table(65, 65, 1, 0), 4, 20)::binary>>

    for format4 <- [
          overlapping,
          short_length,
          cmap_format4_table(70, 65, 1, 0),
          cmap_format4_table(65, 65, 0, 1),
          cmap_format4_table(65, 65, 0, 1000),
          # The pointer reaches trailing cmap bytes but not the declared subtable.
          cmap_format4_table(65, 65, 0, 2) <> <<66::16>>
        ] do
      cmap = <<0::16, 1::16, 3::16, 1::16, 12::32, format4::binary>>
      data = valid_tables() |> Map.put("cmap", cmap) |> ttf_fixture()
      assert :error = Font.load_registry(fonts: [%{family: "Invalid Cmap", data: data}])

      assert {:error, {:invalid_document, diagnostic}} =
               HtmlToPdf.render("<p>A</p>", fonts: [%{family: "Invalid Cmap", data: data}])

      assert diagnostic.reason == :invalid_document
      assert diagnostic.operation == :render
      assert diagnostic.module == HtmlToPdf
      assert is_binary(diagnostic.message)
    end
  end

  test "bounds total font cmap preparation and deduplicates encoding references" do
    format4 = cmap_format4_table(65, 65, 1, 0)
    duplicated = <<0::16, 2::16, 0::16, 0::16, 20::32, 1::16, 0::16, 20::32, format4::binary>>

    Limits.install(%{Limits.effective() | max_font_cmap_work: 4})
    assert {:ok, [[{65, 65, 1, nil}]]} = FontValidator.prepare_cmap(duplicated)

    for budget <- [1, 2, 3] do
      Limits.install(%{Limits.effective() | max_font_cmap_work: budget})

      assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
               FontValidator.prepare_cmap(duplicated)
    end

    for cmap <- [:bad, <<>>, <<0::16, 0::16>>, <<0::16, 1::16>>] do
      assert :error = FontValidator.prepare_cmap(cmap)
    end

    # A skipped subtable still consumes its segment-inspection budget.
    malformed = cmap_format4_table(65, 65, 0, 1000)

    alternatives =
      <<0::16, 2::16, 3::16, 10::16, 20::32, 3::16, 1::16, 44::32, malformed::binary,
        format4::binary>>

    Limits.install(%{Limits.effective() | max_font_cmap_work: 4})

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             FontValidator.prepare_cmap(alternatives)

    Limits.install(%{Limits.effective() | max_font_cmap_work: 6})
    assert {:ok, [[{65, 65, 1, nil}]]} = FontValidator.prepare_cmap(alternatives)
  end

  test "font work limits return diagnostics through file and byte loading" do
    data = ttf_fixture(valid_tables())

    path =
      Path.join(System.tmp_dir!(), "font-work-limit-#{System.unique_integer([:positive])}.ttf")

    File.write!(path, data)
    on_exit(fn -> File.rm(path) end)
    Limits.install(%{Limits.effective() | max_font_cmap_work: 1})

    for source <- [%{data: data}, %{path: path}] do
      fonts = [Map.put(source, :family, "Bounded Font")]

      assert {:error, {:resource_limit_exceeded, diagnostic}} = Font.load_registry(fonts: fonts)
      assert diagnostic.stage == :limits
      assert diagnostic.module == Font
      assert diagnostic.operation == :load_registry
      assert diagnostic.message =~ "character-map work"

      assert {:error, {:resource_limit_exceeded, %{module: Font, operation: :load_registry}}} =
               HtmlToPdf.render("<p>A</p>", fonts: fonts)
    end
  end

  test "format-4 signed deltas wrap modulo 65536 in direct and glyph-array mappings" do
    direct = cmap_format4_table(?A, ?A, -25601, 0)

    array =
      <<4::16, 26::16, 0::16, 2::16, 0::16, 0::16, 0::16, 65::16, 0::16, 65::16,
        -25601::signed-16, 2::16, 65::16>>

    for format4 <- [direct, array] do
      cmap = <<0::16, 1::16, 3::16, 1::16, 12::32, format4::binary>>

      data =
        valid_tables()
        |> Map.put("cmap", cmap)
        |> Map.put("maxp", <<0::32, 40001::16>>)
        |> ttf_fixture()

      assert {:ok, registry} = Font.load_registry(fonts: [%{family: "Wrapped", data: data}])
      assert {:ok, _, font} = Font.resolve("Wrapped", 400, :normal, registry)
      assert font.cmap == %{?A => 40000}
      assert Font.text_width("A", font, 10) == 6.0
    end
  end

  test "uses validated glyph-array slices and preserves missing glyph entries" do
    format4 =
      <<4::16, 30::16, 0::16, 2::16, 0::16, 0::16, 0::16, 67::16, 0::16, 65::16, -1::signed-16,
        2::16, 0::16, 67::16, 68::16>>

    cmap = <<0::16, 1::16, 3::16, 1::16, 12::32, format4::binary>>
    data = valid_tables() |> Map.put("cmap", cmap) |> ttf_fixture()
    assert {:ok, registry} = Font.load_registry(fonts: [%{family: "Glyph Array", data: data}])
    assert {:ok, _, font} = Font.resolve("Glyph Array", 400, :normal, registry)
    assert font.cmap == %{66 => 66, 67 => 67}
  end

  test "load_registry rejects unsupported font config" do
    invalid_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-invalid-font.ttf")
    short_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-short-font.ttf")

    short_directory_path =
      Path.join(System.tmp_dir!(), "native-elixir-pdf-short-directory-font.ttf")

    missing_table_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-missing-table-font.ttf")
    short_head_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-short-head-font.ttf")
    bad_cmap_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-bad-cmap-font.ttf")
    bad_range_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-bad-range-font.ttf")

    File.write!(invalid_path, "not a font")
    File.write!(short_path, <<0, 1, 0>>)
    File.write!(short_directory_path, <<0, 1, 0, 0, 0>>)

    File.write!(missing_table_path, ttf_fixture(%{"head" => head_table()}))

    File.write!(
      short_head_path,
      ttf_fixture(valid_tables() |> Map.put("head", binary_part(head_table(), 0, 42)))
    )

    File.write!(bad_cmap_path, ttf_fixture(valid_tables() |> Map.put("cmap", bad_cmap_table())))

    File.write!(
      bad_range_path,
      ttf_fixture(valid_tables() |> Map.put("cmap", bad_range_cmap_table()))
    )

    assert Font.load_registry(fonts: [%{family: "Bad"}]) == :error
    assert Font.load_registry(fonts: :not_a_list) == :error
    assert Font.load_registry(fonts: ["bad"]) == :error
    assert Font.load_registry(fonts: [[:not_a_keyword]]) == :error
    assert Font.load_registry(:not_a_keyword_list) == :error

    assert Font.load_registry(fonts: [%{family: "Bad", path: __ENV__.file, weight: "950"}]) ==
             :error

    assert Font.load_registry(fonts: [%{family: "Bad", path: __ENV__.file, weight: "boldish"}]) ==
             :error

    assert Font.load_registry(fonts: [%{family: "Bad", path: __ENV__.file, style: "oblique"}]) ==
             :error

    assert Font.load_registry(fonts: [%{family: "Bad", path: invalid_path}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", path: short_path}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", path: short_directory_path}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", path: missing_table_path}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", path: short_head_path}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", path: bad_cmap_path}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", path: bad_range_path}]) == :error

    assert Font.load_registry(fonts: [%{family: "Bad", path: __ENV__.file, weight: 1_000}]) ==
             :error

    assert Font.resolve(:not_a_family, 400, :normal, %{embedded: []}) == :error
    assert Font.resolve(["Missing", 123], 400, :normal, %{embedded: []}) == :error
  after
    invalid_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-invalid-font.ttf")
    short_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-short-font.ttf")

    short_directory_path =
      Path.join(System.tmp_dir!(), "native-elixir-pdf-short-directory-font.ttf")

    missing_table_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-missing-table-font.ttf")
    short_head_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-short-head-font.ttf")
    bad_cmap_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-bad-cmap-font.ttf")
    bad_range_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-bad-range-font.ttf")

    File.rm(invalid_path)
    File.rm(short_path)
    File.rm(short_directory_path)
    File.rm(missing_table_path)
    File.rm(short_head_path)
    File.rm(bad_cmap_path)
    File.rm(bad_range_path)
  end

  test "load_registry and the HTML facade reject malformed zero-table fonts without crashing" do
    path = Path.join(System.tmp_dir!(), "native-elixir-pdf-zero-table-font.ttf")
    File.write!(path, <<0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>)

    assert Font.load_registry(fonts: [%{family: "Bad", path: path}]) == :error

    assert {:error,
            {:invalid_document, %{stage: :style, reason: :invalid_document, operation: :render}}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<p>hello</p>",
               fonts: [%{family: "Bad", path: path}]
             )
  after
    File.rm(Path.join(System.tmp_dir!(), "native-elixir-pdf-zero-table-font.ttf"))
  end

  test "rejects fonts whose OS/2 metadata prohibits outline embedding" do
    restricted_font =
      valid_tables()
      |> Map.put("OS/2", <<0::size(8 * 8), 0x0002::16>>)
      |> ttf_fixture()

    assert {:error,
            {:invalid_document,
             %{
               stage: :font,
               reason: :invalid_document,
               source: "Restricted Sans",
               message: "font \"Restricted Sans\" does not permit outline embedding in a PDF"
             }}} =
             Font.load_registry(fonts: [%{family: "Restricted Sans", data: restricted_font}])

    assert {:error,
            {:invalid_document,
             %{
               stage: :font,
               reason: :invalid_document,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               source: "Restricted Sans"
             }}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<p>hello</p>",
               fonts: [%{family: "Restricted Sans", data: restricted_font}]
             )
  end

  test "rejects variable fonts instead of using their default instance" do
    variable_font =
      valid_tables()
      |> Map.put("fvar", <<1::16, 0::16>>)
      |> ttf_fixture()

    assert {:error,
            {:invalid_document,
             %{
               stage: :font,
               reason: :invalid_document,
               source: "Variable Sans",
               message:
                 "font \"Variable Sans\" uses OpenType variations; provide a static font instance for PDF embedding"
             }}} =
             Font.load_registry(fonts: [%{family: "Variable Sans", data: variable_font}])

    assert {:error,
            {:invalid_document,
             %{
               stage: :font,
               reason: :invalid_document,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               source: "Variable Sans"
             }}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<p>hello</p>",
               fonts: [%{family: "Variable Sans", data: variable_font}]
             )
  end

  test "treats a font without an OS/2 table as installable" do
    font_without_os2 = valid_tables() |> ttf_fixture()

    assert {:ok, registry} =
             Font.load_registry(fonts: [%{family: "Legacy Sans", data: font_without_os2}])

    assert {:ok, ["Legacy Sans"],
            %{family: "Legacy Sans", embedding_flags: 0, variable_font?: false}} =
             Font.resolve(["Legacy Sans"], 400, :normal, registry)
  end

  defp ttf_font_path! do
    [
      Paths.project("priv/fonts/dejavu/DejaVuSans.ttf"),
      "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
      "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
      "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf"
    ]
    |> Enum.find(&File.exists?/1)
    |> case do
      nil -> flunk("No local TTF font fixture found")
      path -> path
    end
  end

  defp font_tables(data) do
    <<_::32, count::16, _::48, records::binary-size(count * 16), _::binary>> = data

    Map.new(
      for <<tag::binary-size(4), _::32, offset::32, length::32 <- records>>,
        do: {tag, binary_part(data, offset, length)}
    )
  end

  defp valid_tables do
    %{
      "head" => head_table(),
      "hhea" => hhea_table(),
      "maxp" => <<0::32, 2::16>>,
      "hmtx" => <<600::16, 0::16>>,
      "cmap" => valid_cmap_table()
    }
  end

  defp ttf_fixture(tables) do
    table_count = map_size(tables)
    table_start = 12 + table_count * 16

    {_offset, records, data} =
      tables
      |> Enum.sort_by(fn {tag, _data} -> tag end)
      |> Enum.reduce({table_start, [], ""}, fn {tag, table_data}, {offset, records, data} ->
        record = <<tag::binary-size(4), 0::32, offset::32, byte_size(table_data)::32>>
        {offset + byte_size(table_data), records ++ [record], data <> table_data}
      end)

    <<0x0001_0000::32, table_count::16, 0::16, 0::16, 0::16>> <> Enum.join(records, "") <> data
  end

  defp head_table do
    <<0::size(18 * 8), 1000::16, 0::size(16 * 8), 0::signed-16, 0::signed-16, 1000::signed-16,
      1000::signed-16>>
  end

  defp hhea_table do
    <<0::32, 800::signed-16, -200::signed-16, 0::size(26 * 8), 1::16>>
  end

  defp valid_cmap_table do
    format4 = cmap_format4_table(65, 65, 1, 0)
    <<0::16, 1::16, 3::16, 1::16, 12::32, format4::binary>>
  end

  defp bad_cmap_table do
    <<0::16, 1::16, 3::16, 1::16, 12::32, 0::16>>
  end

  defp bad_range_cmap_table do
    format4 = cmap_format4_table(65, 65, 0, 1000)
    <<0::16, 1::16, 3::16, 1::16, 12::32, format4::binary>>
  end

  defp cmap_format12_table(groups) do
    records =
      for {first, last, glyph} <- groups, into: <<>>, do: <<first::32, last::32, glyph::32>>

    <<12::16, 0::16, 16 + byte_size(records)::32, 0::32, length(groups)::32, records::binary>>
  end

  defp cmap_format4_table(start_code, end_code, id_delta, range_offset) do
    seg_count = 1
    length = 24

    <<4::16, length::16, 0::16, seg_count * 2::16, 0::16, 0::16, 0::16, end_code::16, 0::16,
      start_code::16, id_delta::signed-16, range_offset::16>>
  end

  defp gsub_table(lookups, features \\ [{"ssty", [0]}], opts \\ []) do
    indices =
      for index <- Enum.with_index(features) |> Enum.map(&elem(&1, 1)),
          into: <<>>,
          do: <<index::16>>

    language =
      if Keyword.get(opts, :default, true),
        do: <<0::16, 0xFFFF::16, length(features)::16, indices::binary>>,
        else: <<>>

    script = Keyword.get(opts, :script, "math")
    default = if language == <<>>, do: 0, else: 4

    scripts =
      <<1::16, script::binary-size(4), Keyword.get(opts, :script_offset, 8)::16, default::16,
        0::16, language::binary>>

    {_, records, bodies} =
      Enum.reduce(features, {2 + length(features) * 6, <<>>, <<>>}, fn {tag, indices},
                                                                       {offset, records, bodies} ->
        indices_data = for index <- indices, into: <<>>, do: <<index::16>>
        body = <<0::16, length(indices)::16, indices_data::binary>>

        {offset + byte_size(body),
         records <> <<tag::binary-size(4), Keyword.get(opts, :feature_offset, offset)::16>>,
         bodies <> body}
      end)

    features_data = <<length(features)::16, records::binary, bodies::binary>>

    {_, offsets, bodies} =
      Enum.reduce(lookups, {2 + length(lookups) * 2, <<>>, <<>>}, fn body,
                                                                     {offset, offsets, bodies} ->
        {offset + byte_size(body), offsets <> <<offset::16>>, bodies <> body}
      end)

    lookup_data = <<length(lookups)::16, offsets::binary, bodies::binary>>

    <<1::16, 0::16, 10::16, 10 + byte_size(scripts)::16,
      10 + byte_size(scripts) + byte_size(features_data)::16, scripts::binary,
      features_data::binary, lookup_data::binary>>
  end

  defp default_constants do
    <<80::signed-16, 60::signed-16, 1200::16, 1800::16, 0::size(204 * 8), 65::signed-16>>
  end

  defp math_table(constants \\ default_constants(), info \\ <<0::64>>, variants \\ <<0::80>>) do
    <<1::16, 0::16, 10::16, 10 + byte_size(constants)::16,
      10 + byte_size(constants) + byte_size(info)::16, constants::binary, info::binary,
      variants::binary>>
  end

  # Negative caller fonts are parsed from their actual repacked byte programs.
  # They never replace original browser fonts or emit positive paint receipts.
  defp math_variation_font(original, glyph, field) do
    device = <<0::16, 1::16, 0x8000::16>>

    info =
      if field == :kern do
        <<0::16, 0::16, 0::16, 8::16, 12::16, 1::16, 18::16, 0::16, 18::16, 0::16, 1::16, 1::16,
          glyph::16, 1::16, 0::signed-16, 14::16, 0::signed-16, 0::16, 0::signed-16, 0::16,
          device::binary>>
      else
        <<8::16, 0::16, 0::16, 0::16, 8::16, 1::16, 0::signed-16, 14::16, 1::16, 1::16, glyph::16,
          device::binary>>
      end

    data =
      original.data
      |> font_tables()
      |> Map.put("MATH", math_table(default_constants(), info))
      |> ttf_fixture()

    assert {:ok, [font]} =
             Font.load_byte_faces([
               %{family: "Unsupported MATH variation", data: data, weight: 400, style: :normal}
             ])

    assert font.data == data and font.data != original.data and font.id != original.id
    font
  end

  defp math_construction_font(font, axis, variants, parts, opts \\ []) do
    <<_::32, count::16, _::48, records::binary-size(count * 16), _::binary>> = font.data

    tables =
      Map.new(
        for <<tag::binary-size(4), _::32, offset::32, length::32 <- records>>,
          do: {tag, binary_part(font.data, offset, length)}
      )

    variant_data = for {id, advance} <- variants, into: <<>>, do: <<id::16, advance::16>>

    part_data =
      for {id, start, finish, advance, flags} <- parts,
          into: <<>>,
          do: <<id::16, start::16, finish::16, advance::16, flags::16>>

    {italic, device} = Keyword.get(opts, :italic, {0, <<>>})
    device_offset = if device == <<>>, do: 0, else: 6 + byte_size(part_data)

    assembly =
      <<italic::signed-16, device_offset::16, length(parts)::16, part_data::binary,
        device::binary>>

    assembly_offset = if parts == [], do: 0, else: 4 + byte_size(variant_data)

    construction =
      <<assembly_offset::16, length(variants)::16, variant_data::binary>> <>
        if(parts == [], do: <<>>, else: assembly)

    {vertical, horizontal, vertical_count, horizontal_count} =
      if axis == :vertical, do: {12, 0, 1, 0}, else: {0, 12, 0, 1}

    data =
      <<20::16, vertical::16, horizontal::16, vertical_count::16, horizontal_count::16, 18::16,
        1::16, 1::16, 1::16, construction::binary>>

    %{
      font
      | data:
          tables
          |> Map.put("MATH", math_table(default_constants(), <<0::64>>, data))
          |> ttf_fixture(),
        cmap: Map.put(font.cmap, ?A, 1)
    }
  end

  defp glyph_info do
    italics =
      <<8::16, 1::16, -12::signed-16, 14::16, 1::16, 1::16, 1::16, 10::16, 12::16, 1::16,
        0xB400::16>>

    accents = <<8::16, 1::16, 333::signed-16, 0::16, 1::16, 1::16, 2::16>>
    extended = <<2::16, 1::16, 1::16, 2::16, 0::16>>

    kerns =
      <<12::16, 1::16, 18::16, 0::16, 18::16, 18::16, 1::16, 1::16, 3::16, 2::16, -5::signed-16,
        0::16, 5::signed-16, 0::16, -2::signed-16, 0::16, 0::signed-16, 0::16, 3::signed-16,
        0::16>>

    <<8::16, 30::16, 44::16, 54::16, italics::binary, accents::binary, extended::binary,
      kerns::binary>>
  end

  defp variants do
    <<20::16, 14::16, 20::16, 1::16, 1::16, 26::16, 64::16, 1::16, 1::16, 1::16, 1::16, 1::16,
      2::16, 12::16, 2::16, 4::16, 500::16, 5::16, 1000::16, 15::signed-16, 0::16, 2::16, 6::16,
      0::16, 100::16, 300::16, 0::16, 7::16, 100::16, 100::16, 200::16, 1::16, 0::16, 1::16,
      7::16, 400::16>>
  end

  defp replace_bytes(data, offset, replacement) do
    length = byte_size(replacement)

    binary_part(data, 0, offset) <>
      replacement <>
      binary_part(data, offset + length, byte_size(data) - offset - length)
  end
end
