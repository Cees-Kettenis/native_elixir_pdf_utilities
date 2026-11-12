defmodule NativeElixirPdfUtilities.HtmlToPdf.VariableFontTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.FontCorpus
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.Sfnt
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.TestSupport.Paths

  alias NativeElixirPdfUtilities.Validators.{
    CffCharstringValidator,
    FontValidator,
    FontVariationValidator,
    TrueTypeSubsetValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "TrueType endpoint and interior instances preserve independent points, metrics, line metrics and PDF programs" do
    input = FontCorpus.path("selected-variable.ttf") |> File.read!()
    manifest = Paths.fixture("fonts/variable-fonts.json") |> File.read!() |> :json.decode()

    for expected <- manifest["instances"] do
      assert {:ok, [font]} =
               Font.load_configured_faces([
                 %{family: "Variable", data: input, variation_settings: expected["coordinates"]}
               ])

      refute font.variable_font?
      assert font.variation_source == input
      assert font.ascent == expected["ascent"]
      assert {:ok, xheight} = FontValidator.prepare_x_height(font)

      assert xheight ==
               List.last(expected["glyphs"][Integer.to_string(font.cmap[?x])]["bounds"]) /
                 font.units_per_em

      assert {:ok, _, directory} = FontValidator.prepare_table_directory(font.data)
      {offset, _length} = directory["OS/2"]
      assert <<expected["x_height"]::signed-16>> == binary_part(font.data, offset + 86, 2)
      assert {:ok, outlines} = TrueTypeSubsetValidator.prepare_outlines(font.data)

      for {id, glyph} <- expected["glyphs"] do
        id = String.to_integer(id)
        actual = elem(outlines.glyphs, id)
        assert Tuple.to_list(actual.bbox) == glyph["bounds"]
        assert Enum.at(font.widths, id) == hd(glyph["metrics"])

        if actual.contours >= 0 do
          assert Enum.map(actual.points, fn {x, y, _} -> [x, y] end) == glyph["points"]
          assert actual.ends == glyph["ends"]
        end
      end

      assert Sfnt.checksum(font.data) == 0xB1B0_AFBA

      assert {:ok, pdf} =
               HtmlToPdf.render("<p style='font-family:Variable;font-size:24pt'>Hx0É</p>",
                 fonts: [
                   %{family: "Variable", data: input, variation_settings: expected["coordinates"]}
                 ],
                 system_font_discovery: false
               )

      assert {:ok, "Hx0É"} = Text.extract(pdf, layout: false)
      assert {:ok, document} = Reader.read(pdf)

      programs =
        for {_ref, %{value: %{"FontFile2" => {:ref, ref}}}} <- document.objects do
          assert {:ok, program} = Reader.decoded_stream(document, {:ref, ref})
          assert {:ok, embedded} = TrueTypeSubsetValidator.prepare_outlines(program)

          for code <- [?H, ?x, ?0, 0xC9] do
            gid = font.cmap[code]
            assert elem(embedded.glyphs, gid) == elem(outlines.glyphs, gid)
          end

          program
        end

      assert length(programs) == 1
    end
  end

  test "CFF2 blend evaluation preserves independent fractional paths, widths and extracted Unicode in CID CFF output" do
    input = FontCorpus.path("selected-variable-cff2.otf") |> File.read!()
    manifest = Paths.fixture("fonts/variable-cff2-fonts.json") |> File.read!() |> :json.decode()

    for expected <- manifest["instances"] do
      assert {:ok, [font]} =
               Font.load_configured_faces([
                 %{family: "CFF2", data: input, variation_settings: expected["coordinates"]}
               ])

      assert font.outline_format == :cff
      refute font.variable_font?

      for {id, glyph} <- expected["glyphs"] do
        id = String.to_integer(id)
        assert_close(reference_commands(elem(font.cff.glyphs, id).commands), glyph["commands"])
        assert Enum.at(font.widths, id) == hd(glyph["metrics"])
      end

      assert {:ok, pdf} =
               HtmlToPdf.render("<p style='font-family:CFF2;font-size:24pt'>Hx0É</p>",
                 fonts: [
                   %{family: "CFF2", data: input, variation_settings: expected["coordinates"]}
                 ],
                 system_font_discovery: false
               )

      assert {:ok, "Hx0É"} = Text.extract(pdf, layout: false)
      assert {:ok, document} = Reader.read(pdf)

      programs =
        for {_ref, %{value: %{"FontFile3" => {:ref, ref}}}} <- document.objects do
          assert {:ok, program} = Reader.decoded_stream(document, {:ref, ref})
          assert {:ok, face} = CffCharstringValidator.prepare(program, 5)

          for {gid, index} <- Enum.with_index([0 | face.charset]) do
            glyph = expected["glyphs"][Integer.to_string(gid)]
            assert_close(reference_commands(elem(face.glyphs, index).commands), glyph["commands"])
          end

          program
        end

      assert length(programs) == 1
    end
  end

  test "named instances, clamping, source retention and explicit coordinates share one selection contract" do
    data = FontCorpus.path("selected-variable.ttf") |> File.read!()
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Variable", data: data}])
    assert {:ok, heavy} = Font.instance(font, %{}, 1)
    assert heavy.variation_coordinates == %{"wght" => 900, "wdth" => 100}
    assert {:ok, explicit} = Font.instance(heavy, %{"wght" => 650, "wdth" => 1000}, 0)
    assert explicit.variation_coordinates == %{"wght" => 650, "wdth" => 125}
    assert explicit.variation_source == data
    assert explicit.id != heavy.id
    assert {:ok, same} = Font.instance(font, %{"wght" => 900})
    assert same.data == heavy.data
    assert {:ok, default} = Font.instance(font, %{"zzzz" => 500})
    assert default.data == font.data
    assert {:error, {:invalid_font, %{message: message}}} = Font.instance(font, %{}, 99)
    assert message =~ "named-instance"
    assert {:error, {:invalid_font, _}} = Font.instance(%{}, %{})

    for settings <- [nil, %{"longtag" => 1}, %{"wght" => "900"}, ["invalid"]] do
      assert {:error, {:invalid_font, _}} = FontVariationValidator.prepare(data, settings)
    end

    assert {:error, {:invalid_font, _}} = FontVariationValidator.prepare(data, %{}, -1)
  end

  test "variation interpolation and reconstruction obey cumulative font work and byte allowances" do
    data = FontCorpus.path("selected-variable.ttf") |> File.read!()

    for limit <- [:max_font_variation_work, :max_font_decoded_bytes] do
      limits = Limits.effective()
      Limits.install(Map.put(limits, limit, 1))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               FontVariationValidator.prepare(data)

      assert diagnostic.message =~ Atom.to_string(limit)
      Limits.install(limits)
    end
  end

  test "gvar deltas leave point-attached composite indices unchanged" do
    component = %{xy?: false, args: {0, 1}}
    glyph = %{contours: -1, components: [component], ends: []}
    payload = <<1::16, 10::16, 7::16, 0x8000::16, 16384::signed-16, 4, 50, 0, 0, 0, 0, 0x84>>

    data =
      <<1::16, 0::16, 1::16, 0::16, 28::32, 1::16, 1::16, 28::32, 0::32, byte_size(payload)::32,
        payload::binary>>

    assert {:ok, [%{glyph: selected}]} =
             NativeElixirPdfUtilities.Validators.GvarValidator.prepare(
               data,
               {glyph},
               [{0, 600, 800, -200}],
               [1]
             )

    assert selected.components == [component]
    glyph = %{glyph | components: [%{component | xy?: true}]}

    assert {:ok, [%{glyph: selected}]} =
             NativeElixirPdfUtilities.Validators.GvarValidator.prepare(
               data,
               {glyph},
               [{0, 600, 800, -200}],
               [1]
             )

    assert hd(selected.components).args == {50, 1}
  end

  defp reference_commands(commands) do
    Enum.map(commands, fn command ->
      case command do
        :close -> ["closePath", []]
        {:move, x, y} -> ["moveTo", [[x, y]]]
        {:line, x, y} -> ["lineTo", [[x, y]]]
        {:curve, a, b, c, d, e, f} -> ["curveTo", [[a, b], [c, d], [e, f]]]
      end
    end)
  end

  defp assert_close(actual, expected) do
    case {actual, expected} do
      {a, b} when is_number(a) and is_number(b) ->
        assert_in_delta a, b, 1 / 65536

      {a, b} when is_list(a) and is_list(b) ->
        assert length(a) == length(b)
        Enum.zip(a, b) |> Enum.each(fn {x, y} -> assert_close(x, y) end)

      {a, b} ->
        assert a == b
    end
  end
end
