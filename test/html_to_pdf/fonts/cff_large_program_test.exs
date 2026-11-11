defmodule NativeElixirPdfUtilities.HtmlToPdf.CffLargeProgramTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.TestSupport.Paths
  alias NativeElixirPdfUtilities.Validators.{CffFontValidator, CffCharstringValidator}

  test "large CFF2 and composed legacy outlines use bounded subroutines and preserve independent bounds and CID Unicode" do
    manifest = Paths.fixture("fonts/cff-large-fonts.json") |> File.read!() |> :json.decode()

    for {file, expected} <- manifest["files"] do
      data = Paths.fixture("fonts/" <> file) |> File.read!()
      config = %{family: "Large", data: data}

      config =
        if expected["cff2"],
          do: Map.put(config, :variation_settings, %{"wght" => 900}),
          else: config

      assert {:ok, [font]} = Font.load_configured_faces([config])
      assert {:ok, bounds} = Font.glyph_bounds(font)

      for {_name, glyph} <- expected["glyphs"] do
        for {actual, independent} <-
              Enum.zip(Tuple.to_list(elem(bounds, glyph["id"])), glyph["bounds"]),
            do: assert_in_delta(actual, independent, 1.0e-8)
      end

      assert {:ok, pdf} =
               HtmlToPdf.render("<p style='font-family:Large'>H0</p>",
                 fonts: [config],
                 system_font_discovery: false
               )

      assert {:ok, "H0"} = Text.extract(pdf, layout: false)
      assert {:ok, document} = Reader.read(pdf)

      programs =
        for {_ref, %{value: %{"FontFile3" => {:ref, ref}}}} <- document.objects do
          assert {:ok, program} = Reader.decoded_stream(document, {:ref, ref})
          assert {:ok, face} = CffFontValidator.prepare(program, 3)
          assert tuple_size(face.global_subrs) > 0
          assert Enum.all?(Tuple.to_list(face.global_subrs), &(byte_size(&1) <= 65535))
          assert Enum.all?(Tuple.to_list(face.charstrings), &(byte_size(&1) <= 65535))
          assert {:ok, evaluated} = CffCharstringValidator.prepare(program, 3)

          for {_name, glyph} <- expected["glyphs"] do
            index = Enum.find_index([0 | face.charset], &(&1 == glyph["id"]))

            for {actual, independent} <-
                  Enum.zip(Tuple.to_list(elem(evaluated.glyphs, index).bbox), glyph["bounds"]),
                do: assert_in_delta(actual, independent, 1.0e-8)
          end

          program
        end

      assert length(programs) == 1
    end
  end

  test "a seeded nonempty random program is frozen once and preserves its actual PDF outline" do
    manifest = Paths.fixture("fonts/cff-large-fonts.json") |> File.read!() |> :json.decode()
    expected = manifest["random"]
    data = Paths.fixture("fonts/" <> expected["file"]) |> File.read!()
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Random", data: data}])
    glyph = elem(font.cff.glyphs, font.cmap[?H])
    assert glyph.static?

    for {actual, independent} <- Enum.zip(Tuple.to_list(glyph.bbox), expected["bounds"]),
        do: assert_in_delta(actual, independent, 1.0e-9)

    assert {:ok, pdf} =
             HtmlToPdf.render("<p style='font-family:Random'>H</p>",
               fonts: [%{family: "Random", data: data}],
               system_font_discovery: false
             )

    assert {:ok, "H"} = Text.extract(pdf, layout: false)
    assert {:ok, document} = Reader.read(pdf)

    programs =
      for {_ref, %{value: %{"FontFile3" => {:ref, ref}}}} <- document.objects do
        assert {:ok, program} = Reader.decoded_stream(document, {:ref, ref})
        assert {:ok, face} = CffCharstringValidator.prepare(program, 2)
        assert elem(face.glyphs, 1).commands == glyph.commands
        refute elem(face.glyphs, 1).static?
        program
      end

    assert length(programs) == 1
  end

  test "expanded programs retain arithmetic operands and fixed CFF 1 program capacities" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Font.Cff
    alias NativeElixirPdfUtilities.Validators.{FontValidator, CffSubsetValidator}

    assert {:ok, face} =
             FontValidator.with_budget(fn ->
               commands = [{:move, 0, 0} | List.duplicate({:line, 100, 100}, 24000)] ++ [:close]
               program = Cff.program(%{width: 40000, commands: commands}, 0)

               Cff.static_face([program], 1000, {0, 0, 100, 100})
               |> CffCharstringValidator.prepare(1)
             end)

    assert elem(face.glyphs, 0).width == 40000

    assert {:error, {:unsupported_font, _}} =
             FontValidator.with_budget(fn ->
               CffSubsetValidator.validate_static_programs([:binary.copy(<<0>>, 65536)], [])
             end)

    limits = NativeElixirPdfUtilities.Limits.effective()
    on_exit(fn -> NativeElixirPdfUtilities.Limits.install(limits) end)

    NativeElixirPdfUtilities.Limits.install(
      Map.put(limits, :max_font_outline_coordinate, Integer.pow(10, 310))
    )

    assert {:error, {:invalid_font, %{message: message}}} =
             CffCharstringValidator.prepare_command_bounds(
               [{:move, 0, 0}, {:curve, 1.0e308, 1.0e308, 0, 0, 1.0e308, 1.0e308}],
               "huge cubic"
             )

    assert message =~ "finite"
  end
end
