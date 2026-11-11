defmodule NativeElixirPdfUtilities.HtmlToPdf.CffFontStructureTest do
  use ExUnit.Case, async: false

  import NativeElixirPdfUtilities.TestSupport.CffFixture

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.TestSupport.Paths
  alias NativeElixirPdfUtilities.Validators.{CffFontValidator, FontValidator}

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    :ok
  end

  test "the licensed CFF corpus has independently measured bounds, metrics and raw table identity" do
    data = File.read!(Paths.fixture("fonts/cff-static-sans.cff"))
    expected = Paths.fixture("fonts/cff-static-sans.json") |> File.read!() |> :json.decode()
    assert Base.encode16(:crypto.hash(:sha256, data), case: :lower) == expected["cff_sha256"]
    assert {:ok, face} = CffFontValidator.prepare(data, length(expected["glyph_order"]))
    assert face.name == "StaticCffSans"
    assert tuple_size(face.charstrings) == 5
    assert face.global_subrs == {} and face.fd_select == {0, 0, 0, 0, 0}

    assert face.font_matrix == [
             1 / expected["units_per_em"],
             0,
             0,
             1 / expected["units_per_em"],
             0,
             0
           ]

    bounds =
      expected["glyphs"]
      |> Map.values()
      |> Enum.map(& &1["bounds"])
      |> Enum.reject(&(&1 == [0, 0, 0, 0]))

    assert face.bbox ==
             {Enum.min(Enum.map(bounds, &Enum.at(&1, 0))),
              Enum.min(Enum.map(bounds, &Enum.at(&1, 1))),
              Enum.max(Enum.map(bounds, &Enum.at(&1, 2))),
              Enum.max(Enum.map(bounds, &Enum.at(&1, 3)))}

    assert elem(face.fonts, 0).local_subrs == {}
    assert elem(face.fonts, 0).default_width == 0
    refute face.cid?

    assert {:ok, [loaded]} =
             Font.load_configured_faces([
               %{
                 family: "Static CFF",
                 data: File.read!(Paths.fixture("fonts/cff-static-sans.otf"))
               }
             ])

    assert loaded.outline_format == :cff
  end

  test "INDEX supports all offset widths, empty objects and nonzero source offsets" do
    for width <- 1..4 do
      bytes = index(["a", "", "bc"], width)

      assert {:ok, {"a", "", "bc"}, finish} =
               CffFontValidator.prepare_index("prefix" <> bytes <> "tail", 6)

      assert finish == byte_size(bytes) + 6
    end

    assert {:ok, {}, 2} = CffFontValidator.prepare_index(<<0, 0, 255>>, 0)

    for {data, offset} <- [
          {nil, 0},
          {<<>>, 0},
          {<<0>>, 0},
          {<<0, 0>>, -1},
          {<<0, 0>>, 0.5},
          {<<0, 1, 0>>, 0},
          {<<0, 1, 5>>, 0},
          {<<0, 1, 1, 0, 0>>, 0},
          {<<0, 2, 1, 1, 3, 2, 0>>, 0},
          {<<0, 1, 1, 1, 5, 0>>, 0}
        ] do
      assert {:error, {:invalid_font, diagnostic}} = CffFontValidator.prepare_index(data, offset)
      assert diagnostic.stage == :font and diagnostic.operation == :prepare_cff_source
      assert is_binary(diagnostic.message) and is_binary(diagnostic.source)
    end
  end

  test "DICT decodes signed integer and real representations with a fixed operand stack" do
    encoded =
      <<32, 246, 247, 0, 250, 255, 251, 0, 254, 255, 28, -32000::signed-16, 29,
        -2_000_000_000::signed-32, 5>>

    assert {:ok, %{5 => [-107, 107, 108, 1131, -108, -1131, -32000, -2_000_000_000]}} =
             CffFontValidator.prepare_dictionary(encoded)

    for {encoded, expected} <- [
          {<<30, 0xE2, 0xA2, 0x5F>>, -2.25},
          {<<30, 0x0A, 0x14, 0x05, 0x41, 0xC3, 0xFF>>, 0.000140541},
          {<<30, 0x1A, 0xFF>>, 1.0},
          {<<30, 0x1A, 0xB2, 0xFF>>, 100.0}
        ] do
      assert {:ok, %{{12, 7} => [value]}} =
               CffFontValidator.prepare_dictionary(encoded <> <<12, 7>>)

      assert_in_delta value, expected, 1.0e-12
    end

    assert {:ok, %{}} = CffFontValidator.prepare_dictionary(<<>>)
    assert {:ok, %{0 => [2]}} = CffFontValidator.prepare_dictionary(<<140, 0, 141, 0>>)

    assert {:ok, %{5 => values}} =
             CffFontValidator.prepare_dictionary(:binary.copy(<<139>>, 48) <> <<5>>)

    assert length(values) == 48

    for bytes <- [
          nil,
          <<12>>,
          <<139>>,
          <<255>>,
          <<247>>,
          <<251>>,
          <<28>>,
          <<29, 0>>,
          <<30>>,
          <<30, 0xD0>>,
          <<30, 0x0D>>,
          <<30, 0xF0>>,
          <<30, 0xFF>>,
          <<30, 0xAA, 0xFF>>,
          :binary.copy(<<139>>, 49) <> <<5>>
        ] do
      assert {:error, {:invalid_font, %{operation: :prepare_cff_source}}} =
               CffFontValidator.prepare_dictionary(bytes)
    end
  end

  test "private dictionaries and shared local/global subroutines retain their owners" do
    bytes = face_fixture()
    assert {:ok, face} = CffFontValidator.prepare(bytes, 3)
    assert face.charset == [1, 2]
    assert face.encoding == {:predefined, 0}
    assert face.fd_select == {0, 0, 0}
    assert face.global_subrs == {<<11>>}
    assert {font} = face.fonts
    assert font.local_subrs == {<<11>>}
    assert font.default_width == 600 and font.nominal_width == 100
    assert is_nil(font.font_matrix)
    assert {:ok, face} = CffFontValidator.prepare(face_fixture(omit_private: true), 3)
    assert elem(face.fonts, 0).private == %{} and elem(face.fonts, 0).local_subrs == {}
    assert {:ok, face} = CffFontValidator.prepare(face_fixture(local_subrs: nil), 3)
    assert elem(face.fonts, 0).local_subrs == {}
    assert {:ok, face} = CffFontValidator.prepare(face_fixture(top: %{18 => [0, 0]}), 3)
    assert elem(face.fonts, 0).private == %{}

    for extra <- [%{18 => [-1, 0]}, %{18 => [1]}, %{18 => [10, 0]}, %{18 => [1, 1_000_000]}] do
      assert {:error, {:invalid_font, _}} = CffFontValidator.prepare(face_fixture(top: extra), 3)
    end

    for private <- [%{19 => [0]}, %{19 => [1, 2]}, %{20 => [1, 2]}, %{21 => [1, 2]}] do
      assert {:error, {:invalid_font, _}} =
               CffFontValidator.prepare(face_fixture(private: private), 3)
    end
  end

  test "CID fonts bind each glyph to its private dictionary with both FDSelect formats" do
    for selector <- [<<0, 0, 0, 1>>, <<3, 2::16, 0::16, 0, 2::16, 1, 3::16>>] do
      assert {:ok, face} =
               CffFontValidator.prepare(
                 face_fixture(cid: true, font_count: 2, fd_select: selector),
                 3
               )

      assert face.cid? and is_nil(face.encoding)
      assert face.fd_select == {0, 0, 1}
      assert tuple_size(face.fonts) == 2
      assert elem(face.fonts, 0).local_subrs == elem(face.fonts, 1).local_subrs
      assert Enum.count(face.owners, &(elem(&1, 0) == :local_subrs)) == 1
    end

    assert {:ok, face} =
             CffFontValidator.prepare(face_fixture(cid: true, fd_matrix: [1, 0, 0, 1, 0, 0]), 3)

    assert elem(face.fonts, 0).font_matrix == [1, 0, 0, 1, 0, 0]

    for options <- [
          [font_count: 0],
          [font_count: 257],
          [fd_select: <<1>>],
          [fd_select: <<0, 0, 2, 0>>],
          [fd_select: <<3, 0::16>>],
          [fd_select: <<3, 1::16, 1::16, 0, 3::16>>],
          [fd_select: <<3, 1::16, 0::16, 0, 2::16>>],
          [fd_select: <<3, 2::16, 0::16, 0, 0::16, 0, 3::16>>],
          [top: %{{12, 30} => [393, 392, 0]}],
          [top: %{{12, 30} => [391, 392]}],
          [top: %{{12, 30} => [391, 392, -1]}],
          [top: %{{12, 34} => [0]}],
          [top: %{{12, 33} => [1]}],
          [top: %{{12, 34} => [1]}],
          [top: %{{12, 34} => [-1]}],
          [fd_name_sid: 999],
          [top: %{15 => [0]}],
          [top: %{16 => [0]}],
          [fd_matrix: [1, 0]]
        ] do
      assert {:error, {:invalid_font, diagnostic}} =
               CffFontValidator.prepare(face_fixture([cid: true] ++ options), 3)

      assert diagnostic.stage == :font and diagnostic.reason == :invalid_font
    end
  end

  test "charset ranges and predefined repertoire sizes are checked without losing glyph order" do
    for {bytes, expected} <- [
          {<<1, 1::16, 1>>, [1, 2]},
          {<<2, 1::16, 1::16>>, [1, 2]},
          {<<1, 2::16, 0, 1::16, 0>>, [2, 1]}
        ] do
      assert {:ok, face} = CffFontValidator.prepare(face_fixture(charset: bytes), 3)
      assert face.charset == expected
    end

    for code <- 0..2 do
      assert {:ok, face} = CffFontValidator.prepare(face_fixture(charset: {:predefined, code}), 3)

      expected =
        NativeElixirPdfUtilities.HtmlToPdf.Font.CffData.predefined_charsets()
        |> elem(code)
        |> Tuple.to_list()
        |> Enum.slice(1, 2)

      assert face.charset == expected
    end

    for bytes <- [
          <<3>>,
          <<0, 1::16, 1::16>>,
          <<0, 0::16, 2::16>>,
          <<0, 391::16, 2::16>>,
          <<1, 1::16, 2>>,
          <<2, 65_535::16, 1::16>>
        ] do
      assert {:error, {:invalid_font, _}} =
               CffFontValidator.prepare(face_fixture(charset: bytes), 3)
    end

    for {code, count} <- [{0, 230}, {1, 167}, {2, 88}] do
      assert {:error, {:invalid_font, %{source: "CFF charset"}}} =
               CffFontValidator.prepare(
                 face_fixture(charset: {:predefined, code}, glyph_count: count),
                 count
               )
    end
  end

  test "custom encodings admit only unique byte codes within the glyph count" do
    for {encoding, expected, supplements} <- [
          {<<0, 2, 65, 66>>, [65, 66], []},
          {<<1, 1, 65, 1>>, [65, 66], []},
          {<<0x80, 1, 65, 1, 66, 1::16>>, [65], [{66, 1}]}
        ] do
      assert {:ok, face} = CffFontValidator.prepare(face_fixture(encoding: encoding), 3)
      assert face.encoding == %{codes: expected, supplements: supplements}
    end

    assert {:ok, %{encoding: {:predefined, 1}}} =
             CffFontValidator.prepare(face_fixture(encoding: {:predefined, 1}), 3)

    for bytes <- [
          <<2, 0>>,
          <<0, 3, 65, 66, 67>>,
          <<0, 2, 65, 65>>,
          <<1, 1, 255, 1>>,
          <<0x80, 1, 65, 1, 66, 2::16>>,
          <<0x80, 0, 2, 65, 1::16, 65, 2::16>>
        ] do
      assert {:error, {:invalid_font, %{source: "CFF Encoding"}}} =
               CffFontValidator.prepare(face_fixture(encoding: bytes), 3)
    end
  end

  test "malformed source headers, font sets, operator operands and overlapping ranges have shared diagnostics" do
    original = face_fixture()
    assert {:ok, _} = CffFontValidator.prepare(face_fixture(top: %{0 => [0]}), 3)
    assert elem(NativeElixirPdfUtilities.HtmlToPdf.Font.CffData.standard_strings(), 1) == "space"
    assert elem(NativeElixirPdfUtilities.HtmlToPdf.Font.CffData.standard_encoding(), 65) == 34

    for {data, glyph_count} <- [
          {original, 0},
          {original, 65_536},
          {original, :bad},
          {<<>>, 3},
          {<<2, 0, 4, 1, 0, 0>>, 3},
          {<<1, 0, 3, 1, 0, 0>>, 3},
          {face_fixture(names: []), 3},
          {face_fixture(names: ["One", "Two"]), 3},
          {face_fixture(names: [""]), 3},
          {face_fixture(names: ["a b"]), 3},
          {face_fixture(names: [String.duplicate("A", 128)]), 3},
          {face_fixture(top: %{0 => [999]}), 3},
          {face_fixture(top: %{{12, 6} => [1]}), 3},
          {face_fixture(top: %{{12, 20} => [0]}), 3},
          {face_fixture(top: %{{12, 5} => [2]}), 3},
          {face_fixture(top: %{{12, 7} => [1]}), 3},
          {face_fixture(top: %{5 => [10, 0, 0, 10]}), 3},
          {face_fixture(top: %{17 => [4]}, glyph_count: 1), 1},
          {face_fixture(top: %{17 => []}), 3},
          {face_fixture(top: %{15 => [-1]}), 3},
          {original, 2}
        ] do
      assert {:error, {:invalid_font, diagnostic}} = CffFontValidator.prepare(data, glyph_count)
      assert %{stage: :font, reason: :invalid_font, message: message, source: source} = diagnostic
      assert is_binary(message) and is_binary(source)
      assert diagnostic.module == CffFontValidator and diagnostic.operation == :prepare_cff_source
    end

    for length <- 0..(byte_size(original) - 1) do
      assert {:error, {:invalid_font, _}} =
               CffFontValidator.prepare(binary_part(original, 0, length), 3)
    end
  end

  test "CFF work is bounded before indexing, decimal scanning or namespace expansion" do
    Limits.install(%{Limits.effective() | max_font_cff_work: 1})

    for run <- [
          fn -> CffFontValidator.prepare_index(index(["a"]), 0) end,
          fn -> CffFontValidator.prepare_dictionary(<<30, 0x1A, 0xFF, 5>>) end,
          fn -> CffFontValidator.prepare(face_fixture(), 3) end
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} = run.()
      assert diagnostic.message =~ "max_font_cff_work" and diagnostic.stage == :font
    end

    Limits.install(Limits.defaults())

    assert {:error, {:invalid_font, _}} =
             CffFontValidator.prepare(face_fixture(strings: List.duplicate("", 65_146)), 3)

    assert {:error, {:resource_limit_exceeded, _}} =
             FontValidator.with_budget(fn ->
               Limits.install(%{Limits.effective() | max_font_source_bytes: 1})
               CffFontValidator.prepare_dictionary(<<139, 5>>)
             end)
  end
end
