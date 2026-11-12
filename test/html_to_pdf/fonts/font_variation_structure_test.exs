Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("font_fixture.ex"))
Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("cff_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.FontVariationStructureTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.FontCorpus
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.TestSupport.{FontFixture, CffFixture}
  alias NativeElixirPdfUtilities.Validators.FontVariationValidator

  test "fvar ranges, names, unique tags, record sizes and missing-instance requests share diagnostics" do
    data = File.read!(FontCorpus.path("selected-variable.ttf"))
    {_, tables} = FontFixture.tables(data)
    fvar = tables["fvar"]

    for {offset, bytes} <- [
          {0, <<2::32>>},
          {6, <<0::16>>},
          {34, <<32768::16>>},
          {10, <<19::16>>},
          {14, <<1::16>>},
          {16, <<0, 0, 0, 0>>},
          {20, <<1000 * 65536::signed-32>>},
          {32, <<2::16>>},
          {34, <<1::16>>},
          {36, "wght"},
          {56, <<1::16>>},
          {58, <<1::16>>},
          {60, <<1000 * 65536::signed-32>>}
        ] do
      assert {:error, {:invalid_font, %{stage: :font, operation: :instantiate_font}}} =
               FontVariationValidator.prepare(
                 FontFixture.rebuild(data, %{"fvar" => FontFixture.replace(fvar, offset, bytes)})
               )
    end

    assert {:error, {:invalid_font, _}} = FontVariationValidator.prepare(data, %{}, 99)
    static = File.read!(FontCorpus.path("selected-container-sans.ttf"))
    assert {:ok, ^static, %{}} = FontVariationValidator.prepare(static)
    assert {:error, {:invalid_font, _}} = FontVariationValidator.prepare(static, %{}, 0)
    assert {:error, {:invalid_font, _}} = FontVariationValidator.prepare_request([1], nil)
    assert {:error, {:invalid_font, _}} = FontVariationValidator.prepare_request(false, nil)

    assert {:error, {:invalid_font, _}} =
             FontVariationValidator.prepare_request(%{"bad" => 2}, nil)

    # A missing name table makes an unavailable localized instance use defaults.
    assert {:ok, _, metadata} =
             FontVariationValidator.prepare(
               FontFixture.rebuild(data, %{"name" => nil}),
               %{},
               "Absent"
             )

    assert metadata.variation_coordinates["wght"] == 400
  end

  test "avar versions, ownership and version-two cross-axis deltas use independent normalized arithmetic" do
    data = File.read!(FontCorpus.path("selected-variable.ttf"))
    segment = <<3::16, -16384::signed-16, -16384::signed-16, 0::16, 0::16, 16384::16, 16384::16>>
    v1 = <<1::16, 0::16, 0::16, 2::16, segment::binary, segment::binary>>
    v2 = FontFixture.replace(v1, 0, <<2::16>>)

    for avar <- [nil, v1, v2 <> <<0::32, 0::32>>] do
      assert {:ok, _, metadata} =
               FontVariationValidator.prepare(FontFixture.rebuild(data, %{"avar" => avar}), %{
                 "wght" => 650
               })

      assert hd(metadata.normalized_coordinates) == 0.5
    end

    # One region on weight, two implicit rows: +4096/16384 and -2048/16384
    # times the region scalar .5 => weight .625, width -.0625.
    store =
      <<1::16, 12::32, 1::16, 28::32, 2::16, 1::16, 0::16, 16384::16, 16384::16, 0::16, 0::16,
        0::16, 2::16, 1::16, 1::16, 0::16, 4096::signed-16, -2048::signed-16>>

    offset = byte_size(v2) + 8

    assert {:ok, _, metadata} =
             FontVariationValidator.prepare(
               FontFixture.rebuild(data, %{"avar" => v2 <> <<0::32, offset::32, store::binary>>}),
               %{"wght" => 650}
             )

    assert metadata.normalized_coordinates == [0.625, -0.0625]

    for avar <- [
          FontFixture.replace(v1, 0, <<3::16>>),
          FontFixture.replace(v1, 8, <<2::16>>),
          FontFixture.replace(v1, 12, <<-16000::signed-16>>),
          v2 <> <<4::32, 0::32>>,
          v2 <> <<0::32, 1::32>>
        ] do
      assert {:error, {:invalid_font, _}} =
               FontVariationValidator.prepare(FontFixture.rebuild(data, %{"avar" => avar}))
    end
  end

  test "vertical TrueType phantoms, partial metrics, explicit bearing maps and invalid metric fields" do
    data = File.read!(FontCorpus.path("selected-variable.ttf"))
    {_, tables} = FontFixture.tables(data)
    vhea = FontFixture.replace(tables["hhea"], 34, <<1::16>>)
    vmtx = <<1000::16, 100::signed-16>> <> :binary.copy(<<100::signed-16>>, 5)

    assert {:ok, selected, _} =
             FontVariationValidator.prepare(
               FontFixture.rebuild(data, %{"vhea" => vhea, "vmtx" => vmtx}),
               %{"wght" => 900}
             )

    {_, selected_tables} = FontFixture.tables(selected)
    assert byte_size(selected_tables["vmtx"]) == 24
    hvar = tables["HVAR"]
    # Default advance mapping is also a legal explicit side-bearing map.
    bearing_map_offset = byte_size(hvar)
    map = <<0, 0, 1::16, 0>>

    assert {:ok, _, _} =
             FontVariationValidator.prepare(
               FontFixture.rebuild(data, %{
                 "HVAR" => FontFixture.replace(hvar, 12, <<bearing_map_offset::32>>) <> map
               }),
               %{"wght" => 900}
             )

    for changes <- [
          %{"hhea" => FontFixture.replace(tables["hhea"], 34, <<0::16>>)},
          %{"hmtx" => FontFixture.replace(tables["hmtx"], 0, <<65535::16>>)},
          %{"HVAR" => FontFixture.replace(hvar, 0, <<2::32>>)},
          %{"hmtx" => nil},
          %{"vmtx" => vmtx}
        ] do
      assert {:error, {:invalid_font, _}} =
               FontVariationValidator.prepare(FontFixture.rebuild(data, changes), %{"wght" => 900})
    end
  end

  test "MVAR record ordering, optional targets, gasp invariants and VORG origins reject unrepresentable results" do
    data = File.read!(FontCorpus.path("selected-gasp-variable.ttf"))
    {_, tables} = FontFixture.tables(data)
    mvar = tables["MVAR"]

    for changes <- [
          %{"MVAR" => FontFixture.replace(mvar, 4, <<1::16>>)},
          %{"MVAR" => FontFixture.replace(mvar, 20, "gsp0")},
          %{"MVAR" => FontFixture.replace(mvar, 12, "gsp9")},
          %{"OS/2" => FontFixture.replace(tables["OS/2"], 68, <<32767::signed-16>>)},
          %{"gasp" => <<1::16, 2::16, 65534::16, 15::16, 65535::16, 15::16>>},
          %{"gasp" => <<1::16, 2::16, 12::16, 15::16, 65534::16, 15::16>>}
        ] do
      assert {:error, {:invalid_font, _}} =
               FontVariationValidator.prepare(FontFixture.rebuild(data, changes), %{"wght" => 900})
    end

    for changes <- [
          %{"MVAR" => FontFixture.replace(mvar, 12, "aaaa")},
          %{"MVAR" => FontFixture.replace(mvar, 28, "vcrs")},
          %{"OS/2" => nil},
          %{"hhea" => FontFixture.replace(tables["hhea"], 4, <<750::16>>)}
        ] do
      assert {:ok, _, _} =
               FontVariationValidator.prepare(FontFixture.rebuild(data, changes), %{"wght" => 900})
    end

    cff = File.read!(FontCorpus.path("selected-vertical-cff2.otf"))
    {_, cff_tables} = FontFixture.tables(cff)

    for vorg <- [
          FontFixture.replace(cff_tables["VORG"], 8, <<65535::16>>),
          FontFixture.replace(cff_tables["VORG"], 4, <<32767::16>>)
        ] do
      assert {:error, {:invalid_font, _}} =
               FontVariationValidator.prepare(FontFixture.rebuild(cff, %{"VORG" => vorg}), %{
                 "wght" => 900
               })
    end
  end

  test "CFF2 outline coexistence, maxp contract and emitted design bounds retain their format limits" do
    data = File.read!(FontCorpus.path("selected-variable-cff2.otf"))
    {_, tables} = FontFixture.tables(data)

    for changes <- [%{"CFF " => <<>>}, %{"loca" => <<>>}, %{"maxp" => <<0x10000::32, 6::16>>}] do
      assert {:error, {:invalid_font, _}} =
               FontVariationValidator.prepare(FontFixture.rebuild(data, changes))
    end

    source =
      CffFixture.face2_fixture(
        top: %{{12, 7} => [1, 0, 0, 1, 0, 0]},
        charstrings: [<<139, 139, 21, 248, 136, 139, 5>>, <<>>]
      )

    changes = %{
      "CFF2" => source,
      "fvar" => nil,
      "HVAR" => nil,
      "MVAR" => nil,
      "avar" => nil,
      "maxp" => <<0x5000::32, 2::16>>,
      "hhea" => FontFixture.replace(tables["hhea"], 34, <<2::16>>)
    }

    assert {:error, {:invalid_font, diagnostic}} =
             FontVariationValidator.prepare(FontFixture.rebuild(data, changes))

    assert diagnostic.message =~ "bounds"
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Variable", data: data}])
    assert {:error, {:invalid_font, _}} = Font.instance(font, %{}, 100)
  end

  test "instantiated coordinate fields, composite arguments and arithmetic failures return diagnostics" do
    limits = NativeElixirPdfUtilities.Limits.effective()
    on_exit(fn -> NativeElixirPdfUtilities.Limits.install(limits) end)
    source = File.read!(FontCorpus.path("selected-variable.ttf"))
    {_, tables} = FontFixture.tables(source)
    # Six source glyphs; a private tuple changes all H points by 40,000 units.
    payload = <<0xCF>> <> :binary.copy(<<40000::signed-32>>, 16) <> <<0x8F>>
    tuple = <<1::16, 10::16, byte_size(payload)::16, 0x8000::16, 16384::16, payload::binary>>

    gvar =
      <<1::16, 0::16, 2::16, 0::16, 0::32, 6::16, 1::16, 48::32, 0::32, 0::32, 0::32,
        byte_size(tuple)::32, byte_size(tuple)::32, byte_size(tuple)::32, byte_size(tuple)::32,
        tuple::binary>>

    # The source has two axes, so embedded peak has two coordinates.
    tuple =
      <<1::16, 12::16, byte_size(payload)::16, 0x8000::16, 16384::16, 0::16, payload::binary>>

    gvar =
      binary_part(gvar, 0, 20) <>
        <<0::32, 0::32, 0::32, byte_size(tuple)::32, byte_size(tuple)::32, byte_size(tuple)::32,
          byte_size(tuple)::32, tuple::binary>>

    assert {:error, {:invalid_font, diagnostic}} =
             FontVariationValidator.prepare(FontFixture.rebuild(source, %{"gvar" => gvar}), %{
               "wght" => 900
             })

    assert diagnostic.message =~ "signed int16"

    # An empty component can have no ink while its selected translation still
    # has to fit the signed 16-bit component argument in the emitted font.
    composite = <<-1::signed-16, 0::64, 3::16, 0::16, 0::32>>
    loca = <<0::32, 0::32, 0::32, 0::32, 0::32, 0::32, byte_size(composite)::32>>
    payload = <<0xC0, 40000::signed-32, 0x83, 0x84>>

    tuple =
      <<1::16, 12::16, byte_size(payload)::16, 0x8000::16, 16384::16, 0::16, payload::binary>>

    gvar =
      <<1::16, 0::16, 2::16, 0::16, 0::32, 6::16, 1::16, 48::32, 0::size(6 * 32),
        byte_size(tuple)::32, tuple::binary>>

    changes = %{
      "glyf" => composite,
      "loca" => loca,
      "gvar" => gvar,
      "head" => FontFixture.replace(tables["head"], 50, <<1::16>>)
    }

    assert {:error, {:invalid_font, diagnostic}} =
             FontVariationValidator.prepare(FontFixture.rebuild(source, changes), %{"wght" => 900})

    assert diagnostic.message =~ "component arguments"
    simple = <<1::16, 0::64, 0::16, 0::16, 49, 0>>
    first = byte_size(simple)

    loca_with_ink =
      <<0::32, first::32, first::32, first::32, first::32, first::32,
        first + byte_size(composite)::32>>

    assert {:error, {:invalid_font, diagnostic}} =
             FontVariationValidator.prepare(
               FontFixture.rebuild(source, %{
                 changes
                 | "glyf" => simple <> composite,
                   "loca" => loca_with_ink
               }),
               %{"wght" => 900}
             )

    assert diagnostic.message =~ "composite bounds"

    cycle = FontFixture.replace(composite, 12, <<5::16>>)

    assert {:error, {:invalid_font, diagnostic}} =
             FontVariationValidator.prepare(
               FontFixture.rebuild(source, %{changes | "glyf" => cycle, "gvar" => nil})
             )

    assert diagnostic.message =~ "cycle"

    cff = File.read!(FontCorpus.path("selected-variable-cff2.otf"))
    {_, cff_tables} = FontFixture.tables(cff)

    cff2 =
      CffFixture.face2_fixture(
        top: %{{12, 7} => [1.0e308, 0, 0, 1, 0, 0]},
        charstrings: [<<139, 139, 21, 248, 136, 139, 5>>, <<>>]
      )

    changes = %{
      "CFF2" => cff2,
      "fvar" => nil,
      "avar" => nil,
      "HVAR" => nil,
      "MVAR" => nil,
      "maxp" => <<0x5000::32, 2::16>>,
      "hhea" => FontFixture.replace(cff_tables["hhea"], 34, <<2::16>>)
    }

    NativeElixirPdfUtilities.Limits.install(
      Map.put(limits, :max_numeric_magnitude, Integer.pow(10, 310))
    )

    assert {:error, {:invalid_font, diagnostic}} =
             FontVariationValidator.prepare(FontFixture.rebuild(cff, changes))

    assert diagnostic.message =~ "finite"
  end

  test "default-instance name IDs, optional PostScript names and inactive axes follow fvar's static fallback" do
    data = File.read!(FontCorpus.path("selected-variable.ttf"))
    {_, tables} = FontFixture.tables(data)
    fvar = tables["fvar"]
    <<header::binary-size(56), instances::binary>> = fvar
    # Three 12-byte records gain the optional name field; the first is default.
    records =
      for <<name::16, flags::16, _weight::32, width::32 <- instances>>,
        do: <<name::16, flags::16, 400 * 65536::32, width::32, 6::16>>

    extended = FontFixture.replace(header, 14, <<14::16>>) <> IO.iodata_to_binary(records)

    for name <- [2, 17] do
      selected = FontFixture.replace(extended, 56, <<name::16>>)

      assert {:ok, _, metadata} =
               FontVariationValidator.prepare(
                 FontFixture.rebuild(data, %{"fvar" => selected}),
                 %{},
                 0
               )

      assert metadata.variation_coordinates == %{"wght" => 400, "wdth" => 100}
    end

    invalid = FontFixture.replace(extended, 68, <<1::16>>)

    assert {:error, {:invalid_font, _}} =
             FontVariationValidator.prepare(FontFixture.rebuild(data, %{"fvar" => invalid}))

    inactive = FontFixture.rebuild(data, %{"fvar" => FontFixture.replace(fvar, 8, <<0::16>>)})
    assert {:ok, static, %{}} = FontVariationValidator.prepare(inactive, %{"wght" => 900})
    {_, static_tables} = FontFixture.tables(static)
    refute Map.has_key?(static_tables, "fvar")
    refute Map.has_key?(static_tables, "gvar")
    assert static_tables["glyf"] == tables["glyf"]
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Static", data: inactive}])
    refute font.variable_font?
    <<_::32, count::16, _::binary>> = inactive

    head_index =
      Enum.find(0..(count - 1), fn index ->
        binary_part(inactive, 12 + index * 16, 4) == "head"
      end)

    missing_head = FontFixture.replace(inactive, 12 + head_index * 16, "haad")

    assert {:error, {:invalid_font, %{message: message}}} =
             FontVariationValidator.prepare(missing_head)

    assert message =~ "head table"
  end
end
