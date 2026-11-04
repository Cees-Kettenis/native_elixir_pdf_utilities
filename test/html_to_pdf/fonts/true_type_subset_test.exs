defmodule NativeElixirPdfUtilities.HtmlToPdf.TrueTypeSubsetTest do
  use ExUnit.Case, async: false

  import Bitwise

  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PdfWriter, TrueTypeSubset}
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Text
  alias NativeElixirPdfUtilities.Validators.TrueTypeSubsetValidator
  alias NativeElixirPdfUtilities.Validators.FontValidator

  setup do
    original_limits = Limits.effective()
    on_exit(fn -> Limits.install(original_limits) end)
    assert {:ok, registry} = Font.load_registry([])
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)
    %{font: font}
  end

  test "glyph ink geometry comes from selected source bytes and refreshes reused font IDs", %{
    font: font
  } do
    assert {:ok, plan} = TrueTypeSubsetValidator.prepare(font, [font.cmap[?A]])
    assert {:ok, bounds} = Font.glyph_bounds(font)
    assert tuple_size(bounds) == plan.glyph_count
    assert elem(bounds, font.cmap[?\s]) == {0, 0, 0, 0}

    glyph = font.cmap[?A]
    offset = table_offset(font.data, "glyf") + elem(plan.offsets, glyph)

    <<_contours::16, xmin::signed-16, ymin::signed-16, xmax::signed-16, ymax::signed-16>> =
      binary_part(font.data, offset, 10)

    assert elem(bounds, glyph) == {xmin, ymin, xmax, ymax}
    changed = %{font | data: replace_u16(font.data, offset + 6, xmax + 1)}

    assert {:ok, changed_bounds} =
             NativeElixirPdfUtilities.Validators.FontValidator.with_budget(fn ->
               assert {:ok, ^bounds} = Font.glyph_bounds(font)
               assert {:ok, ^bounds} = Font.glyph_bounds(font)
               Font.glyph_bounds(changed)
             end)

    assert elem(changed_bounds, glyph) == {xmin, ymin, xmax + 1, ymax}

    short = replace_glyph(font, glyph, <<0::16>>, 2)
    assert {:error, {:invalid_document, diagnostic}} = Font.glyph_bounds(short)
    assert diagnostic.stage == :font
    assert diagnostic.operation == :glyph_bounds
    assert diagnostic.source == font.family
    assert diagnostic.message =~ "ink bounds"
    reversed = %{font | data: replace_u16(font.data, offset + 2, 32767)}
    assert {:error, {:invalid_document, %{message: message}}} = Font.glyph_bounds(reversed)
    assert message =~ "ink bounds"
  end

  test "glyph bounds are lazy and cumulatively bounded, including source bytes", %{font: font} do
    assert {:ok, plan} = TrueTypeSubsetValidator.prepare(font, [0])

    work =
      plan.glyph_count * 11 +
        (plan.glyph_count + 1) * if(plan.location_format == 0, do: 2, else: 4)

    Limits.install(%{Limits.effective() | max_font_math_work: work})
    assert {:ok, _registry} = Font.load_registry(system_font_discovery: false)

    assert {:ok, _} =
             NativeElixirPdfUtilities.Validators.FontValidator.with_budget(fn ->
               assert {:ok, _} = Font.glyph_bounds(font)
               Font.glyph_bounds(font)
             end)

    assert {:error, {:resource_limit_exceeded, %{stage: :font, message: message}}} =
             NativeElixirPdfUtilities.Validators.FontValidator.with_budget(fn ->
               assert {:ok, _} = Font.glyph_bounds(font)
               TrueTypeSubsetValidator.prepare_glyph_bounds(font.data)
             end)

    assert message =~ "max_font_math_work"
    Limits.install(%{Limits.effective() | max_font_math_work: work - 1})

    assert {:error, {:resource_limit_exceeded, %{operation: :glyph_bounds}}} =
             Font.glyph_bounds(font)

    Limits.install(%{Limits.effective() | max_font_source_bytes: 1})

    assert {:error, {:resource_limit_exceeded, %{message: source_message}}} =
             Font.glyph_bounds(font)

    assert source_message =~ "max_font_source_bytes"
  end

  test "glyph geometry rejects absent or malformed outline data with diagnostics", %{font: font} do
    assert {:error, {:invalid_document, %{operation: :glyph_bounds, message: message}}} =
             Font.glyph_bounds(%{type: :built_in})

    assert message =~ "embedded TrueType"

    assert {:error, {:invalid_document, %{stage: :font, operation: :glyph_bounds}}} =
             Font.glyph_bounds(nil)

    assert {:error, {:invalid_document, _}} = TrueTypeSubsetValidator.prepare_glyph_bounds(nil)
    assert {:error, {:invalid_document, _}} = TrueTypeSubsetValidator.prepare_glyph_bounds(<<>>)
    missing = replace_bytes(font.data, table_record_offset(font.data, "glyf"), 4, "NONE")

    assert {:error, {:invalid_document, %{message: missing_message}}} =
             Font.glyph_bounds(%{font | data: missing})

    assert missing_message =~ "missing the glyf"
    loca = replace_u32(font.data, table_record_offset(font.data, "loca") + 12, 0)

    assert {:error, {:invalid_document, %{message: loca_message}}} =
             Font.glyph_bounds(%{font | data: loca})

    assert loca_message =~ "loca"
  end

  test "keeps composite dependencies and produces a checksummed standalone font", %{font: font} do
    glyph_id = font.cmap[?é]
    assert {:ok, original_plan} = TrueTypeSubsetValidator.prepare(font, [glyph_id])
    assert MapSet.subset?(MapSet.new([0, 72, 118, glyph_id]), original_plan.retained)

    assert {:ok, subset} = TrueTypeSubset.subset(font, [glyph_id])
    assert byte_size(subset) < byte_size(font.data) * 0.5

    checksum =
      for <<word::32 <- subset>>, reduce: 0 do
        total -> total + word &&& 0xFFFF_FFFF
      end

    assert checksum == 0xB1B0_AFBA

    assert {:ok, subset_plan} =
             TrueTypeSubsetValidator.prepare(%{font | data: subset}, [glyph_id])

    for component <- [72, 118, glyph_id] do
      assert elem(subset_plan.offsets, component + 1) > elem(subset_plan.offsets, component)
    end

    assert {:ok, loaded} =
             Font.load_registry(fonts: [%{family: "Subset Fixture", data: [subset]}])

    assert {:ok, _, subset_face} = Font.resolve("Subset Fixture", 400, :normal, loaded)
    assert subset_face.cmap[?é] == glyph_id
  end

  test "aliased opaque tables are admitted by padded output size before subset construction", %{
    font: font
  } do
    payload = :binary.copy(<<1>>, 32_769)
    offset = byte_size(font.data)

    data =
      Enum.reduce([{"FFTM", "TST1"}, {"prep", "TST2"}, {"gasp", "TST3"}], font.data, fn
        {original, replacement}, data ->
          record = table_record_offset(data, original)

          data
          |> replace_bytes(record, 4, replacement)
          |> replace_u32(record + 8, offset)
          |> replace_u32(record + 12, byte_size(payload))
      end) <> payload

    aliased = %{font | data: data}
    assert {:ok, _} = FontValidator.prepare_ttf(data)
    assert {:ok, _} = TrueTypeSubsetValidator.prepare_glyph_bounds(data)
    assert {:ok, result} = TrueTypeSubset.subset(aliased, [font.cmap[?A]])
    assert {:ok, plan} = TrueTypeSubsetValidator.prepare(aliased, [font.cmap[?A]])
    assert plan.output_bytes == byte_size(result)
    limits = Limits.effective()
    Limits.install(Map.put(limits, :max_font_subset_bytes, byte_size(result) - 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             TrueTypeSubset.subset(aliased, [font.cmap[?A]])

    assert diagnostic.message =~ "max_font_subset_bytes"

    assert {:error, {:resource_limit_exceeded, _}} =
             TrueTypeSubsetValidator.prepare(aliased, [font.cmap[?A]])

    Limits.install(Map.put(limits, :max_font_subset_bytes, byte_size(result)))
    assert {:ok, ^result} = TrueTypeSubset.subset(aliased, [font.cmap[?A]])
    Limits.install(Map.put(limits, :max_font_subset_checksum_work, div(byte_size(result), 4)))
    assert {:ok, ^result} = TrueTypeSubset.subset(aliased, [font.cmap[?A]])
    Limits.install(Map.put(limits, :max_font_subset_checksum_work, div(byte_size(result), 4) - 1))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             TrueTypeSubsetValidator.prepare(aliased, [font.cmap[?A]])

    assert message =~ "max_font_subset_checksum_work"

    Limits.install(Map.put(limits, :max_font_subset_bytes, 2 * byte_size(result) - 1))

    Limits.install(Map.put(limits, :max_font_subset_bytes, 2 * byte_size(result) - 1))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             FontValidator.with_budget(fn ->
               assert {:ok, ^result} = TrueTypeSubset.subset(aliased, [font.cmap[?A]])
               TrueTypeSubset.subset(aliased, [font.cmap[?A]])
             end)

    assert message =~ "max_font_subset_bytes"

    box = %{
      type: :text,
      text: "A",
      x: 0,
      y: 20,
      font: Font.pdf_name(aliased),
      font_face: aliased,
      font_size: 10,
      color: {0, 0, 0}
    }

    second = %{aliased | id: aliased.id <> "-second"}
    second_box = %{box | font_face: second, font: Font.pdf_name(second)}

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             PdfWriter.render([%{size: {100, 100}, boxes: [box, second_box]}])

    assert diagnostic.operation == :write_pdf and diagnostic.source == "TrueType subset"
    assert diagnostic.message =~ "max_font_subset_bytes"

    Limits.install(Map.put(limits, :max_font_subset_checksum_work, 1))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             PdfWriter.render([%{size: {100, 100}, boxes: [box]}])

    assert message =~ "max_font_subset_checksum_work"
  end

  test "directory policy agrees for loading, glyph metrics and subsetting", %{font: font} do
    duplicate = replace_bytes(font.data, table_record_offset(font.data, "prep"), 4, "name")
    outside = replace_u32(font.data, table_record_offset(font.data, "FFTM") + 8, 0)

    for data <- [duplicate, outside] do
      assert :error = FontValidator.prepare_ttf(data)

      assert {:error, {:invalid_document, diagnostic}} =
               TrueTypeSubsetValidator.prepare_glyph_bounds(data)

      assert diagnostic.source == "TrueType table directory"
      assert {:error, {:invalid_document, _}} = TrueTypeSubset.subset(%{font | data: data}, [0])
      assert {:error, {_reason, _}} = Font.load_configured_faces([%{family: "Bad", data: data}])
    end
  end

  test "subset admission respects fixed sfnt output directory field capacities", %{font: font} do
    <<scaler::32, count::16, _::48, rest::binary>> = font.data
    extra = 4096 - count
    old_directory_bytes = 12 + count * 16

    records =
      for <<tag::binary-size(4), checksum::32, offset::32,
            length::32 <-
              binary_part(rest, 0, count * 16)>>,
          into: <<>> do
        <<tag::binary, checksum::32, offset + extra * 16::32, length::32>>
      end

    custom =
      for index <- 1..extra, into: <<>> do
        tag = index |> Integer.to_string(36) |> String.pad_leading(4, "0")
        <<tag::binary-size(4), 0::32, 12 + 4096 * 16::32, 0::32>>
      end

    payload =
      binary_part(font.data, old_directory_bytes, byte_size(font.data) - old_directory_bytes)

    data = <<scaler::32, 4096::16, 0::48, records::binary, custom::binary, payload::binary>>

    assert {:error, {:invalid_document, %{message: message}}} =
             TrueTypeSubsetValidator.prepare(%{font | data: data}, [0])

    assert message =~ "sfnt directory field capacity"
  end

  test "rejects malformed fonts, offsets, and glyph requests with diagnostics", %{font: font} do
    assert {:error, {:invalid_document, %{stage: :font}}} =
             TrueTypeSubset.subset(%{font | data: <<>>}, [0])

    assert {:error, {:invalid_document, %{message: glyph_message}}} =
             TrueTypeSubset.subset(font, [99_999])

    assert glyph_message =~ "out-of-range glyph"

    invalid_head = replace_u16(font.data, table_offset(font.data, "head") + 50, 3)

    assert {:error, {:invalid_document, %{message: head_message}}} =
             TrueTypeSubset.subset(%{font | data: invalid_head}, [0])

    assert head_message =~ "head or maxp"

    invalid_loca = replace_u16(font.data, table_offset(font.data, "loca"), 1)

    assert {:error, {:invalid_document, %{message: loca_message}}} =
             TrueTypeSubset.subset(%{font | data: invalid_loca}, [0])

    assert loca_message =~ "glyph offsets"
  end

  test "retains checksums and bytes of opaque tables at every word and block boundary", %{
    font: font
  } do
    record = table_record_offset(font.data, "FFTM")
    payload = for byte <- 1..35, into: <<>>, do: <<byte>>
    offset = byte_size(font.data)

    for length <- 0..35 do
      data =
        font.data
        |> replace_bytes(record, 4, "TEST")
        |> replace_u32(record + 8, offset)
        |> replace_u32(record + 12, length)

      assert {:ok, subset} =
               TrueTypeSubset.subset(%{font | data: data <> payload}, [font.cmap[?A]])

      table = table_record_offset(subset, "TEST")
      <<checksum::32, table_offset::32, ^length::32>> = binary_part(subset, table + 4, 12)
      expected = binary_part(payload, 0, length)
      assert binary_part(subset, table_offset, length) == expected

      padded = expected <> :binary.copy(<<0>>, rem(4 - rem(length, 4), 4))

      reference =
        for <<word::32 <- padded>>, reduce: 0 do
          total -> total + word &&& 0xFFFF_FFFF
        end

      assert checksum == reference

      whole_checksum =
        for <<word::32 <- subset>>, reduce: 0 do
          total -> total + word &&& 0xFFFF_FFFF
        end

      assert whole_checksum == 0xB1B0_AFBA
    end
  end

  test "respects no-subsetting licenses and the subset work budget", %{font: font} do
    restricted = replace_u16(font.data, table_offset(font.data, "OS/2") + 8, 0x0100)

    assert {:ok, :keep_full} =
             TrueTypeSubsetValidator.prepare(%{font | data: restricted}, [font.cmap[?A]])

    assert {:ok, ^restricted} =
             TrueTypeSubset.subset(%{font | data: restricted}, [font.cmap[?A]])

    box = %{
      type: :text,
      text: "A",
      x: 0,
      y: 20,
      font: Font.pdf_name(font),
      font_face: %{font | data: restricted},
      font_size: 10,
      color: {0, 0, 0}
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [box]}])
    refute pdf =~ ~r|/BaseFont /[A-Z]{6}\+|
    assert {:ok, "A"} = Text.extract(pdf, layout: false)

    Limits.install(%{Limits.effective() | max_font_subset_work: 1})

    assert {:error,
            {:resource_limit_exceeded,
             %{stage: :limits, message: "font subsetting exceeds max_font_subset_work"}}} =
             TrueTypeSubset.subset(font, [font.cmap[?é]])

    Limits.install(%{Limits.effective() | max_font_subset_work: 3})

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             TrueTypeSubset.subset(font, [font.cmap[?é]])
  end

  test "rejects invalid table directories and missing outline tables", %{font: font} do
    assert {:error, {:invalid_document, %{message: request_message}}} =
             TrueTypeSubset.subset(%{data: nil}, [0])

    assert request_message =~ "requires a TrueType font"

    duplicate = replace_bytes(font.data, table_record_offset(font.data, "glyf"), 4, "head")

    assert {:error, {:invalid_document, %{message: duplicate_message}}} =
             TrueTypeSubset.subset(%{font | data: duplicate}, [0])

    assert duplicate_message =~ "repeats a tag"

    outside = replace_u32(font.data, table_record_offset(font.data, "glyf") + 8, 0)

    assert {:error, {:invalid_document, %{message: outside_message}}} =
             TrueTypeSubset.subset(%{font | data: outside}, [0])

    assert outside_message =~ "outside the font"

    missing = replace_bytes(font.data, table_record_offset(font.data, "glyf"), 4, "ZZZZ")

    assert {:error, {:invalid_document, %{message: missing_message}}} =
             TrueTypeSubset.subset(%{font | data: missing}, [0])

    assert missing_message =~ "missing the glyf table"
  end

  test "supports short loca and validates truncated tables", %{font: font} do
    {:ok, sparse} = TrueTypeSubset.subset(font, [font.cmap[?é]])
    {:ok, plan} = TrueTypeSubsetValidator.prepare(%{font | data: sparse}, [0])

    short_loca =
      for id <- 0..plan.glyph_count, into: <<>>, do: <<div(elem(plan.offsets, id), 2)::16>>

    loca_offset = table_offset(sparse, "loca")
    short_data = replace_bytes(sparse, loca_offset, byte_size(short_loca), short_loca)
    short_data = replace_u16(short_data, table_offset(short_data, "head") + 50, 0)

    assert {:ok, short_subset} =
             TrueTypeSubset.subset(%{font | data: short_data}, [font.cmap[?é]])

    assert {:ok, %{location_format: 0}} =
             TrueTypeSubsetValidator.prepare(%{font | data: short_subset}, [font.cmap[?é]])

    truncated_loca = replace_u32(font.data, table_record_offset(font.data, "loca") + 12, 2)

    assert {:error, {:invalid_document, %{message: loca_message}}} =
             TrueTypeSubset.subset(%{font | data: truncated_loca}, [0])

    assert loca_message =~ "loca table is truncated"

    truncated_os2 = replace_u32(font.data, table_record_offset(font.data, "OS/2") + 12, 4)

    assert {:error, {:invalid_document, %{message: os2_message}}} =
             TrueTypeSubset.subset(%{font | data: truncated_os2}, [0])

    assert os2_message =~ "embedding flags are truncated"

    no_os2 = replace_bytes(font.data, table_record_offset(font.data, "OS/2"), 4, "ZZZZ")
    assert {:ok, _subset} = TrueTypeSubset.subset(%{font | data: no_os2}, [0])
  end

  test "validates embedded outlines and composite component references", %{font: font} do
    glyph_id = font.cmap[?é]
    simple_id = font.cmap[?A]
    simple = replace_glyph(font, simple_id, <<0::16>>, 2)

    assert {:error, {:invalid_document, %{message: simple_message}}} =
             TrueTypeSubset.subset(simple, [simple_id])

    assert simple_message =~ "glyph data is truncated"

    header = <<-1::signed-16, 0::64>>
    out_of_range = replace_glyph(font, glyph_id, header <> <<0::16, 65_535::16, 0::16>>)

    assert {:error, {:invalid_document, %{message: component_message}}} =
             TrueTypeSubset.subset(out_of_range, [glyph_id])

    assert component_message =~ "out-of-range glyph"

    cyclic = replace_glyph(font, glyph_id, header <> <<0::16, glyph_id::16, 0::16>>)

    assert {:error, {:invalid_document, %{message: cycle_message}}} =
             TrueTypeSubset.subset(cyclic, [glyph_id])

    assert cycle_message =~ "contain a cycle"

    conflicting_scale = replace_glyph(font, glyph_id, header <> <<0x0048::16, 72::16, 0::16>>)

    assert {:error, {:invalid_document, %{message: scale_message}}} =
             TrueTypeSubset.subset(conflicting_scale, [glyph_id])

    assert scale_message =~ "conflicting scale flags"

    for {flags, transform_bytes} <- [{0x0008, 2}, {0x0040, 4}, {0x0080, 8}] do
      body = header <> <<flags::16, 72::16, 0::16>> <> :binary.copy(<<0>>, transform_bytes)
      candidate = replace_glyph(font, glyph_id, body)
      assert {:ok, %{retained: retained}} = TrueTypeSubsetValidator.prepare(candidate, [glyph_id])
      assert MapSet.member?(retained, 72)
    end

    instructions =
      replace_glyph(font, glyph_id, header <> <<0x0100::16, 72::16, 0::16, 1::16, 0::8>>)

    assert {:ok, _plan} = TrueTypeSubsetValidator.prepare(instructions, [glyph_id])

    truncated_instructions =
      replace_glyph(font, glyph_id, header <> <<0x0100::16, 72::16, 0::16, 0::8>>, 17)

    assert {:error, {:invalid_document, %{message: instruction_message}}} =
             TrueTypeSubset.subset(truncated_instructions, [glyph_id])

    assert instruction_message =~ "instructions are truncated"

    truncated_component =
      replace_glyph(font, glyph_id, header <> <<0x0001::16, 72::16, 0::16>>, 16)

    assert {:error, {:invalid_document, %{message: truncated_message}}} =
             TrueTypeSubset.subset(truncated_component, [glyph_id])

    assert truncated_message =~ "composite glyph is truncated"

    no_component = replace_glyph(font, glyph_id, header, 10)

    assert {:error, {:invalid_document, %{message: empty_message}}} =
             TrueTypeSubset.subset(no_component, [glyph_id])

    assert empty_message =~ "composite glyph is truncated"

    repeated_components =
      replace_glyph(
        font,
        font.cmap[813],
        header <> <<0x0020::16, 72::16, 0::16, 0x0020::16, 72::16, 0::16, 0::16, 72::16, 0::16>>
      )

    Limits.install(%{Limits.effective() | max_font_subset_work: 2})

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             TrueTypeSubset.subset(repeated_components, [font.cmap[813]])
  end

  defp table_offset(data, tag) do
    offset = table_record_offset(data, tag)
    value_offset = offset + 8
    <<_::binary-size(^value_offset), table_offset::32, _::binary>> = data
    table_offset
  end

  defp table_record_offset(data, tag) do
    <<_scaler::32, count::16, _::binary>> = data

    0..(count - 1)
    |> Enum.find_value(fn index ->
      offset = 12 + index * 16

      case binary_part(data, offset, 16) do
        <<^tag::binary-size(4), _checksum::32, _table_offset::32, _length::32>> -> offset
        _ -> nil
      end
    end)
  end

  defp replace_glyph(font, glyph_id, replacement, new_length \\ nil) do
    {:ok, plan} = TrueTypeSubsetValidator.prepare(font, [0])
    start = elem(plan.offsets, glyph_id)
    finish = elem(plan.offsets, glyph_id + 1)
    span = finish - start

    data =
      replace_bytes(
        font.data,
        table_offset(font.data, "glyf") + start,
        span,
        replacement <> :binary.copy(<<0>>, span - byte_size(replacement))
      )

    data =
      case new_length do
        nil ->
          data

        length ->
          replace_u32(data, table_offset(data, "loca") + (glyph_id + 1) * 4, start + length)
      end

    %{font | data: data}
  end

  defp replace_bytes(data, offset, count, replacement) do
    <<before::binary-size(^offset), _old::binary-size(^count), after_value::binary>> = data
    <<before::binary, replacement::binary, after_value::binary>>
  end

  defp replace_u32(data, offset, value) do
    <<before::binary-size(^offset), _old::32, after_value::binary>> = data
    <<before::binary, value::32, after_value::binary>>
  end

  defp replace_u16(data, offset, value) do
    <<before::binary-size(^offset), _old::16, after_value::binary>> = data
    <<before::binary, value::16, after_value::binary>>
  end
end
