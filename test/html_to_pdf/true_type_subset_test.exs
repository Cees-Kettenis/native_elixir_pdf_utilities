defmodule NativeElixirPdfUtilities.HtmlToPdf.TrueTypeSubsetTest do
  use ExUnit.Case, async: false

  import Bitwise

  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PdfWriter, TrueTypeSubset}
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Text
  alias NativeElixirPdfUtilities.Validators.TrueTypeSubsetValidator

  setup do
    original_limits = Limits.effective()
    on_exit(fn -> Limits.install(original_limits) end)
    assert {:ok, registry} = Font.load_registry([])
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)
    %{font: font}
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
