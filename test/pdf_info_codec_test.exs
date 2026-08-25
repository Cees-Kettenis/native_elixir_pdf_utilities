defmodule NativeElixirPdfUtilities.Pdf.InfoCodecTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Pdf.InfoCodec

  test "decodes PDFDocEncoding, UTF-16BE, and PDF 2 UTF-8 text strings" do
    assert InfoCodec.decode_text("ASCII") == {:ok, "ASCII"}
    assert InfoCodec.decode_text(<<0xFE, 0xFF, 0x00, 0x52, 0x00, 0xE9>>) == {:ok, "Ré"}
    assert InfoCodec.decode_text(<<0xEF, 0xBB, 0xBF, "Résumé">>) == {:ok, "Résumé"}

    assert InfoCodec.decode_text(<<0xFE, 0xFF, 0xD8, 0x00>>) == :error
    assert InfoCodec.decode_text(<<0xEF, 0xBB, 0xBF, 0xFF>>) == :error
    assert InfoCodec.decode_text(<<0>>) == :error
  end

  test "encodes ASCII as literal values and Unicode as UTF-16BE hexadecimal values" do
    assert InfoCodec.encode_text("Title") == {:string, "Title"}

    assert InfoCodec.encode_text("Résumé") ==
             {:hex,
              <<0xFE, 0xFF, 0x00, 0x52, 0x00, 0xE9, 0x00, 0x73, 0x00, 0x75, 0x00, 0x6D, 0x00,
                0xE9>>}
  end

  test "parses partial and zoned PDF dates into wall-clock NaiveDateTime values" do
    assert {:ok, ~N[2026-01-01 00:00:00], %{precision: :year, timezone: nil}} =
             InfoCodec.parse_date("D:2026")

    assert {:ok, ~N[2026-08-01 00:00:00], %{precision: :month}} =
             InfoCodec.parse_date("D:202608")

    assert {:ok, ~N[2026-08-25 14:30:45], %{precision: :second, timezone: :utc}} =
             InfoCodec.parse_date("D:20260825143045Z")

    assert {:ok, ~N[2026-08-25 14:30:45], %{timezone: {:+, 8, 30}}} =
             InfoCodec.parse_date("D:20260825143045+08'30'")

    assert {:ok, ~N[2026-08-25 14:30:45], %{timezone: {:-, 4, 0}}} =
             InfoCodec.parse_date("D:20260825143045-04'00'")

    for invalid <- [
          "20260825",
          "D:202613",
          "D:20260230",
          "D:20260825246000",
          "D:20260825143045+24'00'",
          "D:20260825143045+08'60'",
          "D:20260825143045+08"
        ] do
      assert InfoCodec.parse_date(invalid) == :error
    end
  end

  test "normalizes calendar, ISO 8601, and PDF date inputs" do
    assert InfoCodec.normalize_date(~D[2026-08-25]) == {:ok, "D:20260825"}

    assert InfoCodec.normalize_date(~N[2026-08-25 14:30:45.123456]) ==
             {:ok, "D:20260825143045"}

    assert InfoCodec.normalize_date(~U[2026-08-25 14:30:45.123456Z]) ==
             {:ok, "D:20260825143045+00'00'"}

    assert InfoCodec.normalize_date("2026-08-25") == {:ok, "D:20260825"}

    assert InfoCodec.normalize_date("2026-08-25T14:30:45") ==
             {:ok, "D:20260825143045"}

    assert InfoCodec.normalize_date("2026-08-25T14:30:45+08:30") ==
             {:ok, "D:20260825143045+08'30'"}

    assert InfoCodec.normalize_date("D:20260825143045+0830") ==
             {:ok, "D:20260825143045+08'30'"}

    assert InfoCodec.normalize_date("D:20260825143045Z") ==
             {:ok, "D:20260825143045Z"}

    invalid_offset = %{~U[2026-08-25 14:30:45Z] | utc_offset: 1}
    assert InfoCodec.normalize_date(invalid_offset) == :error

    for invalid <- [:today, "not-a-date", "D:202613"] do
      assert InfoCodec.normalize_date(invalid) == :error
    end
  end

  test "serializes parsed PDF values with escaped names, strings, arrays, and dictionaries" do
    value = %{
      "Name With Space" => {:name, "A/B#C"},
      "Text" => {:string, "line\n(quoted)\\\x00"},
      "Hex" => {:hex, <<0, 255>>},
      "Array" => [nil, true, false, 12, 1.25, {:ref, {4, 2}}]
    }

    assert {:ok, encoded} = InfoCodec.serialize_value(value)
    encoded = IO.iodata_to_binary(encoded)

    assert encoded =~ "/Name#20With#20Space /A#2FB#23C"
    assert encoded =~ "/Text (line\\n\\(quoted\\)\\\\\\000)"
    assert encoded =~ "/Hex <00FF>"
    assert encoded =~ "/Array [null true false 12 1.25 4 2 R]"

    assert InfoCodec.serialize_value(self()) == :error
    assert InfoCodec.serialize_value(%{1 => "invalid key"}) == :error
    assert InfoCodec.serialize_value([self()]) == :error

    deeply_nested = Enum.reduce(1..101, 0, fn _index, nested -> [nested] end)
    assert InfoCodec.serialize_value(deeply_nested) == :error
  end
end
