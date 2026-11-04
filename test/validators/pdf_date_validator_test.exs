defmodule NativeElixirPdfUtilities.Validators.PdfDateValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.PdfDateValidator

  test "defaults omitted timezone minutes to zero and preserves supplied minutes" do
    for {suffix, timezone, canonical} <- [
          {"+08", {:+, 8, 0}, "+08'00'"},
          {"+08'", {:+, 8, 0}, "+08'00'"},
          {"-04", {:-, 4, 0}, "-04'00'"},
          {"-04'", {:-, 4, 0}, "-04'00'"},
          {"+00'", {:+, 0, 0}, "+00'00'"},
          {"-00'", {:-, 0, 0}, "-00'00'"},
          {"+23'", {:+, 23, 0}, "+23'00'"},
          {"-23'", {:-, 23, 0}, "-23'00'"},
          {"+08'30'", {:+, 8, 30}, "+08'30'"},
          {"-04'45'", {:-, 4, 45}, "-04'45'"},
          {"+23'59'", {:+, 23, 59}, "+23'59'"},
          {"-23'59'", {:-, 23, 59}, "-23'59'"}
        ] do
      date = "D:20260101120000" <> suffix

      assert {:ok, ~N[2026-01-01 12:00:00], %{timezone: ^timezone}} =
               PdfDateValidator.parse_date(date)

      assert PdfDateValidator.normalize_date(date) == {:ok, "D:20260101120000" <> canonical}
    end
  end

  test "rejects invalid timezone fields and separators instead of defaulting them" do
    for suffix <- [
          "+24'",
          "-24'",
          "+99",
          "+08'60'",
          "-04'99'",
          "+08'0",
          "-04'000'",
          "+08''",
          "+08'x",
          "+8'",
          "+",
          "Z08'"
        ] do
      date = "D:20260101120000" <> suffix
      assert PdfDateValidator.parse_date(date) == :error
      assert PdfDateValidator.normalize_date(date) == :error
    end
  end
end
