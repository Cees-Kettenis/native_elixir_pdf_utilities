defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlEntitiesTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlEntities
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlEntityData

  test "includes the complete generated WHATWG named-reference table" do
    assert HtmlEntityData.entity_count() == 2_231
    assert HtmlEntityData.max_name_length() == 32
    assert HtmlEntityData.lookup("copy;") == "©"
    assert HtmlEntityData.lookup("NotEqualTilde;") == "≂̸"
    assert HtmlEntityData.lookup("does-not-exist;") == nil
  end

  test "decodes the longest named reference once" do
    assert HtmlEntities.decode(
             "plain &copy &copy; &notin; &NotEqualTilde; &fjlig; &unknown; &amp;lt;",
             :text
           ) == "plain © © ∉ ≂̸ fj &unknown; &lt;"

    assert HtmlEntities.decode("", :text) == ""
  end

  test "applies legacy semicolonless rules for attribute values" do
    assert HtmlEntities.decode(
             "&copy &copy! &copy= &copy0 &copyA &copya &copy;",
             :attribute
           ) == "© ©! &copy= &copy0 &copyA &copya ©"
  end

  test "decodes and normalizes decimal and hexadecimal references" do
    assert HtmlEntities.decode(
             "&#169 &#x2122; &#X20AC; &#128; &#0; &#xD800; &#1114112; &#x; &#;",
             :text
           ) == "© ™ € € � � � &#x; &#;"
  end

  test "numeric references bound significant digits without rejecting leading zeros" do
    for {digits, base, expected} <- [
          {"00000000000000000000000065", 10, ?A},
          {"00000000000000000000000041", 16, ?A},
          {"00000000000000000000000000", 10, 0xFFFD},
          {"1114111", 10, 0x10FFFF},
          {"10ffff", 16, 0x10FFFF},
          {"110000", 16, 0xFFFD},
          {"99999999", 10, 0xFFFD},
          {"fffffff", 16, 0xFFFD},
          {"128", 10, 0x20AC},
          {"dfff", 16, 0xFFFD}
        ] do
      assert NativeElixirPdfUtilities.Validators.HtmlValidator.numeric_character_reference(
               digits,
               base
             ) == expected
    end
  end

  test "public rendering handles huge numeric references in text and attributes" do
    alias NativeElixirPdfUtilities.HtmlToPdf
    opts = [system_font_discovery: false]
    {:ok, replacement} = HtmlToPdf.render("<p>�</p>", opts)
    {:ok, letter} = HtmlToPdf.render("<p>A</p>", opts)
    {:ok, attribute} = HtmlToPdf.render("<p title='�'>Text</p>", opts)

    for prefix <- ["&#", "&#x"] do
      source = prefix <> String.duplicate("9", 1_900_000) <> ";"
      assert {:ok, ^replacement} = HtmlToPdf.render("<p>" <> source <> "</p>", opts)

      assert {:ok, ^attribute} = HtmlToPdf.render("<p title='" <> source <> "'>Text</p>", opts)
      original = NativeElixirPdfUtilities.Limits.effective()
      NativeElixirPdfUtilities.Limits.install(%{original | max_work: 1})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render("<p title='" <> source <> "'>Text</p>", opts)

      NativeElixirPdfUtilities.Limits.install(original)

      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.stage == :html
      assert diagnostic.message =~ "max_work"

      shorter = prefix <> String.duplicate("9", 100_000) <> ";"
      assert {:ok, ^attribute} = HtmlToPdf.render("<p title='" <> shorter <> "'>Text</p>", opts)
    end

    for {prefix, digits} <- [{"&#", "65"}, {"&#x", "41"}] do
      source = prefix <> String.duplicate("0", 1_900_000) <> digits <> ";"
      assert {:ok, ^letter} = HtmlToPdf.render("<p>" <> source <> "</p>", opts)
    end
  end
end
