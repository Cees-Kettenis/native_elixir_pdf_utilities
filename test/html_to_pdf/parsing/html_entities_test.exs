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
end
