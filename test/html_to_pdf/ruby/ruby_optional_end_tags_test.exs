defmodule NativeElixirPdfUtilities.HtmlToPdf.RubyOptionalEndTagsTest do
  use ExUnit.Case, async: false
  import NativeElixirPdfUtilities.HtmlTreeHelpers
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  test "optional ruby group boundaries match explicit trees and retain rtc annotation parent" do
    for {omitted, explicit} <- [
          {"<ruby><rb>A<rb>B<rt>a<rt>b</ruby>",
           "<ruby><rb>A</rb><rb>B</rb><rt>a</rt><rt>b</rt></ruby>"},
          {"<ruby>A<rp>(<rt>a<rp>)</ruby>", "<ruby>A<rp>(</rp><rt>a</rt><rp>)</rp></ruby>"},
          {"<ruby><rb>A<rtc><rt>a<rt>b<rtc>other</ruby>",
           "<ruby><rb>A</rb><rtc><rt>a</rt><rt>b</rt></rtc><rtc>other</rtc></ruby>"},
          {"<ruby><rtc>annotation<rb>B<rt>b</ruby>",
           "<ruby><rtc>annotation</rtc><rb>B</rb><rt>b</rt></ruby>"}
        ] do
      assert {:ok, dom} = HtmlParser.parse_detailed(omitted)
      assert tree_content({:ok, dom}) == tree_content(HtmlParser.parse_detailed(explicit))
    end

    assert {:ok, %{children: [%{children: [rtc]}]}} =
             HtmlParser.parse_detailed("<ruby><rtc><rt>a<rt>b</ruby>")

    assert rtc.tag == "rtc" and Enum.map(rtc.children, & &1.tag) == ["rt", "rt"]
    assert Enum.all?(rtc.children, &(&1.parent_id == rtc.node_id))
  end

  test "ruby start omission uses the existing scope and rtc exclusion rules" do
    for tag <- ~w(rb rt rp), following <- ~w(rb rt rp rtc) do
      assert HtmlValidator.omitted_html_end_tag?(tag, {:start, following}, ["ruby", "body"])

      refute HtmlValidator.omitted_html_end_tag?(tag, {:start, following}, [
               "table",
               "ruby",
               "body"
             ])

      refute HtmlValidator.omitted_html_end_tag?(tag, {:start, following}, [
               "template",
               "ruby",
               "body"
             ])
    end

    assert HtmlValidator.omitted_html_end_tag?("rtc", {:start, "rb"}, ["ruby"])
    assert HtmlValidator.omitted_html_end_tag?("rtc", {:start, "rtc"}, ["ruby"])
    refute HtmlValidator.omitted_html_end_tag?("rtc", {:start, "rt"}, ["ruby"])
    refute HtmlValidator.omitted_html_end_tag?("rtc", {:start, "rp"}, ["ruby"])
    refute HtmlValidator.omitted_html_end_tag?("rt", {:start, "rp"}, ["body"])

    refute HtmlValidator.omitted_html_end_tag?("rt", {:start, "rp"}, [
             {:foreign, :mathml, "mi", %{}},
             "ruby"
           ])

    refute HtmlValidator.omitted_html_end_tag?("rt", {:end, "rt"}, ["ruby"])
  end

  test "optional ruby annotation closure keeps exact authored tokens and required child failures" do
    assert {:ok, %{children: [ruby]}} = HtmlParser.parse_detailed("<ruby>A<rt>a<rp>)</ruby>")
    [_, annotation, fallback] = ruby.children
    assert annotation.source == %{text: "<rt>", line: 1, column: 8}
    assert fallback.source == %{text: "<rp>", line: 1, column: 13}
    assert annotation.node_id != fallback.node_id
    assert annotation.parent_id == ruby.node_id and fallback.parent_id == ruby.node_id

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlParser.parse_detailed("<ruby><rt><strong>Bad<rp>)</ruby>")

    assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
    assert diagnostic.source == "</ruby>" and diagnostic.line == 1 and diagnostic.column == 27
    assert diagnostic.message =~ "unsupported or malformed"

    assert {:error, {:unsupported_html, public}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<ruby><rt><strong>Bad<rp>)</ruby>")

    assert public.operation == :render and public.module == NativeElixirPdfUtilities.HtmlToPdf
    assert public.source == diagnostic.source and public.line == 1 and public.column == 27
  end
end
