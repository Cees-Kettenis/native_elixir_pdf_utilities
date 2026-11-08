defmodule NativeElixirPdfUtilities.Validators.CssStylesheetValidatorTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Validators.CssStylesheetValidator
  alias NativeElixirPdfUtilities.TestSupport.CssStyles

  test "invalid layer names and scope preludes recover without leaking their body" do
    for source <- [
          "@layer initial{p{color:red}}",
          "@layer InHeRiT{p{color:red}}",
          "@layer a,b{p{color:red}}",
          "@layer a..b{p{color:red}}",
          "@scope invalid{p{color:red}}",
          "@scope (.root) bad (.limit){p{color:red}}",
          "@scope bad (.limit){p{color:red}}",
          "@scope (::before){p{color:red}}",
          "@scope (::part(label)){p{color:red}}",
          "@scope (::slotted(span)){p{color:red}}",
          "@scope (:unknown){p{color:red}}",
          "@scope (.root) to (:unknown){p{color:red}}",
          "@unknown{p{color:red}}",
          "@!bad{p{color:red}}",
          "broken;",
          "p:unknown{color:red}"
        ] do
      assert {:ok, %{rules: []}} = CssStylesheetValidator.prepare(source)
    end
  end

  test "conditional nesting empty rules opaque tokens and scope limits retain source order" do
    source = """
    @layer a,b;
    @layer a {p{}}
    @supports (color:red) { .outer {--opaque:{ x:y }; color:blue; & span[data-mark='&']{color:red} } }
    @scope to (.stop) { p {color:green} }
    """

    assert {:ok, prepared} = CssStylesheetValidator.prepare(source)
    assert length(prepared.rules) == 4
    assert hd(prepared.rules).declarations == ""
    assert Enum.at(prepared.rules, 1).condition_active
    assert Enum.at(prepared.rules, 2).selectors =~ "[data-mark='&']"
    assert [%{roots: nil, limits: [_]}] = List.last(prepared.rules).scopes

    nodes =
      CssStyles.by_id(
        "<div><style>@scope to (.stop){p{color:blue}}</style><p id='a'>Text</p><div class='stop'><p id='b'>Text</p></div></div>"
      )

    assert nodes["a"].color == {0, 0, 1}
    assert nodes["b"].color == {0, 0, 0}
  end

  test "inactive definitions and style-nested registrations cannot change an active document" do
    nodes =
      CssStyles.by_id("<p id='a' style='width:var(--size,3pt)'>Text</p>", [
        """
        @supports (color:bad) {@property --size{syntax:'<length>';inherits:false;initial-value:8pt}}
        p {@property --size{syntax:'<length>';inherits:false;initial-value:9pt}}
        """
      ])

    assert nodes["a"].width == 3.0
  end

  test "source preparation retains active definitions and discards inactive registrations and layer events" do
    assert {:ok, prepared} =
             NativeElixirPdfUtilities.HtmlToPdf.CssParser.prepare_source_stylesheet("""
             @layer first,last;
             @property --value{syntax:'<number>';inherits:false;initial-value:1}
             @media screen{@layer inactive;@property --value{syntax:'<number>';inherits:false;initial-value:2}}
             @scope { [data-mark='value'] {color:blue} }
             """)

    assert prepared.cascade.registrations["--value"].initial == "1.0"
    assert Enum.map(prepared.cascade.layers, & &1.path) == [["first"], ["last"]]
  end
end
