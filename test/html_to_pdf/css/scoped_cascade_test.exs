defmodule NativeElixirPdfUtilities.HtmlToPdf.ScopedCascadeTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.CssStyles

  # Cascade 6, 6 September 2024, section 2.5; Nesting 1, 22 January 2026, section 3.3.
  test "roots limits and re-entered roots constrain only the selected subjects" do
    nodes =
      CssStyles.by_id(
        """
        <div id='root' class='scope'><p id='inside'>Text</p>
          <div class='limit'><p id='outside'>Text</p>
            <div class='scope'><p id='reentry'>Text</p></div>
          </div>
        </div><p id='unscoped'>Text</p>
        """,
        ["@scope (.scope) to (.limit) { p {color:blue} :scope {font-style:italic} }"]
      )

    assert nodes["inside"].color == {0, 0, 1}
    assert nodes["outside"].color == {0, 0, 0}
    assert nodes["reentry"].color == {0, 0, 1}
    assert nodes["unscoped"].color == {0, 0, 0}
    assert nodes["root"].font_style == :italic
  end

  test "specificity precedes scope proximity which precedes source order" do
    nodes =
      CssStyles.by_id(
        "<div class='outer'><div class='inner'><p id='a'>Text</p><p id='b'>Text</p></div></div>",
        [
          """
          @scope (.inner) {p {color:blue}}
          @scope (.outer) {p {color:red}}
          p {color:purple}
          #b {color:green}
          """
        ]
      )

    assert nodes["a"].color == {0, 0, 1}
    assert nodes["b"].color == {0, 0.5019607843, 0}
  end

  test "an implicit scope belongs to its own style owner's parent" do
    nodes =
      CssStyles.by_id("""
      <div><style>@scope {p {color:blue}}</style><p id='inside'>Text</p></div>
      <p id='outside'>Text</p>
      """)

    assert nodes["inside"].color == {0, 0, 1}
    assert nodes["outside"].color == {0, 0, 0}
  end

  test "nested scopes interpret roots and limits against the surrounding scope" do
    nodes =
      CssStyles.by_id(
        """
        <div class='outer'><p id='outer'>Text</p>
          <div class='inner'><p id='inner'>Text</p><div class='limit'><p id='limited'>Text</p></div></div>
        </div><div class='inner'><p id='outside'>Text</p></div>
        """,
        ["@scope (.outer) {@scope (:scope > .inner) to (> .limit) {p {color:blue}}}"]
      )

    assert nodes["inner"].color == {0, 0, 1}
    assert nodes["outer"].color == {0, 0, 0}
    assert nodes["limited"].color == {0, 0, 0}
    assert nodes["outside"].color == {0, 0, 0}
  end

  test "explicit scope selectors can reference an outside ancestor but implicit descendants cannot" do
    nodes =
      CssStyles.by_id("<div class='scope' id='root'><p id='a'>Text</p></div>", [
        """
        @scope (.scope) {body p {color:red}.scope{color:red}body :scope > p {color:blue}}
        """
      ])

    assert nodes["root"].color == {0, 0, 0}
    assert nodes["a"].color == {0, 0, 1}
  end

  test "scope nesting selectors have zero specificity and use the current root" do
    nodes =
      CssStyles.by_id("<div id='root'><p id='a'>Text</p></div>", [
        """
        @scope (#root) { :scope > p {color:blue} & > p {color:red} }
        """
      ])

    assert nodes["a"].color == {0, 0, 1}
  end

  test "a scope nested in a style rule uses the style selector only for its start" do
    nodes =
      CssStyles.by_id(
        "<div class='parent'><section class='scope'><p id='a'>Text</p><div class='limit'><p id='b'>Text</p></div></section></div>",
        [
          """
          .parent { @scope (& > .scope) to (& .limit) { & > p {color:blue} } }
          """
        ]
      )

    assert nodes["a"].color == {0, 0, 1}
    assert nodes["b"].color == {0, 0, 0}
  end

  test "implicit shadow scopes can style their host without leaking into the document" do
    nodes =
      CssStyles.by_id(
        """
        <div id='host'><template shadowrootmode='open'>
          <style>@scope {:scope {font-style:italic}p{color:blue}}</style>
          <p id='shadow'>Text</p>
        </template></div><p id='outside'>Text</p>
        """,
        ["@scope (#host) {p{color:red}}"]
      )

    assert nodes["host"].font_style == :italic
    assert nodes["shadow"].color == {0, 0, 1}
    assert nodes["outside"].color == {0, 0, 0}
  end

  test "scoped part and slot rules retain the exposing author tree" do
    nodes =
      CssStyles.by_id(
        """
        <div class='outer'><div id='host'><span id='light' slot='label'>Light</span>
          <template shadowrootmode='open'>
            <style>@scope (:host) {slot::slotted(span){color:green}}</style>
            <slot name='label'></slot><span part='label' id='part'>Part</span>
          </template>
        </div></div><p id='outside'>Text</p>
        """,
        ["@scope (.outer){#host::part(label){color:blue}}"]
      )

    assert nodes["light"].color == {0, 0.5019607843, 0}
    assert nodes["part"].color == {0, 0, 1}
    assert nodes["outside"].color == {0, 0, 0}
  end

  test "scope roots and limits retain inherited language selection" do
    nodes =
      CssStyles.by_id(
        """
        <div lang='en'><section><p id='selected'>Text</p>
          <div lang='fr'><p id='limited'>Text</p></div>
        </section></div>
        """,
        ["@scope (:lang(en)) to (:lang(fr)) {p{color:blue}}"]
      )

    assert nodes["selected"].color == {0, 0, 1}
    assert nodes["limited"].color == {0, 0, 0}
  end

  test "implicit external scopes use the document root and select the nearest matching root" do
    nodes =
      CssStyles.by_id("<div class='root'><div class='root'><p id='a'>Text</p></div></div>", [
        "@scope {p {font-style:italic}} @scope (.root){:scope > p {color:blue}}"
      ])

    assert nodes["a"].color == {0, 0, 1}
    assert nodes["a"].font_style == :italic
  end
end
