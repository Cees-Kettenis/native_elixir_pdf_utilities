defmodule NativeElixirPdfUtilities.HtmlToPdf.UntilFoundInitialTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Forms, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, HtmlVisibilityValidator}

  test "until-found preserves block and dialog frames without painted text fields or resources" do
    html =
      "<div hidden=UNTIL-FOUND id=box style='height:20pt;border:1pt solid red'>Hidden <img src=never.png><input name=child value=Hidden></div><dialog open hidden=until-found style='position:static;height:20pt'>Hidden dialog</dialog><input hidden=until-found name=field value=Hidden style='display:block'><p>Visible</p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert hd(dom.children).children != []

    assert {:ok, styled} =
             Style.compute_detailed(dom,
               forms: :static,
               asset_resolver: fn request -> flunk("skipped resource #{inspect(request)}") end
             )

    [box, dialog, input, _visible] = styled.children

    for node <- [box, dialog, input] do
      assert node.style._html_content_skipped
      assert node.children == []
      refute Map.has_key?(node.style, :_form_control)
    end

    assert box.style.height == 20.0
    assert box.attributes["hidden"] == "UNTIL-FOUND"
    assert input.attributes["value"] == "Hidden"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               asset_resolver: fn request -> flunk("skipped resource #{inspect(request)}") end
             )

    assert {:ok, []} = Forms.fields(pdf)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Visible"
    refute text =~ "Hidden"
  end

  test "inline and contents hosts retain visible children while author visibility overrides defaults" do
    html =
      "<span hidden=until-found>Inline</span><div hidden=until-found style='display:contents'>Contents</div><div hidden=until-found style='content-visibility:visible'>Override</div><p hidden style='display:block'>Display override</p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert Enum.all?(styled.children, &(not Map.has_key?(&1.style, :_html_content_skipped)))
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    for word <- ["Inline", "Contents", "Override", "Display override"], do: assert(text =~ word)
    assert {:ok, blank} = HtmlToPdf.render("<p style='content-visibility:hidden'>Unprinted</p>")
    assert {:error, {:no_extractable_text, _}} = Text.extract(blank)
  end

  test "skipped content excludes generated quotations and source-only CSS remains authoritative" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<style>div::before{content:'Unprinted'}</style><div hidden=until-found>Unprinted</div><q>Visible</q>"
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Visible"
    refute text =~ "Unprinted"

    assert {:ok, source} =
             HtmlParser.parse_detailed(
               "<div hidden=until-found><p style='unknown-property:bad'>Unprinted</p></div>"
             )

    assert {:error, {:invalid_css, diagnostic}} =
             Style.with_source_style_contexts(source, [], HtmlValidator.new_css_budget(), fn _,
                                                                                             _,
                                                                                             _ ->
               :ok
             end)

    assert diagnostic.stage == :css and diagnostic.source == "unknown-property:bad"
  end

  test "qualified and foreign hidden metadata have no HTML hidden semantics" do
    node = %{
      type: :element,
      tag: "p",
      attributes: %{"hidden" => "until-found"},
      attribute_namespaces: %{"hidden" => "urn:other"},
      children: [%{type: :text, text: "Visible"}]
    }

    assert HtmlVisibilityValidator.defaults(node) == %{}
    assert HtmlVisibilityValidator.defaults(Map.put(node, :namespace, :svg)) == %{}

    assert {:ok, %{children: [styled]}} =
             Style.compute_detailed(%{type: :document, children: [node]})

    assert styled.style.display == :block
    refute Map.has_key?(styled.style, :_html_content_skipped)
    assert hd(styled.children).text == "Visible"
  end

  test "flat hidden hosts suppress shadow and slotted painting without changing authored metadata" do
    html =
      "<x-hidden hidden=until-found style='display:block;height:20pt;border:1pt solid red'><span slot=label>Assigned source</span><template shadowrootmode=open><slot name=label></slot><p>Shadow source<img src=never.png></p></template></x-hidden><p>Visible tail</p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    host = hd(dom.children)
    assert hd(host.children).attributes["slot"] == "label"
    assert host.attributes["hidden"] == "until-found"

    assert {:ok, styled} =
             Style.compute_detailed(dom,
               asset_resolver: fn request ->
                 flunk("shadow skipped resource #{inspect(request)}")
               end
             )

    assert hd(styled.children).style._html_content_skipped
    assert hd(styled.children).children == []

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               asset_resolver: fn request ->
                 flunk("shadow skipped resource #{inspect(request)}")
               end
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Visible tail"
    refute text =~ "Assigned source"
    refute text =~ "Shadow source"
  end

  test "final atomic display publishes one canonical skip fact for auxiliary controls" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<progress hidden=until-found value=.5></progress><meter hidden=until-found value=.5></meter>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)

    for control <- styled.children do
      assert control.style.display == :inline_block
      assert control.style._html_content_skipped
      assert control.children == []
      assert :ok = HtmlVisibilityValidator.validate_layout_metadata(control)
    end
  end

  test "contained document boxes disable body background propagation without extending canvas transport" do
    body = %{background_color: {1, 0, 0}, content_visibility: :hidden}

    assert HtmlValidator.prepare_document_canvas(nil, body) == %{
             style: %{},
             body_background_propagated: false
           }

    assert HtmlValidator.prepare_document_canvas(%{content_visibility: :hidden}, %{
             background_color: {1, 0, 0}
           }) == %{style: %{}, body_background_propagated: false}
  end

  test "unsupported automatic skipping and forged style transport retain public diagnostics" do
    assert {:error, {:invalid_css, diagnostic}} =
             HtmlToPdf.render("<div style='content-visibility:auto'>Visible</div>")

    assert diagnostic.stage == :css and diagnostic.reason == :invalid_css
    assert diagnostic.source == "content-visibility:auto" and diagnostic.line == 1
    assert {:ok, dom} = HtmlParser.parse_detailed("<p>Visible</p>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    forged = put_in(styled, [:children, Access.at(0), :style, :_html_content_skipped], true)
    assert {:error, {:invalid_document, diagnostic}} = Layout.layout(forged)
    assert diagnostic.stage == :layout and diagnostic.reason == :invalid_document
    assert diagnostic.source == "p" and diagnostic.message =~ "canonical"
  end
end
