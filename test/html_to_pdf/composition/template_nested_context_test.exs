alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateNestedContextTest do
  use ExUnit.Case, async: false
  Code.require_file(Paths.support("png_fixture.ex"))
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlTree, ResourceContext}
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    TemplateExecutionValidator,
    TemplatePatchValidator
  }

  @opts [system_font_discovery: false, compress_streams: false]

  test "nested matching template remains inert when its outer candidate executes" do
    html =
      nested("<p>Inner replacement</p>")
      |> String.replace("<template", "<TeMpLaTe")
      |> String.replace("</template>", "</TeMpLaTe>")

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Inner original" and text =~ "Outer after" and text =~ "Document after"
    refute text =~ "Inner replacement"
    assert Process.get({TemplatePatchValidator, :candidate_capture}) == nil
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "invalid nested inert CSS keeps its exact original declaration diagnostic" do
    html = nested("<p style='width:bananas'>Inner invalid</p>")
    assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.stage == :css and diagnostic.operation == :source_style_contexts
    assert diagnostic.source == "width:bananas"
    assert diagnostic.line == 1
    assert diagnostic.column == byte_size(hd(String.split(html, "width:bananas"))) + 1
    assert Process.get({TemplatePatchValidator, :candidate_capture}) == nil
  end

  test "nested inert resources still require original authorization and bytes" do
    html = nested("<img src='missing-nested.png'>")
    assert {:error, {:invalid_document, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.stage == :asset and diagnostic.source == "missing-nested.png"
    assert diagnostic.operation == :render
    assert diagnostic.message =~ "not an authorized local file"
    assert Process.get({TemplatePatchValidator, :candidate_capture}) == nil
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "nested original glyph failures are retained before outer content can replace" do
    html = nested("<p>&#x10FFFF;</p>")

    assert {:error, {:unsupported_glyph, diagnostic}} =
             HtmlToPdf.render(html, Keyword.put(@opts, :unsupported_glyphs, :error))

    assert diagnostic.stage == :font and diagnostic.operation == :resolve_fonts
    assert diagnostic.source == <<0x10FFFF::utf8>>
    assert diagnostic.message =~ "U+10FFFF"
    assert Process.get({TemplatePatchValidator, :candidate_capture}) == nil
  end

  test "normal resource callbacks cannot borrow the candidate parse admission" do
    html = nested("<img src='nested.png'>")
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    resolver = fn _ ->
      assert Process.get({TemplatePatchValidator, :candidate_capture}) == nil

      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlParser.parse_document_detailed(
                 "<?marker name='B'><template for='B'><p>Forged</p></template>"
               )

      assert diagnostic.module == TemplatePatchValidator and diagnostic.operation == :parse
      assert diagnostic.message =~ "original_source_policy"
      {:ok, png}
    end

    assert {:ok, _} = HtmlToPdf.render(html, Keyword.put(@opts, :asset_resolver, resolver))
    assert Process.get({TemplatePatchValidator, :candidate_capture}) == nil
  end

  test "a passive capture marker cannot authorize execution and cleans after an exception" do
    prefix = "<body><?marker name='A'>"
    token = "<template for='A'>"
    {:ok, dom} = HtmlParser.parse_document_detailed(prefix <> "</body>")
    [html] = dom.children
    [_, body] = html.children
    tree = HtmlTree.from_document(dom)
    source = prefix <> token
    owner = tree.nodes[tree.root]

    tree = %{
      tree
      | nodes:
          Map.put(tree.nodes, tree.root, %{owner | source: %{text: source, line: 1, column: 1}})
    }

    origin = %{
      source: %{text: token, line: 1, column: byte_size(prefix) + 1},
      offset: byte_size(prefix),
      tokenizer_context: "body",
      ancestors: []
    }

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      TemplatePatchValidator.with_parser_invocation(source, fn _ ->
        TemplatePatchValidator.with_insertion_context(
          tree,
          body.node_id,
          "A",
          origin,
          fn handle ->
            TemplatePatchValidator.with_planned_insertion(handle, tree, fn _ ->
              assert_raise RuntimeError, "capture callback", fn ->
                TemplatePatchValidator.with_candidate_input(handle, tree, fn _ ->
                  assert Process.get({TemplatePatchValidator, :candidate_capture}).handle ==
                           handle

                  refute TemplateExecutionValidator.inert_source_view?(
                           %{"for" => "B"},
                           {tree, body.node_id, origin}
                         )

                  assert {:error, {:unsupported_html, diagnostic}} =
                           TemplatePatchValidator.authorize_mutation(handle, tree)

                  assert diagnostic.message =~ "original_source_policy"
                  raise "capture callback"
                end)
              end

              assert Process.get({TemplatePatchValidator, :candidate_capture}) == nil
            end)
          end
        )
      end)
    end)

    assert Process.get({TemplatePatchValidator, :candidate_capture}) == nil
  end

  defp nested(content) do
    "<body><div><?marker name='A'></div><template for='A'><section><?start name='B'><p>Inner original</p><?end?></section><template for='B'>#{content}</template><p>Outer after</p></template><p>Document after</p></body>"
  end
end
