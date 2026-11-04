defmodule NativeElixirPdfUtilities.HtmlToPdf.SourceStyleDiagnosticsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{AssetLoader, Font, HtmlParser, Style}

  alias NativeElixirPdfUtilities.Validators.{
    CssValueValidator,
    HtmlCssSourceValidator,
    HtmlValidator
  }

  test "original-context policy callback preserves its shared failure and exact declaration origin" do
    traced = [
      {Font, :load_registry, 1},
      {Font, :resolve, 4},
      {AssetLoader, :resolve, 4},
      {File, :read, 1},
      {File, :stat, 1},
      {File, :lstat, 1}
    ]

    Enum.each(traced, fn {module, _, _} -> Code.ensure_loaded!(module) end)
    Enum.each(traced, fn mfa -> assert :erlang.trace_pattern(mfa, true, [:local]) == 1 end)

    task =
      Task.async(fn ->
        receive do
          :validate ->
            html = "<div>🙂\r\n  <p style='width:3pt'>Original</p></div>"
            assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
            assert {:ok, provenance} = HtmlCssSourceValidator.prepare(dom)
            parent = self()

            failure =
              Diagnostics.error(:font, :unsupported_html, "selected font policy is unresolved",
                operation: :selected_font_policy,
                source: "unselected registry",
                line: 99,
                column: 88
              )

            result =
              Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn node,
                                                                                           pseudo,
                                                                                           context ->
                if node.tag == "p" and pseudo == nil do
                  assert context.status == :resolved
                  assert context.style.width == 3.0
                  assert context.style.font_face == nil
                  assert context.style._font_registry == nil
                  [declaration] = provenance.records[node.node_id]
                  send(parent, {:original_context, node.node_id, declaration.source})
                  HtmlValidator.source_style_error(node, declaration, failure)
                else
                  :ok
                end
              end)

            assert {:error, {:unsupported_html, diagnostic}} = result
            assert_receive {:original_context, node_id, source}
            assert is_reference(node_id)
            assert source.text == "width:3pt" and source.line == 2 and source.column == 13
            assert diagnostic.reason == :unsupported_html
            assert diagnostic.message == "selected font policy is unresolved"
            assert diagnostic.stage == :css
            assert diagnostic.module == HtmlValidator
            assert diagnostic.operation == :source_style_contexts
            assert diagnostic.source == "width:3pt"
            assert diagnostic.line == 2 and diagnostic.column == 13
            refute Map.has_key?(diagnostic, :certificate)
            refute Map.has_key?(diagnostic, :source_obligations)
            :ok
        end
      end)

    try do
      :erlang.trace(task.pid, true, [:call])
      send(task.pid, :validate)
      assert :ok = Task.await(task)
      refute_receive {:trace, _, :call, _}
    after
      Enum.each(traced, &:erlang.trace_pattern(&1, false, [:local]))
    end
  end

  test "structured grid failure preserves original declaration context through the shared source cascade" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    html = "<div>🙂\r\n  <p style='grid-template-columns:repeat(2, 1fr)'>Original</p></div>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, provenance} = HtmlCssSourceValidator.prepare(dom)
    budget = HtmlValidator.new_css_budget()

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Style.with_source_style_contexts(dom, [], budget, fn node, pseudo, context ->
               if node.tag == "p" and pseudo == nil do
                 assert context.status == :resolved
                 assert context.style.font_face == nil
                 assert context.style._font_registry == nil
                 [declaration] = provenance.records[node.node_id]
                 assert declaration.source.text == "grid-template-columns:repeat(2, 1fr)"
                 assert declaration.source.line == 2 and declaration.source.column == 13
                 Limits.install(Map.put(limits, :max_layout_cardinality, 1))

                 CssValueValidator.validate_source_cascade(
                   context.style,
                   [declaration],
                   budget,
                   node
                 )
               else
                 :ok
               end
             end)

    assert diagnostic.stage == :css
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message == "grid track count exceeds the 1-item limit"
    assert diagnostic.module == HtmlValidator
    assert diagnostic.operation == :source_style_contexts
    assert diagnostic.source == "grid-template-columns:repeat(2, 1fr)"
    assert diagnostic.line == 2 and diagnostic.column == 13
    refute Map.has_key?(diagnostic, :certificate)
  end
end
