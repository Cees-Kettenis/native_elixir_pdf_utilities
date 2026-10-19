alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateSourceRuntimeContextTest do
  use ExUnit.Case, async: false
  Code.require_file(Paths.support("png_fixture.ex"))
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlTree, Style}
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  alias NativeElixirPdfUtilities.Validators.{
    TemplateExecutionValidator,
    TemplateRendererContextValidator
  }

  @opts [system_font_discovery: false, compress_streams: false]

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "removed original MathML relative and scoped variable declarations execute with selected metrics" do
    html =
      "<body><?start name='A'><math style='--size:2rem;font-size:18px'><mspace style='width:var(--size);height:2px'></mspace></math><?end?><template for='A'><p>Good</p></template></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, reference} = HtmlToPdf.render("<p>Good</p>", @opts)
    assert pdf == reference
    assert {:ok, "Good"} = Text.extract(pdf)
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
  end

  test "original inherited stylesheet variables and percentage font sizes complete before removal" do
    css =
      "body{--length:2rem}math{font-size:18px}mspace{width:var(--length);height:2px}mstyle{font-size:125%}"

    html =
      "<style>#{css}</style><body><?start name='A'><math><mspace></mspace><mstyle style='font-size:125%'></mstyle></math><?end?><template for='A'><p>Good</p></template></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, expected} = HtmlToPdf.render("<p>Good</p>", @opts)
    assert pdf == expected
  end

  test "original detached MathML percentage and custom-property checks keep their exact scope" do
    html =
      "<body><?marker name='A'><template for='A'><math style='--size:2em;font-size:18px'><mstyle style='font-size:125%'><mspace style='width:var(--size);height:2px;background-color:blue'></mspace></mstyle></math></template></body>"

    reference =
      "<body><math style='--size:2em;font-size:18px'><mstyle style='font-size:125%'><mspace style='width:var(--size);height:2px;background-color:blue'></mspace></mstyle></math></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, expected} = HtmlToPdf.render(reference, @opts)
    assert pdf == expected
    assert {:error, {:no_extractable_text, diagnostic}} = Text.extract(pdf)
    assert diagnostic.stage == :text_extraction
  end

  test "runtime source observations retain exact original owners and resist resource callback reentry" do
    html =
      "<body><?start name='A'><math style='--size:2rem'><mspace style='width:var(--size);height:2px'></mspace></math><?end?><template for='A'><img src='proof.png'></template></body>"

    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    {:ok, foreign} =
      HtmlParser.parse_document_detailed(
        "<math style='--size:999px'><mspace style='width:var(--size)'></mspace><mi>Foreign</mi></math>"
      )

    resolver = fn _ ->
      frame = Process.get({TemplateExecutionValidator, :execution})

      if frame.mode == :original do
        records =
          Enum.filter(
            frame.declaration_contexts,
            &(&1.source.text in ["--size:2rem", "width:var(--size)"])
          )

        assert length(records) == 2

        for record <- records do
          assert record.status == :checked_in_original_context
          assert record.source.provenance == :original_html
          assert [%{execution: :original_runtime} = context] = record.contexts
          assert context.owner.node_id == record.source.owner.node_id

          assert binary_part(html, record.source.offset, record.source.length) ==
                   record.source.text
        end

        preparation = Process.get({TemplateRendererContextValidator, :preparation})

        assert {:error, {:invalid_document, denied}} =
                 TemplateRendererContextValidator.with_original_source_execution(
                   foreign,
                   %{},
                   frame.css_budget,
                   fn -> flunk("foreign source runtime scope admitted") end
                 )

        assert denied.operation == :original_runtime_source and
                 denied.source == "original runtime scope"

        prior = preparation.cascades
        assert preparation.original_css_budget == frame.css_budget
        used = :atomics.get(frame.css_budget, 1)
        assert {:ok, _} = Style.compute_detailed(foreign, @opts)
        assert Process.get({TemplateRendererContextValidator, :preparation}).cascades == prior

        assert Process.get({TemplateExecutionValidator, :execution}).declaration_contexts ==
                 frame.declaration_contexts

        assert :atomics.get(frame.css_budget, 1) == used
        send(self(), :verified_original_runtime_context)
      end

      {:ok, png}
    end

    assert {:ok, _} = HtmlToPdf.render(html, Keyword.put(@opts, :asset_resolver, resolver))
    assert_received :verified_original_runtime_context
  end

  test "resource callback cannot reopen the exact enclosing original source authority" do
    html =
      "<body><?start name='A'><p style='font-size:20pt;width:50%'>Original</p><?end?><template for='A'><img src='proof.png'></template></body>"

    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    resolver = fn _ ->
      frame = Process.get({TemplateExecutionValidator, :execution})
      preparation = Process.get({TemplateRendererContextValidator, :preparation})

      if frame.mode == :original and preparation[:original_css_budget] do
        dom = HtmlTree.fetch(frame.original, frame.original.root)
        prior_budget = :atomics.get(frame.css_budget, 1)

        assert {:error, {:invalid_document, diagnostic}} =
                 TemplateRendererContextValidator.with_original_source_execution(
                   dom,
                   frame.source_provenance,
                   frame.css_budget,
                   fn -> flunk("callback reopened its enclosing original source authority") end
                 )

        assert diagnostic.operation == :original_runtime_source
        assert diagnostic.source == "original runtime scope"
        assert diagnostic.message =~ "own issued resource scope"
        assert Process.get({TemplateRendererContextValidator, :preparation}) == preparation
        assert Process.get({TemplateExecutionValidator, :execution}) == frame
        assert :atomics.get(frame.css_budget, 1) == prior_budget
        assert TemplateRendererContextValidator.original_css_budget() == nil

        nested =
          "<body><?marker name='N'><template for='N'><p style='font-size:10pt;width:2em'>Independent</p></template></body>"

        assert {:ok, nested_pdf} =
                 HtmlToPdf.render(nested, Keyword.put(@opts, :page_size, {200, 160}))

        assert {:ok, "Independent"} = Text.extract(nested_pdf)
        assert Process.get({TemplateRendererContextValidator, :preparation}) == preparation
        assert Process.get({TemplateExecutionValidator, :execution}) == frame
        assert :atomics.get(frame.css_budget, 1) == prior_budget
        send(self(), :denied_enclosing_original_source)
      end

      {:ok, png}
    end

    assert {:ok, _pdf} = HtmlToPdf.render(html, Keyword.put(@opts, :asset_resolver, resolver))
    assert_received :denied_enclosing_original_source
    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "unmatched and inactive source stays symbolic while MathML owners execute independently" do
    for css <- [
          ".absent{width:2rem}",
          "@media screen{mi{font-size:125%}}",
          ".absent{--size:2rem;width:var(--size)}"
        ] do
      html =
        "<style>#{css}</style><body><?start name='A'><math><mspace width='2em'></mspace></math><?end?><template for='A'><p>Good</p></template></body>"

      assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
      assert {:ok, expected} = HtmlToPdf.render("<p>Good</p>", @opts)
      assert pdf == expected
      assert {:ok, "Good"} = Text.extract(pdf)
    end
  end

  test "original MathML text completes using its source-bound selected glyph" do
    html =
      "<body><?start name='A'><math style='--size:2rem'><mspace style='width:var(--size);height:2px'></mspace><mi>x</mi></math><?end?><template for='A'><p>Good</p></template></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, expected} = HtmlToPdf.render("<p>Good</p>", @opts)
    assert pdf == expected
    assert {:ok, "Good"} = Text.extract(pdf)
  end

  test "invalid original scoped variables still fail before replacement and preserve their source" do
    html =
      "<body><?start name='A'><math><mspace style='width:var(--missing);height:2px'></mspace><mi>x</mi></math><?end?><template for='A'><p>Good</p></template></body>"

    assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.stage == :css
    assert diagnostic.source == "width:var(--missing)"
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
  end

  test "nested head target diagnostics preserve original CRLF and grapheme coordinates" do
    for html <- [
          "<style>.unmatched{width:3px}</style><?marker name='A'><template for='A'><p>Good</p></template>",
          "<style>/*🙂é*/.unmatched{width:3px}</style>\r\n<?marker name='A'><template for='A'>\r\n  <p>Good</p></template>"
        ] do
      prefix = hd(String.split(html, "<p>"))
      lines = String.split(prefix, ["\r\n", "\n", "\r"])
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, @opts)
      assert diagnostic.source == "<p>" and diagnostic.stage == :html
      assert diagnostic.line == length(lines)
      assert diagnostic.column == String.length(List.last(lines)) + 1
      refute diagnostic.message =~ "unmatched_original_context"
    end
  end
end
