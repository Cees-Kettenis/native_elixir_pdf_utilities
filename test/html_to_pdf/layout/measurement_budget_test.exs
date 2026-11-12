Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.MeasurementBudgetTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    HtmlParser,
    Layout,
    Pagination,
    ResourceContext,
    Style
  }

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlValidator}

  test "admitted initial percentages use the initial width and font-unit failures retain their diagnostic" do
    alias NativeElixirPdfUtilities.HtmlToPdf.InitialLetter
    alias NativeElixirPdfUtilities.Validators.{CssInitialLetterValidator, CssLengthValidator}

    assert {:ok, dom} =
             HtmlParser.parse(
               "<style>p{font:12pt/16pt serif}p::first-letter{initial-letter:3}</style><p>Alphabet text</p>"
             )

    assert {:ok, %{children: [parent]}} =
             Style.compute_detailed(dom, system_font_discovery: false)

    prepare = fn style ->
      node = put_in(parent.style._first_letter_style, style)

      FontValidator.with_budget(fn ->
        HtmlValidator.with_render_budget(fn ->
          InitialLetter.prepare_runs(node.children, node.style, fn _, _, _, fallback ->
            fallback
          end)
        end)
      end)
    end

    assert {:ok, [ordinary | _]} = prepare.(parent.style._first_letter_style)
    percent = Map.put(parent.style._first_letter_style, :initial_letter_wrap, {:percent, 0.2})
    assert :ok = CssInitialLetterValidator.validate_style(percent, "p::first-letter")
    assert {:ok, [expanded | _]} = prepare.(percent)

    assert_in_delta expanded._inline_content_width - ordinary._inline_content_width,
                    ordinary._initial_letter_box.width * 0.2,
                    0.0001

    # The percentage denominator becomes zero only at the actual initial width.
    source = "calc(1pt / (100% / 1pt - #{ordinary._initial_letter_box.width}))"
    assert {:ok, singular} = CssLengthValidator.prepare(source, :margin)
    singular_style = Map.put(parent.style._first_letter_style, :initial_letter_wrap, singular)
    assert :ok = CssInitialLetterValidator.validate_style(singular_style, "p::first-letter")
    assert {:error, {:resource_limit_exceeded, calculation_diagnostic}} = prepare.(singular_style)
    assert calculation_diagnostic.stage == :layout
    assert calculation_diagnostic.source == source
    assert calculation_diagnostic.message =~ "finite layout numeric range"

    assert {:ok, calculation} = CssLengthValidator.prepare("calc(10% + 1ex)", :margin)

    unmeasurable =
      Map.merge(parent.style._first_letter_style, %{
        initial_letter_wrap: calculation,
        font_face: NativeElixirPdfUtilities.TestSupport.OpenTypeFixture.font()
      })

    assert :ok = CssInitialLetterValidator.validate_style(unmeasurable, "p::first-letter")
    assert {:error, {reason, diagnostic}} = prepare.(unmeasurable)
    assert reason == diagnostic.reason and diagnostic.stage == :font
    assert diagnostic.message =~ "glyf"
  end

  test "initial grid and percentage wrapping preserve late calculation budget failures" do
    alias NativeElixirPdfUtilities.HtmlToPdf.InitialLetter
    alias NativeElixirPdfUtilities.TestSupport.ResourceBudget
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    for {wrap, limit} <- [{"grid", :max_work}, {"calc(100000 * 100%)", :max_numeric_magnitude}] do
      Limits.install(limits)

      html =
        "<style>p{font:12pt/16pt serif;margin:0}p::first-letter{initial-letter:3;initial-letter-wrap:#{wrap}}</style><p>Alphabet text</p>"

      assert {:ok, dom} = HtmlParser.parse(html)

      assert {:ok, %{children: [parent]}} =
               Style.compute_detailed(dom, system_font_discovery: false)

      operation = fn ->
        FontValidator.with_budget(fn ->
          HtmlValidator.with_render_budget(fn ->
            InitialLetter.prepare_runs(parent.children, parent.style, fn _, _, _, fallback ->
              fallback
            end)
          end)
        end)
      end

      assert {:ok, _} = operation.()
      assert {{:ok, _}, work} = ResourceBudget.measure(:max_work, operation)
      maximum = if limit == :max_work, do: work - 1, else: 100_000
      Limits.install(Map.put(limits, limit, maximum))
      assert {:error, {:resource_limit_exceeded, diagnostic}} = operation.()
      assert diagnostic.stage in [:style, :layout] and diagnostic.message =~ Atom.to_string(limit)
    end
  end

  test "inline metric and text-width consumers preserve selected-font failures" do
    alias NativeElixirPdfUtilities.TestSupport.{OpenTypeFixture, PngFixture}
    font = OpenTypeFixture.font() |> Map.update!(:cmap, &Map.put(&1, ?x, 1))
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    for {kind, html} <- [
          {:outline,
           "<p>X<span style='alignment-baseline:middle;baseline-shift:1pt'>A</span></p>"},
          {:outline,
           "<p>X<span style='display:inline-block;alignment-baseline:middle;baseline-shift:1pt'>A</span></p>"},
          {:outline, "<p>X<input type=color style='vertical-align:middle'></p>"},
          {:outline, "<p>X<img src='asset:dot' style='vertical-align:middle'></p>"},
          {:width, "<p>ABC</p>"},
          {:width, "<p>A<span style='font-weight:bold'>B</span>C</p>"},
          {:width, "<table><tr><td>A</td></tr></table>"}
        ] do
      assert {:ok, dom} = HtmlParser.parse(html)

      assert {:ok, styled} =
               Style.compute_detailed(dom,
                 system_font_discovery: false,
                 assets: %{"asset:dot" => {:bytes, png}}
               )

      selected = if kind == :width, do: Map.put(font, :shaping_language, 123), else: font

      replace = fn replace, node ->
        node =
          if Map.has_key?(node, :style), do: put_in(node.style.font_face, selected), else: node

        if Map.has_key?(node, :children),
          do:
            Map.update!(node, :children, &Enum.map(&1, fn child -> replace.(replace, child) end)),
          else: node
      end

      assert {:error, {reason, diagnostic}} = Layout.layout(replace.(replace, styled))
      assert reason == diagnostic.reason and diagnostic.stage == :font
      assert is_atom(diagnostic.operation) and diagnostic.message != ""
    end
  end

  test "orthogonal used sizing retains errors after the styled tree has been admitted" do
    alias NativeElixirPdfUtilities.HtmlToPdf.WritingModeLayout
    alias NativeElixirPdfUtilities.Validators.CssIntrinsicSizingValidator
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    assert {:ok, dom} = HtmlParser.parse("<span style='writing-mode:vertical-rl'>AB</span>")
    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, system_font_discovery: false)

    assert {:ok, intrinsic} =
             CssIntrinsicSizingValidator.prepare("calc-size(max-content, size * 100)")

    assert {:ok, calculated} =
             NativeElixirPdfUtilities.Validators.CssLengthValidator.prepare(
               "calc(100 * 1em)",
               :margin
             )

    Limits.install(%{limits | max_numeric_magnitude: 1000})

    for size <- [intrinsic, calculated] do
      node = Map.update!(node, :style, &Map.put(&1, :height, size))

      assert {:render_resource_limit, {:error, {:resource_limit_exceeded, diagnostic}}} =
               catch_throw(
                 WritingModeLayout.layout(
                   node,
                   0,
                   0,
                   200,
                   fn _, _, _, _ -> flunk("failed used size reached child layout") end,
                   fn _, _ -> 40.0 end
                 )
               )

      assert diagnostic.message =~ "max_numeric_magnitude"
    end
  end

  test "orthogonal calculated intrinsic sizes retain the common numeric magnitude ceiling" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    assert {:ok, dom} =
             HtmlParser.parse(
               "<span style='writing-mode:vertical-rl;inline-size:calc-size(max-content, size * 100);font:10pt monospace'>AB CD</span>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    Limits.install(%{limits | max_numeric_magnitude: 1000})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Layout.layout(styled, page_size: {200, 180}, margin: 0)

    assert diagnostic.reason == :resource_limit_exceeded and is_atom(diagnostic.stage)
    assert diagnostic.message =~ "max_numeric_magnitude"
  end

  test "calculated indentation and tab stops retain numeric resolution diagnostics" do
    alias NativeElixirPdfUtilities.HtmlToPdf.TextSpacing
    alias NativeElixirPdfUtilities.Validators.{CssLengthValidator, CssWhiteSpaceValidator}
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    assert {:ok, calculated} = CssLengthValidator.prepare("calc(100 * 1em)", :margin)
    Limits.install(%{limits | max_numeric_magnitude: 1000})
    style = %{font_size: 12, text_indent: %{length: calculated, hanging: false, each_line: false}}

    for operation <- [
          fn -> TextSpacing.start_line([%{text: "A", style: style}], 200) end,
          fn ->
            CssWhiteSpaceValidator.tab_advance(
              Map.put(style, :tab_size, {:length, calculated}),
              0,
              6
            )
          end
        ] do
      assert {:render_resource_limit, {:error, {:resource_limit_exceeded, diagnostic}}} =
               catch_throw(operation.())

      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.message =~ "max_numeric_magnitude"
      assert is_atom(diagnostic.stage) and is_atom(diagnostic.module)
    end
  end

  test "measurement reports exhausted paint budgets locally and preserves subsequent document work" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    assert :checked_inside_issued_document =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
               assert {:ok, dom} =
                        HtmlParser.parse_detailed("<span>Measured <b>content</b></span>")

               assert {:ok, %{children: [node]}} =
                        Style.compute_detailed(dom, [], context.image_budget)

               before = ResourceContext.current()
               Limits.install(Map.put(limits, :max_entries, 1))

               assert {:error, {:resource_limit_exceeded, diagnostic}} =
                        Layout.measure_child(node, context)

               assert diagnostic.stage in [:layout, :font]
               assert diagnostic.reason == :resource_limit_exceeded
               assert diagnostic.message =~ "max_entries"
               assert ResourceContext.current() == before

               Limits.install(limits)
               assert {:ok, child} = Layout.measure_child(node, context)
               assert Enum.map_join(child.boxes, &Map.get(&1, :text, "")) == "Measured content"
               assert ResourceContext.current() == before
               :checked_inside_issued_document
             end)

    assert ResourceContext.current() == nil
  end

  test "native inline and table measurements retain exhausted font-work diagnostics" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    for html <- [
          "<math><mi>x</mi></math>",
          "<ruby>Base<rt>Reading</rt></ruby>",
          "<span style='display:inline-block'><ruby>Base<rt>Reading</rt></ruby></span>",
          "<table><tr><td style='vertical-align:baseline'><ruby>Base<rt>Reading</rt></ruby></td><td>Peer</td></tr></table>"
        ] do
      Limits.install(limits)

      ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
        assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

        assert {:ok, styled} =
                 Style.compute_detailed(dom, [system_font_discovery: false], context.image_budget)

        # Retain the work actually spent preparing this source. The short
        # remaining allowance fails while measuring its real native artwork.
        used =
          limits.max_work -
            NativeElixirPdfUtilities.Validators.ResourceBudgetValidator.remaining(:max_work)

        Limits.install(%{limits | max_work: used + 7})

        assert {:error, {:resource_limit_exceeded, diagnostic}} = Layout.layout(styled)
        assert diagnostic.stage in [:layout, :font]
        assert diagnostic.module in [HtmlValidator, FontValidator]
        assert diagnostic.message =~ "max_work"
      end)
    end
  end

  test "pagination retains the measured Math font-work diagnostic" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<math><mi>x</mi></math>")
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled)
    Limits.install(%{limits | max_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} = Pagination.paginate(layout)
    assert diagnostic.stage == :font
    assert diagnostic.module == FontValidator
    assert diagnostic.operation == :measure_math
    assert diagnostic.message =~ "max_work"
  end

  test "middle-aligned color controls retain the parent font metric diagnostic" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<p style='font-family:DejaVu Sans'><input type=color style='vertical-align:middle'></p>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    Limits.install(%{limits | max_work: 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = Layout.layout(styled)
    assert diagnostic.stage == :font
    assert diagnostic.operation == :load_registry
    assert diagnostic.module == FontValidator
    assert diagnostic.message =~ "max_work"
  end

  test "spanning and positioned controls preserve field-count and appearance-work diagnostics" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    for fixture <- ~w(html_table_spanning_controls html_positioned_widget_fragment_source),
        key <- [:max_entries, :max_output_bytes] do
      html =
        File.read!("test/fixtures/html_to_pdf/browser_parity/#{fixture}.html")
        |> String.replace("<input ", "<input name='budget-extra' value='Second'><input ",
          global: false
        )

      Limits.install(Map.put(limits, key, 1))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html,
                 system_font_discovery: false,
                 page_size: {100, 100},
                 margin: 0
               )

      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.message =~ Atom.to_string(key)
      assert diagnostic.stage in [:html, :style]
    end
  end

  test "positioned source controls retain a late field allocation failure during pagination" do
    alias NativeElixirPdfUtilities.TestSupport.{Paths, ResourceBudget}
    alias NativeElixirPdfUtilities.Validators.ResourceBudgetValidator
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    source =
      File.read!(
        Paths.fixture("html_to_pdf/browser_parity/html_positioned_widget_fragment_source.html")
      )

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} = HtmlParser.parse_document_detailed(source)

      assert {:ok, document} =
               NativeElixirPdfUtilities.HtmlToPdf.prepare_document(
                 dom,
                 [system_font_discovery: false, page_size: {120, 90}, margin: 0],
                 context.image_budget
               )

      operation = fn -> Pagination.paginate(document.layout_tree, document.options) end
      assert {{:ok, pages}, needed} = ResourceBudget.measure(:max_entries, operation)
      assert length(pages) == 2 and needed > 0
      spent = Map.get(ResourceBudgetValidator.snapshot(), :max_entries, 0)
      Limits.install(%{limits | max_entries: spent + div(needed, 2)})

      assert {:render_resource_limit, {:error, {:resource_limit_exceeded, diagnostic}}} =
               catch_throw(operation.())

      assert diagnostic.stage == :forms
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.message =~ "max_entries"
    end)
  end

  test "spanning cell paint retains out-of-range source geometry diagnostics" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_numeric_magnitude: 1000})

    html =
      "<style>@page{size:100pt 100pt;margin:0}html,body{margin:0}table{margin-top:-1000pt}</style>" <>
        "<table><tr><td rowspan=2>Spanning<br>Text</td><td>One</td></tr><tr><td>Two</td></tr></table>"

    assert {:error, {:invalid_layout, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               system_font_discovery: false,
               page_size: {100, 100},
               margin: 0
             )

    assert diagnostic.stage == :layout
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlTableValidator
    assert diagnostic.operation == :prepare_cell_band_paint
    assert diagnostic.message =~ "bounded"
  end

  test "an oversized cell outside the PDF geometry bound retains its pagination diagnostic" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    html =
      "<style>@page{size:100pt 100pt;margin:10pt}html,body{margin:0;font-family:DejaVu Sans;font-size:10pt;line-height:15pt}td{height:15pt}</style>" <>
        "<table><tr><td><span style='display:inline-block;width:20pt;height:1050pt;background:blue'></span></td></tr></table>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {100, 100}, margin: 10)
    Limits.install(%{limits | max_numeric_magnitude: 1000})
    assert {:error, {:invalid_layout, diagnostic}} = Pagination.paginate(layout)

    assert diagnostic.stage == :pagination
    assert diagnostic.operation == :prepare_row_page_fragment
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlTableValidator
    assert diagnostic.message =~ "bounded row edges"
  end

  test "repeated text measurement preserves positions and rechecks edited source owners" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} = HtmlParser.parse_detailed("<p style='margin:0'>Alpha</p>")

      assert {:ok, styled} =
               Style.compute_detailed(dom, [system_font_discovery: false], context.image_budget)

      assert {:ok, original} = Layout.layout(styled, margin: 0)
      assert {:ok, repeated} = Layout.layout(styled, margin: 0)

      projection = fn layout ->
        for box <- layout.boxes,
            box.type == :text,
            do: {box.text, box.x, box.y, box.width, box.font_size}
      end

      assert projection.(repeated) == projection.(original)
      [paragraph] = styled.children
      [text] = paragraph.children
      changed = %{styled | children: [%{paragraph | children: [%{text | text: "Beta"}]}]}
      assert {:ok, edited} = Layout.layout(changed, margin: 0)
      assert [{"Beta", _, _, _, _}] = projection.(edited)
      assert {:ok, restored} = Layout.layout(styled, margin: 0)
      assert projection.(restored) == projection.(original)
    end)
  end
end
