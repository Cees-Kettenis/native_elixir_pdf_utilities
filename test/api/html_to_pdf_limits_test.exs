defmodule NativeElixirPdfUtilities.HtmlToPdfLimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.TestSupport.{PngFixture, ResourceBudget}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  test "transformed overflow creates a counted sheet with that page's geometry and furniture" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    html = """
    <style>@page{size:200pt 220pt;margin:16pt;@top-center{content:counter(page) '/' counter(pages);font-size:8pt}}
    @page:first{size:200pt 130pt;margin:10pt}body{margin:0}</style>
    <div style="transform:translateY(115pt)">MOVED</div>
    """

    Limits.install(%{limits | max_pages: 1})

    assert {:error, {:resource_limit_exceeded, %{stage: :pagination, message: message}}} =
             HtmlToPdf.render(html)

    assert message =~ "max_pages"

    Limits.install(%{limits | max_pages: 2})
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, %{pages: [first, second]}} = Text.extract_spans(pdf)
    assert first.media_box == [0, 0, 200, 130]
    assert second.media_box == [0, 0, 200, 220]
    assert Enum.map_join(first.spans, & &1.text) =~ "1/2"
    assert Enum.map_join(second.spans, & &1.text) =~ "2/2"
    assert Enum.map_join(second.spans, & &1.text) =~ "MOVED"
  end

  test "oversized legacy literal input returns public shared diagnostics" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    for tag <- ~w(xmp noembed plaintext) do
      source = "<#{tag}>é\r\n" <> String.duplicate("<img src=denied.png>", 20)
      Limits.install(%{limits | max_input_bytes: byte_size(source) - 1})
      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(source)
      assert diagnostic.stage == :html and diagnostic.operation == :render
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.message =~ "max_input_bytes"
    end
  end

  test "itemref lookup consumes the existing shared work budget before asset resolution" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_work: 1})
    owner = self()

    resolver = fn request ->
      send(owner, {:metadata_budget_asset, request})
      :not_found
    end

    html = "<div itemscope itemref=last>Item</div><p id=last>Target</p><img src=denied>"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(html, asset_resolver: resolver)

    assert diagnostic.stage == :html and diagnostic.reason == :resource_limit_exceeded
    assert is_atom(diagnostic.operation) and diagnostic.message =~ "max_work"
    refute_received {:metadata_budget_asset, _}
  end

  test "inactive templates count nodes and nesting against the unchanged limits" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_entries: 5})

    assert {:error,
            {:resource_limit_exceeded,
             %{stage: :html, reason: :resource_limit_exceeded, message: message}}} =
             HtmlToPdf.render("<template><p>A</p><p>B</p></template><p>Visible</p>")

    assert message =~ "max_entries"
    Limits.install(%{limits | max_depth: 2})

    assert {:error, {:resource_limit_exceeded, %{message: depth_message}}} =
             HtmlToPdf.render(
               "<template><template><p>Hidden</p></template></template><p>Visible</p>"
             )

    assert depth_message =~ "max_depth"
  end

  test "data scripts retain shared HTML source and rendered text limits" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_input_bytes: 100})

    assert {:error,
            {:resource_limit_exceeded,
             %{stage: :html, reason: :resource_limit_exceeded, message: source_message}}} =
             HtmlToPdf.render("<script type='text/plain'>#{String.duplicate("x", 100)}</script>")

    assert source_message =~ "max_input_bytes"
    Limits.install(limits)

    assert {:ok, hidden} =
             HtmlToPdf.render("<script type='text/plain'>hidden content</script><p>OK</p>")

    assert {:ok, "OK"} = Text.extract(hidden, layout: false)
    Limits.install(%{limits | max_output_bytes: 3})

    assert {:error, {:resource_limit_exceeded, %{stage: :writer}}} =
             HtmlToPdf.render("<script type='text/plain'>hidden content</script><p>OK</p>")

    assert {:error,
            {:resource_limit_exceeded, %{reason: :resource_limit_exceeded, message: text_message}}} =
             HtmlToPdf.render(
               "<body><script type='text/plain' style='display:block'>shown content</script></body>"
             )

    assert text_message =~ "max_output_bytes"
  end

  test "comments consume the shared HTML token budget" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_entries: 4})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<!-- one --><!-- two --><!-- three --><p>Text</p>")

    assert diagnostic.stage == :html
    assert diagnostic.operation == :render
    assert diagnostic.message =~ "max_entries"
  end

  test "background tile limits preserve their actionable public diagnostic" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_entries: 4})
    source = "data:image/png;base64,#{Base.encode64(PngFixture.solid_rgb(1, 1))}"

    for tile_size <- ["1pt", "0.1pt"], tag <- ["div", "body"] do
      html =
        ~s|<#{tag} style="width: 4pt; height: 4pt; background-image: url('#{source}'); background-size: #{tile_size} #{tile_size}"></#{tag}>|

      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.stage in [:html, :font]
      assert is_atom(diagnostic.operation)

      assert diagnostic.module in [
               HtmlValidator,
               NativeElixirPdfUtilities.Validators.FontValidator
             ]

      assert diagnostic.message =~ "max_entries"

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlValidator.validate_background_image_tile_count(5)

      assert diagnostic.message == "background image tile count exceeds the 4-tile limit"
    end
  end

  test "HTML attribute case folding consumes the shared work budget" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    value = String.duplicate("a", 1000)
    css = ~s(<style>input[disabled="#{value}"] { color:red; }</style>)
    exact = css <> ~s(<input disabled="#{value}" value="Text">)
    mixed = css <> ~s(<input disabled="#{String.upcase(value)}" value="Text">)

    # Compare selector work after both inputs' shared physical-font data is
    # admitted. A cold classifier parse belongs to its first operation and
    # must not be mistaken for case-folding work in the comparison below.
    assert {:ok, _} = HtmlToPdf.render(exact)
    assert {:ok, _} = HtmlToPdf.render(mixed)

    assert {{:ok, _}, exact_work} =
             ResourceBudget.measure_operation(:max_work, fn -> HtmlToPdf.render(exact) end)

    assert {{:ok, _}, mixed_work} =
             ResourceBudget.measure_operation(:max_work, fn -> HtmlToPdf.render(mixed) end)

    assert mixed_work > exact_work
    Limits.install(%{limits | max_work: exact_work})
    assert {:ok, _} = HtmlToPdf.render(exact)
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(mixed)
    assert diagnostic.reason == :resource_limit_exceeded
    assert is_atom(diagnostic.operation) and diagnostic.message =~ "max_work"
  end

  test "all positional sibling scans consume the shared CSS work budget" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_work: 1000})
    roots = String.duplicate("<p>Text</p>", 600)

    for selector <-
          ~w|first-child last-child first-of-type last-of-type only-child only-of-type nth-child(1)| do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render(roots, stylesheets: [{:css, "p:" <> selector <> "{color:red}"}])

      assert diagnostic.stage == :html
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :render
      assert diagnostic.message =~ "max_work"
    end
  end

  test "attribute token and substring comparisons consume the shared CSS allowance" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    value = String.duplicate("a", 2000)
    node = %{attributes: %{"data-kind" => value}}
    assert {:ok, exact, ""} = HtmlValidator.parse_css_attribute_selector("[data-kind=a]")

    selectors =
      Enum.map(["~=", "|=", "^=", "$=", "*=", "=a i"], fn op ->
        source = if op == "=a i", do: "[data-kind=a i]", else: "[data-kind#{op}a]"
        assert {:ok, selector, ""} = HtmlValidator.parse_css_attribute_selector(source)
        selector
      end)

    Limits.install(%{limits | max_work: 1000})
    refute HtmlValidator.attribute_selector_matches?(exact, node)

    for selector <- selectors do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlValidator.with_render_budget(fn ->
                 HtmlValidator.attribute_selector_matches?(selector, node)
               end)

      assert diagnostic.stage == :css and diagnostic.message =~ "max_work"
    end
  end

  test "decoration depth uses the shared entry allowance" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    assert {:ok, pdf} = HtmlToPdf.render("<p><u>A</u> <s>B</s></p>")
    assert {:ok, extracted} = Text.extract(pdf, layout: false)
    for expected <- ["A", "B"], do: assert(extracted =~ expected)

    assert {:ok, dom} =
             NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse_detailed("<p><u>A</u></p>")

    assert {:ok, styled} = NativeElixirPdfUtilities.HtmlToPdf.Style.compute_detailed(dom)
    [paragraph] = styled.children
    [underline] = paragraph.children
    Limits.install(%{limits | max_entries: 1})

    assert {:ok, _} =
             HtmlValidator.resolve_text_decorations(
               Map.delete(underline.style, :_text_decorations)
             )

    doubled = %{underline.style | text_decoration_line: [:line_through]}

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.resolve_text_decorations(doubled)
             end)

    assert diagnostic.stage == :style and diagnostic.message =~ "max_entries"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p><u><s>A</s></u></p>")

    assert is_atom(diagnostic.operation) and diagnostic.message =~ "max_entries"
  end

  test "repeated page furniture shares the complete render allowance" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    image = "data:image/png;base64,#{Base.encode64(PngFixture.solid_rgb(1, 1))}"

    html =
      "<p>First body page</p><div style='page-break-after:always'></div><p>Second body page</p>"

    options = [
      page_size: {200, 100},
      margin: 20,
      page_furniture: [header: ~s(<img src="#{image}" style="width:1pt;height:1pt">)]
    ]

    assert {{:ok, _}, entries} =
             ResourceBudget.measure_operation(:max_entries, fn ->
               HtmlToPdf.render(html, options)
             end)

    Limits.install(%{limits | max_entries: entries})
    assert {:ok, _} = HtmlToPdf.render(html, options)
    Limits.install(%{limits | max_entries: entries - 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html, options)
    assert is_atom(diagnostic.operation) and diagnostic.message =~ "max_entries"
  end

  test "cached PNG placements still consume aggregate decoded-image budget" do
    original_limits = Limits.effective()
    on_exit(fn -> Limits.install(original_limits) end)
    image = "data:image/png;base64,#{Base.encode64(PngFixture.solid_rgb(1, 1))}"
    html = ~s(<img src="#{image}"><img src="#{image}">)

    Limits.install(%{
      original_limits
      | max_decoded_bytes: 6
    })

    assert {:ok, _pdf} = HtmlToPdf.render(html)

    Limits.install(%{
      original_limits
      | max_decoded_bytes: 5
    })

    assert {:error,
            {:resource_limit_exceeded,
             %{stage: :limits, reason: :resource_limit_exceeded, message: message}}} =
             HtmlToPdf.render(html)

    assert message =~ "max_decoded_bytes"
  end

  test "repeated SVG placements keep raster dimensions and aggregate limits" do
    original_limits = Limits.effective()
    on_exit(fn -> Limits.install(original_limits) end)

    svg =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="1" height="1"><rect width="1" height="1" fill="red"/></svg>)

    source = "data:image/svg+xml;base64,#{Base.encode64(svg)}"
    repeated = ~s(<img src="#{source}"><img src="#{source}">)

    raster_bytes = 4 * Limits.get(:svg_raster_scale) ** 2

    Limits.install(%{
      original_limits
      | max_decoded_bytes: 2 * raster_bytes
    })

    assert {:ok, _pdf} = HtmlToPdf.render(repeated)

    Limits.install(%{
      original_limits
      | max_decoded_bytes: 2 * raster_bytes - 1
    })

    assert {:error,
            {:resource_limit_exceeded,
             %{stage: :limits, reason: :resource_limit_exceeded, message: message}}} =
             HtmlToPdf.render(repeated)

    assert message =~ "max_decoded_bytes"
  end

  test "render rejects aggregate decoded PNG bytes before inflating the excess image" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    src = "data:image/png;base64,#{Base.encode64(PngFixture.solid_rgb(20, 20))}"
    image = ~s(<img src="#{src}" style="width:20pt">)

    assert {{:ok, _}, decoded} =
             ResourceBudget.measure_operation(:max_decoded_bytes, fn ->
               HtmlToPdf.render(String.duplicate(image, 2))
             end)

    Limits.install(%{limits | max_decoded_bytes: decoded})
    assert {:ok, _} = HtmlToPdf.render(String.duplicate(image, 2))
    Limits.install(%{limits | max_decoded_bytes: decoded - 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(String.duplicate(image, 2))

    assert diagnostic.operation == :render and diagnostic.message =~ "max_decoded_bytes"
  end

  test "image sources consume the shared entry allowance before style traversal" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    src = "data:image/png;base64,#{Base.encode64(PngFixture.solid_rgb(1, 1))}"
    image = ~s(<img src="#{src}" style="width:1pt">)
    assert {:ok, _} = HtmlToPdf.render(String.duplicate(image, 2))
    Limits.install(%{limits | max_entries: 2})
    budget = HtmlValidator.new_image_budget()
    assert :ok = HtmlValidator.reserve_image_source(budget, 1)
    assert :ok = HtmlValidator.reserve_image_source(budget, 1)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.reserve_image_source(budget, 1)

    assert diagnostic.message =~ "image count"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(String.duplicate(image, 3))

    assert is_atom(diagnostic.operation) and diagnostic.message =~ "max_entries"
  end

  test "password display byte limits fail with an actionable style diagnostic" do
    saved = Limits.effective()
    on_exit(fn -> Limits.install(saved) end)
    Limits.install(Map.put(saved, :max_output_bytes, 8))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(~s(<input type="password" value="ABC">), forms: :static)

    assert diagnostic.stage == :style and diagnostic.operation == :render
    assert diagnostic.message =~ "max_output_bytes"
    refute diagnostic.message =~ "ABC"
  end
end
