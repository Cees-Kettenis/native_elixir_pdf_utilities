defmodule NativeElixirPdfUtilities.HtmlToPdf.ContainerQueriesTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  @opts [page_size: {300, 300}, margin: 0, system_font_discovery: false]

  test "named size queries use the laid out content box and preserve order and scope" do
    html = """
    <style>
    .item {height:10pt}
    @container card (100pt <= width < 160pt) and (height >= 50pt) {
      .item {height:20pt;width:50cqw}
      @container (inline-size > 60pt) {.item {background:red}}
    }
    @container card (width > 160pt) {.item {height:30pt}}
    </style>
    <div style="width:170pt;height:100pt;border:10pt solid;padding:5pt;
                box-sizing:border-box;container:card/size">
      <div style="container:other/inline-size;width:80pt"><div class=item>Query</div></div>
    </div>
    """

    assert {:ok, %{children: [outer]} = tree} = styles(html)
    assert [inner] = Enum.filter(outer.children, &(&1.type == :element))
    assert [item] = inner.children
    assert item.style.height == 20.0
    assert item.style.background_color == {1, 0, 0}
    assert {:ok, layout} = Layout.layout(tree, @opts)
    assert Enum.any?(layout.boxes, &match?(%{fill_color: {1, 0, 0}, width: 40.0}, &1))
  end

  test "ineligible missing axes and unknown features cannot be negated into matches" do
    html = """
    <style>
    @container card not (height > 0) {.item {height:90pt}}
    @container card not scroll-state(stuck: top) {.item {height:91pt}}
    @container card style(--mode: compact) {.item {width:30pt}}
    @container card style((--mode: full) or (--other: yes)) {.item {width:90pt}}
    </style>
    <div style="container:card/inline-size;--mode:compact">
      <div class=item>Child</div>
    </div>
    """

    assert {:ok, %{children: [%{children: children}]}} = styles(html)
    assert [item] = Enum.filter(children, &(&1.type == :element))
    assert item.style.width == 30.0
    refute Map.has_key?(item.style, :height)
  end

  test "contained auto sizes use intrinsic placeholders without suppressing visible children" do
    html = """
    <div style="contain:size;contain-intrinsic-size:auto 40pt 20pt;background:red">
      <div style="height:60pt">Visible</div>
    </div>
    <div style="content-visibility:hidden;contain-intrinsic-height:12pt;background:blue">
      Skipped
    </div>
    <div style="content-visibility:auto">Also visible</div>
    """

    assert {:ok, tree} = styles(html)
    assert {:ok, layout} = Layout.layout(tree, @opts)
    assert %{height: height} = Enum.find(layout.boxes, &match?(%{fill_color: {1, 0, 0}}, &1))
    assert_in_delta height, 20.0, 0.375
    assert Enum.any?(layout.boxes, &match?(%{fill_color: {0, 0, 1}, height: 12.0}, &1))
    assert Enum.any?(layout.boxes, &match?(%{text: "Visible"}, &1))
    refute Enum.any?(layout.boxes, &match?(%{text: "Skipped"}, &1))
  end

  test "size queries see flex allocation and independently select a container for each axis" do
    html = """
    <style>
      @container wrong [width] {.item {color:purple}}
      @container outer (block-size:100pt) {
        .item {width:calc(50cqi + 5cqb);height:10cqmin;margin-left:1cqmax;padding-top:1cqh}
      }
    </style>
    <div style="width:200pt;height:100pt;container:outer/size;display:flex">
      <div style="flex:0 0 60pt;container-type:inline-size"><div class=item style="background:red"></div></div>
      <div style="width:10pt">Sibling</div>
    </div>
    """

    assert {:ok, tree} = styles(html)
    assert {:ok, layout} = Layout.layout(tree, @opts)

    assert %{width: width, height: height, x: x} =
             Enum.find(walk(layout.boxes), &match?(%{fill_color: {1, 0, 0}}, &1))

    assert_in_delta width, 35.0, 0.375
    assert_in_delta height, 7.0, 0.375
    assert_in_delta x, 1.0, 0.375
  end

  test "root and embedded query containers are measured in their own physical surface" do
    for height <- ["height:80pt", "contain-intrinsic-height:80pt"] do
      html = """
      <style>
        html {width:120pt;#{height};container:root/size}
        @container root (width > 110pt) and (height:80pt) {.root {background:red;width:10pt;height:10pt}}
      </style>
      <div class=root></div>
      <iframe style="display:block;width:150pt;height:30pt" srcdoc="<style>html{scrollbar-width:none}.card{container-type:inline-size;width:100pt}@container (width > 90pt){.item{color:red}}</style><div class='card'><div class='item'>Child</div></div>"></iframe>
      """

      alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
      alias NativeElixirPdfUtilities.Validators.HtmlValidator

      budget = HtmlValidator.new_image_budget()

      ResourceContext.run([], budget, fn _ ->
        assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
        assert {:ok, tree} = Style.compute_detailed(dom, @opts)
        assert {:ok, layout} = Layout.layout(tree, @opts)
        assert Enum.any?(layout.boxes, &match?(%{fill_color: {1, 0, 0}}, &1))
        assert Enum.any?(walk(layout.boxes), &match?(%{text: "Child", color: {1, 0, 0}}, &1))
      end)
    end
  end

  test "size-contained flex and grid clip paint to the allocated padding box" do
    for display <- ["flex", "grid"] do
      html = """
      <div style="display:#{display};contain:strict;width:80pt;height:15pt">
        <div style="flex-shrink:0;width:140pt;height:55pt;background:red"></div>
      </div>
      """

      assert {:ok, tree} = styles(html)
      assert {:ok, layout} = Layout.layout(tree, @opts)

      assert %{clip: %{width: 80.0, height: 15.0}} =
               Enum.find(walk(layout.boxes), &match?(%{fill_color: {1, 0, 0}}, &1))
    end
  end

  test "unpositioned style conditions use cached ancestors without inventing their geometry" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{ContainerQuery, RenderCache}
    alias NativeElixirPdfUtilities.Validators.CssContainerQueryValidator

    defined = %{_selector_id: make_ref()}
    unstyled = %{_selector_id: make_ref()}
    parent = %{_custom_properties: %{"--mode" => "ready"}}
    assert {:ok, conditions} = CssContainerQueryValidator.prepare("style(--mode:ready)")

    RenderCache.run(fn cache ->
      context = %{
        cache: cache,
        query_sizes: %{defined._selector_id => %{width: 200.0, height: 40.0}}
      }

      RenderCache.fetch_partition(cache, :cascade_styles, defined._selector_id, fn ->
        {:ok, parent}
      end)

      assert ContainerQuery.matches?([conditions], context, [unstyled, defined])
      assert ContainerQuery.unit_context(%{}, context, [defined]) == %{}
      assert ContainerQuery.unit_context(%{}, %{context | query_sizes: %{}}, [defined]) == %{}
      assert ContainerQuery.register(parent, %{}, defined._selector_id) == parent
      assert :ok = ContainerQuery.measure(%{_query_container: :invalid}, 10.0, 10.0)
    end)
  end

  test "a gated pseudo-element reports unsupported paint only in an active container branch" do
    for {size, expected} <- [{110, :ok}, {10, :error}] do
      html = """
      <style>@container (inline-size > #{size}pt){p::first-line{color:red}}</style>
      <div style="container-type:inline-size;width:100pt"><p>Visible</p></div>
      """

      assert elem(styles(html), 0) == expected
    end
  end

  test "a nonconvergent set of measured containers returns a bounded diagnostic" do
    alias NativeElixirPdfUtilities.HtmlToPdf.ContainerQuery

    style = fn scope, geometry ->
      node =
        ContainerQuery.register(
          %{display: :block, container_type: :size},
          %{query_scope: scope},
          :test
        )

      assert %{display: :none} =
               ContainerQuery.register(
                 %{display: :none, container_type: :size},
                 %{query_scope: scope},
                 :hidden
               )

      {:ok, {node, Map.get(geometry, :test, %{width: 0.0}).width}}
    end

    measure = fn {node, width} ->
      :ok = ContainerQuery.measure(node, 60.0 - width, 10.0)
      {:ok, :layout}
    end

    assert {:error, {:invalid_document, %{operation: :compute_container_queries}}} =
             ContainerQuery.settle(style, measure)
  end

  defp walk(boxes), do: Enum.flat_map(boxes, &[&1 | walk(Map.get(&1, :boxes, []))])

  defp styles(html) do
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    Style.compute_detailed(dom, @opts)
  end
end
