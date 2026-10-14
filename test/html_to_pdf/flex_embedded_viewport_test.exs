defmodule NativeElixirPdfUtilities.HtmlToPdf.FlexEmbeddedViewportTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style, ResourceContext}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @fixture Path.expand(
             "../fixtures/html_to_pdf/browser_parity/html_flex_embedded_surfaces.html",
             __DIR__
           )

  test "flex HTML surfaces paint child content in allocated clipped boxes with local link scopes" do
    html = File.read!(@fixture)
    opts = [base_url: Path.dirname(@fixture)]

    ResourceContext.run(
      [authorization_root: Path.dirname(@fixture), base_url: Path.dirname(@fixture)],
      HtmlValidator.new_image_budget(),
      fn context ->
        assert {:ok, dom} = HtmlParser.parse_detailed(html)
        assert {:ok, styled} = Style.compute_detailed(dom, opts, context.image_budget)
        assert {:ok, layout} = Layout.layout(styled, opts)
        hosts = embedded_hosts(layout.boxes)
        assert length(hosts) == 5
        assert length(Enum.uniq(Enum.map(hosts, & &1.link_scope))) == 5

        for group <- hosts do
          assert group.type == :drawing_group
          assert group.width > 0 and group.height > 0
          assert group.clip == %{x: 0.0, y: 0.0, width: group.width, height: group.height}
          assert group.transform == {1, 0, 0, 1, group.x, group.y}
          assert Enum.any?(group.boxes, &(Map.get(&1, :text) == "Child viewport"))
          assert Enum.any?(group.boxes, &Map.has_key?(&1, :link_destination))
        end

        [first, second, third, upper, lower] = hosts
        assert_in_delta first.width, 96.0, 0.0001
        assert_in_delta second.width, first.width, 0.0001
        assert_in_delta third.width, first.width, 0.0001
        assert_in_delta upper.width, 138.0, 0.0001
        assert_in_delta lower.width, upper.width, 0.0001
        assert upper.height > 24.0 and lower.height > 24.0
      end
    )

    assert {:ok, _pdf} = HtmlToPdf.render(html, opts)
  end

  test "replaced defaults and CSS auto stay intrinsic outside flex and use final flex allocation" do
    fixture = Path.join(Path.dirname(@fixture), "html_embedded_intrinsic_css_auto.html")
    html = File.read!(fixture)
    opts = [base_url: Path.dirname(fixture)]

    ResourceContext.run(
      [authorization_root: Path.dirname(fixture), base_url: Path.dirname(fixture)],
      HtmlValidator.new_image_budget(),
      fn context ->
        assert {:ok, dom} = HtmlParser.parse_detailed(html)
        assert {:ok, styled} = Style.compute_detailed(dom, opts, context.image_budget)
        assert {:ok, layout} = Layout.layout(styled, opts)
        hosts = embedded_hosts(layout.boxes)
        assert length(hosts) == 5
        [default, auto, border_box, first_flex, second_flex] = hosts

        for host <- [default, auto] do
          assert_in_delta host.width, 225.0, 0.0001
          assert_in_delta host.height, 112.5, 0.0001
        end

        assert_in_delta border_box.width, 139.0, 0.0001
        assert_in_delta border_box.height, 79.0, 0.0001

        for host <- [first_flex, second_flex] do
          assert_in_delta host.width, 100.0, 0.0001
          assert_in_delta host.height, 60.0, 0.0001
          assert host.clip == %{x: 0.0, y: 0.0, width: 100.0, height: 60.0}
        end
      end
    )
  end

  test "inline CSS auto retains the replaced fallback even in a narrow containing block" do
    html = """
    <div style='width:100pt'><iframe style='border:0;width:auto;height:auto' srcdoc='&lt;body style="margin:0;overflow:hidden"&gt;Child&lt;/body&gt;'></iframe></div>
    """

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom, [], context.image_budget)
      assert {:ok, layout} = Layout.layout(styled, [])
      [host] = embedded_hosts(layout.boxes)
      assert_in_delta host.width, 225.0, 0.0001
      assert_in_delta host.height, 112.5, 0.0001
      assert host.transform == {1, 0, 0, 1, host.x, host.y}
      assert Enum.any?(host.boxes, &(Map.get(&1, :text) == "Child"))
    end)
  end

  test "allocated flex reflow failures return the child resource diagnostic" do
    html = """
    <div style='display:flex;width:60pt'><iframe style='display:block;border:0;width:120pt;height:40pt' srcdoc="&lt;style&gt;body{overflow:hidden} @media print and (max-width:100px){body{background-image:url(missing-flex-child.png)}}&lt;/style&gt;&lt;body&gt;Child&lt;/body&gt;"></iframe></div>
    """

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render(html, base_url: Path.dirname(@fixture))

    assert diagnostic.stage == :asset
    assert diagnostic.operation == :layout_embedded_viewport
    assert diagnostic.module == NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewport
    assert diagnostic.source =~ "missing-flex-child.png"
    assert diagnostic.message != ""
  end

  # Inline hosts may have an outer overflow clip; inspect their actual owner
  # without flattening its local child coordinates or losing the clip.
  defp embedded_hosts(boxes) do
    Enum.flat_map(boxes, fn box ->
      case box do
        %{role: :embedded_viewport_host} ->
          [box]

        %{type: :drawing_group, boxes: children} ->
          embedded_hosts(children)

        _ ->
          []
      end
    end)
  end
end
