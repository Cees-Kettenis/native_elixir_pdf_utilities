defmodule NativeElixirPdfUtilities.HtmlToPdf.StyleClosureTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgValidator}

  @opts [system_font_discovery: false, forms: :static]

  test "nested definition lists retain their zero-margin native defaults" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<dl><dt>Outer</dt><dd><dl><dt>Inner</dt><dd>Value</dd></dl></dd></dl>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom, @opts)
    [outer] = styled.children
    [_, %{children: [inner]}] = outer.children
    assert inner.style.margin == %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    assert inner.style.margin_after == 0.0
  end

  test "a customizable select without a button uses the actual selected option contents" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<select style='appearance:base-select'><option selected><b>Actual choice</b></option><option>Other</option></select>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom, @opts)
    [select] = styled.children
    assert select.style.form_kind == :base_select
    assert HtmlValidator.descendant_text_content(select.children) =~ "Actual choice"
    refute HtmlValidator.descendant_text_content(select.children) =~ "Other"
  end

  test "important font-family retains the real authored scope and selected bundled face" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div><template shadowrootmode=open><style>p{font-family:DejaVu Sans!important}</style><p>Actual text</p></template></div>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom, @opts)
    [host] = styled.children
    [paragraph] = host.children
    [logical_host] = dom.children
    [template] = logical_host.children
    assert paragraph.style.font_face.family == "DejaVu Sans"
    assert paragraph.style._font_family_scope == template.content.tree_scope_id
  end

  test "SVG list artwork loads its own real source and intrinsic raster size" do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='6'><rect width='8' height='6' fill='red'/></svg>"

    uri = "data:image/svg+xml;base64," <> Base.encode64(source)

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<ul style='list-style-image:url(#{uri})'><li>Actual item</li></ul>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom, @opts)
    [list] = styled.children
    assert list.style.list_style_image.width == 6.0
    assert list.style.list_style_image.height == 4.5
  end

  test "dimension preparation keeps the nested raster budget diagnostic" do
    source = "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='6'/>"
    assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :image})
    old = Limits.effective()
    Limits.install(Map.put(old, :max_aggregate_svg_raster_pixels, 1))

    try do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               SvgValidator.validate_dimensions(
                 summary.root,
                 [],
                 HtmlValidator.new_image_budget()
               )

      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.module == HtmlValidator
      assert diagnostic.operation == :rasterize
      assert diagnostic.source == "SVG"
      assert diagnostic.message =~ "max_aggregate_svg_raster_pixels"
    after
      Limits.install(old)
    end
  end
end
