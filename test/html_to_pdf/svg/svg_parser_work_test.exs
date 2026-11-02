defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgParserWorkTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgParserWorkValidator, SvgValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "distinct attributes are rejected before SAX or native parsing" do
    attrs = Enum.map_join(1..8_000, " ", &"a#{&1}='0'")
    source = "<svg width='1' height='1' #{attrs}/>"

    for result <- [
          SvgValidator.validate_processing(source, %{mode: :image}),
          SvgValidator.validate_extraction(source),
          SvgRasterizer.rasterize(source, [], nil),
          HtmlToPdf.render(source, system_font_discovery: false)
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} = result
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.message =~ "max_html_tree_work"
      assert diagnostic.source == "SVG"
    end
  end

  test "name and namespace lengths contribute to comparison work" do
    for attrs <- [
          Enum.map_join(1..100, " ", &"#{String.duplicate("a", 10_000)}#{&1}='0'"),
          Enum.map_join(1..100, " ", &"xmlns:n#{&1}='urn:#{String.duplicate("a", 10_000)}'")
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               SvgValidator.validate_processing("<svg #{attrs}/>", %{mode: :image})

      assert diagnostic.message =~ "max_html_tree_work"
    end
  end

  test "standalone and nested scans enforce the same cumulative budget", %{limits: limits} do
    source = "<svg/>"
    # Six scanned bytes, three name bytes and the implicit XML namespace binding.
    exact = byte_size(source) + 3 + 4 + byte_size("http://www.w3.org/XML/1998/namespace")
    Limits.install(%{limits | max_html_tree_work: exact})
    assert :ok = SvgParserWorkValidator.validate(source)
    assert :ok = SvgParserWorkValidator.validate(source)

    assert {:error, {:resource_limit_exceeded, _}} =
             HtmlValidator.with_render_budget(fn ->
               assert :ok = SvgParserWorkValidator.validate(source)
               SvgParserWorkValidator.validate(source)
             end)

    Limits.install(%{limits | max_html_tree_work: exact - 1})
    assert {:error, {:resource_limit_exceeded, _}} = SvgParserWorkValidator.validate(source)
    Limits.install(%{limits | max_html_tree_work: exact * 2})

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgParserWorkValidator.validate("<svg><g/><g/></svg>")
  end

  test "quotes comments CDATA and processing instructions cannot create fake attributes", %{
    limits: limits
  } do
    fake = Enum.map_join(1..1_000, " ", &"a#{&1}='0'")

    source =
      "<?xml version='1.0'?><svg width='1' height='1'><desc data-q='\" &gt; a=1'>" <>
        "<![CDATA[<g #{fake}/>]]><!--<g #{fake}/>--><?example <g #{fake}/>?></desc></svg>"

    Limits.install(%{limits | max_html_tree_work: byte_size(source) + 5_000})
    assert :ok = SvgParserWorkValidator.validate(source)

    assert {:error, {:invalid_document, diagnostic}} =
             SvgValidator.validate_processing(source, %{mode: :image})

    assert diagnostic.message =~ "processing instructions"
    valid = String.replace(source, "<?example <g #{fake}/>?>", "")
    assert {:ok, _} = SvgValidator.validate_processing(valid, %{mode: :image})
  end

  test "namespace scopes end at their element and ordinary SVG still converts", %{limits: limits} do
    child = "<g xmlns:p='urn:#{String.duplicate("a", 100)}'><desc p:title='ok'/></g>"
    source = "<svg width='1' height='1'>" <> String.duplicate(child, 100) <> "</svg>"
    Limits.install(%{limits | max_html_tree_work: 1_000_000})
    assert :ok = SvgParserWorkValidator.validate(source)
    assert {:ok, _} = SvgValidator.validate_processing(source, %{mode: :image})
    Limits.install(limits)

    assert {:ok, _} =
             SvgRasterizer.rasterize(
               "<svg width='1' height='1'><rect width='1' height='1' fill='red'/></svg>",
               [],
               nil
             )
  end

  test "namespace insertion work includes declarations on separate siblings" do
    children =
      Enum.map_join(1..2_000, fn index ->
        "<g xmlns:p='urn:#{index}'/>"
      end)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgValidator.validate_processing("<svg>" <> children <> "</svg>", %{mode: :image})

    assert diagnostic.message =~ "max_html_tree_work"
  end

  test "namespace merging includes bindings inherited from ancestors" do
    bindings = Enum.map_join(1..100, " ", &"xmlns:p#{&1}='urn:#{&1}'")
    children = String.duplicate("<g xmlns:q='urn:q'/>", 200)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgValidator.validate_processing("<svg #{bindings}>#{children}</svg>", %{
               mode: :image
             })

    assert diagnostic.message =~ "max_html_tree_work"
  end

  test "converter preparation charges generated XML as well as authored XML", %{limits: limits} do
    source = "<svg width='1' height='1'><rect width='1' height='1' fill='red'/></svg>"

    assert {:ok, prepared} =
             NativeElixirPdfUtilities.Validators.SvgConverterValidator.prepare(
               source,
               [],
               nil,
               [],
               %{mode: :image}
             )

    assert prepared.source =~ "xmlns="
    Limits.install(%{limits | max_html_tree_work: 1_500})
    assert {:ok, _} = SvgValidator.validate_processing(source, %{mode: :image})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgRasterizer.rasterize(source, [], nil)

    assert diagnostic.message =~ "max_html_tree_work"
    assert diagnostic.source == "SVG"

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgParserWorkValidator.validate(prepared.source)
  end

  test "nested SVG images charge their prepared native input", %{limits: limits} do
    child = "<svg width='1' height='1'><rect width='1' height='1' fill='red'/></svg>"

    source =
      "<svg width='1' height='1'><image href='svg-resource:child' width='1' height='1'/></svg>"

    context = %{
      mode: :reference,
      resources: %{"svg-resource:child" => %{data: child, media_type: "image/svg+xml"}}
    }

    assert {:ok, _} = SvgRasterizer.rasterize(source, [], nil, [], context)
    Limits.install(%{limits | max_html_tree_work: 4_500})
    assert {:ok, _} = SvgValidator.validate_processing(child, %{mode: :image})
    assert {:ok, _} = SvgValidator.validate_processing(source, context)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgRasterizer.rasterize(source, [], nil, [], context)

    assert diagnostic.message =~ "max_html_tree_work"
    assert diagnostic.source == "SVG"
  end

  test "only prefixed attributes incur namespace URI comparison work", %{limits: limits} do
    binding = "xmlns:p='urn:#{String.duplicate("a", 10_000)}'"
    Limits.install(%{limits | max_html_tree_work: 500_000})
    plain = Enum.map_join(1..32, " ", &"a#{&1}='0'")
    qualified = Enum.map_join(1..32, " ", &"p:a#{&1}='0'")

    assert {:ok, _} =
             SvgValidator.validate_processing("<svg #{binding} #{plain}/>", %{mode: :image})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgValidator.validate_processing("<svg #{binding} #{qualified}/>", %{mode: :image})

    assert diagnostic.message =~ "max_html_tree_work"
  end

  test "identical namespace bindings do not inflate the native namespace pool", %{limits: limits} do
    source = "<svg>" <> String.duplicate("<g xmlns:p='urn:shared'/>", 2_000) <> "</svg>"
    Limits.install(%{limits | max_html_tree_work: 1_000_000})
    assert {:ok, _} = SvgValidator.validate_processing(source, %{mode: :image})
  end

  test "lexically invalid XML retains parser diagnostics" do
    for source <- [
          "<svg a",
          "<svg a=1/>",
          "<svg a='",
          "<svg =''/>",
          "<svg><",
          "<svg><!--",
          "<svg><![CDATA[",
          "<?xml",
          "</svg>",
          "<svg a''/>"
        ] do
      assert :ok = SvgParserWorkValidator.validate(source)

      assert {:error, {:invalid_document, diagnostic}} =
               SvgValidator.validate_processing(source, %{mode: :image})

      assert diagnostic.source == "SVG"
    end

    assert {:error, {:invalid_document, diagnostic}} =
             SvgValidator.validate_processing("<!DOCTYPE svg><svg/>", %{mode: :image})

    assert diagnostic.message =~ "DTD"
  end
end
