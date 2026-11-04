defmodule NativeElixirPdfUtilities.HtmlToPdf.CssTransformTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    DrawingGeometry,
    Font,
    HtmlParser,
    Layout,
    Pagination,
    ResourceContext,
    Style
  }

  alias NativeElixirPdfUtilities.Validators.{CssTransformValidator, HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    style = %{
      font_face: font,
      font_size: 12.0,
      line_height: 14.4,
      _root_font_face: font,
      _root_font_size: 12.0,
      _root_line_height: 14.4,
      _position_viewport: %{width: 400.0, height: 300.0}
    }

    {:ok, style: style}
  end

  test "canonical CSS function lists retain syntax, dimensions and multiplication order", %{
    style: style
  } do
    assert {:ok, style} =
             CssTransformValidator.apply(style, "transform", "translate(10px, 20px)scale(2, 3)")

    assert {:ok, style} = CssTransformValidator.apply(style, "transform-origin", "0 0")

    assert {:ok, matrix} =
             CssTransformValidator.resolve(style, %{x: 0, top: 0, width: 100, height: 60})

    assert matrix == {2.0, 0.0, 0.0, 3.0, 7.5, -15.0}

    assert CssTransformValidator.valid_style?(style)
    refute CssTransformValidator.valid_style?(%{css_transform: [{"rotate", [{:angle, "bogus"}]}]})
    refute CssTransformValidator.valid_style?(%{css_transform: :none, transform_origin: :bogus})
    assert {:ok, :none} = CssTransformValidator.prepare("NoNe")
    assert {:ok, _} = CssTransformValidator.prepare("\\72otate(calc(1turn / 4)) skew(0, 10deg)")

    for invalid <- [
          "rotate(90)",
          "translate(1)",
          "translate(2pt,,3pt)",
          "scale(1 2)",
          "matrix(1,0,0,1,0)",
          "rotateX(20deg)",
          "perspective(10pt)",
          "rotate (10deg)"
        ] do
      assert :error = CssTransformValidator.prepare(invalid)
    end
  end

  test "percent translation and origins use border sizes with font and viewport calculations",
       %{style: style} do
    assert {:ok, style} =
             CssTransformValidator.apply(
               style,
               "transform",
               "translate(calc(50% + 1em), calc(25% + 2vh)) scale(calc(50% + .5))"
             )

    assert {:ok, style} = CssTransformValidator.apply(style, "transform-origin", "right bottom 0")

    assert {:ok, matrix} =
             CssTransformValidator.resolve(style, %{x: 10, top: 90, width: 100, height: 60})

    assert matrix == {1.0, 0.0, 0.0, 1.0, 62.0, -19.5}

    assert :error = CssTransformValidator.prepare_origin("left top 20%", style)
    assert :error = CssTransformValidator.prepare_origin("left 10px top 10px", style)
  end

  test "rotation and reflection preserve the used point origin", %{style: style} do
    assert {:ok, style} =
             CssTransformValidator.apply(style, "transform", "rotate(90deg) scaleX(-1)")

    assert {:ok, style} = CssTransformValidator.apply(style, "transform-origin", "25% 75%")

    assert {:ok, matrix} =
             CssTransformValidator.resolve(style, %{x: 20, top: 100, width: 80, height: 40})

    {x, y} = DrawingGeometry.transform_point(40, 70, matrix)
    assert_in_delta x, 40, 0.0001
    assert_in_delta y, 70, 0.0001
    assert {:ok, zero} = CssTransformValidator.apply(style, "transform", "scale(0)")

    assert {:ok, :empty} =
             CssTransformValidator.resolve(zero, %{x: 20, top: 100, width: 80, height: 40})
  end

  test "authored transforms retain source flow and projected link quadrilaterals" do
    html =
      "<style>html,body{margin:0;font-family:DejaVu Sans;font-size:12pt;line-height:15pt}div{width:80pt;height:30pt;background:#eef4f8;margin:20pt;transform:matrix(1,.25,.5,1,0,0);transform-origin:0 0}p{margin:0}</style><div><a href='https://example.com/transform'>Actual link</a></div><p>Following flow</p>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, page_size: {240, 180}, margin: 0, compress_streams: false)

    assert pdf =~ "1 -0.25 -0.5 1"
    assert pdf =~ "/QuadPoints ["
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert String.replace(text, ~r/\s+/, "") =~ "Actuallink"
    assert text =~ "Following flow"
  end

  test "transform establishes a absolute child containing block and does not inherit" do
    html =
      "<style>html,body{margin:0;font-size:12pt;line-height:15pt}section{width:100pt;height:60pt;margin:20pt;transform:translate(10pt,5pt)}div{position:absolute;left:25%;top:50%;width:30pt;height:15pt;background:red}p{margin:0}</style><section><div></div><p>Uninherited child</p></section>"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      assert {:ok, dom} = HtmlParser.parse_detailed(html)

      assert {:ok, tree} =
               Style.compute_detailed(dom,
                 system_font_discovery: false,
                 page_size: {240, 180},
                 margin: 0
               )

      assert {:ok, tree} = NativeElixirPdfUtilities.HtmlToPdf.FontFallback.resolve(tree)
      assert {:ok, layout} = Layout.layout(tree, page_size: {240, 180}, margin: 0)
      rect = Enum.find(layout.boxes, &(Map.get(&1, :fill_color) == {1.0, 0.0, 0.0}))
      assert rect.x == 45 and rect.y == 115
      assert length(rect.css_transform_scopes) == 1
      assert {:ok, [page]} = Pagination.paginate(layout)

      assert Enum.any?(page.boxes, fn box ->
               Enum.any?(Map.get(box, :boxes, []), &(Map.get(&1, :role) == :css_transform_plane))
             end)
    end)
  end

  test "transformed controls keep editable names, values and affine appearance Forms" do
    html =
      "<style>html,body{margin:0;font-size:12pt}div{margin:20pt;width:100pt;height:45pt;transform:rotate(90deg);transform-origin:0 0}input{width:60pt;height:18pt}</style><div><input name='actual' value='Initial'></div>"

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 180}, margin: 0)
    assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert field.name == "actual" and field.value == "Initial"
    assert {:ok, changed} = NativeElixirPdfUtilities.Forms.fill(pdf, %{"actual" => "Changed"})
    assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(changed)
    assert field.value == "Changed"
    assert {:ok, flat} = NativeElixirPdfUtilities.Forms.flatten(changed)
    assert {:ok, _} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(flat)

    assert {:ok, static} =
             HtmlToPdf.render(html, page_size: {240, 180}, margin: 0, forms: :static)

    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(static)
  end

  test "function work limits and invalid visible declarations use canonical diagnostics" do
    Limits.install(%{Limits.effective() | max_css_transform_operations: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssTransformValidator.prepare("scale(2) rotate(1deg)")

    assert diagnostic.message =~ "max_css_transform_operations"

    assert {:error, {:invalid_css, diagnostic}} =
             HtmlToPdf.render("<div style='transform:rotate(1)'>Invalid</div>")

    assert diagnostic.stage == :css
    assert diagnostic.source =~ "transform:rotate(1)"
  end

  test "table cell scopes retain one authentic widget despite separated background and content layers" do
    html =
      "<style>html,body{margin:0;font-family:DejaVu Sans;font-size:12pt;line-height:15pt}table{border-spacing:0;width:180pt}td{padding:3pt;background:#eef4f8;transform:rotate(3deg)}input{font-family:DejaVu Sans;font-size:12pt;width:60pt;height:18pt}</style><table><tr><td><input name='cell' value='Actual'></td><td>Peer</td></tr></table>"

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 180}, margin: 0)
    assert {:ok, [%{name: "cell"}]} = NativeElixirPdfUtilities.Forms.fields(pdf)
  end

  test "singular source paint stays invisible without changing following flow" do
    html =
      "<style>html,body{margin:0;font-family:DejaVu Sans;font-size:12pt;line-height:15pt}div{height:30pt;transform:scale(0)}p{margin:0}</style><div><a href='https://example.com/hidden'>Hidden source</a></div><p>Visible following</p>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, page_size: {240, 180}, margin: 0, compress_streams: false)

    refute pdf =~ "https://example.com/hidden"
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Visible following"
  end

  test "ordinary inline boxes ignore transform while inline blocks retain used origins" do
    html =
      "<style>html,body{margin:0;font-family:DejaVu Sans;font-size:12pt;line-height:15pt}span{transform:translate(10pt)}i{font-style:normal;display:inline-block;width:30pt;height:15pt;transform:translate(25%,0);transform-origin:right bottom}</style><span>Inline</span><i>Box</i>"

    budget = HtmlValidator.new_image_budget()

    opts = [
      system_font_discovery: false,
      fonts: [%{family: "DejaVu Sans", path: "priv/fonts/dejavu/DejaVuSans.ttf", weight: 400}],
      page_size: {240, 180},
      margin: 0
    ]

    ResourceContext.run(opts, budget, fn _ ->
      assert {:ok, prepared} =
               NativeElixirPdfUtilities.Validators.TemplateExecutionValidator.prepare_source(
                 html,
                 opts,
                 budget
               )

      layout = prepared.layout_tree
      inline = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Inline"))
      box = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Box"))
      assert Map.get(inline, :css_transform_scopes, []) == []
      assert [%{matrix: matrix, border_box: %{width: width}}] = box.css_transform_scopes
      assert width == 30
      assert elem(matrix, 4) == 7.5
    end)
  end

  test "transformed overflow prepares local fields before affine and clip projection" do
    html =
      "<style>html,body{margin:0;font-family:DejaVu Sans;font-size:12pt;line-height:15pt}section{width:90pt;height:24pt;overflow:hidden;transform:rotate(8deg);margin:24pt}input{font-family:DejaVu Sans;font-size:12pt;width:72pt;height:18pt;margin-left:30pt}</style><section><input name='overflow' value='Actual'></section>"

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 180}, margin: 0)

    assert {:ok, [%{name: "overflow", value: "Actual"}]} =
             NativeElixirPdfUtilities.Forms.fields(pdf)

    assert {:ok, filled} = NativeElixirPdfUtilities.Forms.fill(pdf, %{"overflow" => "Changed"})
    assert {:ok, [%{value: "Changed"}]} = NativeElixirPdfUtilities.Forms.fields(filled)
  end

  test "explicit inheritance retains computed parent font units with the child reference size", %{
    style: style
  } do
    assert {:ok, parent} =
             CssTransformValidator.apply(style, "transform", "translate(1em) scale(2)")

    assert {:ok, parent} = CssTransformValidator.apply(parent, "transform-origin", "1em 0")

    child =
      style
      |> Map.merge(%{font_size: 24.0, line_height: 28.8})
      |> Map.put(:_inherited_css_transform, parent.css_transform)
      |> Map.put(
        :_inherited_css_transform_metrics,
        CssTransformValidator.metric_style(parent, "transform")
      )
      |> Map.put(:_inherited_transform_origin, parent.transform_origin)
      |> Map.put(
        :_inherited_transform_origin_metrics,
        CssTransformValidator.metric_style(parent, "transform-origin")
      )

    assert {:ok, child} = CssTransformValidator.apply(child, "transform", "inherit")

    assert {:ok, matrix} =
             CssTransformValidator.resolve(child, %{x: 0, top: 100, width: 100, height: 40})

    assert elem(matrix, 4) == -38
    assert {:ok, child} = CssTransformValidator.apply(child, "transform-origin", "inherit")

    assert {:ok, matrix} =
             CssTransformValidator.resolve(child, %{x: 0, top: 100, width: 100, height: 40})

    assert elem(matrix, 4) == 0
    assert {:ok, child} = CssTransformValidator.apply(child, "transform-origin", "1em 0")

    assert {:ok, matrix} =
             CssTransformValidator.resolve(child, %{x: 0, top: 100, width: 100, height: 40})

    assert elem(matrix, 4) == -12
  end

  test "cross-page transformed fields retain one editable source and clipped page appearances" do
    html =
      File.read!("test/fixtures/html_to_pdf/browser_parity/html_css_affine_widget_slices.html")

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 180}, margin: 12)
    assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert field.name == "actual-slice" and field.value == "Actual source"
    assert Enum.map(field.widgets, & &1.page) == [1, 2]

    assert Enum.all?(field.widgets, fn %{rect: [left, bottom, right, top]} ->
             left >= 12 and right <= 228 and bottom >= 12 and top <= 168
           end)

    assert {:ok, changed} =
             NativeElixirPdfUtilities.Forms.fill(pdf, %{"actual-slice" => "Changed"})

    assert {:ok, [%{value: "Changed", widgets: widgets}]} =
             NativeElixirPdfUtilities.Forms.fields(changed)

    assert Enum.map(widgets, & &1.page) == [1, 2]
    assert {:ok, flattened} = NativeElixirPdfUtilities.Forms.flatten(changed)
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(flattened)
    assert {:ok, _} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(flattened)

    rotated =
      File.read!(
        "test/fixtures/html_to_pdf/browser_parity/html_css_affine_rotated_widget_slices.html"
      )

    assert {:ok, rotated_pdf} = HtmlToPdf.render(rotated, page_size: {240, 180}, margin: 12)

    assert {:ok, [%{widgets: rotated_widgets}]} =
             NativeElixirPdfUtilities.Forms.fields(rotated_pdf)

    assert Enum.map(rotated_widgets, & &1.page) == [1, 2]

    assert {:ok, edited} =
             NativeElixirPdfUtilities.Forms.fill(rotated_pdf, %{"actual-slice" => "Changed"})

    assert {:ok, _} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(edited)
  end

  test "transformed descendant sheets retain links and respect shared page ceilings" do
    html =
      File.read!(
        "test/fixtures/html_to_pdf/browser_parity/html_css_affine_descendant_sheets.html"
      )

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 180}, margin: 12)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    links =
      for {_, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects, do: link

    assert [%{"QuadPoints" => points}] = links
    assert length(points) == 8
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Source overflow"
    clipped = String.replace(html, "background:#eef4f8", "overflow:hidden;background:#eef4f8")

    assert {:ok, hidden} =
             HtmlToPdf.render(clipped, page_size: {240, 180}, margin: 12, compress_streams: false)

    refute hidden =~ "https://example.com/overflow"
    Limits.install(Map.put(Limits.effective(), :max_rendered_pages, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(html, page_size: {240, 180}, margin: 12)

    assert diagnostic.stage == :pagination
    assert diagnostic.message =~ "max_rendered_pages"
  end

  test "checkbox, radio and unnamed text page slices retain source ownership and generated names" do
    for control <- [
          "<input type='checkbox' name='actual' checked>",
          "<input type='radio' name='actual' value='picked' checked>",
          "<input value='Actual'>"
        ] do
      html =
        "<style>html,body{margin:0;font-family:DejaVu Sans;font-size:12pt}section{height:40pt;transform:translate(0,140pt)}input{display:block;width:80pt;height:28pt;box-sizing:border-box;margin:0}.space{height:160pt}</style><section>" <>
          control <> "</section><div class='space'></div>"

      assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 180}, margin: 12)
      assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(pdf)
      assert Enum.map(field.widgets, & &1.page) == [1, 2]

      value =
        case field.type do
          :text -> "Changed"
          :radio -> "picked"
          _ -> false
        end

      assert {:ok, filled} = NativeElixirPdfUtilities.Forms.fill(pdf, %{field.name => value})
      assert {:ok, [%{widgets: widgets}]} = NativeElixirPdfUtilities.Forms.fields(filled)
      assert length(widgets) == 2
    end
  end

  test "positioned cell, flex and grid descendants use their track border boxes" do
    html =
      File.read!(
        "test/fixtures/html_to_pdf/browser_parity/html_css_affine_positioned_tracks.html"
      )

    opts = [
      system_font_discovery: false,
      fonts: [%{family: "DejaVu Sans", path: "priv/fonts/dejavu/DejaVuSans.ttf", weight: 400}],
      page_size: {300, 300},
      margin: 12
    ]

    budget = HtmlValidator.new_image_budget()

    ResourceContext.run(opts, budget, fn _ ->
      assert {:ok, prepared} =
               NativeElixirPdfUtilities.Validators.TemplateExecutionValidator.prepare_source(
                 html,
                 opts,
                 budget
               )

      children =
        Enum.filter(
          prepared.layout_tree.boxes,
          &(Map.get(&1, :fill_color) == {48 / 255, 94 / 255, 132 / 255})
        )

      assert length(children) == 3

      for child <- children do
        scope = List.last(child.css_transform_scopes)
        assert_in_delta child.x, scope.border_box.x + scope.border_box.width * 0.25, 0.0001

        assert_in_delta child.y + child.height,
                        scope.border_box.top - scope.border_box.height * 0.5,
                        0.0001

        assert Map.get(child, :out_of_flow, false)
      end
    end)
  end

  test "a transformed overflow target binds its first actually visible additional sheet" do
    html =
      "<style>html,body{margin:0;font-family:DejaVu Sans;font-size:12pt;line-height:15pt}section{height:40pt;width:150pt;transform:translate(0,5pt)}div{position:absolute;left:18pt;top:180pt;width:110pt;height:25pt;transform:skewX(5deg);transform-origin:0 0}p{margin:0}</style><section><p>Parent flow</p><div id='actual-target'>Actual target</div></section><p><a href='#actual-target'>Jump to target</a></p>"

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 180}, margin: 12)
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)
    assert length(context.pages) == 2
    second = List.last(context.pages).ref

    destinations =
      for {_ref, %{value: %{"Subtype" => {:name, "Link"}, "Dest" => destination}}} <-
            context.document.objects,
          do: destination

    assert [[{:ref, ^second}, {:name, "XYZ"}, x, y, nil]] = destinations
    assert x >= 12 and x <= 228 and y >= 12 and y <= 168
  end
end
