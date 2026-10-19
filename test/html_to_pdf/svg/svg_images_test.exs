alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgImagesTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{ResourceContext, SvgRasterizer}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  setup do
    limits = Limits.effective()
    dir = Path.join(System.tmp_dir!(), "svg-images-#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(dir, "nested"))

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(dir)
    end)

    png =
      PngFixture.build(2, 1, 2, 8, 0, fn x, _ ->
        if x == 0 do
          [255, 0, 0]
        else
          [0, 0, 255]
        end
      end)

    File.write!(Path.join(dir, "nested/paint.png"), png)
    %{dir: dir, png: png, limits: limits}
  end

  test("unused definitions and display none never call the root resolver", %{png: png}) do
    parent = self()

    resolver = fn %{reference: ref} ->
      send(parent, {:loaded, ref})
      {:ok, png}
    end

    html =
      "<svg width=40 height=20><defs><image id=unused href='unused.png' width=2 height=1 /><image id=selected href='selected.png' width=2 height=1 /></defs><g style='display:none'><image href='hidden.png' width=20 height=20 /></g><use href='#selected' width=20 height=10 /><image href='visible.png' x=20 width=20 height=20 /></svg>"

    assert {:ok, _} = HtmlToPdf.render(html, asset_resolver: resolver)
    assert_received {:loaded, "selected.png"}
    assert_received {:loaded, "visible.png"}
    refute_received {:loaded, "unused.png"}
    refute_received {:loaded, "hidden.png"}
  end

  test("nested SVG converts to PNG and resolves child images against its source directory", %{
    dir: dir
  }) do
    File.write!(
      Path.join(dir, "nested/art.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='10' viewBox='0 0 20 10'><image href='paint.png' width='20' height='10' /><rect x='8' width='4' height='10' fill='yellow' /></svg>"
    )

    html =
      "<svg width=100 height=50><image href='nested/art.svg' width=100 height=50 preserveAspectRatio='none' /></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html, base_url: dir)
    assert pdf =~ "/Subtype /Image"
    assert {:ok, _} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end

  test("nested local resources cannot acquire root mappings resolver or broader permission", %{
    dir: dir,
    png: png
  }) do
    parent = self()

    resolver = fn request ->
      send(parent, {:resolver, request})
      {:ok, png}
    end

    for ref <- ["https://private.invalid/art.png", "../../private.png"] do
      File.write!(
        Path.join(dir, "nested/art.svg"),
        "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='10'><image href='#{ref}' width='20' height='10' /></svg>"
      )

      assert {:error, {:resource_denied, diagnostic}} =
               HtmlToPdf.render(
                 "<svg width=20 height=10><image href='nested/art.svg' width=20 height=10 /></svg>",
                 base_url: dir,
                 assets: %{ref => {:bytes, png}},
                 asset_resolver: resolver
               )

      assert diagnostic.source == ref
      assert diagnostic.stage == :resource
      assert diagnostic.message != ""
      refute_received {:resolver, _}
    end
  end

  test("nested image cycles report the actual selected reference", %{dir: dir}) do
    File.write!(
      Path.join(dir, "nested/cycle.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='10'><image href='cycle.svg' width='20' height='10' /></svg>"
    )

    assert {:error, {:resource_cycle, diagnostic}} =
             HtmlToPdf.render(
               "<svg width=20 height=10><image href='nested/cycle.svg' width=20 height=10 /></svg>",
               base_url: dir
             )

    assert diagnostic.source == "cycle.svg"
    assert diagnostic.message =~ "ancestor"
  end

  test("scene paint cache keys include viewport source fonts styles and issued source scope", %{
    dir: dir,
    png: png
  }) do
    ResourceContext.run([authorization_root: dir], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} =
               NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse_detailed(
                 "<svg width=40 height=20><image href='paint.png' width=40 height=20 /></svg>"
               )

      assert {:ok, %{children: [node]}} =
               NativeElixirPdfUtilities.HtmlToPdf.Style.compute_detailed(
                 dom,
                 [assets: %{"paint.png" => {:bytes, png}}],
                 context.image_budget
               )

      scene = node.style.svg_scene
      assert {:ok, first} = NativeElixirPdfUtilities.HtmlToPdf.SvgScene.paint(scene, 30, 15)
      used = :atomics.get(context.image_budget, 3)
      assert {:ok, ^first} = NativeElixirPdfUtilities.HtmlToPdf.SvgScene.paint(scene, 30, 15)
      assert :atomics.get(context.image_budget, 3) > used

      flexible = %{
        scene
        | attributes: %{},
          style:
            Map.merge(scene.style, %{svg_width: {:percent, 1.0}, svg_height: {:percent, 1.0}})
      }

      assert {:ok, wider} = NativeElixirPdfUtilities.HtmlToPdf.SvgScene.paint(flexible, 60, 30)
      refute wider.image.data == first.image.data

      assert {:ok, _, child} =
               NativeElixirPdfUtilities.Validators.ResourceValidator.authorize_document(
                 {:srcdoc, ""},
                 context
               )

      ResourceContext.with_document(child, fn ->
        assert {:error, {:invalid_document, %{message: message}}} =
                 NativeElixirPdfUtilities.HtmlToPdf.SvgScene.paint(scene, 30, 15)

        assert message =~ "issued source document scope"
      end)
    end)
  end

  test(
    "root file mappings use their actual source directory and remote byte sources cannot borrow a local base",
    %{dir: dir, png: png}
  ) do
    File.write!(
      Path.join(dir, "nested/mapped.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='10'><image href='paint.png' width='20' height='10'/></svg>"
    )

    assert {:ok, _} =
             HtmlToPdf.render(
               "<svg width=20 height=10><image href='alias.svg' width=20 height=10 /></svg>",
               base_url: dir,
               assets: %{"alias.svg" => {:file, Path.join(dir, "nested/mapped.svg")}}
             )

    bytes =
      "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='10'><image href='nested/paint.png' width='20' height='10'/></svg>"

    assert {:error, {:resource_denied, %{source: "nested/paint.png"}}} =
             HtmlToPdf.render(
               "<svg width=20 height=10><image href='https://root.invalid/art.svg' width=20 height=10 /></svg>",
               base_url: dir,
               assets: %{
                 "https://root.invalid/art.svg" => {:bytes, bytes},
                 "nested/paint.png" => {:bytes, png}
               }
             )
  end

  test("unused and hidden use reference chains do not select image resources", %{png: png}) do
    caller = self()

    resolver = fn request ->
      send(caller, request.reference)
      {:ok, png}
    end

    html =
      "<svg width=40 height=20><defs><g id=unused><use href='#asset' /></g><image id=asset href='never.png' width=2 height=1 /></defs><use href='#asset' display=none /><rect width=40 height=20 fill=red /></svg>"

    assert {:ok, _} = HtmlToPdf.render(html, asset_resolver: resolver)
    refute_received "never.png"
  end

  test("nested source failures remain diagnostics and cannot turn XML into unrelated HTML", %{
    dir: dir
  }) do
    for bytes <- ["<p>Wrong document</p>", "<svg><rect></svg>", "<!DOCTYPE svg><svg/>"] do
      File.write!(Path.join(dir, "nested/invalid.svg"), bytes)

      assert {:error, {_, %{message: message, stage: stage}}} =
               HtmlToPdf.render(
                 "<svg width=20 height=10><image href='nested/invalid.svg' width=20 height=10 /></svg>",
                 base_url: dir
               )

      assert stage in [:html, :style, :layout]
      assert message != ""
    end
  end

  test "JPEG resources and invalid selected JPEGs use the shared diagnostic contract" do
    jpeg =
      File.read!(Paths.fixture("html_to_pdf/browser_parity/assets/embedded-neutral.jpg"))

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<svg width=80 height=40><image href='photo.jpg' width=80 height=40 /></svg>",
               assets: %{"photo.jpg" => {:bytes, jpeg}}
             )

    assert pdf =~ "/Subtype /Image"

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render(
               "<svg width=80 height=40><image href='photo.jpg' width=80 height=40 /></svg>",
               assets: %{"photo.jpg" => {:bytes, <<255, 216, 0>>}}
             )

    assert diagnostic.stage == :style
    assert diagnostic.message =~ "JPEG resource bytes"
  end

  test("unsized nested SVGs receive the real replaced document default viewport", %{dir: dir}) do
    File.write!(
      Path.join(dir, "nested/default.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg'><rect width='300' height='150' fill='red'/></svg>"
    )

    assert {:ok, _} =
             HtmlToPdf.render(
               "<svg width=120 height=60><image href='nested/default.svg' width=120 height=60 /></svg>",
               base_url: dir
             )
  end

  test("cache hits cannot authorize a previously loaded file for a narrower document", %{dir: dir}) do
    ResourceContext.run([authorization_root: dir], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, _} =
               NativeElixirPdfUtilities.HtmlToPdf.AssetLoader.resolve_local(
                 "nested/paint.png",
                 :image,
                 context
               )

      assert {:ok, _, child} =
               NativeElixirPdfUtilities.Validators.ResourceValidator.authorize_document(
                 {:srcdoc, ""},
                 context
               )

      assert {:ok, child} =
               ResourceContext.rebase(child, {:document_url, "https://child.invalid/view.svg"})

      assert {:error, {:resource_denied, %{source: "nested/paint.png"}}} =
               NativeElixirPdfUtilities.HtmlToPdf.AssetLoader.resolve_local(
                 "nested/paint.png",
                 :image,
                 child
               )
    end)
  end

  test("nested foreignObject paints issued HTML text images and transformed links", %{dir: dir}) do
    File.write!(
      Path.join(dir, "nested/html.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='160' height='60'><foreignObject x='4' y='2' width='152' height='56'><div xmlns='http://www.w3.org/1999/xhtml' style='font-family:DejaVu Sans;font-size:14px'><a href='https://example.test/nested'>Nested café Ω</a><img src='paint.png' width='20' height='10'/></div></foreignObject></svg>"
    )

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(
                 "<svg width=240 height=90><g transform='translate(8 4)'><image href='nested/html.svg' width=200 height=75 /></g></svg>",
                 base_url: dir
               )

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :convert
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test(
    "nested foreignObject keeps distinct issued scopes at repeated sibling and grandchild slots",
    %{dir: dir}
  ) do
    for {name, text} <- [{"first", "First Ω"}, {"second", "Second café"}] do
      File.write!(
        Path.join(dir, "nested/#{name}.svg"),
        "<svg xmlns='http://www.w3.org/2000/svg' width='100' height='30'><foreignObject width='100' height='30'><div xmlns='http://www.w3.org/1999/xhtml' style='font-family:DejaVu Sans;font-size:14px'>#{text}</div></foreignObject></svg>"
      )
    end

    File.write!(
      Path.join(dir, "nested/parent.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='200' height='60'><image href='first.svg' width='100' height='30'/><image href='second.svg' x='100' width='100' height='30'/></svg>"
    )

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(
                 "<svg width=200 height=90><foreignObject width='100' height='30'><div style='font-family:DejaVu Sans;font-size:14px'>Outer</div></foreignObject><image href='nested/parent.svg' y='30' width='200' height='60'/><image href='nested/first.svg' y='60' width='100' height='30'/></svg>",
                 base_url: dir
               )

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test("nested foreignObject local HTML resources cannot use the host resolver", %{
    dir: dir,
    png: png
  }) do
    parent = self()

    File.write!(
      Path.join(dir, "nested/denied-html.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='100' height='30'><foreignObject width='100' height='30'><div xmlns='http://www.w3.org/1999/xhtml'><img src='https://private.invalid/p.png'/></div></foreignObject></svg>"
    )

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<svg width=100 height=30><image href='nested/denied-html.svg' width=100 height=30 /></svg>",
               base_url: dir,
               asset_resolver: fn request ->
                 send(parent, {:host_resolver, request})
                 {:ok, png}
               end
             )

    assert diagnostic.stage == :layout
    assert diagnostic.operation == :convert
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.source == "foreignObject"
    assert diagnostic.message =~ "does not render foreignObject"
    refute_received {:host_resolver, _}
  end

  test(
    "unsupported nested HTML is rejected before decoding its images, including repeated byte hits",
    %{dir: dir, limits: limits}
  ) do
    File.write!(
      Path.join(dir, "nested/budget-html.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='4' height='2'><foreignObject width='4' height='2'><div xmlns='http://www.w3.org/1999/xhtml'><img src='paint.png' width='2' height='1'/></div></foreignObject></svg>"
    )

    Limits.install(Map.put(limits, :max_aggregate_decoded_image_bytes, 10))

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<svg width='8' height='2'><image href='nested/budget-html.svg' width='4' height='2'/><image href='nested/budget-html.svg' x='4' width='4' height='2'/></svg>",
               base_url: dir
             )

    assert diagnostic.stage == :layout
    assert diagnostic.operation == :convert
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.source == "foreignObject"
    assert diagnostic.message =~ "does not render foreignObject"
  end

  test("nested HTML retains caller-selected font bytes with system discovery disabled", %{
    dir: dir
  }) do
    data = File.read!(Paths.project("priv/fonts/dejavu/DejaVuSansMono.ttf"))

    File.write!(
      Path.join(dir, "nested/font-html.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='140' height='30'><foreignObject width='140' height='30'><div xmlns='http://www.w3.org/1999/xhtml' style='font-family:IssuedFace;font-size:14px'>Issued café Ω</div></foreignObject></svg>"
    )

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(
                 "<svg width='140' height='30'><image href='nested/font-html.svg' width='140' height='30'/></svg>",
                 base_url: dir,
                 default_font: "IssuedFace",
                 fonts: [%{family: "IssuedFace", data: data}],
                 system_font_discovery: false
               )

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :convert
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test("parent filters sample actual scoped nested HTML slots", %{dir: dir}) do
    File.write!(
      Path.join(dir, "nested/filter-html.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='100' height='30'><foreignObject width='100' height='30'><div xmlns='http://www.w3.org/1999/xhtml'>Actual HTML</div></foreignObject></svg>"
    )

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(
                 "<svg width=100 height=30><defs><filter id='f'><feOffset dx='1'/></filter></defs><g filter='url(#f)'><image href='nested/filter-html.svg' width=100 height=30 /></g></svg>",
                 base_url: dir
               )

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :convert
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test("parent filter sampling follows nested HTML through patterns masks and use references", %{
    dir: dir
  }) do
    File.write!(
      Path.join(dir, "nested/filter-reference-html.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='10' height='10'><foreignObject width='10' height='10'><div xmlns='http://www.w3.org/1999/xhtml' style='background:red;width:10px;height:10px'/></foreignObject></svg>"
    )

    for {definitions, artwork} <- [
          {"<pattern id='p' patternUnits='userSpaceOnUse' width='10' height='10'><image href='nested/filter-reference-html.svg' width='10' height='10'/></pattern>",
           "<rect width='40' height='20' fill='url(#p)'/>"},
          {"<mask id='m'><image href='nested/filter-reference-html.svg' width='10' height='10'/></mask>",
           "<rect width='40' height='20' mask='url(#m)'/>"},
          {"<image id='i' href='nested/filter-reference-html.svg' width='10' height='10'/>",
           "<use href='#i'/>"}
        ] do
      (
        assert {:error, {:unsupported_html, diagnostic}} =
                 HtmlToPdf.render(
                   "<svg width='40' height='20'><defs><filter id='f'><feOffset dx='1'/></filter>#{definitions}</defs><g filter='url(#f)'>#{artwork}</g></svg>",
                   base_url: dir
                 )

        assert diagnostic.reason == :unsupported_html
        assert diagnostic.stage == :layout
        assert diagnostic.operation == :convert
        assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
        assert diagnostic.source == "foreignObject"
        assert diagnostic.message =~ "foreignObject"
      )

      if String.contains?(definitions, "<mask") do
      else
      end
    end
  end

  test("blank scene cache hits charge only the raster work performed by a cold paint", %{dir: dir}) do
    ResourceContext.run([authorization_root: dir], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} =
               NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse_detailed(
                 "<svg width=20 height=10><rect width=20 height=10 fill=none /></svg>"
               )

      assert {:ok, %{children: [node]}} =
               NativeElixirPdfUtilities.HtmlToPdf.Style.compute_detailed(
                 dom,
                 [],
                 context.image_budget
               )

      assert {:ok, %{image: %{alpha_data: alpha}}} =
               NativeElixirPdfUtilities.HtmlToPdf.SvgScene.paint(node.style.svg_scene, 15, 7.5)

      assert Enum.all?(:binary.bin_to_list(alpha), &(&1 == 0))
      cold_bytes = :atomics.get(context.image_budget, 3)

      assert {:ok, %{image: %{alpha_data: alpha}}} =
               NativeElixirPdfUtilities.HtmlToPdf.SvgScene.paint(node.style.svg_scene, 15, 7.5)

      assert Enum.all?(:binary.bin_to_list(alpha), &(&1 == 0))
      assert :atomics.get(context.image_budget, 3) - cold_bytes == cold_bytes
    end)
  end

  test("standalone restricted SVG image API still rejects all image dependencies", %{png: png}) do
    uri = "data:image/png;base64," <> Base.encode64(png)

    assert {:error, {:invalid_document, %{message: message}}} =
             SvgRasterizer.rasterize(
               "<svg width='2' height='1'><image href='#{uri}' width='2' height='1' /></svg>",
               [],
               HtmlValidator.new_image_budget()
             )

    assert message =~ "not authorized"
  end

  alias NativeElixirPdfUtilities.HtmlToPdf.{PngDecoder}
  alias NativeElixirPdfUtilities.{Limits}

  test("selected PNG resources paint in SVG order with meet slice and affine groups", %{png: png}) do
    html =
      "<svg width=100 height=60><rect width=100 height=60 fill=green /><g transform='translate(10 5)'><image href='paint.png' width=80 height=40 preserveAspectRatio='none' /><rect x=35 width=10 height=40 fill=yellow /></g></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               assets: %{"paint.png" => {:bytes, png}}
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    context = %{
      mode: :inline,
      resources: %{"svg-resource:paint" => %{data: png, media_type: "image/png"}}
    }

    assert {:ok, raster} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               "<svg width='4' height='1'><image href='svg-resource:paint' width='4' height='1' preserveAspectRatio='none' /></svg>",
               [width: 4, height: 1],
               context
             )

    assert {:ok, image} = PngDecoder.decode(raster, HtmlValidator.new_image_budget(), false)
    assert binary_part(image.data, 0, 3) == <<255, 0, 0>>
    assert binary_part(image.data, 9, 3) == <<0, 0, 255>>
  end
end
