defmodule NativeElixirPdfUtilities.HtmlToPdf.IntegrationWave10Test do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedResource, HtmlParser, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, ObjectValidator}

  setup do
    root = Path.join(System.tmp_dir!(), "native-wave10-#{System.unique_integer([:positive])}")
    File.mkdir_p!(root)
    on_exit(fn -> File.rm_rf!(root) end)
    %{root: root}
  end

  test "local object transport preserves static range clipping and suppresses fallback", %{
    root: root
  } do
    File.write!(Path.join(root, "child.html"), """
    <style>body{margin:0;overflow:hidden}p{margin:0}</style>
    <p>Child range</p><input type=range min=0 max=10 value=5 style='width:160px'>
    """)

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<object data='child.html' type='text/html' style='width:90px;height:60px'>Unused fallback</object><p>After object</p>",
               base_url: root,
               forms: :static,
               default_font: "Helvetica",
               system_font_discovery: false,
               compress_streams: false,
               page_size: {240, 180}
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ ~r/Child\s+range/
    assert text =~ "After object"
    refute text =~ "Unused fallback"
    assert pdf =~ "W n"
    refute pdf =~ "/AcroForm"
  end

  test "default object-loading options preserve local authorization and reject inline descriptors",
       %{root: root} do
    html = "<p>Default object</p>"
    path = Path.join(root, "default.html")
    File.write!(path, html)

    assert :ok =
             ResourceContext.run(
               [authorization_root: root, base_url: root],
               HtmlValidator.new_image_budget(),
               fn parent ->
                 assert {:ok, default} = EmbeddedResource.load_object_html("default.html", parent)

                 assert {:ok, explicit} =
                          EmbeddedResource.load_object_html("default.html", parent, [])

                 assert default.html == explicit.html and default.html == html
                 assert default.source == {:file, path}
                 assert default.context.authorization_root == parent.authorization_root
                 assert default.context.scope == parent.scope
                 refute default.context.document_id == parent.document_id
                 assert ResourceContext.current() == parent

                 assert {:error, {:invalid_document, diagnostic}} =
                          EmbeddedResource.load_object_html({:srcdoc, html}, parent)

                 assert diagnostic.stage == :resource
                 assert diagnostic.reason == :invalid_document
                 assert diagnostic.operation == :load_embedded_resource
                 assert diagnostic.module == ObjectValidator
                 assert diagnostic.message =~ "selected local file reference"
                 :ok
               end
             )

    assert ResourceContext.current() == nil
  end

  test "source contexts visit object fallback ranges without reading selected child documents" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed("""
             <object data='never-read.html' style='width:100px'>
             <input type=range style='accent-color:teal'>
             </object>
             <template><input type=range style='accent-color:orange'></template>
             """)

    assert {:ok, result} =
             Style.with_source_style_contexts(
               dom,
               [],
               HtmlValidator.new_css_budget(),
               fn _node, _pseudo, _context -> :ok end
             )

    obligations = result.source_obligations.declaration_contexts
    assert length(obligations) == 3

    assert Enum.count(obligations, &(&1.status == :checked_in_original_context)) == 2

    assert Enum.any?(obligations, fn obligation ->
             obligation.status == :unresolved and
               obligation.reason == :template_insertion_context
           end)

    assert result.source_obligations.resource_references == :unresolved
    assert :resources in result.runtime_obligations
  end

  test "selected object children retain table presentation and native SVG shading resources", %{
    root: root
  } do
    File.write!(Path.join(root, "combined.html"), """
    <style>
      body{margin:0}svg{width:120px;height:40px}
      td{vertical-align:top}caption{caption-side:bottom}
    </style>
    <svg viewBox='0 0 120 40'>
      <defs>
        <linearGradient id='shade'>
          <stop offset='0' stop-color='red'/><stop offset='1' stop-color='blue'/>
        </linearGradient>
        <rect id='tile' width='30' height='20' fill='url(#shade)'/>
      </defs>
      <use href='#tile' x='5' y='5'/><use href='#tile' x='45' y='5'/>
    </svg>
    <table cellspacing=3 cellpadding=2 bgcolor='#eeeeee'>
      <caption>Child caption</caption><tr><td>Child grid</td></tr>
    </table>
    """)

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<object data='combined.html' type='text/html' style='width:150px;height:120px'>Unused child</object><p>Parent tail</p>",
               base_url: root,
               forms: :static,
               default_font: "Helvetica",
               system_font_discovery: false,
               compress_streams: false,
               page_size: {240, 220}
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ ~r/Child\s+grid/
    assert text =~ ~r/Child\s+caption/
    assert text =~ ~r/Parent\s+tail/
    refute text =~ "Unused child"
    assert pdf =~ "/ShadingType 2"
    assert pdf =~ "/FunctionType 2"
    assert pdf =~ "/Shading"
    assert pdf =~ "W n"
    refute pdf =~ "/AcroForm"
  end
end
