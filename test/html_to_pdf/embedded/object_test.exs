defmodule NativeElixirPdfUtilities.HtmlToPdf.ObjectTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedViewport, ResourceContext}
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, ObjectValidator}

  setup do
    root = Path.join(System.tmp_dir!(), "native-object-#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(root, "child"))

    File.write!(Path.join(root, "child/document.html"), """
    <style>body{margin:0;overflow:hidden}p{margin:0;color:red}</style>
    <p id='target'>Selected child</p><a href='#target'>Child link</a>
    """)

    limits = Limits.effective()

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(root)
    end)

    %{root: root}
  end

  test "absent data and unsupported MIME expose normal authored fallback without fetching" do
    owner = self()
    resolver = fn request -> send(owner, {:resolved, request}) end

    for attributes <- [
          "",
          "data='https://never.test/no-file' type='application/x-unknown'",
          "data='https://never.test/no-file' type='malformed'",
          "data='https://never.test/no-file'"
        ] do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<p>Before <object #{attributes}><b>Fallback</b></object> after</p>",
                 asset_resolver: resolver
               )

      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
      assert text =~ "Before Fallback after"
      refute_received {:resolved, _}
    end

    assert {:ok, pdf} = HtmlToPdf.render("<object><div>Flow fallback</div></object>")
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Flow fallback"

    assert {:error, {:unsupported_html, _}} =
             HtmlToPdf.render("<p><object><div>Invalid phrasing fallback</div></object></p>")

    assert {:error, {:unsupported_html, _}} =
             HtmlToPdf.render(
               "<a href='#x'><object usemap='#m'>Interactive fallback</object></a>"
             )
  end

  test "selected local HTML suppresses fallback resources and keeps styles and links isolated", %{
    root: root
  } do
    html = """
    <style>p{color:blue}</style><p id='target'>Parent text</p>
    <object data=' child/document.html?query=1#target ' type=' TEXT/HTML ; charset=UTF-8'
            width='120' height='80' style='border:1px solid black'>
      <img src='https://never.test/fallback.png'><input name='same' value='Fallback field'>
      <p>Unused fallback</p>
    </object><p>After object</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, base_url: root, forms: :static)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Parent text" and text =~ "Selected child" and text =~ "After object"
    refute text =~ "Unused fallback"
    refute text =~ "Fallback field"
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert length(document.pages) == 1
    assert pdf =~ "/Dest"

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, surface} =
               EmbeddedViewport.prepare_object_document("child/document.html", parent)

      assert surface.host == :object
      assert surface.context.authorization_root == parent.authorization_root
      assert surface.context.image_budget == parent.image_budget
      assert surface.context.document_id != parent.document_id
      assert surface.context.tree_scope_id != parent.tree_scope_id

      assert {:error, {:invalid_document, diagnostic}} =
               EmbeddedViewport.prepare_object_document({:srcdoc, "<p>Inline bypass</p>"}, parent)

      assert diagnostic.stage == :resource and diagnostic.message =~ "local file"

      assert {:error, {:invalid_document, _}} =
               EmbeddedViewport.prepare_object_document(nil, parent)

      assert {:ok, group} = EmbeddedViewport.layout(surface, 90, 60)
      assert group.clip == %{x: 0.0, y: 0.0, width: 90, height: 60}
      assert Enum.any?(group.boxes, &(&1.type == :text and &1.text == "Selected child"))
      assert ResourceContext.current() == parent
    end)
  end

  test "inferred HTML and explicit extensionless HTML use selected sources", %{root: root} do
    File.cp!(Path.join(root, "child/document.html"), Path.join(root, "child/typed"))

    for attributes <- ["data='child/document.html'", "data='child/typed' type='text/html'"] do
      assert {:ok, pdf} =
               HtmlToPdf.render("<object #{attributes}>Unused</object>", base_url: root)

      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
      assert text =~ "Selected child"
      refute text =~ "Unused"
    end
  end

  test "hidden objects never select their denied source" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<object style='display:none' data='https://denied.test/document.html'></object><p>Visible</p>"
             )

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Visible"
  end

  test "authorization, missing files and content failures do not become fallback", %{root: root} do
    owner = self()
    resolver = fn request -> send(owner, {:resolved, request}) end
    File.ln_s!(Path.join(root, "child/document.html"), Path.join(root, "link.html"))

    for source <- [
          "https://denied.test/document.html",
          "//denied.test/document.html",
          "../escape.html",
          "link.html"
        ] do
      assert {:error, {:resource_denied, diagnostic}} =
               HtmlToPdf.render("<object data='#{source}'>Fallback</object>",
                 base_url: root,
                 assets: %{source => {:bytes, "<p>Mapped bypass</p>"}},
                 asset_resolver: resolver
               )

      assert diagnostic.stage == :resource
      assert diagnostic.operation == :load_embedded_resource
      assert diagnostic.source == source
      assert diagnostic.message != ""
      refute_received {:resolved, _}
    end

    assert {:error, {:resource_not_found, diagnostic}} =
             HtmlToPdf.render("<object data='missing.html'>Fallback</object>", base_url: root)

    assert diagnostic.source == "missing.html" and diagnostic.message != ""
    File.write!(Path.join(root, "invalid.html"), "%PDF-1.7\n")

    assert {:error, {:invalid_mime_type, diagnostic}} =
             HtmlToPdf.render("<object data='invalid.html'>Fallback</object>", base_url: root)

    assert diagnostic.stage == :resource and diagnostic.message =~ "MIME"
    assert diagnostic.source == Path.join(root, "invalid.html")
    File.write!(Path.join(root, "invalid.html"), <<255>>)

    assert {:error, {:invalid_encoding, _}} =
             HtmlToPdf.render("<object data='invalid.html'>Fallback</object>", base_url: root)
  end

  test "PDF embedding diagnoses its 1.5 dependency after source authorization", %{root: root} do
    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:error, {:not_implemented, diagnostic}} =
               ObjectValidator.select(
                 %{"data" => "child.pdf", "type" => "application/pdf"},
                 parent
               )

      assert diagnostic.stage == :resource and diagnostic.source == "child.pdf"
      assert diagnostic.message =~ "1.5.0"

      assert {:error, {:resource_denied, _}} =
               ObjectValidator.select(
                 %{"data" => "https://denied.test/child.pdf", "type" => "application/pdf"},
                 parent
               )
    end)
  end

  test "selected child cycles, depth and document budgets remain cumulative", %{root: root} do
    File.write!(Path.join(root, "child/document.html"), "<object data='document.html'></object>")

    assert {:error, {:resource_cycle, diagnostic}} =
             HtmlToPdf.render("<object data='child/document.html'></object>", base_url: root)

    assert diagnostic.message =~ "ancestor"
    File.write!(Path.join(root, "child/document.html"), "<p>Leaf</p>")
    Limits.install(Map.put(Limits.defaults(), :max_embedded_documents, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<object data='child/document.html'></object><object data='child/document.html'></object>",
               base_url: root
             )

    assert diagnostic.message =~ "max_embedded_documents"
  end
end
