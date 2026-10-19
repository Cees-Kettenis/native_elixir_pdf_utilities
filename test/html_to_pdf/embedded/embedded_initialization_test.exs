defmodule NativeElixirPdfUtilities.HtmlToPdf.EmbeddedInitializationTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedResource, EmbeddedViewport, ResourceContext}
  alias NativeElixirPdfUtilities.Limits

  alias NativeElixirPdfUtilities.Validators.{
    HtmlUrlValidator,
    HtmlValidator,
    ObjectValidator,
    ResourceValidator
  }

  setup do
    root = Path.join(System.tmp_dir!(), "native-initial-#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(root, "nested"))

    File.write!(
      Path.join(root, "nested/child.html"),
      "<style>p{color:red}</style><p>Selected local child</p>"
    )

    File.write!(Path.join(root, "nested/leaf.html"), "<p>Nested leaf child</p>")
    limits = Limits.effective()

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(root)
    end)

    %{root: root}
  end

  test "blank URL classification follows scheme, opaque path, host and credentials authority" do
    for source <- [
          "about:blank",
          "ABOUT:blank",
          "about:blank#target",
          "about:blank?query#target",
          " \tabout:blank?query\n#target\r"
        ] do
      assert {:ok, url} = HtmlUrlValidator.blank_document_url(source)
      assert URI.parse(url).scheme == "about"
      assert URI.parse(url).path == "blank"
    end

    for source <- [
          nil,
          12,
          <<255>>,
          "",
          "about:Blank",
          "about:blank/",
          "about:srcdoc",
          "about://blank",
          "about://host/blank",
          "about://user:secret@host/blank",
          "about:bl%61nk",
          "https://example.test/about:blank",
          "about:blank space"
        ] do
      assert :not_blank = HtmlUrlValidator.blank_document_url(source)
    end
  end

  test "blank sources preserve requested spelling and inherit issued file bases without resource lookup",
       %{root: root} do
    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      active = ResourceContext.current()

      assert {:ok, parent} =
               ResourceContext.rebase(
                 parent,
                 {:document_url, "file://" <> Path.join(root, "nested/parent.html")}
               )

      for source <- ["ABOUT:blank", "about:blank?query#target"] do
        assert {:blank, ^source} = ResourceValidator.iframe_source(%{"src" => source})

        assert {:ok, {:blank, ^source}} =
                 ObjectValidator.select(%{"src" => source}, parent, :embed)

        assert {:ok, first} = EmbeddedViewport.prepare_iframe_document({:blank, source}, parent)

        assert {:ok, {:viewport, second}} =
                 EmbeddedViewport.prepare_selected({:blank, source}, parent, [], :embed)

        assert first.source == {:blank, source} and second.source == {:blank, source}
        assert first.document_url == parent.resolution_base
        assert first.context.resolution_base == parent.resolution_base
        assert first.context.authorization_root == parent.authorization_root
        assert first.context.image_budget == parent.image_budget
        assert first.context.document_id != second.context.document_id
        assert first.context.tree_scope_id != second.context.tree_scope_id
        assert first.context.depth == parent.depth + 1
        assert {:ok, group} = EmbeddedViewport.layout(first, 90, 45)
        assert group.clip == %{x: 0.0, y: 0.0, width: 90, height: 45}
        assert ResourceContext.current() == active
      end

      for source <- ["https://denied.test/file", "about://host/blank", "about:Blank"] do
        assert {:error, {:invalid_document, diagnostic}} =
                 EmbeddedResource.load_html({:blank, source}, parent)

        assert diagnostic.stage == :resource and diagnostic.message =~ "about:blank"
        assert diagnostic.source == source

        assert {:error, {:invalid_document, _}} =
                 ObjectValidator.prepare_selection({:blank, source}, :embed)
      end
    end)
  end

  test "selected srcdoc never resolves src and an invalid selected document never falls back", %{
    root: root
  } do
    owner = self()

    resolver = fn request ->
      send(owner, {:resolved, request})
      {:ok, "<p>Replacement</p>"}
    end

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      for html <- ["", "<p>Selected inline child</p>"] do
        assert {:srcdoc, ^html} =
                 ResourceValidator.iframe_source(%{
                   "srcdoc" => html,
                   "src" => "https://denied.test/file"
                 })

        assert {:ok, child} =
                 EmbeddedViewport.prepare_iframe_document({:srcdoc, html}, parent,
                   asset_resolver: resolver
                 )

        assert child.source == {:srcdoc, html}
        assert child.context.authorization_root == parent.authorization_root
      end

      source =
        ResourceValidator.iframe_source(%{"srcdoc" => <<255>>, "src" => "nested/child.html"})

      assert {:error, {:invalid_encoding, diagnostic}} =
               EmbeddedViewport.prepare_iframe_document(source, parent, asset_resolver: resolver)

      assert diagnostic.stage == :html and diagnostic.operation == :load_embedded_resource
      assert diagnostic.source == "about:srcdoc"
    end)

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<iframe src='https://denied.test/file' srcdoc='&lt;p&gt;Selected inline child&lt;/p&gt;'></iframe>",
               base_url: root,
               asset_resolver: resolver
             )

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Selected inline child"
    refute_receive {:resolved, _}
  end

  test "nested srcdoc inherits its selected local document base and keeps sibling target scopes distinct",
       %{root: root} do
    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, local} = EmbeddedViewport.prepare_iframe_document("nested/child.html", parent)
      assert {:ok, local} = EmbeddedViewport.style_viewport(local, 150, 90)
      [local_body] = local.tree.children

      assert Enum.find(local_body.children, &(Map.get(&1, :tag) == "p")).style.color ==
               {1.0, 0.0, 0.0}

      child_html =
        "<a href='#target'>Jump</a><p id='target'>Target</p><iframe src='leaf.html'></iframe>"

      assert {:ok, first} =
               EmbeddedViewport.prepare_iframe_document({:srcdoc, child_html}, local.context)

      assert {:ok, second} =
               EmbeddedViewport.prepare_iframe_document({:srcdoc, child_html}, local.context)

      assert first.context.resolution_base == local.context.resolution_base
      assert first.context.authorization_root == root
      assert first.context.image_budget == parent.image_budget
      assert first.context.depth == 2
      assert first.context.document_id != second.context.document_id
      assert {:ok, first} = EmbeddedViewport.style_viewport(first, 150, 90)
      assert {:ok, second} = EmbeddedViewport.style_viewport(second, 150, 90)
      [inline_body] = first.tree.children

      assert Enum.find(inline_body.children, &(Map.get(&1, :tag) == "p")).style.color ==
               {0.0, 0.0, 0.0}

      assert first.tree.link_scope != second.tree.link_scope
      assert first.tree.link_scope == {first.context.document_id, first.context.tree_scope_id}
      assert ResourceContext.current() == parent
    end)
  end

  test "blank children share document/depth ceilings and nested selected files retain cycle denial",
       %{root: root} do
    Limits.install(Map.put(Limits.effective(), :max_embedded_documents, 2))

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, child} = EmbeddedResource.load_html({:blank, "about:blank?one"}, parent)
      assert {:ok, _} = EmbeddedResource.load_html({:blank, "about:blank#two"}, parent)

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               EmbeddedResource.load_html({:blank, "about:blank"}, parent)

      assert diagnostic.message =~ "max_embedded_documents"
      assert child.context.image_budget == parent.image_budget
    end)

    Limits.install(Map.put(Limits.effective(), :max_embedded_document_depth, 1))

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, child} = EmbeddedResource.load_html({:blank, "about:blank"}, parent)

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               EmbeddedResource.load_html({:srcdoc, "<p>Nested</p>"}, child.context)

      assert diagnostic.message =~ "max_embedded_document_depth"
    end)

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, child} = EmbeddedResource.load_html("nested/child.html", parent)

      assert {:error, {:resource_cycle, diagnostic}} =
               EmbeddedResource.load_html("child.html", child.context)

      assert diagnostic.source == "child.html"
    end)
  end
end
