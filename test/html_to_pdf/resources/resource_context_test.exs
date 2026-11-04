alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.ResourceContextTest do
  use ExUnit.Case, async: false

  Code.require_file(Paths.support("png_fixture.ex"))

  alias NativeElixirPdfUtilities.HtmlToPdf.AssetLoader
  alias NativeElixirPdfUtilities.HtmlToPdf.CssParser
  alias NativeElixirPdfUtilities.HtmlToPdf.EmbeddedResource
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.HtmlToPdf.PngDecoder
  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache
  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.TestSupport.PngFixture
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.ResourceValidator

  setup do
    directory =
      Path.join(System.tmp_dir!(), "native-pdf-context-#{System.unique_integer([:positive])}")

    root = Path.join(directory, "root")
    File.mkdir_p!(Path.join(root, "children"))
    File.mkdir_p!(Path.join(directory, "root-other"))
    File.write!(Path.join(root, "children/child.html"), "<p>Child</p>")
    File.write!(Path.join(root, "shared.bin"), "shared")
    File.write!(Path.join(root, "children/style.css"), "p { color: red }")
    File.write!(Path.join(directory, "outside.html"), "<p>Outside</p>")
    limits = Limits.effective()

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(directory)
    end)

    %{root: root, directory: directory}
  end

  test "local documents have separate identities and inherit immutable permissions and budgets",
       %{root: root} do
    run(root, fn context ->
      path = Path.join(root, "children/child.html")

      assert {:ok, {:file, ^path}, authorized} =
               ResourceValidator.authorize_document("children/child.html", context)

      assert authorized.authorization_root == root
      assert authorized.image_budget == context.image_budget

      assert {:ok, child} = EmbeddedResource.load_html("children/child.html", context)
      assert child.html == "<p>Child</p>"
      assert child.dom.type == :document
      assert child.context.authorization_root == root
      assert child.context.resolution_base == Path.join(root, "children")
      assert child.context.scope == context.scope
      assert child.context.image_budget == context.image_budget
      refute child.context.document_id == context.document_id
      refute child.context.tree_scope_id == context.tree_scope_id
      assert child.context.depth == 1

      assert {:ok, "shared"} = AssetLoader.resolve_local("../shared.bin", :image, child.context)

      assert {:ok, "shared"} =
               AssetLoader.resolve_local(Path.join(root, "shared.bin"), :font, child.context)

      assert {:ok, sibling} =
               EmbeddedResource.load_html(Path.join(root, "children/child.html"), context)

      refute child.context.document_id == sibling.context.document_id
      assert sibling.context.ancestors == child.context.ancestors
    end)
  end

  test "srcdoc is HTML rather than a path, including empty content and no local root", %{
    root: root
  } do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, %{html: "", source: {:srcdoc, ""}}} =
               EmbeddedResource.load_html({:srcdoc, ""}, context)

      assert {:ok, %{html: "children/child.html"}} =
               EmbeddedResource.load_html({:srcdoc, "children/child.html"}, context)

      assert {:error, {:resource_denied, _}} =
               EmbeddedResource.load_html("children/child.html", context)

      for kind <- [:image, :background_image, :font, :stylesheet] do
        assert {:ok, "inline bytes"} =
                 AssetLoader.resolve_local({:bytes, "inline bytes"}, kind, context)
      end
    end)

    run(root, fn context ->
      assert {:error, {:invalid_encoding, %{stage: :html, message: message}}} =
               EmbeddedResource.load_html({:srcdoc, <<255>>}, context)

      assert message =~ "UTF-8"
      assert {:error, {:invalid_html, _}} = EmbeddedResource.load_html({:srcdoc, nil}, context)

      assert {:error, {:unsupported_html, %{line: 1, column: 1}}} =
               EmbeddedResource.load_html({:srcdoc, "<script>alert(1)</script>"}, context)

      assert {:error, {:invalid_document, %{stage: :resource}}} =
               EmbeddedResource.load_html({:bytes, "<p>x</p>"}, context)
    end)
  end

  test "document schemes are rejected before mappings resolvers or poisoned cache results", %{
    root: root
  } do
    run(root, fn context ->
      for reference <- [
            "http://x.test/child.html",
            "https://x.test/child.html",
            "//x.test/child.html",
            "file://#{root}/children/child.html",
            "data:text/html,child",
            "ftp://x.test/child",
            "javascript:alert(1)"
          ] do
        RenderCache.fetch(context.scope, reference, fn -> {:ok, "<p>Cached</p>"} end)
        resolver = fn _ -> flunk("child resolver invoked") end
        opts = [assets: %{reference => {:bytes, "<p>Mapped</p>"}}, asset_resolver: resolver]

        assert {:error, {:resource_denied, diagnostic}} =
                 EmbeddedResource.load_html(reference, context, opts)

        assert diagnostic.stage == :resource
        assert diagnostic.reason == :resource_denied
        assert diagnostic.source == reference
        assert diagnostic.operation == :load_embedded_resource
        assert diagnostic.module == ResourceValidator
        assert diagnostic.message =~ "local path"

        for kind <- [:image, :background_image, :font, :stylesheet] do
          assert {:error, {:resource_denied, %{source: ^reference}}} =
                   AssetLoader.resolve_local(reference, kind, context, opts)
        end
      end

      assert {:error, {:resource_not_found, %{source: "missing.html"}}} =
               EmbeddedResource.load_html("missing.html", context,
                 assets: %{"missing.html" => {:bytes, "<p>Mapped</p>"}},
                 asset_resolver: fn _ -> flunk("document resolver invoked") end
               )
    end)
  end

  test "child mappings require authorized references and files and never fall back to callbacks",
       %{root: root, directory: directory} do
    run(root, fn context ->
      assert {:ok, "mapped"} =
               AssetLoader.resolve_local("missing.png", :image, context,
                 assets: %{"missing.png" => {:bytes, "mapped"}}
               )

      assert {:ok, "shared"} =
               AssetLoader.resolve_local("missing.png", :background_image, context,
                 assets: %{"missing.png" => {:file, Path.join(root, "shared.bin")}}
               )

      assert {:error, {:resource_denied, _}} =
               AssetLoader.resolve_local("missing.png", :image, context,
                 assets: %{"missing.png" => {:file, Path.join(directory, "outside.html")}}
               )

      assert {:error, {:resource_denied, _}} =
               AssetLoader.resolve_local("missing.png", :font, context,
                 assets: %{"missing.png" => {:file, "https://x.test/font"}}
               )

      assert {:error, {:invalid_document, %{stage: :asset}}} =
               AssetLoader.resolve_local("missing.png", :image, context,
                 asset_resolver: fn _ -> flunk("missing child callback") end
               )

      assert {:error, {:invalid_options, _}} =
               AssetLoader.resolve_local("shared.bin", :image, context,
                 assets: %{"shared.bin" => {:bytes, 12}}
               )

      assert {:error, {:invalid_options, _}} =
               AssetLoader.resolve_local("shared.bin", :image, context, asset_resolver: :bad)

      assert {:error, {:invalid_options, _}} =
               AssetLoader.resolve_local("shared.bin", :other, context)

      assert {:error, {:invalid_options, _}} =
               AssetLoader.resolve_local("shared.bin", :image, context, %{})

      assert {:error, {:invalid_options, _}} =
               EmbeddedResource.load_html("children/child.html", context, %{})
    end)
  end

  test "base changes can resolve siblings but never widen permissions or enable remote child loads",
       %{root: root, directory: directory} do
    run(root, fn context ->
      assert {:ok, based} = ResourceContext.rebase(context, "children")
      assert {:ok, child} = EmbeddedResource.load_html("child.html", based)
      assert child.context.authorization_root == root
      assert {:ok, _} = ResourceContext.rebase(context, root)
      assert {:ok, file_base} = ResourceContext.rebase(context, "file://localhost#{root}")
      assert file_base.resolution_base == root
      assert {:ok, no_base} = ResourceContext.rebase(context, nil)

      assert {:error, {:resource_denied, _}} =
               EmbeddedResource.load_html("children/child.html", no_base)

      assert {:error, {:resource_denied, _}} = ResourceContext.rebase(context, directory)
      assert {:error, {:invalid_options, _}} = ResourceContext.rebase(context, 12)
      assert {:error, {:resource_denied, _}} = ResourceContext.rebase(context, "ftp://x.test")

      for base <- ["https://x.test/children/", "http://x.test/", "//x.test/children/"] do
        assert {:ok, remote} = ResourceContext.rebase(context, base)
        assert {:ok, restored} = ResourceContext.rebase(remote, root)
        assert {:ok, _} = EmbeddedResource.load_html("children/child.html", restored)

        assert {:ok, %{html: "<p>inline</p>"}} =
                 EmbeddedResource.load_html({:srcdoc, "<p>inline</p>"}, remote)

        for reference <- ["child.html", Path.join(root, "children/child.html")] do
          assert {:error, {:resource_denied, _}} = EmbeddedResource.load_html(reference, remote)

          assert {:error, {:resource_denied, _}} =
                   AssetLoader.resolve_local(reference, :font, remote,
                     assets: %{reference => {:bytes, "bytes"}},
                     asset_resolver: fn _ -> flunk("remote base callback") end
                   )
        end
      end
    end)
  end

  test "traversal prefix-confusable roots encoded paths and invalid references cannot bypass authorization",
       %{root: root, directory: directory} do
    run(root, fn context ->
      assert {:ok, %{html: "<p>Child</p>"}} =
               EmbeddedResource.load_html("children/%63hild.html?query=1#anchor", context)

      for source <- [
            "../outside.html",
            "%2e%2e/outside.html",
            Path.join(directory, "root-other/child.html"),
            "bad\0path",
            "bad%00path",
            "bad\\path",
            "bad%5cpath",
            ""
          ] do
        assert {:error, {:resource_denied, _}} = EmbeddedResource.load_html(source, context)
      end
    end)
  end

  test "root and descendant symlinks are denied including dangling and newly replaced resources",
       %{root: root, directory: directory} do
    File.ln_s!(Path.join(directory, "outside.html"), Path.join(root, "link.html"))
    File.ln_s!(Path.join(directory, "absent"), Path.join(root, "dangling.html"))
    File.ln_s!(directory, Path.join(root, "linked-dir"))
    File.ln_s!(root, Path.join(directory, "linked-root"))
    File.ln_s!(directory, Path.join(directory, "linked-ancestor"))

    run(root, fn context ->
      for source <- ["link.html", "dangling.html", "linked-dir/outside.html"] do
        assert {:error, {:resource_denied, _}} = EmbeddedResource.load_html(source, context)
      end

      assert {:ok, _} = EmbeddedResource.load_html("children/child.html", context)
      File.rm!(Path.join(root, "children/child.html"))
      File.ln_s!(Path.join(directory, "outside.html"), Path.join(root, "children/child.html"))

      assert {:error, {:resource_denied, _}} =
               EmbeddedResource.load_html("children/child.html", context)

      assert {:error, {:resource_denied, _}} =
               AssetLoader.resolve_local("shared.bin", :font, context,
                 assets: %{"shared.bin" => {:file, "link.html"}}
               )
    end)

    for auth_root <- [
          Path.join(directory, "linked-root"),
          Path.join(directory, "linked-ancestor/root"),
          Path.join(directory, "absent"),
          Path.join(directory, "outside.html")
        ] do
      assert {:error, {:resource_denied, %{message: message}}} =
               run(auth_root, fn _ -> flunk("invalid root used") end)

      assert message =~ "root"
    end

    assert :ok =
             run("/", fn context ->
               assert {:ok, _} =
                        ResourceValidator.authorize_reference(
                          Path.join(root, "shared.bin"),
                          context
                        )

               :ok
             end)
  end

  test "authorization is checked again after root or ancestor symlink replacement", %{
    root: root,
    directory: directory
  } do
    run(root, fn context ->
      moved = root <> "-moved"
      File.rename!(root, moved)
      File.ln_s!(moved, root)

      assert {:error, {:resource_denied, _}} =
               EmbeddedResource.load_html("children/child.html", context)

      File.rm!(root)
      File.rename!(moved, root)
      renamed = directory <> "-moved"
      File.rename!(directory, renamed)
      File.ln_s!(renamed, directory)

      assert {:error, {:resource_denied, _}} =
               EmbeddedResource.load_html("children/child.html", context)

      File.rm!(directory)
      File.rename!(renamed, directory)
    end)
  end

  test "authorized missing content remains distinct from denial and invalid file content", %{
    root: root
  } do
    File.write!(Path.join(root, "invalid.html"), <<255>>)

    run(root, fn context ->
      assert {:ok, _path} = ResourceValidator.authorize_reference("absent/child.html", context)

      assert {:error,
              {:resource_not_found,
               %{stage: :resource, source: "absent/child.html", message: message}}} =
               EmbeddedResource.load_html("absent/child.html", context)

      assert message =~ "enoent"

      assert {:error, {:invalid_document, %{stage: :file}}} =
               EmbeddedResource.load_html("children", context)

      assert {:error, {:invalid_encoding, %{stage: :html}}} =
               EmbeddedResource.load_html("invalid.html", context)

      assert {:error, {:invalid_document, _}} = EmbeddedResource.load_html(nil, context)

      assert {:error, {:resource_not_found, %{stage: :resource}}} =
               EmbeddedResource.load_html("shared.bin/child.html", context)

      assert {:error, {:invalid_document, %{stage: :resource}}} =
               ResourceValidator.file_identity("\0")
    end)

    unreadable = Path.join(root, "unreadable.html")
    File.write!(unreadable, "<p>Private</p>")
    File.chmod!(unreadable, 0o000)

    # A privileged CI container can read mode-000 files. Match the actual OS
    # result rather than treating permission bits as a separate resource policy.
    case File.read(unreadable) do
      {:error, :eacces} ->
        assert {:error,
                {:invalid_document,
                 %{stage: :resource, source: "unreadable.html", message: message}}} =
                 run(root, &EmbeddedResource.load_html("unreadable.html", &1))

        assert message =~ "eacces"

      {:ok, "<p>Private</p>"} ->
        assert {:ok, %{html: "<p>Private</p>", source: {:file, ^unreadable}}} =
                 run(root, &EmbeddedResource.load_html("unreadable.html", &1))

      result ->
        flunk("unexpected filesystem result for the permission fixture: #{inspect(result)}")
    end
  end

  test "ancestor cycles include hard-link aliases but repeated sibling documents remain allowed",
       %{root: root} do
    File.ln!(Path.join(root, "children/child.html"), Path.join(root, "alias.html"))

    run(root, fn context ->
      assert {:ok, first} = EmbeddedResource.load_html("children/child.html", context)
      assert {:ok, _} = EmbeddedResource.load_html("children/child.html", context)

      for reference <- ["child.html", "../alias.html", "../children/child.html#other"] do
        assert {:error,
                {:resource_cycle, %{stage: :resource, source: ^reference, message: message}}} =
                 EmbeddedResource.load_html(reference, first.context)

        assert message =~ "ancestor"
      end

      assert {:ok, inline} =
               EmbeddedResource.load_html({:srcdoc, "<p>Nested inline</p>"}, first.context)

      assert inline.context.ancestors == first.context.ancestors

      assert {:error, {:resource_cycle, _}} =
               EmbeddedResource.load_html("child.html", inline.context)
    end)
  end

  test "inline and file children share cumulative count and depth ceilings", %{root: root} do
    Limits.install(
      Map.merge(Limits.effective(), %{max_embedded_documents: 2, max_embedded_document_depth: 1})
    )

    assert {:error, {:resource_limit_exceeded, %{stage: :resource, message: message}}} =
             run(root, fn context ->
               assert {:ok, child} = EmbeddedResource.load_html("children/child.html", context)

               assert {:error, {:resource_limit_exceeded, %{message: depth_message}}} =
                        EmbeddedResource.load_html({:srcdoc, ""}, child.context)

               assert depth_message =~ "max_embedded_document_depth"
               assert {:ok, _} = EmbeddedResource.load_html({:srcdoc, ""}, context)
               EmbeddedResource.load_html("children/child.html", context)
             end)

    assert message =~ "max_embedded_documents"
    assert {:ok, _} = run(root, &EmbeddedResource.load_html({:srcdoc, ""}, &1))
  end

  test "children charge existing aggregate HTML node and source limits including parent work", %{
    root: root
  } do
    Limits.install(Map.put(Limits.effective(), :max_aggregate_html_source_bytes, 20))

    assert {:error, {:resource_limit_exceeded, %{stage: :html, message: message}}} =
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, _} = HtmlParser.parse_detailed("<p>Parent</p>")
               run(root, &EmbeddedResource.load_html("children/child.html", &1))
             end)

    assert message =~ "max_aggregate_html_source_bytes"

    Limits.install(Map.put(Limits.defaults(), :max_html_nodes, 12))

    assert {:error, {:resource_limit_exceeded, %{stage: :html, message: message}}} =
             run(root, fn context ->
               for _ <- 1..4 do
                 assert {:ok, _} = EmbeddedResource.load_html({:srcdoc, "<p>x</p>"}, context)
               end
             end)

    assert message =~ "max_html_nodes"

    Limits.install(Map.put(Limits.defaults(), :max_html_source_bytes, 5))

    assert {:error, {:resource_limit_exceeded, %{stage: :file}}} =
             run(root, &EmbeddedResource.load_html("children/child.html", &1))

    assert {:error, {:resource_limit_exceeded, %{stage: :html}}} =
             run(root, &EmbeddedResource.load_html({:srcdoc, "<p>x</p>"}, &1))
  end

  test "styles use shared CSS scopes and hit accounting after local authorization", %{root: root} do
    css = "p { color: red }"

    Limits.install(
      Map.put(Limits.effective(), :max_aggregate_css_source_bytes, byte_size(css) * 2)
    )

    assert {:error, {:resource_limit_exceeded, %{stage: :css, message: message}}} =
             run(root, fn context ->
               for _ <- 1..3 do
                 assert {:ok, bytes} =
                          AssetLoader.resolve_local("children/style.css", :stylesheet, context)

                 assert {:ok, _} = CssParser.prepare_stylesheet(bytes)
               end
             end)

    assert message =~ "max_aggregate_css_source_bytes"
    Limits.install(Map.put(Limits.defaults(), :max_css_source_bytes, 5))

    assert {:error, {:resource_limit_exceeded, %{stage: :file}}} =
             run(root, &AssetLoader.resolve_local("children/style.css", :stylesheet, &1))

    assert {:error, {:invalid_document, %{stage: :asset}}} =
             run(root, &AssetLoader.resolve_local("missing.css", :stylesheet, &1))

    assert {:ok, ^css} =
             run(
               root,
               &AssetLoader.resolve_local("inline.css", :stylesheet, &1,
                 assets: %{"inline.css" => {:bytes, css}}
               )
             )
  end

  test "child fonts share parent fingerprints aggregate bytes and occurrence limits", %{
    root: root
  } do
    Limits.install(Map.put(Limits.effective(), :max_aggregate_font_source_bytes, 10))

    assert {:error, {:resource_limit_exceeded, %{stage: :font, message: message}}} =
             FontValidator.with_budget(fn ->
               FontValidator.reserve_source("parent")
               run(root, &AssetLoader.resolve_local("shared.bin", :font, &1))
             end)

    assert message =~ "max_aggregate_font_source_bytes"

    assert :ok =
             run(root, fn context ->
               assert {:ok, "shared"} = AssetLoader.resolve_local("shared.bin", :font, context)

               assert {:ok, "shared"} =
                        AssetLoader.resolve_local("inline.ttf", :font, context,
                          assets: %{"inline.ttf" => {:bytes, "shared"}}
                        )

               :ok
             end)

    Limits.install(Map.put(Limits.defaults(), :max_aggregate_font_source_bytes, 6))

    assert :ok =
             run(root, fn context ->
               assert {:ok, "shared"} = AssetLoader.resolve_local("shared.bin", :font, context)
               assert {:ok, "shared"} = AssetLoader.resolve_local("shared.bin", :font, context)
               :ok
             end)

    Limits.install(Map.put(Limits.defaults(), :max_font_candidates, 1))

    assert {:error, {:resource_limit_exceeded, %{stage: :font}}} =
             run(root, fn context ->
               assert {:ok, _} = AssetLoader.resolve_local("shared.bin", :font, context)
               AssetLoader.resolve_local("shared.bin", :font, context)
             end)

    Limits.install(Map.put(Limits.defaults(), :max_font_source_bytes, 1))

    assert {:error, {:resource_limit_exceeded, %{stage: :font}}} =
             run(root, &AssetLoader.resolve_local("shared.bin", :font, &1))
  end

  test "all child images share the supplied count source and decoded storage budgets", %{
    root: root
  } do
    budget = HtmlValidator.new_image_budget()
    Limits.install(Map.put(Limits.effective(), :max_image_count, 1))
    assert :ok = HtmlValidator.reserve_image_source(budget, 1)

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             ResourceContext.run([authorization_root: root], budget, fn context ->
               AssetLoader.resolve_local("shared.bin", :image, context)
             end)

    Limits.install(Map.put(Limits.defaults(), :max_aggregate_image_source_bytes, 10))

    assert {:error, {:resource_limit_exceeded, %{stage: :file}}} =
             run(root, fn context ->
               assert {:ok, first} = EmbeddedResource.load_html({:srcdoc, ""}, context)
               assert {:ok, second} = EmbeddedResource.load_html({:srcdoc, ""}, context)

               assert {:ok, "shared"} =
                        AssetLoader.resolve_local("shared.bin", :image, first.context)

               AssetLoader.resolve_local("shared.bin", :background_image, second.context)
             end)

    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)
    Limits.install(Map.put(Limits.defaults(), :max_aggregate_decoded_image_bytes, 5))

    run(root, fn context ->
      assert {:ok, first} = EmbeddedResource.load_html({:srcdoc, ""}, context)
      assert {:ok, second} = EmbeddedResource.load_html({:srcdoc, ""}, context)
      assert first.context.image_budget == second.context.image_budget

      assert {:ok, bytes} =
               AssetLoader.resolve_local("inline.png", :image, first.context,
                 assets: %{"inline.png" => {:bytes, png}}
               )

      assert {:ok, _} = PngDecoder.decode(bytes, first.context.image_budget, true)

      assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
               PngDecoder.decode(bytes, second.context.image_budget, true)
    end)
  end

  test "contexts expire restore enclosing scopes and cannot borrow another root's permission", %{
    root: root,
    directory: directory
  } do
    context = run(root, & &1)
    assert {:error, {:invalid_options, _}} = ResourceContext.rebase(context, root)

    assert {:error, {:invalid_options, _}} =
             AssetLoader.resolve_local("shared.bin", :font, context)

    assert {:error, {:invalid_options, _}} = EmbeddedResource.load_html({:srcdoc, ""}, %{})

    run(root, fn active ->
      altered = %{active | authorization_root: directory}

      assert {:error, {:invalid_options, _}} =
               AssetLoader.resolve_local("outside.html", :image, altered)

      for altered <- [
            %{active | depth: -1},
            %{active | depth: nil},
            %{active | ancestors: nil},
            %{active | document_id: make_ref()},
            %{active | tree_scope_id: make_ref()}
          ] do
        assert {:error, {:invalid_options, %{stage: :resource}}} =
                 EmbeddedResource.load_html("children/child.html", altered)
      end

      assert {:ok, child} = EmbeddedResource.load_html("children/child.html", active)

      assert {:error, {:invalid_options, _}} =
               EmbeddedResource.load_html("child.html", %{child.context | ancestors: [], depth: 0})

      run(Path.join(directory, "root-other"), fn narrow ->
        assert {:error, {:resource_denied, _}} =
                 AssetLoader.resolve_local(Path.join(root, "shared.bin"), :font, narrow)
      end)

      assert {:ok, _} = AssetLoader.resolve_local("shared.bin", :font, active)
    end)

    assert_raise RuntimeError, "scope failure", fn ->
      run(root, fn _ -> raise "scope failure" end)
    end

    assert {:ok, _} = run(root, &EmbeddedResource.load_html({:srcdoc, ""}, &1))
  end

  test "changed file contents are read again within a scope and across scopes", %{root: root} do
    run(root, fn context ->
      assert {:ok, %{html: "<p>Child</p>"}} =
               EmbeddedResource.load_html("children/child.html", context)

      File.write!(Path.join(root, "children/child.html"), "<p>Other</p>")

      assert {:ok, %{html: "<p>Other</p>"}} =
               EmbeddedResource.load_html("children/child.html", context)

      assert {:ok, "shared"} = AssetLoader.resolve_local("shared.bin", :font, context)
      File.write!(Path.join(root, "shared.bin"), "edited")
      assert {:ok, "edited"} = AssetLoader.resolve_local("shared.bin", :font, context)
    end)

    assert {:ok, %{html: "<p>Other</p>"}} =
             run(root, &EmbeddedResource.load_html("children/child.html", &1))
  end

  test "context setup failures return diagnostics without running execution", %{root: root} do
    for opts <- [
          %{},
          [:bad],
          [authorization_root: 12],
          [authorization_root: "https://x.test/root"]
        ] do
      assert {:error, {:invalid_options, _}} =
               ResourceContext.run(opts, HtmlValidator.new_image_budget(), fn _ ->
                 flunk("invalid context executed")
               end)
    end

    assert {:error, {:invalid_document, %{stage: :style}}} =
             ResourceContext.run([authorization_root: root], make_ref(), fn _ ->
               flunk("invalid budget executed")
             end)

    assert {:error, {:invalid_document, %{stage: :style}}} =
             ResourceContext.run([authorization_root: root], :atomics.new(2, []), fn _ ->
               flunk("incorrect budget shape executed")
             end)

    assert {:error, {:resource_denied, _}} =
             ResourceContext.run([base_url: root], HtmlValidator.new_image_budget(), fn _ ->
               flunk("base used as permission")
             end)

    assert {:error, {:invalid_options, _}} =
             run(root, fn context ->
               EmbeddedResource.load_html("children/child.html", context,
                 assets: %{"bad" => {:bytes, 1}}
               )
             end)
  end

  defp run(root, fun) do
    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fun)
  end
end
