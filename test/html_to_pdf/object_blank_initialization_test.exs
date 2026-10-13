defmodule NativeElixirPdfUtilities.HtmlToPdf.ObjectBlankInitializationTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedViewport, ResourceContext}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, ObjectValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "object MIME hints distinguish document initialization from absent-data fallback" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      for type <- ["text/html", " TEXT/PLAIN ; charset=UTF-8", "image/svg+xml"] do
        for attributes <- [%{"type" => type}, %{"type" => type, "data" => " \t\r\n"}] do
          assert {:ok, :blank} = ObjectValidator.select(attributes, context)
        end

        for source <- ["about:blank", "ABOUT:blank?requested=1#original"] do
          assert {:ok, {:blank, ^source}} =
                   ObjectValidator.select(%{"type" => type, "data" => source}, context)
        end
      end

      assert {:ok, {:blank, "about:blank#original"}} =
               ObjectValidator.select(%{"data" => "about:blank#original"}, context)

      for attributes <- [
            %{},
            %{"data" => ""},
            %{"data" => " \t"},
            %{"type" => "application/x-unknown"},
            %{"data" => "about:blank", "type" => "application/x-unknown"}
          ] do
        assert {:ok, :fallback} = ObjectValidator.select(attributes, context)
      end

      for type <- ["image/png", "image/jpeg"] do
        assert {:ok, {:image, "about:blank", ^type}} =
                 ObjectValidator.select(%{"data" => "about:blank", "type" => type}, context)

        assert {:error, {:resource_denied, diagnostic}} =
                 EmbeddedViewport.prepare_selected(
                   {:image, "about:blank", type},
                   context,
                   [],
                   :object
                 )

        assert diagnostic.stage == :resource and diagnostic.source == "about:blank"
      end
    end)
  end

  test "blank object documents issue separate child identities without source reads or mappings" do
    owner = self()
    resolver = fn request -> send(owner, {:resolved, request}) end
    budget = HtmlValidator.new_image_budget()

    ResourceContext.run([authorization_root: File.cwd!()], budget, fn parent ->
      assert {:ok, parent} =
               ResourceContext.rebase(
                 parent,
                 {:document_url, "file://" <> Path.join(parent.authorization_root, "parent.html")}
               )

      for selection <- [:blank, {:blank, "ABOUT:blank?requested=1#original"}] do
        options = [
          asset_resolver: resolver,
          assets: %{"about:blank" => {:bytes, "<p>Injected</p>"}}
        ]

        assert {:ok, {:viewport, first}} =
                 EmbeddedViewport.prepare_selected(selection, parent, options, :object)

        assert {:ok, {:viewport, second}} =
                 EmbeddedViewport.prepare_selected(selection, parent, options, :object)

        assert first.host == :object and second.host == :object
        assert first.prepared_dom.type == :document and second.prepared_dom.type == :document
        assert first.prepared_dom.tree_scope_id != second.prepared_dom.tree_scope_id

        assert first.source ==
                 {:blank, if(selection == :blank, do: "about:blank", else: elem(selection, 1))}

        assert first.document_url == parent.resolution_base
        assert first.context.resolution_base == parent.resolution_base
        assert first.context.authorization_root == parent.authorization_root
        assert first.context.image_budget == budget
        assert first.context.document_id != second.context.document_id
        assert first.context.tree_scope_id != second.context.tree_scope_id
        assert first.context.depth == parent.depth + 1
        assert {:ok, group} = EmbeddedViewport.layout(first, 90, 45)
        refute Enum.any?(group.boxes, &(&1.type in [:text, :image, :link]))
        assert ResourceContext.current().document_id != first.context.document_id
        assert :atomics.get(budget, 2) == 0 and :atomics.get(budget, 3) == 0
        refute_received {:resolved, _}
      end
    end)
  end

  test "selected blank suppresses inactive fallback reads and fields through the public renderer" do
    owner = self()
    resolver = fn request -> send(owner, {:resolved, request}) end

    for attributes <- [
          "type='text/html'",
          "data='  ' type='text/plain'",
          "data='about:blank?original#target'",
          "data='about:blank' type='image/svg+xml'"
        ] do
      source = """
      <p>Before</p><object #{attributes} style='display:block;width:90pt;height:30pt'>
        <img src='https://denied.test/fallback.png'><input name='same' value='Inactive field'>
        <p>Inactive fallback</p>
      </object><p>After</p>
      """

      assert {:ok, pdf} = HtmlToPdf.render(source, asset_resolver: resolver)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Before" and text =~ "After"
      refute text =~ "Inactive"
      refute pdf =~ "/AcroForm"
      refute_received {:resolved, _}
    end
  end

  test "blank source validation and the shared child-depth ceiling fail without fallback" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      for source <- ["about:Blank", "about://host/blank", "https://denied.test/blank", 12] do
        assert {:error, {:invalid_document, diagnostic}} =
                 EmbeddedViewport.prepare_selected({:blank, source}, parent, [], :object)

        assert diagnostic.stage == :resource and diagnostic.message =~ "about:blank"
      end

      Limits.install(Map.put(Limits.effective(), :max_embedded_document_depth, 0))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               EmbeddedViewport.prepare_selected(:blank, parent, [], :object)

      assert diagnostic.stage == :resource and diagnostic.message =~ "depth"
      assert ResourceContext.current() == parent
    end)
  end

  test "blank objects charge the shared child count even though they allocate no source bytes" do
    Limits.install(Map.put(Limits.effective(), :max_embedded_documents, 1))

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, {:viewport, first}} =
               EmbeddedViewport.prepare_selected(:blank, parent, [], :object)

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               EmbeddedViewport.prepare_selected(:blank, parent, [], :object)

      assert diagnostic.stage == :resource and diagnostic.message =~ "max_embedded_documents"

      assert diagnostic.operation == :load_embedded_resource and
               diagnostic.source == "about:blank"

      assert ResourceContext.current() == parent
      assert {:ok, _group} = EmbeddedViewport.layout(first, 90, 45)
      :callback_continued
    end)
    |> then(&assert(&1 == :callback_continued))
  end
end
