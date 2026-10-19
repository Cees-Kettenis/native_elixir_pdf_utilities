alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentVectorTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator}

  setup do
    root =
      Path.join(System.tmp_dir!(), "svg-vector-document-#{System.unique_integer([:positive])}")

    File.mkdir_p!(root)
    limits = Limits.effective()

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(root)
    end)

    %{root: root, limits: limits}
  end

  test(
    "document graphics modes retain original source and use the approved encoded print paint",
    %{root: root}
  ) do
    ResourceContext.run(
      [authorization_root: root],
      HtmlValidator.new_image_budget(),
      fn context ->
        for body <- [
              "<rect width='3' height='1' color-interpolation='linearRGB' fill='red'/>",
              "<rect width='3' height='1' style='color-interpolation:linearRGB' fill='red'/>",
              "<g color-interpolation='linearRGB'><rect width='3' height='1' fill='red'/></g>",
              "<defs><g id='source' color-interpolation='linearRGB'><rect width='3' height='1' fill='red'/></g></defs><use href='#source'/>"
            ] do
          source = "<svg width='3' height='1'>#{body}</svg>"

          assert {:ok, doc} =
                   NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.document(
                     source,
                     context.image_budget,
                     "graphics.svg"
                   )

          assert {:ok, _image} =
                   NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.compose_document(
                     doc,
                     2.25,
                     0.75
                   )

          assert ResourceContext.current() == context
          File.write!(Path.join(root, "graphics.svg"), source)

          assert {:ok, public_pdf} =
                   NativeElixirPdfUtilities.HtmlToPdf.render("<embed src='graphics.svg'>",
                     base_url: root
                   )

          refute public_pdf =~ "/Subtype /Link"

          assert :ok =
                   NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                     public_pdf,
                     false
                   )
        end

        hidden =
          "<svg width='3' height='1'><defs><g color-interpolation='linearRGB'><rect width='3' height='1'/></g></defs><rect width='3' height='1' fill='red'/></svg>"

        assert {:ok, doc} =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.document(
                   hidden,
                   context.image_budget
                 )

        assert {:ok, _image} =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.compose_document(
                   doc,
                   2.25,
                   0.75
                 )
      end
    )
  end

  test "selected marker documents use Form resources through the existing PDF writer" do
    root = Paths.fixture("html_to_pdf/browser_parity")
    source = File.read!(Path.join(root, "svg_marker_document_contexts.html"))

    assert {:ok, __pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source, base_url: root, fonts: [])
  end
end
