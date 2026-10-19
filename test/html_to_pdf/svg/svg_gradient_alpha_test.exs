alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgGradientAlphaTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits, data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")}
  end

  test "host stop CSS supplies independent opacity and malformed opacity remains diagnostic" do
    html =
      "<style>#a{stop-opacity:.15}#b{stop-opacity:.9}</style><svg width='140' height='50'><defs><linearGradient id='g'><stop id='a' offset='0' stop-color='red'/><stop id='b' offset='1' stop-color='blue'/></linearGradient></defs><rect width='140' height='50' fill='url(#g)'/></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

    source =
      "<svg width='100' height='40'><defs><linearGradient id='g'><stop offset='0' stop-opacity='bad'/><stop offset='1'/></linearGradient></defs><rect width='100' height='40' fill='url(#g)'/></svg>"

    assert {:error, {:invalid_css, diagnostic}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    assert diagnostic.stage == :css
    assert diagnostic.reason == :invalid_css
    assert diagnostic.source != nil and diagnostic.operation == :prepare

    (
      assert diagnostic.source == "stop-opacity"
      assert diagnostic.message =~ "bad"
    )
  end
end
