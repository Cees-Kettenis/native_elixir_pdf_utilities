alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.CurvedBorderBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf}
  @moduletag :browser_parity
  setup_all do
    %{browser_reference_scope: start_supervised!({Agent, fn -> %{} end})}
  end

  setup context do
    name =
      if context.reference_appearance == :initial,
        do: "html_curved_border_sides.html",
        else: "html_curved_border_sides_selected.html"

    reference =
      PdfVisualCompare.scoped_browser_reference!(
        context.browser_reference_scope,
        Paths.fixture("html_to_pdf/browser_parity/" <> name),
        []
      )

    %{browser_reference: reference}
  end

  for mode <- [:static, :interactive, :flattened] do
    @mode mode
    @tag mode: mode
    @tag reference_appearance: :initial
    test "thin thick dotted dashed and per-edge curved borders match Chromium in #{@mode} output",
         %{mode: mode, browser_reference: reference} do
      renderer = fn html, options ->
        with {:ok, pdf} <- HtmlToPdf.render(html, options) do
          if mode == :flattened, do: Forms.flatten(pdf), else: {:ok, pdf}
        end
      end

      stats =
        PdfVisualCompare.assert_browser_match!(
          Paths.fixture("html_to_pdf/browser_parity/html_curved_border_sides.html"),
          reference: reference,
          render_opts: [forms: if(mode == :static, do: :static, else: :interactive)],
          native_renderer: renderer,
          artifact_dir: "tmp/browser_parity/html_curved_border_sides_#{mode}",
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.scripting == :disabled and stats.page_count == 1
    end
  end

  for mode <- [:interactive, :flattened] do
    @mode mode
    @tag mode: mode
    @tag reference_appearance: :selected
    test "filled field matches authored selected option in #{@mode} output", %{
      mode: mode,
      browser_reference: reference
    } do
      original =
        File.read!(Paths.fixture("html_to_pdf/browser_parity/html_curved_border_sides.html"))

      renderer = fn _authored_selected_html, options ->
        with {:ok, pdf} <- HtmlToPdf.render(original, options),
             {:ok, filled} <- Forms.fill(pdf, %{"pick" => "b"}) do
          if mode == :flattened, do: Forms.flatten(filled), else: {:ok, filled}
        end
      end

      stats =
        PdfVisualCompare.assert_browser_match!(
          Paths.fixture("html_to_pdf/browser_parity/html_curved_border_sides_selected.html"),
          reference: reference,
          render_opts: [forms: :interactive],
          native_renderer: renderer,
          artifact_dir: "tmp/browser_parity/html_curved_border_sides_filled_#{mode}",
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.scripting == :disabled and stats.page_count == 1
    end
  end
end
