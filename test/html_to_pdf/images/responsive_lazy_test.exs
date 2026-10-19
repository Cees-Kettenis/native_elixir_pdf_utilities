alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.ResponsiveLazyTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Validators.ResponsiveImageValidator

  test "auto eligibility follows authored lazy state and initial sizes token" do
    for {loading, sizes, expected} <- [
          {"lazy", "auto", true},
          {"LAZY", "AUTO, 100vw", true},
          {" lazy", "auto", false},
          {"lazy", " auto", false},
          {"eager", "auto", false},
          {"lazy", "(width > 1px) auto", false}
        ] do
      assert ResponsiveImageValidator.allows_auto?(%{
               tag: "img",
               attributes: %{"loading" => loading, "sizes" => sizes}
             }) == expected
    end

    refute ResponsiveImageValidator.allows_auto?(%{
             tag: "input",
             attributes: %{"loading" => "lazy", "sizes" => "auto"}
           })
  end

  test "lazy auto selects only the final shrunk flex and grid candidates" do
    image =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(64, 32, 2, 8, 0, fn _, _ ->
        [0, 128, 64]
      end)

    for container <- [
          "display:flex;width:48pt;height:24pt",
          "display:grid;width:48pt;grid-template-columns:1fr"
        ],
        child <-
          if(String.contains?(container, "flex"),
            do: ["width:96pt;min-width:0;flex-shrink:1"],
            else: ["width:100%;min-width:0"]
          ) do
      html =
        "<div style='#{container}'><img loading='lazy' sizes='auto' src='unused' srcset='selected 64w, unused-large 128w' style='#{child};height:24pt'></div>"

      assert {:ok, _} =
               HtmlToPdf.render(html,
                 asset_resolver: fn request ->
                   assert request.reference == "selected"
                   send(self(), {:loaded, request.reference})
                   {:ok, image}
                 end
               )

      assert_received {:loaded, "selected"}
      refute_received {:loaded, _}
    end
  end

  test "picture omitted source sizes uses auto width and source dimensions" do
    image =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(64, 32, 2, 8, 0, fn _, _ ->
        [0, 128, 64]
      end)

    html =
      "<picture><source srcset='selected 64w, unused-large 128w' width='64' height='32'><img loading='lazy' sizes='auto' src='unused'></picture>"

    assert {:ok, _} =
             HtmlToPdf.render(html,
               asset_resolver: fn request ->
                 assert request.reference == "selected"
                 {:ok, image}
               end
             )
  end

  test "hidden lazy images never load and selected resource failure is retained" do
    assert {:ok, _} =
             HtmlToPdf.render("<img loading=lazy sizes=auto src=unused style='display:none'>",
               asset_resolver: fn _ -> flunk("hidden image requested") end
             )

    assert {:error, {_, diagnostic}} =
             HtmlToPdf.render("<img loading=lazy sizes=auto src=chosen width=64 height=32>",
               asset_resolver: fn request ->
                 assert request.reference == "chosen"
                 :error
               end
             )

    assert diagnostic.stage in [:asset, :resource, :style]
    assert diagnostic.source == "chosen"
  end

  test "expired and forged pending requests return the shared diagnostic contract" do
    request = %{reference: make_ref(), scope: make_ref()}

    assert {:error, {:invalid_layout, diagnostic}} =
             ResponsiveImageValidator.resolve_request(request, 64, fn _, _ ->
               flunk("forged request executed")
             end)

    assert diagnostic.stage == :layout
    assert diagnostic.reason == :invalid_layout
    assert diagnostic.source == "img"
    assert diagnostic.module == ResponsiveImageValidator
  end

  @tag :browser_parity
  @tag :responsive_lazy_auto
  test "lazy auto sizes and final containment geometry match original Chromium" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/html_responsive_lazy_auto.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_responsive_lazy_auto",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
