defmodule NativeElixirPdfUtilities.HtmlToPdf.FixturePageTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.{
    BrowserFixturePages,
    BrowserFixturePageValidator,
    Paths
  }

  alias NativeElixirPdfUtilities.TestSupport.BrowserReferenceValidator

  setup do
    directory = Path.expand("tmp/page-contract-#{System.unique_integer([:positive])}")
    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)

    write = fn name, body, css ->
      html =
        "<html lang=\"lt\"><head><style>@page{size:180pt 120pt;margin:0}#{css}</style></head><body dir=\"ltr\">#{body}</body></html>"

      File.write!(Path.join(directory, name), html)

      %{
        "name" => name,
        "sha256" => Base.encode16(:crypto.hash(:sha256, html), case: :lower),
        "page_count" => 1,
        "page_css" => "size:180pt 120pt;margin:0"
      }
    end

    group = %{
      "page_mode" => "named",
      "page_css" => "size:180pt 120pt;margin:0",
      "sources" => [
        write.("first.html", "<p>First</p>", ""),
        write.("second.html", "<p>Second</p>", "")
      ]
    }

    %{directory: directory, write: write, group: group}
  end

  test "literal scoping preserves specificity, selector commas and separate root hints", %{
    directory: directory,
    group: group,
    write: write
  } do
    first =
      write.(
        "first.html",
        "<p data-value=\"a,b\">First</p>",
        "html,body{margin:0}p:is(.a,.b),[data-value='a,b']{color:red}"
      )

    group = %{group | "sources" => [first, List.last(group["sources"])]}
    html = BrowserFixturePages.build!(directory, group)

    assert html =~ "section:where(#page_0),div:where(#body_0){margin:0}"
    assert html =~ ":where(#body_0) p:is(.a,.b),:where(#body_0) [data-value='a,b']"
    assert html =~ "id=\"page_0\" lang=\"lt\""
    assert html =~ "id=\"body_0\" dir=\"ltr\""
    refute html =~ "<iframe"

    frozen = Map.put(group, "html_sha256", String.duplicate("0", 64))

    assert_raise ExUnit.AssertionError, ~r/composition changed/, fn ->
      BrowserFixturePages.build!(directory, frozen)
    end

    File.write!(Path.join(directory, "second.html"), "changed")

    assert_raise ExUnit.AssertionError, ~r/source changed/, fn ->
      BrowserFixturePages.build!(directory, group)
    end
  end

  test "neighboring IDs, radio names and missing targets cannot change document lookup", %{
    directory: directory,
    group: group,
    write: write
  } do
    for {left, right} <- [
          {"<p id=target>First</p>", "<p id='target'>Second</p>"},
          {"<input type=radio name=choice>", "<input type=radio name=choice>"},
          {"<label for=target>Missing</label>", "<input id=target>"},
          {"<a href='#target'>Missing</a>", "<p id=target>Second</p>"},
          {"<p id=body_1>Reserved</p>", "<p>Second</p>"}
        ] do
      changed = %{
        group
        | "sources" => [write.("first.html", left, ""), write.("second.html", right, "")]
      }

      assert_raise ExUnit.AssertionError, fn -> BrowserFixturePages.build!(directory, changed) end
    end

    changed = %{
      group
      | "sources" => [
          write.(
            "first.html",
            "<?start name=A><p>Original</p><?end><template for=A><p id=target></p></template>",
            ""
          ),
          write.("second.html", "<p id=target>Second</p>", "")
        ]
    }

    html = BrowserFixturePages.build!(directory, changed)

    assert html =~
             "<?start name=A><p>Original</p><?end><template for=A><p id=target></p></template>"
  end

  test "original document contexts reject composition", %{
    directory: directory,
    group: group,
    write: write
  } do
    for {body, css} <- [
          {"<p>Fixed</p>", "p{position:fixed}"},
          {"<p>Root</p>", ":root{--color:red}"},
          {"<p>Media</p>", "@media print{p{color:red}}"}
        ] do
      changed = %{
        group
        | "sources" => [write.("first.html", body, css), List.last(group["sources"])]
      }

      assert_raise ExUnit.AssertionError, fn -> BrowserFixturePages.build!(directory, changed) end
    end
  end

  test "unreviewed browser option variants retain original captures" do
    group =
      :json.decode(
        ~s({"sources":[{"name":"source.html","options":{"chromium_transform":null,"svg_reference":null,"render_opts":{}}}]})
      )

    [{"source.html", options}] = BrowserFixturePages.entries(group)
    assert Keyword.fetch!(options, :chromium_transform) == nil
    assert Keyword.fetch!(options, :svg_reference) == nil
    assert BrowserFixturePageValidator.same_browser_options?([], options)

    assert BrowserFixturePageValidator.same_browser_options?([render_opts: [forms: :interactive]],
             render_opts: [forms: :static]
           )

    refute BrowserFixturePageValidator.same_browser_options?([dpi: 192], [])

    refute BrowserFixturePageValidator.same_browser_options?(
             [render_opts: [base_url: "/different"]],
             []
           )

    refute BrowserFixturePageValidator.same_browser_options?([chromium_page_margin: 12], [])
  end

  test "reviewed options decode in a fresh VM before renderer modules load", %{
    directory: directory
  } do
    paths =
      :code.get_path()
      |> Enum.map(&List.to_string/1)
      |> Enum.filter(&(Path.basename(&1) == "ebin"))
      |> Enum.flat_map(&["-pa", &1])

    manifest = Paths.fixture("html_to_pdf/browser_parity/pages.json")
    result = Path.join(directory, "decoded-options.etf")

    script = """
    try do
      String.to_existing_atom("html_recovery")
      raise "The fresh-VM probe already loaded the renderer option atom"
    rescue
      ArgumentError -> :ok
    end

    Code.require_file(#{inspect(Paths.support("paths.ex"))})
    Code.require_file(#{inspect(Paths.support("browser_fixture_pages.ex"))})
    groups = :json.decode(File.read!(#{inspect(manifest)}))
    entries = Enum.flat_map(groups, &NativeElixirPdfUtilities.TestSupport.BrowserFixturePages.entries/1)
    File.write!(#{inspect(result)}, :erlang.term_to_binary(entries))
    """

    {output, status} =
      System.cmd(System.find_executable("elixir"), paths ++ ["-e", script],
        stderr_to_stdout: true
      )

    assert status == 0, output

    expected =
      manifest |> File.read!() |> :json.decode() |> Enum.flat_map(&BrowserFixturePages.entries/1)

    assert result |> File.read!() |> :erlang.binary_to_term([:safe]) == expected
  end

  test "page boundaries reject missing, duplicate, moved and resized original scenarios" do
    alias NativeElixirPdfUtilities.Pdf.{Reader, IncrementalWriter}

    sources =
      for marker <- ["first", "second"],
          do: %{marker: marker, metadata: %{"name" => marker, "page_count" => 1}}

    html = """
    <html><head><style>@page{size:180pt 120pt;margin:0}</style></head><body>
    <a href="#{BrowserFixturePageValidator.marker_uri("first")}">First</a>
    <div style="break-before:page"><a href="#{BrowserFixturePageValidator.marker_uri("second")}">Second</a></div>
    </body></html>
    """

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

    assert {_clean, [1..1, 2..2]} = BrowserFixturePageValidator.clean_boundaries!(pdf, sources)
    assert {:ok, context} = Reader.read_validated(pdf)
    [first, second] = context.pages
    assert {:ok, dictionary} = Reader.dictionary(context.document, {:ref, first.ref})
    assert {:ok, other} = Reader.dictionary(context.document, {:ref, second.ref})
    assert {:ok, annotations} = Reader.resolve(context.document, dictionary["Annots"])
    assert {:ok, other_annotations} = Reader.resolve(context.document, other["Annots"])
    {object, generation} = first.ref

    for {replacement, message} <- [
          {[], ~r/Missing fixture page boundary/},
          {annotations ++ annotations, ~r/Duplicate fixture page boundary/},
          {other_annotations, ~r/Fixture page boundary moved/}
        ] do
      assert {:ok, changed} =
               IncrementalWriter.write(context, [
                 {object, generation, {:value, Map.put(dictionary, "Annots", replacement)}}
               ])

      assert_raise ExUnit.AssertionError, message, fn ->
        BrowserFixturePageValidator.clean_boundaries!(changed, sources)
      end
    end

    changed = put_in(hd(sources), [:metadata, "page_count"], 2)

    assert_raise ExUnit.AssertionError, ~r/page count changed/, fn ->
      BrowserFixturePageValidator.clean_boundaries!(pdf, [changed, List.last(sources)])
    end

    changed = put_in(hd(sources), [:metadata, "media_boxes"], [[0, 0, 181, 120]])

    assert_raise ExUnit.AssertionError, ~r/page bounds changed/, fn ->
      BrowserFixturePageValidator.clean_boundaries!(pdf, [changed, List.last(sources)])
    end
  end

  test "unknown browsers, fonts and environment cannot use a stored equivalence proof", %{
    directory: directory
  } do
    profile = %{
      browser: "Chromium reviewed",
      rasterizer: "Poppler reviewed",
      adapter: "pipe",
      fonts: [%{family: "Example", sha256: "hash"}],
      environment: %{"LANG" => "C.UTF-8"}
    }

    proof = %{
      "browser" => profile.browser,
      "rasterizer" => profile.rasterizer,
      "fonts" => [["Example", "hash", 400, "normal"]],
      "fontconfig_sha256" => "",
      "fontconfig_present" => false,
      "environment" => profile.environment
    }

    assert BrowserReferenceValidator.page_profile_supported?(proof, profile)

    for changed <- [
          %{profile | browser: "Chromium updated"},
          %{profile | adapter: "cli"},
          %{profile | fonts: []},
          %{profile | environment: %{"LANG" => "different"}},
          %{profile | environment: Map.put(profile.environment, "FONTCONFIG_FILE", "/missing")}
        ] do
      refute BrowserReferenceValidator.page_profile_supported?(proof, changed)
    end

    configuration = Path.join(directory, "fonts.conf")
    File.write!(configuration, "reviewed bytes")

    configured = %{
      profile
      | environment: Map.put(profile.environment, "FONTCONFIG_FILE", configuration)
    }

    proof =
      Map.merge(proof, %{
        "fontconfig_present" => true,
        "fontconfig_sha256" =>
          Base.encode16(:crypto.hash(:sha256, File.read!(configuration)), case: :lower)
      })

    assert BrowserReferenceValidator.page_profile_supported?(proof, configured)
    File.write!(configuration, "changed bytes at the same path")
    refute BrowserReferenceValidator.page_profile_supported?(proof, configured)
  end
end
