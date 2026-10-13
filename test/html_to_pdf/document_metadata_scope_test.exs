defmodule NativeElixirPdfUtilities.HtmlToPdf.DocumentMetadataScopeTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Info}
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  test "processing instructions preserve actual head metadata and caller precedence" do
    html =
      "<?document?><html><?before-head?><head><?inside?><title>Report</title><meta name=author content=Finance><meta name=description content=Monthly><meta name=keywords content='one,two'></head><body><p>Visible</p></body></html>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, info} = Info.get(pdf)
    assert info.title == "Report"
    assert info.author == "Finance"
    assert info.subject == "Monthly"
    assert info.keywords == "one, two"
    assert {:ok, pdf} = HtmlToPdf.render(html, metadata: [author: "Caller"])
    assert {:ok, %{author: "Caller", subject: "Monthly"}} = Info.get(pdf)
  end

  test "foreign artwork titles never become the enclosing HTML document title" do
    html =
      "<svg width=40 height=20><title><span>Artwork title</span></title><rect width=40 height=20 fill=red></rect></svg>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert Keyword.get(HtmlValidator.document_metadata_options(dom, []), :metadata, %{}) == %{}
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, info} = Info.get(pdf)
    assert info.title in [nil, ""]
  end

  test "qualified and foreign metadata lookalikes remain inert and field names use ASCII folding" do
    meta = %{
      type: :element,
      tag: "meta",
      attributes: %{"name" => "AUTHOR", "content" => "Real"},
      children: []
    }

    lookalikes = [
      Map.put(meta, :namespace, :svg),
      Map.put(meta, :attribute_namespaces, %{"name" => "urn:other"}),
      Map.put(meta, :attribute_namespaces, %{"content" => "urn:other"}),
      %{meta | attributes: %{"name" => "Keywords", "content" => "Wrong"}}
    ]

    dom = %{
      type: :document,
      children: [
        %{type: :processing_instruction, target: "marker", data: ""},
        %{
          type: :element,
          tag: "html",
          attributes: %{},
          children: [
            %{type: :processing_instruction, target: "head", data: ""},
            %{type: :element, tag: "head", attributes: %{}, children: lookalikes ++ [meta]}
          ]
        }
      ]
    }

    assert HtmlValidator.document_metadata_options(dom, []) == [metadata: %{author: "Real"}]
  end

  test "mixed multipage source keeps parent metadata and actual scope links and fields" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_mixed_document_scopes.html",
        __DIR__
      )

    font_root = Path.expand("../../priv/fonts/dejavu", __DIR__)

    fonts =
      for {suffix, weight, style} <- [
            {"", 400, :normal},
            {"-Bold", 700, :normal},
            {"-Oblique", 400, :italic},
            {"-BoldOblique", 700, :italic}
          ],
          do: %{
            family: "DejaVu Sans",
            data: File.read!(Path.join(font_root, "DejaVuSans" <> suffix <> ".ttf")),
            weight: weight,
            style: style
          }

    assert {:ok, pdf} =
             HtmlToPdf.render(File.read!(fixture),
               fonts: fonts,
               compress_streams: false,
               asset_resolver: fn request -> flunk("Unexpected resource #{inspect(request)}") end
             )

    assert {:ok, 2} = Info.page_count(pdf)

    assert {:ok,
            %{
              title: "Scoped monthly report",
              author: "Parent Finance",
              subject: "Scope integration",
              keywords: "report, scopes"
            }} = Info.get(pdf)

    assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert field.value == "Selected report choice"
    assert hd(field.widgets).page == 1

    for url <- [
          "https://parent.example/reports/summary",
          "https://parent.example/reports/next",
          "https://child.example/reports/leaf"
        ],
        do: assert(pdf =~ "/URI (" <> url <> ")")

    refute pdf =~ "never.example"
    assert {:ok, flattened} = NativeElixirPdfUtilities.Forms.flatten(pdf)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(flattened)
    assert text =~ "Selected report choice"
    assert text =~ "Final parent language"
    refute text =~ "Unused title"
  end
end
