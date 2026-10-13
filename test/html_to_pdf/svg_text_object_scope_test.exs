defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextObjectScopeTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend
  alias NativeElixirPdfUtilities.Pdf.Reader

  test "real selected text initializes ActualText geometry for external readers", %{test: name} do
    directory = Path.join(System.tmp_dir!(), "svg-text-object-#{:erlang.phash2(name)}")
    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg width='200' height='50'><text x='5' y='25' font-size='20' font-family='NativeSvgFont0'>ffi á̈ Ω</text></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "ffi á̈ Ω"} = Text.extract(pdf)
    assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
    assert Enum.all?(page.spans, &(&1.paints_text? and &1.font_size == 20))
    assert {:ok, document} = Reader.read(pdf)
    assert Enum.any?(Map.values(document.objects), &(Map.get(&1, :stream) == data))
    path = Path.join(directory, "real-selected-glyphs.pdf")
    File.write!(path, pdf)
    tool = System.find_executable("pdftotext")

    assert is_binary(tool),
           "Poppler pdftotext is required for the selected-text bounding-box contract"

    assert {bbox, 0} = System.cmd(tool, ["-bbox", path, "-"], stderr_to_stdout: true)

    assert [_, x1, y1, x2, y2] =
             Regex.run(
               ~r/<word xMin="([^"]+)" yMin="([^"]+)" xMax="([^"]+)" yMax="([^"]+)">ffi<\/word>/u,
               bbox
             )

    [x1, y1, x2, y2] = Enum.map([x1, y1, x2, y2], &String.to_float/1)
    [cluster | _] = page.spans
    assert_in_delta x1, cluster.x, 0.001
    assert_in_delta x2, cluster.end_x, 0.001
    assert y2 - y1 > 10
    assert_in_delta x1, 3.75, 0.001
    refute pdf =~ "3 Tr"
  end

  test "a multi-glyph replacement uses one genuine text object and keeps its complete source selection" do
    html = "<svg width=200 height=50><text x=5 y=25 font-size=20>á̈ á̈</text></svg>"
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "á̈ á̈"} = Text.extract(pdf)
    assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
    clusters = Enum.filter(page.spans, &(&1.text == "á̈"))
    assert length(clusters) == 2
    assert Enum.all?(clusters, &(&1.paints_text? and &1.end_x > &1.x))
    assert {:ok, document} = Reader.read(pdf)

    stream =
      document.objects
      |> Map.values()
      |> Enum.map(&(Map.get(&1, :stream) || ""))
      |> Enum.find(&String.contains?(&1, "/ActualText"))

    replacements = Regex.scan(~r/BT.*?Tf.*?Tm.*?BDC(.*?)EMC\s*ET/s, stream)

    assert Enum.count(replacements, fn [_whole, body] ->
             length(Regex.scan(~r/ Tj/, body)) >= 2
           end) == 2

    refute stream =~ "3 Tr"
  end
end
