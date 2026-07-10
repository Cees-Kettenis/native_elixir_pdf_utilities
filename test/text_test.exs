defmodule NativeElixirPdfUtilities.TextTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Text

  defp text_pdf(content, opts \\ []) do
    stream =
      if Keyword.get(opts, :flate?, false) do
        :zlib.compress(content)
      else
        content
      end

    filter = if Keyword.get(opts, :flate?, false), do: "/Filter /FlateDecode ", else: ""

    """
    %PDF-1.7
    1 0 obj << /#{Keyword.get(opts, :stream_name, "Contents")} 2 0 R >> endobj
    2 0 obj << #{filter}/Length #{byte_size(stream)} >> stream
    #{stream}
    endstream endobj
    %%EOF
    """
  end

  test "extracts embedded text from flate encoded content stream with ToUnicode cmap" do
    cmap = """
    /CIDInit /ProcSet findresource begin
    begincmap
    1 begincodespacerange <0000> <FFFF> endcodespacerange
    3 beginbfchar
    <0001> <0048>
    <0002> <0069>
    <0003> <0021>
    endbfchar
    endcmap
    """

    content = """
    BT
    /F1 12 Tf
    1 0 0 -1 10 20 Tm
    [<000100020003>] TJ
    ET
    """

    cmap_stream = :zlib.compress(cmap)
    content_stream = :zlib.compress(content)

    pdf = """
    %PDF-1.7
    1 0 obj << /Type /Font /ToUnicode 2 0 R >> endobj
    2 0 obj << /Filter /FlateDecode /Length #{byte_size(cmap_stream)} >> stream
    #{cmap_stream}
    endstream endobj
    3 0 obj << /Font << /F1 1 0 R >> >> endobj
    4 0 obj << /Filter /FlateDecode /Length #{byte_size(content_stream)} >> stream
    #{content_stream}
    endstream endobj
    %%EOF
    """

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Hi!"
  end

  test "extracts text with plain layout and file helpers" do
    pdf =
      text_pdf("""
      BT
      1 0 0 -1 10 20 Tm
      (Hello) Tj
      1 0 0 -1 50 20 Tm
      (World) Tj
      ET
      """)

    assert Text.extract(pdf, layout: false) == {:ok, "Hello World"}

    path = Path.join(System.tmp_dir!(), "native-elixir-pdf-text-test.pdf")
    File.write!(path, pdf)

    assert Text.extract_file(path, layout: false) == {:ok, "Hello World"}
    missing_path = path <> ".missing"

    assert {:error,
            {:enoent,
             %{
               stage: :file,
               reason: :enoent,
               operation: :read,
               module: NativeElixirPdfUtilities.Text,
               source: ^missing_path,
               message: "file read failed: enoent"
             }}} = Text.extract_file(missing_path)
  after
    path = Path.join(System.tmp_dir!(), "native-elixir-pdf-text-test.pdf")
    File.rm(path)
  end

  test "extract reports empty pdf text" do
    assert Text.extract("%PDF-1.7\n1 0 obj << /Type /Catalog >> endobj\n%%EOF") ==
             {:error,
              {:empty_pdf_text,
               %{
                 stage: :text_extraction,
                 reason: :empty_pdf_text,
                 operation: :extract,
                 module: NativeElixirPdfUtilities.Text,
                 message: "PDF contains no extractable text"
               }}}
  end

  test "extract rejects invalid inputs with diagnostic details" do
    assert {:error,
            {:invalid_pdf_input,
             %{
               stage: :text_extraction,
               reason: :invalid_pdf_input,
               operation: :extract,
               module: NativeElixirPdfUtilities.Text,
               message: "PDF input must be a binary"
             }}} = Text.extract(:not_pdf)

    assert {:error,
            {:invalid_options,
             %{
               stage: :options,
               reason: :invalid_options,
               operation: :extract,
               module: NativeElixirPdfUtilities.Text,
               message: "extract options must be a keyword list"
             }}} = Text.extract("%PDF-1.7", [:not_options])

    assert {:error,
            {:invalid_path,
             %{
               stage: :file,
               reason: :invalid_path,
               operation: :extract_file,
               module: NativeElixirPdfUtilities.Text,
               message: "path must be a string"
             }}} = Text.extract_file(:not_path)

    assert {:error,
            {:invalid_pdf_input,
             %{
               stage: :text_extraction,
               reason: :invalid_pdf_input,
               operation: :extract,
               module: NativeElixirPdfUtilities.Text,
               message: "PDF input is malformed or contains unsupported syntax"
             }}} = Text.extract("%PDF-1.7\n1 0 obj <4142")
  end

  test "extract ignores CMaps whose ranges exceed the safe expansion limit" do
    cmap = "begincmap\n1 beginbfrange\n<0000> <186A0> <0041>\nendbfrange\nendcmap"
    content = "BT (safe) Tj ET"

    pdf =
      "%PDF-1.7\n" <>
        "1 0 obj << /Length #{byte_size(cmap)} >> stream\n#{cmap}\nendstream endobj\n" <>
        "2 0 obj << /Length #{byte_size(content)} >> stream\n#{content}\nendstream endobj\n%%EOF\n"

    assert {:error,
            {:empty_pdf_text,
             %{stage: :text_extraction, reason: :empty_pdf_text, operation: :extract}}} =
             Text.extract(pdf, layout: false)
  end

  test "extract ignores CMaps larger than the safe input limit" do
    cmap = "begincmap\n" <> String.duplicate("x", 1_000_001)

    pdf =
      "%PDF-1.7\n1 0 obj << /Length #{byte_size(cmap)} >> stream\n#{cmap}\nendstream endobj\n%%EOF\n"

    assert {:error,
            {:empty_pdf_text,
             %{stage: :text_extraction, reason: :empty_pdf_text, operation: :extract}}} =
             Text.extract(pdf)
  end

  test "extract_file adds file context to extraction diagnostics" do
    path = Path.join(System.tmp_dir!(), "native-elixir-pdf-empty-text-test.pdf")
    File.write!(path, "%PDF-1.7\n1 0 obj << /Type /Catalog >> endobj\n%%EOF")

    assert {:error,
            {:empty_pdf_text,
             %{
               stage: :text_extraction,
               reason: :empty_pdf_text,
               operation: :extract,
               module: NativeElixirPdfUtilities.Text,
               source: ^path,
               message: "PDF contains no extractable text"
             }}} = Text.extract_file(path)
  after
    path = Path.join(System.tmp_dir!(), "native-elixir-pdf-empty-text-test.pdf")
    File.rm(path)
  end

  test "extracts text through text positioning operators and cmap ranges" do
    cmap = """
    begincmap
    1 beginbfrange
    <0001> <0003> <0041>
    endbfrange
    1 beginbfchar
    <0004> <110000>
    endbfchar
    endcmap
    """

    content = """
    BT
    /F1 12 Tf
    1.0 0 0 -1.0 10.5 20.5 Tm
    [<000100020003> 12 (Z)] TJ
    (bad) Tm
    /Name Td
    5 7 Td
    (A) Tj
    2 3 TD
    (B) '
    4 5 TD
    (C) "
    T*
    12 Tj
    (ignored) Do
    [<0004>] TJ
    ET
    """

    cmap_stream = :zlib.compress(cmap)
    content_stream = :zlib.compress(content)

    pdf = """
    %PDF-1.7
    1 0 obj << /Type /Font /ToUnicode 2 0 R >> endobj
    2 0 obj << /Filter /FlateDecode /Length #{byte_size(cmap_stream)} >> stream
    #{cmap_stream}
    endstream endobj
    3 0 obj << /Font << /F1 1 0 R /FX 99 0 R >> >> endobj
    4 0 obj << /Filter /FlateDecode /Length #{byte_size(content_stream)} >> stream
    #{content_stream}
    endstream endobj
    5 0 obj << /Filter /FlateDecode /Length 7 >> stream
    invalid
    endstream endobj
    %%EOF
    """

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "ABCZ"
    assert text =~ "A B C"
  end

  test "extracts fallback 16-bit text and tolerates loose object streams" do
    pdf = """
    %PDF-1.7
    xref
    1 0 obj << /Length 30 >> stream
    BT
    [<0041> /Ignored] TJ
    ET
    endstream
    """

    assert Text.extract(pdf, layout: false) == {:ok, "A"}
  end

  test "layout keeps absolute columns on continuation lines" do
    content = """
    BT
    /F1 12 Tf
    1 0 0 -1 10 20 Tm
    (DESCx) Tj
    1 0 0 -1 240 20 Tm
    (USE1x) Tj
    1 0 0 -1 60 32 Tm
    (DESC2) Tj
    1 0 0 -1 240 32 Tm
    (USE2x) Tj
    ET
    """

    content_stream = :zlib.compress(content)

    pdf = """
    %PDF-1.7
    1 0 obj << /Font << /F1 2 0 R >> >> endobj
    2 0 obj << /Type /Font >> endobj
    3 0 obj << /Filter /FlateDecode /Length #{byte_size(content_stream)} >> stream
    #{content_stream}
    endstream endobj
    %%EOF
    """

    assert {:ok, text} = Text.extract(pdf)
    [first, second] = String.split(text, "\n")

    assert String.starts_with?(String.trim_leading(first), "DESCx")
    assert String.starts_with?(String.trim_leading(second), "DESC2")
    assert :binary.match(first, "USE1x") == :binary.match(second, "USE2x")
  end
end
