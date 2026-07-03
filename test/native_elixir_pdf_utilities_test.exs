defmodule NativeElixirPdfUtilitiesTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Text
  alias NativeElixirPdfUtilities.Tokenizer

  defp toks(input) do
    st = Tokenizer.new(input)

    st
    |> Stream.unfold(fn
      %Tokenizer{} = s ->
        {tok, s2} = Tokenizer.next(s)
        if tok == {:eof, nil}, do: nil, else: {tok, s2}
    end)
    |> Enum.to_list()
  end

  test "skips whitespace and comments" do
    input = "  % comment\r\n%more\n1 2  % trailing\n3"
    assert [{:int, 1}, {:int, 2}, {:int, 3}] = toks(input)
  end

  test "numbers and keywords" do
    input = "1 0 obj endobj xref trailer startxref 12"

    assert [
             {:int, 1},
             {:int, 0},
             :obj,
             :endobj,
             :xref,
             :trailer,
             :startxref,
             {:int, 12}
           ] = toks(input)
  end

  test "names with hex escapes" do
    # /A#20B#2fC -> 'A B/C'
    input = "/A#20B#2fC"
    assert [{:name, "A B/C"}] = toks(input)
  end

  test "literal strings with escapes and nesting" do
    # Use a non-parenthesis sigil delimiter to avoid nesting confusion
    input = ~S|(Hello \(world\)\n\101 )|
    [{:string, s}] = toks(input)
    assert s == "Hello (world)\nA "
  end

  test "hex strings" do
    input = "<48656C6C6F> << >>"
    assert [{:hex_string, "Hello"}, :dict_start, :dict_end] = toks(input)
  end

  test "arrays and refs" do
    input = "[ 3 0 R ]"
    assert [:lbracket, {:int, 3}, {:int, 0}, :R, :rbracket] = toks(input)
  end

  test "simple object header snippet" do
    input = "1 0 obj << /Type /Catalog /Pages 3 0 R >> endobj"

    assert [
             {:int, 1},
             {:int, 0},
             :obj,
             :dict_start,
             {:name, "Type"},
             {:name, "Catalog"},
             {:name, "Pages"},
             {:int, 3},
             {:int, 0},
             :R,
             :dict_end,
             :endobj
           ] = toks(input)
  end

  test "operators returned as op tokens" do
    input = "BT 1 0 0 1 0 0 cm ET"

    assert [
             {:op, "BT"},
             {:int, 1},
             {:int, 0},
             {:int, 0},
             {:int, 1},
             {:int, 0},
             {:int, 0},
             {:op, "cm"},
             {:op, "ET"}
           ] = toks(input)
  end

  test "stream data read by /Length" do
    input = "1 0 obj << /Length 5 >> stream\nHello\nendstream endobj"

    assert [
             {:int, 1},
             {:int, 0},
             :obj,
             :dict_start,
             {:name, "Length"},
             {:int, 5},
             :dict_end,
             :stream,
             {:stream_data, "Hello"},
             :endstream,
             :endobj
           ] = toks(input)
  end

  test "stream data fallback without /Length" do
    input = "stream\nabc\nendstream"
    assert [:stream, {:stream_data, "abc"}, :endstream] = toks(input)
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
