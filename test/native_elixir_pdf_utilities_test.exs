defmodule NativeElixirPdfUtilitiesTest do
  use ExUnit.Case

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
end
