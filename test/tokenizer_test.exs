defmodule NativeElixirPdfUtilities.TokenizerTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Tokenizer

  defp toks(input) do
    input
    |> Tokenizer.new()
    |> Tokenizer.tokenize_all()
  end

  test "skips whitespace and comments" do
    input = "  % comment\r\n%more\n1 2  % trailing\n3"
    assert [{:int, 1}, {:int, 2}, {:int, 3}] = toks(input)
  end

  test "numbers and keywords" do
    input = "1 0 obj endobj xref trailer startxref 12 -3 +4 5.25 .bad +bad"

    assert [
             {:int, 1},
             {:int, 0},
             :obj,
             :endobj,
             :xref,
             :trailer,
             :startxref,
             {:int, 12},
             {:int, -3},
             {:int, 4},
             {:real, 5.25},
             {:error, {:not_a_number, ".bad"}},
             {:error, {:not_a_number, "+bad"}}
           ] = toks(input)
  end

  test "names with hex escapes" do
    input = "/A#20B#2fC /A#zzB"
    assert [{:name, "A B/C"}, {:name, "A#zzB"}] = toks(input)
  end

  test "literal strings with escapes and nesting" do
    input = "(a(b)c\\r\\t\\b\\f\\\\\\\n\\\r\n\\101\\4\\z)"
    [{:string, s}] = toks(input)
    assert s == "a(b)c\r\t\b\f\\A" <> <<4>> <> "z"
    assert [{:error, {:unterminated_literal_string, 0}}] = toks("(unterminated")
    assert [{:error, {:unterminated_literal_string, 0}}] = toks("(unterminated\\")
  end

  test "hex strings" do
    input = "<48656C6C6F> <4 1 % comment\n2> << >> >"

    assert [
             {:hex_string, "Hello"},
             {:hex_string, <<0x41, 0x20>>},
             :dict_start,
             :dict_end,
             {:error, {:unexpected_gt, _}}
           ] = toks(input)

    assert [{:error, {:unterminated_hex_string, 0}}] = toks("<48656C6C6F")
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
    input = "BT 1 0 0 1 0 0 cm ET @ true false null"

    assert [
             {:op, "BT"},
             {:int, 1},
             {:int, 0},
             {:int, 0},
             {:int, 1},
             {:int, 0},
             {:int, 0},
             {:op, "cm"},
             {:op, "ET"},
             {:error, {:unexpected_char, ?@, _}},
             true,
             false,
             :null
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

  test "stream data supports CRLF and is clamped to remaining bytes" do
    assert [
             :dict_start,
             {:name, "Length"},
             {:int, 10},
             :dict_end,
             :stream,
             {:stream_data, "abc"}
           ] = toks("<< /Length 10 >> stream\r\nabc")

    assert [
             :dict_start,
             {:name, "Length"},
             {:int, 1},
             {:int, 2},
             {:int, 3},
             :dict_end
           ] = toks("<< /Length 1 2 3 >>")
  end

  test "stream data fallback without /Length" do
    assert [:stream, {:stream_data, "abc"}, :endstream] = toks("stream\nabc\nendstream")
    assert [:stream, {:stream_data, "abc"}, :endstream] = toks("stream\nabc\r\nendstream")
    assert [:stream, {:stream_data, " abc "}, :endstream] = toks("stream abc endstream")
    assert [:stream, {:stream_data, ""}, :endstream] = toks("stream\nendstream")
  end

  test "stream data fallback scans a valid endstream marker" do
    input = "stream\nabcxendstream\nreal\rendstream"
    assert [:stream, {:stream_data, "abcxendstream\nreal"}, :endstream] = toks(input)
    assert [:stream, {:stream_data, "abc"}] = toks("stream\nabc")
  end

  test "peek and span APIs preserve tokenizer position metadata" do
    st = Tokenizer.new("  1 << /Length 2 >> stream\r\nHi\r\nendstream")

    assert Tokenizer.peek(st) == {:int, 1}

    assert {{:int, 1}, %{from: 2, to: 3, stream_mode?: nil}} =
             elem(Tokenizer.next_with_span(st), 0)

    spans = Tokenizer.tokenize_all_with_spans(st)

    assert {{:stream_data, "Hi"}, stream_span} =
             Enum.find(spans, fn {token, _span} -> match?({:stream_data, _}, token) end)

    assert stream_span.stream_mode? == :length
    assert binary_part(st.bin, stream_span.from, 2) == "Hi"

    assert [
             {:stream, %{stream_mode?: nil}},
             {{:stream_data, "abc"}, %{stream_mode?: :scanned}}
             | _
           ] = Tokenizer.tokenize_all_with_spans(Tokenizer.new("stream\nabc\nendstream"))

    assert [
             {:stream, %{stream_mode?: nil}},
             {{:stream_data, ""}, %{from: 6, to: 6, stream_mode?: :scanned}}
           ] = Tokenizer.tokenize_all_with_spans(Tokenizer.new("stream"))

    assert [
             {:stream, %{stream_mode?: nil}},
             {{:stream_data, " abc "}, %{from: 6, stream_mode?: :scanned}},
             {:endstream, %{stream_mode?: nil}}
           ] = Tokenizer.tokenize_all_with_spans(Tokenizer.new("stream abc endstream"))
  end

  test "pending stream length reports direct indirect and unknown hints" do
    {_, st} = Tokenizer.next(Tokenizer.new("<< /Length 3 >> stream\nabc"))
    {_, st} = Tokenizer.next(st)
    {_, st} = Tokenizer.next(st)
    {_, st} = Tokenizer.next(st)
    assert Tokenizer.pending_stream_length(st) == {:direct, 3}

    {_, st} = Tokenizer.next(Tokenizer.new("<< /Length 9 0 R >> stream\nabc\nendstream"))
    {_, st} = Tokenizer.next(st)
    {_, st} = Tokenizer.next(st)
    {_, st} = Tokenizer.next(st)
    {_, st} = Tokenizer.next(st)
    {_, st} = Tokenizer.next(st)
    assert Tokenizer.pending_stream_length(st) == {:indirect, {9, 0}}

    assert Tokenizer.pending_stream_length(Tokenizer.new("stream\nabc")) == :unknown
  end
end
