defmodule NativeElixirPdfUtilities.Tokenizer do
  @moduledoc """
  A PDF tokenizer in pure Elixir.

  Produces a stream of tokens from a PDF byte stream. It skips whitespace and comments.
  It recognizes:
    * numbers (integers and reals)
    * keywords: `obj`, `endobj`, `stream`, `endstream`, `xref`, `trailer`, `startxref`
    * booleans `true`/`false`, `null`
    * names `/Name` (with `#xx` hex escapes decoded)
    * strings: literal `( ... )` with escapes and nesting; hex `<...>`
    * punctuation: `[` `]` `<<` `>>` and `R`
    * operators: any other keyword returned as `{:op, word}`

  Streams:
    * After emitting `:stream`, the next token is `{:stream_data, binary}` either read by
      the preceding dictionary `/Length` (if an integer) or by scanning until `endstream`.
    * The tokenizer does not resolve indirect `/Length` references.
  """

  defstruct bin: <<>>,
            pos: 0,
            size: 0,
            dict_depth: 0,
            last_name: nil,
            last_length: nil,
            last_length_ref: nil,
            pending_length_first: nil,
            pending_length_second: nil,
            in_stream: false

  @type t :: %__MODULE__{bin: binary(), pos: non_neg_integer(), size: non_neg_integer()}

  @type token ::
          {:int, integer()}
          | {:real, float()}
          | {:name, binary()}
          | {:string, binary()}
          | {:hex_string, binary()}
          | {:stream_data, binary()}
          | {:op, binary()}
          | :null
          | true
          | false
          | :lbracket
          | :rbracket
          | :dict_start
          | :dict_end
          | :obj
          | :endobj
          | :stream
          | :endstream
          | :xref
          | :trailer
          | :startxref
          | :R
          | {:eof, nil}

  # NUL, HT, LF, FF, CR, SP
  @whitespace [0, 9, 10, 12, 13, 32]
  @delims ~c"()<>[]{}/%"

  @doc """
  Initialize the tokenizer from a binary.
  """
  @spec new(binary()) :: t()
  def new(bin) when is_binary(bin), do: %__MODULE__{bin: bin, pos: 0, size: byte_size(bin)}

  @doc """
  Return the next token and updated tokenizer state.
  """
  @spec next(t()) :: {token(), t()}
  def next(%__MODULE__{} = st) do
    # If we are currently positioned at stream data, return it first
    if st.in_stream do
      return_stream_data(st)
    else
      st = skip_ws_and_comments(st)

      if st.pos >= st.size do
        {{:eof, nil}, st}
      else
        case byte_at(st) do
          ?/ ->
            parse_name(st)

          ?( ->
            parse_literal_string(st)

          ?< ->
            parse_lt(st)

          ?> ->
            parse_gt(st)

          ?[ ->
            {:lbracket, bump(st, 1)}

          ?] ->
            {:rbracket, bump(st, 1)}

          c when (c >= ?0 and c <= ?9) or c == ?+ or c == ?- or c == ?. ->
            parse_number_or_keyword(st)

          c when (c >= ?A and c <= ?Z) or (c >= ?a and c <= ?z) ->
            parse_keyword(st)

          _ ->
            {{:error, {:unexpected_char, byte_at(st), st.pos}}, bump(st, 1)}
        end
      end
    end
  end

  @doc """
  Return the next token along with its byte span in the original binary.

  The span is a map with `:from` and `:to` (exclusive end index). For `{:stream_data, _}`
  tokens, `from` excludes the EOL after the `stream` keyword.
  """
  @spec next_with_span(t()) ::
          {{token(),
            %{from: non_neg_integer(), to: non_neg_integer(), stream_mode?: atom() | nil}}, t()}
  def next_with_span(%__MODULE__{} = st) do
    if st.in_stream do
      # compute start after the single EOL that precedes stream data
      start = st.pos + eol_len_at_pos(st)
      mode = if is_integer(st.last_length) and st.last_length >= 0, do: :length, else: :scanned
      {tok, st2} = return_stream_data(st)
      {{tok, %{from: start, to: st2.pos, stream_mode?: mode}}, st2}
    else
      st_ws = skip_ws_and_comments(st)
      start = st_ws.pos
      {tok, st2} = next(st)
      {{tok, %{from: start, to: st2.pos, stream_mode?: nil}}, st2}
    end
  end

  @doc """
  Look at the next token without advancing the tokenizer state.
  """
  @spec peek(t()) :: token()
  def peek(%__MODULE__{} = st) do
    {tok, _} = next(st)
    tok
  end

  @doc """
  Tokenize the entire binary, returning a list of tokens.
  """
  @spec tokenize_all(t()) :: [token()]
  def tokenize_all(%__MODULE__{} = st) do
    st
    |> Stream.unfold(fn
      %__MODULE__{} = s ->
        {tok, s2} = next(s)
        if tok == {:eof, nil}, do: nil, else: {tok, s2}
    end)
    |> Enum.to_list()
  end

  @doc """
  Tokenize the entire binary, returning a list of {token, span}.
  """
  @spec tokenize_all_with_spans(t()) :: [
          {token(), %{from: non_neg_integer(), to: non_neg_integer(), stream_mode?: atom() | nil}}
        ]
  def tokenize_all_with_spans(%__MODULE__{} = st) do
    st
    |> Stream.unfold(fn
      %__MODULE__{} = s ->
        {{tok, span}, s2} = next_with_span(s)
        if tok == {:eof, nil}, do: nil, else: {{tok, span}, s2}
    end)
    |> Enum.to_list()
  end

  @doc """
  If called after a `:stream` token, returns a hint for the stream length:
  - `{:direct, int}` when `/Length` was a direct integer
  - `{:indirect, {obj, gen}}` when `/Length` was an indirect reference
  - `:unknown` when no hint is available
  """
  @spec pending_stream_length(t()) ::
          {:direct, non_neg_integer()} | {:indirect, {integer(), integer()}} | :unknown
  def pending_stream_length(%__MODULE__{} = st) do
    cond do
      is_integer(st.last_length) and st.last_length >= 0 ->
        {:direct, st.last_length}

      match?({a, b} when is_integer(a) and is_integer(b), st.last_length_ref) ->
        {:indirect, st.last_length_ref}

      true ->
        :unknown
    end
  end

  # ===== Core scanning helpers =====

  # Skip whitespace and comments from the current position.
  defp skip_ws_and_comments(%__MODULE__{} = st) do
    do_skip(st)
  end

  # Worker to skip whitespace/comments; fast-path when at or beyond EOF.
  defp do_skip(%__MODULE__{pos: pos, size: size} = st) when pos >= size, do: st

  # skip whitespace and comments
  defp do_skip(%__MODULE__{} = st) do
    b = byte_at(st)

    cond do
      b in @whitespace -> do_skip(bump(st, 1))
      b == ?% -> do_skip(skip_comment(st))
      true -> st
    end
  end

  # Skip a single comment line starting with '%', until EOL or EOF.
  defp skip_comment(%__MODULE__{} = st) do
    # skip until CR or LF or EOF
    advance_to_eol(st)
  end

  # Advance until an end-of-line (CR or LF) or EOF.
  defp advance_to_eol(%__MODULE__{pos: pos, size: size} = st) when pos >= size, do: st

  defp advance_to_eol(%__MODULE__{} = st) do
    case byte_at(st) do
      # LF
      10 -> bump(st, 1)
      # CR
      13 -> bump(st, 1)
      _ -> advance_to_eol(bump(st, 1))
    end
  end

  # Increase the position in the underlying binary by n bytes.
  defp bump(%__MODULE__{pos: pos} = st, n), do: %{st | pos: pos + n}

  # Read the byte at the current position.
  defp byte_at(%__MODULE__{bin: bin, pos: pos}), do: :binary.at(bin, pos)

  # Slice `len` bytes from the current position.
  defp slice(%__MODULE__{bin: bin, pos: pos}, len), do: :binary.part(bin, pos, len)

  # Whether a byte is a delimiter per PDF syntax.
  defp is_delim?(c) when is_integer(c), do: c in @delims

  # ===== Name =====

  # Parse a name object starting with '/'.
  defp parse_name(%__MODULE__{} = st) do
    # skip '/'
    st1 = bump(st, 1)
    {bytes, st2} = take_until_delim(st1)

    name = decode_name(bytes)
    st2 = put_last_name(st2, name)
    {{:name, name}, st2}
  end

  # Decode a PDF name byte sequence (handles #xx hex escapes) into a binary.
  defp decode_name(bytes) do
    decode_name(bytes, [])
    |> IO.iodata_to_binary()
  end

  # Worker for name decoding (accumulates codepoints/bytes in reverse).
  defp decode_name(<<>>, acc), do: Enum.reverse(acc)

  defp decode_name(<<?#, a, b, rest::binary>>, acc) do
    if hex_char?(a) and hex_char?(b) do
      <<val>> = <<hexval(a) * 16 + hexval(b)>>
      decode_name(rest, [val | acc])
    else
      # not a valid #xx escape; keep '#' literal and continue
      decode_name(<<a, b, rest::binary>>, [?# | acc])
    end
  end

  defp decode_name(<<c, rest::binary>>, acc), do: decode_name(rest, [c | acc])

  # True if byte is a hex digit.
  defp hex_char?(c),
    do: (c >= ?0 and c <= ?9) or (c >= ?A and c <= ?F) or (c >= ?a and c <= ?f)

  # Convert a hex digit byte to its integer value.
  defp hexval(c) when c >= ?0 and c <= ?9, do: c - ?0
  defp hexval(c) when c >= ?A and c <= ?F, do: c - ?A + 10
  defp hexval(c) when c >= ?a and c <= ?f, do: c - ?a + 10

  # Read bytes until a delimiter or whitespace is reached.
  defp take_until_delim(%__MODULE__{} = st), do: take_until_delim(st, 0)

  defp take_until_delim(%__MODULE__{pos: pos, size: size} = st, n) when pos + n >= size,
    do: {slice(st, n), bump(st, n)}

  defp take_until_delim(%__MODULE__{} = st, n) do
    c = :binary.at(st.bin, st.pos + n)

    cond do
      c in @whitespace -> {slice(st, n), bump(st, n)}
      c in @delims -> {slice(st, n), bump(st, n)}
      true -> take_until_delim(st, n + 1)
    end
  end

  # ===== Literal String "(...)" =====

  # Parse a literal string enclosed in parentheses, handling escapes and nesting.
  defp parse_literal_string(%__MODULE__{} = st) do
    # skip '('
    st1 = bump(st, 1)
    {iodata, st2} = collect_lit(st1, 0, [])
    {{:string, IO.iodata_to_binary(iodata)}, st2}
  end

  # balance is nesting depth of parentheses
  defp collect_lit(%__MODULE__{pos: pos, size: size} = st, _balance, acc) when pos >= size do
    {Enum.reverse(acc), st}
  end

  defp collect_lit(%__MODULE__{} = st, balance, acc) do
    case byte_at(st) do
      # escape
      ?\\ ->
        collect_escape(bump(st, 1), balance, acc)

      ?( ->
        collect_lit(bump(st, 1), balance + 1, [?( | acc])

      ?) ->
        if balance == 0 do
          # end of string
          {Enum.reverse(acc), bump(st, 1)}
        else
          collect_lit(bump(st, 1), balance - 1, [?) | acc])
        end

      c ->
        collect_lit(bump(st, 1), balance, [c | acc])
    end
  end

  # Handle a backslash escape sequence within a literal string.
  defp collect_escape(%__MODULE__{} = st, balance, acc) do
    c = byte_at(st)

    cond do
      c == ?n ->
        collect_lit(bump(st, 1), balance, [?\n | acc])

      c == ?r ->
        collect_lit(bump(st, 1), balance, [?\r | acc])

      c == ?t ->
        collect_lit(bump(st, 1), balance, [?\t | acc])

      c == ?b ->
        collect_lit(bump(st, 1), balance, [?\b | acc])

      c == ?f ->
        collect_lit(bump(st, 1), balance, [?\f | acc])

      c == ?( ->
        collect_lit(bump(st, 1), balance, [?( | acc])

      c == ?) ->
        collect_lit(bump(st, 1), balance, [?) | acc])

      c == ?\\ ->
        collect_lit(bump(st, 1), balance, [?\\ | acc])

      c >= ?0 and c <= ?7 ->
        {val, st2} = collect_octal(st, 0, 0)
        collect_lit(st2, balance, [val | acc])

      # line continuation: backslash followed by CR or LF (or CRLF) is ignored
      # LF
      c == 10 ->
        collect_lit(bump(st, 1), balance, acc)

      c == 13 ->
        st2 = bump(st, 1)
        st3 = if st2.pos < st2.size and byte_at(st2) == 10, do: bump(st2, 1), else: st2
        collect_lit(st3, balance, acc)

      true ->
        # unknown escape -> keep char as-is
        collect_lit(bump(st, 1), balance, [c | acc])
    end
  end

  # Parse up to three octal digits after a backslash escape and emit the byte.
  defp collect_octal(%__MODULE__{} = st, nread, acc) when nread >= 3 do
    {<<acc>>, st}
  end

  defp collect_octal(%__MODULE__{pos: pos, size: size} = st, _nread, acc) when pos >= size do
    {<<acc>>, st}
  end

  defp collect_octal(%__MODULE__{} = st, nread, acc) do
    c = byte_at(st)

    cond do
      c >= ?0 and c <= ?7 -> collect_octal(bump(st, 1), nread + 1, acc * 8 + (c - ?0))
      true -> {<<acc>>, st}
    end
  end

  # ===== Hex String or Dict Start =====

  # Parse either a dict start '<<' or a hex string '<...>'.
  defp parse_lt(%__MODULE__{} = st) do
    if st.pos + 1 < st.size and :binary.at(st.bin, st.pos + 1) == ?< do
      st2 = bump(st, 2)
      {:dict_start, inc_dict_depth(st2)}
    else
      parse_hex_string(bump(st, 1))
    end
  end

  # Parse a hex string '<...>' into bytes (odd nibbles are padded with 0).
  defp parse_hex_string(%__MODULE__{} = st) do
    {hexes, st2} = collect_until_gt(st, [])
    # Remove all non-hex (whitespace/comments already skipped, but be safe)
    hex_only =
      for <<c <- IO.iodata_to_binary(hexes)>>,
          (c >= ?0 and c <= ?9) or (c >= ?A and c <= ?F) or (c >= ?a and c <= ?f),
          do: c

    # If odd count, pad with 0
    hex_only = if rem(length(hex_only), 2) == 1, do: hex_only ++ [?0], else: hex_only
    bytes = hex_pairs_to_bin(hex_only, [])
    {{:hex_string, IO.iodata_to_binary(bytes)}, st2}
  end

  # Collect raw bytes until the closing '>' of a hex string (skip nested comments conservatively).
  defp collect_until_gt(%__MODULE__{} = st, acc) do
    case byte_at(st) do
      ?> -> {Enum.reverse(acc), bump(st, 1)}
      ?% -> collect_until_gt(skip_comment(st), acc)
      c -> collect_until_gt(bump(st, 1), [c | acc])
    end
  end

  # Convert a flat list of hex digit bytes to byte values as iodata.
  defp hex_pairs_to_bin([], acc), do: Enum.reverse(acc)

  defp hex_pairs_to_bin([a, b | rest], acc) do
    <<val>> = <<hexval(a) * 16 + hexval(b)>>
    hex_pairs_to_bin(rest, [val | acc])
  end

  # ===== Dict End or stray '>' =====

  # Parse a dict end '>>' (finalizing pending /Length) or flag an unexpected single '>'.
  defp parse_gt(%__MODULE__{} = st) do
    if st.pos + 1 < st.size and :binary.at(st.bin, st.pos + 1) == ?> do
      st2 = bump(st, 2)
      st3 = maybe_finalize_pending_length(st2)
      {:dict_end, dec_dict_depth(st3)}
    else
      {{:error, {:unexpected_gt, st.pos}}, bump(st, 1)}
    end
  end

  # ===== Numbers & Keywords =====

  # Parse a number or a keyword that looks like a number prefix.
  defp parse_number_or_keyword(%__MODULE__{} = st) do
    {word, st2} = take_word(st)

    case word do
      "obj" -> {:obj, st2}
      "endobj" -> {:endobj, st2}
      "stream" -> begin_stream(st2)
      "endstream" -> {:endstream, st2}
      "xref" -> {:xref, st2}
      "trailer" -> {:trailer, st2}
      "startxref" -> {:startxref, st2}
      "true" -> {true, st2}
      "false" -> {false, st2}
      "null" -> {:null, st2}
      _ -> parse_number_from_word(word, st2)
    end
  end

  # Parse a bareword keyword (operators or known symbols like obj/endobj/R/etc.).
  defp parse_keyword(%__MODULE__{} = st) do
    {word, st2} = take_word(st)

    case word do
      "obj" ->
        {:obj, st2}

      "endobj" ->
        {:endobj, st2}

      "stream" ->
        begin_stream(st2)

      "endstream" ->
        {:endstream, st2}

      "xref" ->
        {:xref, st2}

      "trailer" ->
        {:trailer, st2}

      "startxref" ->
        {:startxref, st2}

      "R" ->
        st3 = maybe_capture_length_ref_on_R(st2)
        {:R, st3}

      "true" ->
        {true, st2}

      "false" ->
        {false, st2}

      "null" ->
        {:null, st2}

      _ ->
        {{:op, word}, st2}
    end
  end

  # Read a bareword until a delimiter or whitespace.
  defp take_word(%__MODULE__{} = st), do: take_word(st, 0)

  defp take_word(%__MODULE__{pos: pos, size: size} = st, n) when pos + n >= size,
    do: {slice(st, n), bump(st, n)}

  defp take_word(%__MODULE__{} = st, n) do
    c = :binary.at(st.bin, st.pos + n)

    cond do
      c in @whitespace -> {slice(st, n), bump(st, n)}
      is_delim?(c) -> {slice(st, n), bump(st, n)}
      true -> take_word(st, n + 1)
    end
  end

  # Interpret a bareword as a PDF integer or real; otherwise return {:error, {:not_a_number, word}}.
  defp parse_number_from_word(word, st) do
    # PDF numbers don't have exponents per spec; we allow a leading +/-, decimal dot.
    if String.contains?(word, ".") do
      case Float.parse(word) do
        {f, ""} -> {{:real, f}, st}
        _ -> {{:error, {:not_a_number, word}}, st}
      end
    else
      case Integer.parse(word) do
        {i, ""} ->
          st2 = maybe_capture_length_int(st, i)
          {{:int, i}, st2}

        _ ->
          {{:error, {:not_a_number, word}}, st}
      end
    end
  end

  # ===== Stream data handling =====

  # Switch into stream mode. The next token will be {:stream_data, bin}.
  defp begin_stream(%__MODULE__{} = st) do
    # After 'stream' keyword there must be a single EOL (CR, LF, or CRLF) which is not part of the data
    # We set in_stream so next/1 returns {:stream_data, bin}
    {:stream, %{st | in_stream: true}}
  end

  # Emit stream data either by known /Length (direct) or scanning until 'endstream'.
  defp return_stream_data(%__MODULE__{} = st) do
    st1 = skip_single_eol(st)

    cond do
      is_integer(st.last_length) and st.last_length >= 0 ->
        len = st.last_length
        data = safe_slice(st1, len)
        st2 = bump(st1, byte_size(data))
        # reset stream markers; length applies to this stream only
        st3 = %{
          st2
          | in_stream: false,
            last_length: nil,
            last_length_ref: nil,
            pending_length_first: nil,
            pending_length_second: nil,
            last_name: nil
        }

        {{:stream_data, data}, st3}

      true ->
        # Fallback: scan until the next 'endstream' and take bytes up to (without trailing EOL)
        {data, st2} = take_until_endstream(st1)

        st3 = %{
          st2
          | in_stream: false,
            last_length: nil,
            last_length_ref: nil,
            pending_length_first: nil,
            pending_length_second: nil,
            last_name: nil
        }

        {{:stream_data, data}, st3}
    end
  end

  # Consume a single CR, LF or CRLF sequence if present at current position.
  defp skip_single_eol(%__MODULE__{} = st) do
    cond do
      st.pos >= st.size ->
        st

      byte_at(st) == 13 ->
        st2 = bump(st, 1)
        if st2.pos < st2.size and byte_at(st2) == 10, do: bump(st2, 1), else: st2

      byte_at(st) == 10 ->
        bump(st, 1)

      true ->
        st
    end
  end

  # Return the EOL length (0/1/2) at current position without advancing.
  defp eol_len_at_pos(%__MODULE__{} = st) do
    cond do
      st.pos >= st.size ->
        0

      byte_at(st) == 13 ->
        if st.pos + 1 < st.size and :binary.at(st.bin, st.pos + 1) == 10, do: 2, else: 1

      byte_at(st) == 10 ->
        1

      true ->
        0
    end
  end

  # Slice up to `len` bytes, clamped to remaining document size.
  defp safe_slice(%__MODULE__{bin: bin, pos: pos, size: size}, len) do
    max_len = max(0, min(len, size - pos))
    :binary.part(bin, pos, max_len)
  end

  # Find the next 'endstream' and return bytes up to it (minus a trailing single EOL, if present).
  defp take_until_endstream(%__MODULE__{} = st) do
    # Find the next 'endstream' occurrence and consider preceding EOL not part of data
    idx = find_endstream_index(st)

    if idx == nil do
      # No endstream found; consume rest of file
      data = safe_slice(st, st.size - st.pos)
      {data, %{st | pos: st.size}}
    else
      data = :binary.part(st.bin, st.pos, idx - st.pos)
      data2 = drop_trailing_single_eol(data)
      {data2, %{st | pos: idx}}
    end
  end

  # Return the absolute index of the next 'endstream' keyword, or nil if not found.
  defp find_endstream_index(%__MODULE__{} = st) do
    bin = st.bin
    from = st.pos
    find_from(bin, from)
  end

  # Search for 'endstream' starting at `from` with simple pre-check for preceding whitespace.
  defp find_from(_bin, from) when from < 0, do: nil
  defp find_from(bin, from) when from >= byte_size(bin), do: nil

  defp find_from(bin, from) do
    case :binary.match(bin, "endstream", scope: {from, byte_size(bin) - from}) do
      :nomatch ->
        nil

      {idx, 9} ->
        # Check that preceding byte (if any) is whitespace/EOL; this reduces false positives
        prev = if idx == 0, do: nil, else: :binary.at(bin, idx - 1)

        if prev in [nil, 9, 10, 12, 13, 32] do
          idx
        else
          find_from(bin, idx + 1)
        end
    end
  end

  # Drop a single trailing CR, LF or CRLF from a binary, if present.
  defp drop_trailing_single_eol(<<>>), do: <<>>

  defp drop_trailing_single_eol(bin) do
    len = byte_size(bin)

    cond do
      len >= 2 and :binary.at(bin, len - 2) == 13 and :binary.at(bin, len - 1) == 10 ->
        :binary.part(bin, 0, len - 2)

      len >= 1 and (:binary.at(bin, len - 1) == 13 or :binary.at(bin, len - 1) == 10) ->
        :binary.part(bin, 0, len - 1)

      true ->
        bin
    end
  end

  # ===== State helpers =====

  # Track nested dictionary depth (used to detect /Length inside dicts).
  defp inc_dict_depth(%__MODULE__{} = st), do: %{st | dict_depth: st.dict_depth + 1}

  defp dec_dict_depth(%__MODULE__{} = st), do: %{st | dict_depth: max(st.dict_depth - 1, 0)}

  # Remember the last seen name; special handling for /Length to latch following number/ref.
  defp put_last_name(%__MODULE__{} = st, name) do
    st2 = maybe_finalize_pending_length(st)

    if name == "Length" do
      %{
        st2
        | last_name: name,
          pending_length_first: nil,
          pending_length_second: nil,
          last_length_ref: nil
      }
    else
      %{st2 | last_name: name}
    end
  end

  # If inside a dict and after /Length, capture an immediate integer as the stream length.
  defp maybe_capture_length_int(%__MODULE__{} = st, int_val) do
    if st.dict_depth > 0 and st.last_name == "Length" do
      cond do
        is_nil(st.pending_length_first) -> %{st | pending_length_first: int_val}
        is_nil(st.pending_length_second) -> %{st | pending_length_second: int_val}
        true -> st
      end
    else
      st
    end
  end

  # If we saw two ints followed by R after /Length, store it as an indirect reference hint.
  defp maybe_capture_length_ref_on_R(%__MODULE__{} = st) do
    if st.dict_depth > 0 and st.last_name == "Length" and not is_nil(st.pending_length_first) and
         not is_nil(st.pending_length_second) do
      %{
        st
        | last_length_ref: {st.pending_length_first, st.pending_length_second},
          pending_length_first: nil,
          pending_length_second: nil
      }
    else
      st
    end
  end

  # When leaving a context where /Length is complete, finalize pending direct length if present.
  defp maybe_finalize_pending_length(%__MODULE__{} = st) do
    if st.dict_depth > 0 and st.last_name == "Length" and is_nil(st.last_length_ref) and
         is_nil(st.last_length) and is_integer(st.pending_length_first) do
      %{
        st
        | last_length: st.pending_length_first,
          pending_length_first: nil,
          pending_length_second: nil
      }
    else
      st
    end
  end
end
