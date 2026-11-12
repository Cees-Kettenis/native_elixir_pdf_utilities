defmodule NativeElixirPdfUtilities.HtmlToPdf.UnicodeLineBreak do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{UnicodeCharacterProperty, UnicodeTextData}

  # Unicode 17.0 UAX14 revision 55. Data and provenance share UnicodeTextData.
  @pictographic Regex.compile!(
                  "\\A(?:" <>
                    UnicodeCharacterProperty.expression("Extended_Pictographic", false) <> ")\\z",
                  "u"
                )
  @edge %{
    class: :sot,
    category: :Lu,
    wide?: false,
    codepoint: 0,
    joiner?: false,
    numeric?: false,
    regional: 0,
    nonspace: -1
  }
  @hard [:BK, :CR, :LF, :NL]
  @letters [:AL, :HL]
  @hangul [:JL, :JV, :JT, :H2, :H3]

  @doc "Returns Unicode 17 line-break byte offsets for validator-admitted UTF-8."
  @spec opportunities(binary()) :: [{non_neg_integer(), boolean()}]
  @spec opportunities(binary(), map()) :: [{non_neg_integer(), boolean()}]
  def opportunities(text, policy \\ %{}) do
    if Map.get(policy, :word_break) != :break_all and
         Regex.match?(~r/\A[A-Za-z0-9 .,:-]*\z/, text) do
      ascii_words(text, 0, nil, nil, [])
    else
      records = units(text, 0, [], policy) |> Enum.reverse()

      {records, _} =
        records
        |> Enum.with_index()
        |> Enum.map_reduce(@edge, fn {record, index}, previous ->
          record =
            Map.merge(record, %{
              nonspace: if(previous.class == :SP, do: previous.nonspace, else: index - 1),
              numeric?: record.class == :NU or (record.class in [:SY, :IS] and previous.numeric?),
              regional: if(record.class == :RI, do: previous.regional + 1, else: 0)
            })

          {record, record}
        end)

      records = List.to_tuple(records)

      for(
        index <- 1..(tuple_size(records) - 1)//1,
        decision = boundary(records, index),
        decision != :none,
        do: {elem(records, index).start, decision == :mandatory}
      ) ++ [{byte_size(text), true}]
    end
  end

  # Within AL/NU/SP/HY/IS, LB18, LB20a, LB21, LB25 and LB29 determine every
  # nonfinal boundary. Keep numeric punctuation and word-initial hyphens joined.
  defp ascii_words(text, offset, before, previous, output) do
    case text do
      <<character, rest::binary>> ->
        allowed? =
          (previous == 32 and character != 32 and
             (character not in [?., ?,, ?:] or
                case rest do
                  <<next, _::binary>> -> next in ?0..?9
                  _ -> false
                end)) or
            (previous == ?- and before not in [nil, 32] and
               (character in ?A..?Z or character in ?a..?z))

        output = if allowed?, do: [{offset, false} | output], else: output
        ascii_words(rest, offset + 1, previous, character, output)

      "" ->
        Enum.reverse([{offset, true} | output])
    end
  end

  defp units(text, offset, records, policy) do
    case text do
      <<codepoint::utf8, rest::binary>> ->
        {raw, wide?} = UnicodeTextData.line_break(codepoint)
        properties = UnicodeTextData.properties(codepoint)
        category = elem(properties, 2)

        class =
          case raw do
            value when value in [:AI, :SG, :XX] -> :AL
            :SA -> if category in [:Mn, :Mc], do: :CM, else: :AL
            :CJ -> :NS
            value -> value
          end

        mode = Map.get(policy, :line_break, :normal)
        cjk? = Map.get(policy, :language, "und") in ["ja", "zh"]
        script = elem(properties, 0)

        class =
          cond do
            Map.get(policy, :word_break) == :break_all and class in [:AL, :HL, :NU] ->
              :ID

            Map.get(policy, :word_break) == :keep_all and
              script in ~w(Hani Hira Kana Hang Bopo Yiii) and category in [:Lo, :Lm, :Nl] ->
              :AL

            mode in [:normal, :loose] and cjk? and codepoint in [0x301C, 0x30A0] ->
              :ID

            mode == :loose and
                (raw == :CJ or codepoint in [0x3005, 0x303B, 0x309D, 0x309E, 0x30FD, 0x30FE]) ->
              :ID

            mode == :loose and cjk? and
                codepoint in [
                  0x30FB,
                  0xFF1A,
                  0xFF1B,
                  0xFF65,
                  0x203C,
                  0x2047,
                  0x2048,
                  0x2049,
                  0xFF01,
                  0xFF1F
                ] ->
              :ID

            true ->
              class
          end

        record = %{
          codepoint: codepoint,
          class: class,
          category: category,
          wide?: wide?,
          joiner?: raw == :ZWJ,
          start: offset
        }

        record =
          Map.merge(record, %{
            raw: raw,
            policy: policy,
            relaxed_currency?:
              mode == :loose and cjk? and raw in [:PR, :PO] and
                (wide? or UnicodeTextData.ambiguous_width?(codepoint))
          })

        records =
          case {class, records} do
            {combining, [previous | tail]} when combining in [:CM, :ZWJ] ->
              if previous.class in (@hard ++ [:SP, :ZW]) do
                [
                  Map.merge(record, %{class: :AL, category: :Lu, wide?: false, codepoint: ?A})
                  | records
                ]
              else
                [%{previous | joiner?: raw == :ZWJ} | tail]
              end

            {combining, []} when combining in [:CM, :ZWJ] ->
              [Map.merge(record, %{class: :AL, category: :Lu, wide?: false, codepoint: ?A})]

            _ ->
              [record | records]
          end

        units(rest, offset + byte_size(<<codepoint::utf8>>), records, policy)

      "" ->
        records
    end
  end

  defp at(records, index) do
    cond do
      index < 0 -> @edge
      index >= tuple_size(records) -> %{@edge | class: :eot}
      true -> elem(records, index)
    end
  end

  defp boundary(records, index) do
    left = at(records, index - 1)
    right = at(records, index)
    before = at(records, index - 2)
    after_right = at(records, index + 1)
    nonspace_index = if left.class == :SP, do: left.nonspace, else: index - 1
    nonspace = at(records, nonspace_index)
    before_nonspace = at(records, nonspace_index - 1)
    brahmic_left? = left.class in [:AK, :AS] or left.codepoint == 0x25CC
    brahmic_right? = right.class in [:AK, :AS] or right.codepoint == 0x25CC

    cond do
      # LB4–LB8a. Mandatory breaks precede every tailorable rule.
      left.class == :BK ->
        :mandatory

      left.class == :CR and right.class == :LF ->
        :none

      left.class in [:CR, :LF, :NL] ->
        :mandatory

      right.class in (@hard ++ [:SP, :ZW]) ->
        :none

      nonspace.class == :ZW ->
        :allowed

      left.joiner? ->
        :none

      # LB9/10 were resolved by the unit builder; their absorbed scalars have
      # no internal boundary and do not duplicate RI or numeric context.
      left.class == :WJ or right.class == :WJ ->
        :none

      left.class == :GL ->
        :none

      right.class == :GL and left.class not in [:SP, :BA, :HY, :HH] ->
        :none

      right.class in [:CL, :CP, :EX, :SY] ->
        :none

      nonspace.class == :OP ->
        :none

      # LB15a–LB17. Initial/final quotes depend on general category and context.
      nonspace.class == :QU and nonspace.category == :Pi and
          before_nonspace.class in [:sot, :BK, :CR, :LF, :NL, :OP, :QU, :GL, :SP, :ZW] ->
        :none

      right.class == :QU and right.category == :Pf and
          after_right.class in [
            :SP,
            :GL,
            :WJ,
            :CL,
            :QU,
            :CP,
            :EX,
            :IS,
            :SY,
            :BK,
            :CR,
            :LF,
            :NL,
            :ZW,
            :eot
          ] ->
        :none

      left.class == :SP and right.class == :IS and after_right.class == :NU ->
        :allowed

      right.class == :IS ->
        :none

      nonspace.class in [:CL, :CP] and right.class == :NS ->
        :none

      nonspace.class == :B2 and right.class == :B2 ->
        :none

      left.class == :SP ->
        :allowed

      # LB19/19a: unresolved quotes are open only between East Asian scalars.
      right.class == :QU and right.category != :Pi ->
        :none

      left.class == :QU and left.category != :Pf ->
        :none

      right.class == :QU and (not left.wide? or not after_right.wide?) ->
        :none

      left.class == :QU and (not right.wide? or not before.wide?) ->
        :none

      left.class == :CB or right.class == :CB ->
        :allowed

      left.class in [:HY, :HH] and right.class in @letters and
          before.class in [:sot, :BK, :CR, :LF, :NL, :SP, :ZW, :CB, :GL] ->
        :none

      left.class == :BB ->
        :none

      css_boundary(left, right) == :allowed ->
        :allowed

      right.class in [:BA, :HH, :HY, :NS] ->
        :none

      before.class == :HL and left.class in [:HY, :HH] and right.class != :HL ->
        :none

      left.class == :SY and right.class == :HL ->
        :none

      right.class == :IN ->
        :none

      # LB23–LB25. Numeric context is accumulated once rather than rescanned.
      left.class in @letters and right.class == :NU ->
        :none

      left.class == :NU and right.class in @letters ->
        :none

      left.class == :PR and right.class in [:ID, :EB, :EM] ->
        :none

      left.class in [:ID, :EB, :EM] and right.class == :PO ->
        :none

      left.class in [:PR, :PO] and right.class in @letters ->
        :none

      left.class in @letters and right.class in [:PR, :PO] ->
        :none

      right.class in [:PO, :PR] and
          (left.numeric? or (left.class in [:CL, :CP] and before.numeric?)) ->
        :none

      left.class in [:PO, :PR] and right.class == :OP and
          (after_right.class == :NU or
             (after_right.class == :IS and at(records, index + 2).class == :NU)) ->
        :none

      left.class in [:PO, :PR, :HY, :IS] and right.class == :NU ->
        :none

      left.numeric? and right.class == :NU ->
        :none

      # LB26–LB30b. Syllables, ordinary words, parentheses and emoji.
      left.class == :JL and right.class in [:JL, :JV, :H2, :H3] ->
        :none

      left.class in [:JV, :H2] and right.class in [:JV, :JT] ->
        :none

      left.class in [:JT, :H3] and right.class == :JT ->
        :none

      left.class in @hangul and right.class == :PO ->
        :none

      left.class == :PR and right.class in @hangul ->
        :none

      left.class in @letters and right.class in @letters ->
        :none

      left.class == :AP and brahmic_right? ->
        :none

      brahmic_left? and right.class in [:VF, :VI] ->
        :none

      left.class == :VI and (before.class in [:AK, :AS] or before.codepoint == 0x25CC) and
          (right.class == :AK or right.codepoint == 0x25CC) ->
        :none

      brahmic_left? and brahmic_right? and after_right.class == :VF ->
        :none

      left.class == :IS and right.class in @letters ->
        :none

      left.class in [:AL, :HL, :NU] and right.class == :OP and not right.wide? ->
        :none

      left.class == :CP and not left.wide? and right.class in [:AL, :HL, :NU] ->
        :none

      left.class == :RI and right.class == :RI and rem(left.regional, 2) == 1 ->
        :none

      right.class == :EM and
          (left.class == :EB or
             (left.category == :Cn and Regex.match?(@pictographic, <<left.codepoint::utf8>>))) ->
        :none

      true ->
        :allowed
    end
  end

  defp css_boundary(left, right) do
    if Map.get(left.policy, :line_break) == :loose do
      cond do
        left.class == :ID and right.codepoint in [0x2010, 0x2013] -> :allowed
        left.class == :IN and right.class == :IN -> :allowed
        left.class == :PR and left.relaxed_currency? -> :allowed
        right.class == :PO and right.relaxed_currency? -> :allowed
        true -> nil
      end
    end
  end
end
