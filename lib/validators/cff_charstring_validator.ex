defmodule NativeElixirPdfUtilities.Validators.CffCharstringValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.CffData
  alias NativeElixirPdfUtilities.Validators.{CffFontValidator, FontValidator}

  @doc "Executes every owned CFF 1 Type 2 glyph under the font work budget, retaining static outlines, advances and exact cubic bounds."
  @spec prepare(term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(data, glyph_count) do
    FontValidator.with_budget(fn ->
      try do
        with {:ok, face} <- CffFontValidator.prepare(data, glyph_count) do
          # Charstring/subroutine lengths, operand/hint stacks and subroutine
          # nesting below are fixed Type 2 protocol bounds, not tuning options.
          Enum.each(
            [face.global_subrs | Enum.map(Tuple.to_list(face.fonts), & &1.local_subrs)],
            fn programs ->
              Enum.each(Tuple.to_list(programs), fn program ->
                unless byte_size(program) <= 65_535,
                  do:
                    invalid(
                      "Type 2 subroutine exceeds its fixed 65,535-byte program size",
                      face.name
                    )
              end)
            end
          )

          identifiers =
            Map.new(Enum.with_index([0 | face.charset]), fn {sid, glyph} -> {sid, glyph} end)

          glyphs =
            for glyph <- 0..(glyph_count - 1) do
              program = elem(face.charstrings, glyph)

              unless byte_size(program) <= 65_535,
                do: invalid("Type 2 glyph exceeds its fixed 65,535-byte program size", face.name)

              font = elem(face.fonts, elem(face.fd_select, glyph))

              state = %{
                x: 0,
                y: 0,
                stack: [],
                width: nil,
                hints: 0,
                transient: %{},
                commands: [],
                open?: false,
                bbox: nil,
                source: "#{face.name} glyph #{glyph}",
                seac: nil,
                cid?: face.cid?,
                identifiers: identifiers
              }

              state = execute(program, [], state, font, face.global_subrs)

              %{
                commands: Enum.reverse(state.commands),
                width: state.width,
                bbox: state.bbox || {0, 0, 0, 0},
                seac: state.seac,
                component_depth: 1
              }
            end
            |> List.to_tuple()

          {_cache, resolved} =
            Enum.map_reduce(0..(glyph_count - 1), %{}, fn glyph, cache ->
              {value, cache} =
                resolve_components(glyph, glyphs, cache, 0, face.name)

              {value, cache}
            end)
            |> then(fn {values, cache} -> {cache, List.to_tuple(values)} end)

          {:ok, Map.put(face, :glyphs, resolved)}
        end
      rescue
        ArithmeticError ->
          failure("Type 2 arithmetic exceeds a finite representable result", "CFF charstring")
      catch
        {:cff_charstring_invalid, error} -> error
      end
    end)
  end

  @doc "Converts decoded CFF paths to OpenType design-unit bounds using their owning FontMatrix."
  @spec prepare_metrics(term(), term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_metrics(data, glyph_count, units_per_em) do
    FontValidator.with_budget(fn ->
      try do
        unless is_integer(units_per_em) and units_per_em in 16..16_384,
          do: invalid("CFF metric integration requires valid OpenType unitsPerEm", "CFF metrics")

        with {:ok, face} <- prepare(data, glyph_count) do
          bounds =
            face.glyphs
            |> Tuple.to_list()
            |> Enum.with_index()
            |> Enum.map(fn {glyph, index} ->
              [a, b, c, d, e, f] = face.font_matrix
              font = elem(face.fonts, elem(face.fd_select, index))

              [g, h, i, j, k, l] =
                if face.cid?, do: font.font_matrix || [1, 0, 0, 1, 0, 0], else: [1, 0, 0, 1, 0, 0]

              transform = fn x, y ->
                {units_per_em * (a * (g * x + i * y + k) + c * (h * x + j * y + l) + e),
                 units_per_em * (b * (g * x + i * y + k) + d * (h * x + j * y + l) + f)}
              end

              FontValidator.reserve(:max_font_cff_work, length(glyph.commands))

              commands =
                Enum.map(glyph.commands, fn command ->
                  case command do
                    :close ->
                      :close

                    {kind, x, y} ->
                      {x, y} = transform.(x, y)
                      {kind, x, y}

                    {:curve, x1, y1, x2, y2, x3, y3} ->
                      {x1, y1} = transform.(x1, y1)
                      {x2, y2} = transform.(x2, y2)
                      {x3, y3} = transform.(x3, y3)
                      {:curve, x1, y1, x2, y2, x3, y3}
                  end
                end)

              command_bounds(commands, face.name)
            end)

          {:ok, Map.put(face, :glyph_bounds, List.to_tuple(bounds))}
        end
      rescue
        ArithmeticError ->
          failure("CFF FontMatrix exceeds a finite representable result", "CFF metrics")
      catch
        {:cff_charstring_invalid, error} -> error
      end
    end)
  end

  defp execute(program, frames, state, font, globals) do
    FontValidator.reserve(:max_font_cff_work, 1)

    case program do
      <<>> ->
        invalid("Type 2 program ends without endchar or return", state.source)

      <<value, rest::binary>> when value in 32..246 ->
        execute(rest, frames, push(state, value - 139), font, globals)

      <<value, next, rest::binary>> when value in 247..250 ->
        execute(rest, frames, push(state, (value - 247) * 256 + next + 108), font, globals)

      <<value, next, rest::binary>> when value in 251..254 ->
        execute(rest, frames, push(state, -(value - 251) * 256 - next - 108), font, globals)

      <<28, value::signed-16, rest::binary>> ->
        execute(rest, frames, push(state, value), font, globals)

      <<255, value::signed-32, rest::binary>> ->
        execute(rest, frames, push(state, value / 65_536), font, globals)

      <<operator, rest::binary>> when operator in [10, 29] ->
        {index, state} = pop(state)
        index = whole(index, state.source)
        subrs = if operator == 10, do: font.local_subrs, else: globals
        count = tuple_size(subrs)

        bias =
          cond do
            count < 1240 -> 107
            count < 33900 -> 1131
            true -> 32768
          end

        index = index + bias

        unless index >= 0 and index < count,
          do: invalid("Type 2 subroutine index is outside its owning INDEX", state.source)

        unless length(frames) < 10,
          do: invalid("Type 2 exceeds its fixed ten-level subroutine stack", state.source)

        execute(elem(subrs, index), [rest | frames], state, font, globals)

      <<11, _rest::binary>> ->
        case frames do
          [next | frames] -> execute(next, frames, state, font, globals)
          [] -> invalid("Type 2 return has no calling subroutine", state.source)
        end

      <<14, _rest::binary>> ->
        {args, state} = arguments(state, font, :endchar)
        state = close_path(state)

        case args do
          [] ->
            state

          [dx, dy, base, accent] ->
            if state.cid?,
              do:
                invalid(
                  "CID-keyed Type 2 glyphs cannot use deprecated seac composition",
                  state.source
                )

            base = component_glyph(base, state)
            accent = component_glyph(accent, state)
            %{state | seac: {base, accent, dx, dy}}

          _ ->
            invalid(
              "Type 2 endchar requires no operands or its four legacy composition operands",
              state.source
            )
        end

      <<operator, rest::binary>> when operator in [1, 3, 18, 23, 19, 20] ->
        {args, state} = arguments(state, font, :stems)

        unless rem(length(args), 2) == 0 and (operator in [19, 20] or length(args) >= 2),
          do: invalid("Type 2 stem hints require complete operand pairs", state.source)

        hints = state.hints + div(length(args), 2)
        unless hints <= 96, do: invalid("Type 2 exceeds its fixed 96-hint stack", state.source)
        state = %{state | hints: hints}

        rest =
          if operator in [19, 20] do
            bytes = div(hints + 7, 8)

            unless byte_size(rest) >= bytes,
              do: invalid("Type 2 hint mask is truncated", state.source)

            FontValidator.reserve(:max_font_cff_work, bytes)
            binary_part(rest, bytes, byte_size(rest) - bytes)
          else
            rest
          end

        execute(rest, frames, state, font, globals)

      <<operator, rest::binary>> when operator in [4, 21, 22] ->
        count = if operator == 21, do: 2, else: 1
        {args, state} = arguments(state, font, {:move, count})

        unless length(args) == count,
          do: invalid("Type 2 moveto has an invalid operand count", state.source)

        {dx, dy} =
          case {operator, args} do
            {4, [dy]} -> {0, dy}
            {22, [dx]} -> {dx, 0}
            {21, [dx, dy]} -> {dx, dy}
          end

        state = close_path(state)
        x = state.x + dx
        y = state.y + dy
        FontValidator.check(:max_font_outline_coordinate, ceil(max(abs(x), abs(y))))
        state = %{state | open?: true, x: x, y: y}
        state = %{state | commands: [{:move, x, y} | state.commands]}
        execute(rest, frames, state, font, globals)

      <<operator, rest::binary>> when operator in [5, 6, 7, 8, 24, 25, 26, 27, 30, 31] ->
        unless state.open?,
          do: invalid("Type 2 path segment has no preceding moveto", state.source)

        args = Enum.reverse(state.stack)
        state = path(operator, args, %{state | stack: []})
        execute(rest, frames, state, font, globals)

      <<12, operator, rest::binary>> ->
        state = escaped(operator, state)
        execute(rest, frames, state, font, globals)

      _ ->
        invalid("Type 2 program contains a reserved or truncated operator/operand", state.source)
    end
  end

  defp arguments(state, font, kind) do
    args = Enum.reverse(state.stack)

    if is_nil(state.width) do
      width? =
        case kind do
          :stems -> rem(length(args), 2) == 1
          :endchar -> length(args) in [1, 5]
          {:move, count} -> length(args) == count + 1
        end

      {width, args} =
        if width?, do: {font.nominal_width + hd(args), tl(args)}, else: {font.default_width, args}

      FontValidator.check(:max_font_numeric_magnitude, ceil(abs(width)))
      {args, %{state | stack: [], width: width}}
    else
      {args, %{state | stack: []}}
    end
  end

  defp path(operator, args, state) do
    count = length(args)

    case operator do
      5 ->
        unless count >= 2 and rem(count, 2) == 0,
          do: invalid("Type 2 rlineto requires coordinate pairs", state.source)

        Enum.reduce(Enum.chunk_every(args, 2), state, fn [dx, dy], state ->
          line(state, dx, dy)
        end)

      value when value in [6, 7] ->
        unless count > 0, do: invalid("Type 2 axis lineto requires operands", state.source)

        {state, _} =
          Enum.reduce(args, {state, operator == 6}, fn delta, {state, horizontal?} ->
            state = if horizontal?, do: line(state, delta, 0), else: line(state, 0, delta)
            {state, not horizontal?}
          end)

        state

      8 ->
        unless count >= 6 and rem(count, 6) == 0,
          do: invalid("Type 2 rrcurveto requires six operands per curve", state.source)

        Enum.reduce(Enum.chunk_every(args, 6), state, &curve(&2, &1))

      24 ->
        unless count >= 8 and rem(count - 2, 6) == 0,
          do: invalid("Type 2 rcurveline requires curves followed by a line", state.source)

        {curves, [dx, dy]} = Enum.split(args, count - 2)
        state = Enum.reduce(Enum.chunk_every(curves, 6), state, &curve(&2, &1))
        line(state, dx, dy)

      25 ->
        unless count >= 8 and rem(count - 6, 2) == 0,
          do: invalid("Type 2 rlinecurve requires lines followed by a curve", state.source)

        {lines, curve} = Enum.split(args, count - 6)

        state =
          Enum.reduce(Enum.chunk_every(lines, 2), state, fn [dx, dy], state ->
            line(state, dx, dy)
          end)

        curve(state, curve)

      value when value in [26, 27] ->
        unless count >= 4 and rem(count, 4) in [0, 1],
          do:
            invalid(
              "Type 2 aligned curves require four operands per curve and an optional first displacement",
              state.source
            )

        {extra, args} = if rem(count, 4) == 1, do: {hd(args), tl(args)}, else: {0, args}

        {state, _} =
          Enum.reduce(Enum.chunk_every(args, 4), {state, extra}, fn [a, b, c, d],
                                                                    {state, extra} ->
            deltas = if operator == 27, do: [a, extra, b, c, d, 0], else: [extra, a, b, c, 0, d]
            {curve(state, deltas), 0}
          end)

        state

      value when value in [30, 31] ->
        unless count >= 4 and rem(count, 4) in [0, 1],
          do: invalid("Type 2 alternating curves have an invalid operand count", state.source)

        alternating_curves(args, state, operator == 31)
    end
  end

  defp alternating_curves(args, state, horizontal?) do
    case args do
      [] ->
        state

      [a, b, c, d, extra] ->
        deltas = if horizontal?, do: [a, 0, b, c, extra, d], else: [0, a, b, c, d, extra]
        curve(state, deltas)

      [a, b, c, d | rest] ->
        deltas = if horizontal?, do: [a, 0, b, c, 0, d], else: [0, a, b, c, d, 0]
        alternating_curves(rest, curve(state, deltas), not horizontal?)
    end
  end

  defp escaped(operator, state) do
    case operator do
      0 ->
        state

      value when value in [3, 4, 10, 11, 12, 15, 24] ->
        {right, state} = pop(state)
        {left, state} = pop(state)

        result =
          case operator do
            3 ->
              if left != 0 and right != 0, do: 1, else: 0

            4 ->
              if left != 0 or right != 0, do: 1, else: 0

            10 ->
              left + right

            11 ->
              left - right

            12 ->
              if right == 0, do: invalid("Type 2 division has a zero divisor", state.source)
              left / right

            15 ->
              if left == right, do: 1, else: 0

            24 ->
              left * right
          end

        push(state, result)

      value when value in [5, 9, 14, 26] ->
        {value, state} = pop(state)

        result =
          case operator do
            5 ->
              if value == 0, do: 1, else: 0

            9 ->
              abs(value)

            14 ->
              -value

            26 ->
              if value < 0,
                do: invalid("Type 2 square root requires a nonnegative operand", state.source)

              :math.sqrt(value)
          end

        push(state, result)

      18 ->
        {_discard, state} = pop(state)
        state

      20 ->
        {index, state} = pop(state)
        {value, state} = pop(state)
        index = transient_index(index, state.source)
        %{state | transient: Map.put(state.transient, index, value)}

      21 ->
        {index, state} = pop(state)
        index = transient_index(index, state.source)

        case Map.fetch(state.transient, index) do
          {:ok, value} -> push(state, value)
          :error -> invalid("Type 2 get reads an uninitialized transient value", state.source)
        end

      22 ->
        {v2, state} = pop(state)
        {v1, state} = pop(state)
        {second, state} = pop(state)
        {first, state} = pop(state)
        push(state, if(v1 <= v2, do: first, else: second))

      23 ->
        throw(
          {:cff_charstring_invalid,
           Diagnostics.error(
             :font,
             :unsupported_font,
             "Type 2 random programs cannot provide stable static outline bounds",
             module: __MODULE__,
             operation: :prepare_cff_outlines,
             source: state.source
           )}
        )

      27 ->
        {value, after_pop} = pop(state)
        push(after_pop, value) |> push(value)

      28 ->
        {first, state} = pop(state)
        {second, state} = pop(state)
        push(state, first) |> push(second)

      29 ->
        {index, state} = pop(state)
        index = whole(index, state.source)

        if state.stack == [],
          do: invalid("Type 2 index requires a remaining stack operand", state.source)

        index = max(0, index)

        unless index < length(state.stack),
          do: invalid("Type 2 index exceeds its remaining operand stack", state.source)

        push(state, Enum.at(state.stack, index))

      30 ->
        {shift, state} = pop(state)
        {count, state} = pop(state)
        shift = whole(shift, state.source)
        count = whole(count, state.source)

        unless count >= 0 and count <= length(state.stack),
          do: invalid("Type 2 roll count exceeds its operand stack", state.source)

        if count == 0 do
          state
        else
          {selected, rest} = Enum.split(state.stack, count)
          {first, last} = Enum.split(selected, Integer.mod(shift, count))
          %{state | stack: last ++ first ++ rest}
        end

      value when value in [34, 35, 36, 37] ->
        unless state.open?, do: invalid("Type 2 flex requires a preceding moveto", state.source)
        args = Enum.reverse(state.stack)
        state = %{state | stack: []}

        deltas =
          case {operator, args} do
            {34, [a, b, c, d, e, f, g]} ->
              [a, 0, b, c, d, 0, e, 0, f, -c, g, 0]

            {35, [a, b, c, d, e, f, g, h, i, j, k, l, _depth]} ->
              [a, b, c, d, e, f, g, h, i, j, k, l]

            {36, [a, b, c, d, e, f, g, h, i]} ->
              [a, b, c, d, e, 0, f, 0, g, h, i, -(b + d + h)]

            {37, [a, b, c, d, e, f, g, h, i, j, last]} ->
              dx = a + c + e + g + i
              dy = b + d + f + h + j

              if abs(dx) > abs(dy),
                do: [a, b, c, d, e, f, g, h, i, j, last, -dy],
                else: [a, b, c, d, e, f, g, h, i, j, -dx, last]

            _ ->
              invalid("Type 2 flex has an invalid operand count", state.source)
          end

        [first, second] = Enum.chunk_every(deltas, 6)
        curve(curve(state, first), second)

      _ ->
        invalid("Type 2 escape operator is reserved or unsupported", state.source)
    end
  end

  defp push(state, value) do
    FontValidator.check(:max_font_numeric_magnitude, ceil(abs(value)))

    unless length(state.stack) < 48,
      do: invalid("Type 2 exceeds its fixed 48-operand stack", state.source)

    %{state | stack: [value | state.stack]}
  end

  defp pop(state) do
    case state.stack do
      [value | rest] -> {value, %{state | stack: rest}}
      [] -> invalid("Type 2 operand stack underflow", state.source)
    end
  end

  defp whole(value, source) do
    unless is_number(value) and value == trunc(value),
      do: invalid("Type 2 stack index requires an integer value", source)

    trunc(value)
  end

  defp transient_index(value, source) do
    index = whole(value, source)

    unless index in 0..31,
      do: invalid("Type 2 transient index exceeds its fixed 32-element array", source)

    index
  end

  defp close_path(state) do
    if state.open?, do: %{state | open?: false, commands: [:close | state.commands]}, else: state
  end

  defp point(state, x, y) do
    FontValidator.check(:max_font_outline_coordinate, ceil(max(abs(x), abs(y))))

    bbox =
      case state.bbox do
        nil -> {x, y, x, y}
        {a, b, c, d} -> {min(a, x), min(b, y), max(c, x), max(d, y)}
      end

    %{state | x: x, y: y, bbox: bbox}
  end

  defp line(state, dx, dy) do
    state = point(state, state.x, state.y)
    state = point(state, state.x + dx, state.y + dy)
    %{state | commands: [{:line, state.x, state.y} | state.commands]}
  end

  defp curve(state, [dx1, dy1, dx2, dy2, dx3, dy3]) do
    state = point(state, state.x, state.y)
    x0 = state.x
    y0 = state.y
    x1 = x0 + dx1
    y1 = y0 + dy1
    x2 = x1 + dx2
    y2 = y1 + dy2
    x3 = x2 + dx3
    y3 = y2 + dy3

    FontValidator.check(
      :max_font_outline_coordinate,
      ceil(Enum.max(Enum.map([x1, y1, x2, y2, x3, y3], &abs/1)))
    )

    times = Enum.uniq(extrema(x0, x1, x2, x3) ++ extrema(y0, y1, y2, y3))
    state = point(state, x3, y3)

    state =
      Enum.reduce(times, state, fn t, state ->
        point(state, cubic(x0, x1, x2, x3, t), cubic(y0, y1, y2, y3, t))
      end)

    %{state | x: x3, y: y3, commands: [{:curve, x1, y1, x2, y2, x3, y3} | state.commands]}
  end

  defp extrema(a, b, c, d) do
    quadratic = -a + 3 * b - 3 * c + d
    linear = 2 * (a - 2 * b + c)
    constant = b - a

    ratios =
      cond do
        quadratic == 0 and linear == 0 ->
          []

        quadratic == 0 ->
          [{-constant, linear}]

        true ->
          discriminant = linear * linear - 4 * quadratic * constant

          if discriminant < 0 do
            []
          else
            # The usual quadratic formula loses the small root when the
            # cubic is nearly quadratic. Use its product-of-roots identity.
            signed_root =
              if linear < 0, do: -:math.sqrt(discriminant), else: :math.sqrt(discriminant)

            q = -0.5 * (linear + signed_root)
            [{q, quadratic}, {constant, q}]
          end
      end

    Enum.flat_map(ratios, fn {numerator, denominator} ->
      # Divide only roots inside the curve interval. A tiny denominator can
      # otherwise overflow for an irrelevant root far outside the segment.
      if (denominator > 0 and numerator > 0 and numerator < denominator) or
           (denominator < 0 and numerator < 0 and numerator > denominator),
         do: [numerator / denominator],
         else: []
    end)
  end

  defp cubic(a, b, c, d, t) do
    u = 1 - t
    u * u * u * a + 3 * u * u * t * b + 3 * u * t * t * c + t * t * t * d
  end

  defp component_glyph(code, state) do
    code = whole(code, state.source)

    unless code in 0..255,
      do: invalid("Type 2 seac code is outside StandardEncoding", state.source)

    sid = elem(CffData.standard_encoding(), code)

    case Map.fetch(state.identifiers, sid) do
      {:ok, glyph} -> glyph
      :error -> invalid("Type 2 seac refers to a missing StandardEncoding glyph", state.source)
    end
  end

  defp resolve_components(glyph, glyphs, cache, depth, source) do
    FontValidator.check(:max_font_glyph_depth, depth + 1)
    FontValidator.reserve(:max_font_cff_work, 1)

    case Map.fetch(cache, glyph) do
      {:ok, value} ->
        FontValidator.check(:max_font_glyph_depth, depth + value.component_depth)
        {value, cache}

      :error ->
        value = elem(glyphs, glyph)

        {value, cache} =
          case value.seac do
            nil ->
              {value, cache}

            {base, accent, dx, dy} ->
              # Adobe 5177 Appendix C forbids nested seac composition.
              if elem(glyphs, base).seac != nil or elem(glyphs, accent).seac != nil,
                do: invalid("Type 2 seac components cannot themselves use seac", source)

              {base, cache} = resolve_components(base, glyphs, cache, depth + 1, source)

              {accent, cache} =
                resolve_components(accent, glyphs, cache, depth + 1, source)

              FontValidator.reserve(
                :max_font_cff_work,
                length(base.commands) + length(accent.commands)
              )

              commands =
                value.commands ++
                  base.commands ++
                  Enum.map(accent.commands, fn command ->
                    case command do
                      :close ->
                        :close

                      {:move, x, y} ->
                        {:move, x + dx, y + dy}

                      {:line, x, y} ->
                        {:line, x + dx, y + dy}

                      {:curve, a, b, c, d, e, f} ->
                        {:curve, a + dx, b + dy, c + dx, d + dy, e + dx, f + dy}
                    end
                  end)

              bbox = command_bounds(commands, source)

              {%{
                 value
                 | commands: commands,
                   bbox: bbox,
                   component_depth: 1 + max(base.component_depth, accent.component_depth)
               }, cache}
          end

        FontValidator.check(:max_font_glyph_depth, depth + value.component_depth)
        {value, Map.put(cache, glyph, value)}
    end
  end

  defp command_bounds(commands, source) do
    Enum.reduce(commands, %{x: 0, y: 0, bbox: nil, commands: [], source: source}, fn command,
                                                                                     state ->
      case command do
        :close ->
          state

        {:move, x, y} ->
          FontValidator.check(:max_font_outline_coordinate, ceil(max(abs(x), abs(y))))
          %{state | x: x, y: y}

        {:line, x, y} ->
          line(state, x - state.x, y - state.y)

        {:curve, a, b, c, d, e, f} ->
          curve(state, [a - state.x, b - state.y, c - a, d - b, e - c, f - d])
      end
    end).bbox || {0, 0, 0, 0}
  end

  defp invalid(message, source), do: throw({:cff_charstring_invalid, failure(message, source)})

  defp failure(message, source) do
    Diagnostics.error(:font, :invalid_font, message,
      module: __MODULE__,
      operation: :prepare_cff_outlines,
      source: source
    )
  end
end
