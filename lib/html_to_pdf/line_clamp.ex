defmodule NativeElixirPdfUtilities.HtmlToPdf.LineClamp do
  @moduledoc "Counts shaped lines across one block formatting context and selects its final line paint."
  alias NativeElixirPdfUtilities.Validators.CssDisplayValidator
  @key {__MODULE__, :scope}

  @doc "Shares a discard budget with in-flow block descendants, separating independent formatting contexts."
  @spec layout(map(), (-> term())) :: term()
  @spec layout(map(), (-> term()), map() | nil) :: term()
  def layout(style, operation, region \\ nil) do
    if Process.get(@key) != nil and CssDisplayValidator.independent?(style),
      do: separate(fn -> discard_layout(style, operation, region) end),
      else: discard_layout(style, operation, region)
  end

  defp discard_layout(style, operation, region) do
    maximum = Map.get(style, :max_lines, :none)

    cond do
      Map.get(style, :continue, :auto) == :discard and (is_integer(maximum) or region != nil) ->
        previous = Process.get(@key, [])

        scope = %{
          id: make_ref(),
          remaining: maximum,
          more: false,
          candidate: nil,
          bottom: if(region, do: region.top - region.height),
          seen: false
        }

        Process.put(@key, [scope | previous])

        try do
          result = operation.()
          [scope | _] = Process.get(@key)

          case result do
            {:ok, boxes, height} ->
              boxes =
                Enum.flat_map(boxes, fn box ->
                  case Map.get(box, :_line_clamp_choice) do
                    {ids, alternative?} ->
                      wanted? = {scope.id, scope.candidate} in ids and scope.more
                      remaining = Enum.reject(ids, &(elem(&1, 0) == scope.id))

                      cond do
                        wanted? and alternative? ->
                          [Map.delete(box, :_line_clamp_choice)]

                        wanted? ->
                          []

                        remaining != [] ->
                          [Map.put(box, :_line_clamp_choice, {remaining, alternative?})]

                        alternative? ->
                          []

                        true ->
                          [Map.delete(box, :_line_clamp_choice)]
                      end

                    _ ->
                      [box]
                  end
                end)

              {:ok, boxes, height}

            other ->
              other
          end
        after
          [_ | tail] = Process.get(@key)
          if tail == [], do: Process.delete(@key), else: Process.put(@key, tail)
        end

      true ->
        operation.()
    end
  end

  @doc "Separates line counting while retaining an enclosing region's physical break boundary."
  @spec separate((-> term())) :: term()
  def separate(operation) do
    previous = Process.get(@key, [])
    regions = for scope <- previous, scope.bottom != nil, do: %{scope | remaining: :none}

    if regions == [] do
      suspend(operation)
    else
      Process.put(@key, regions)

      try do
        operation.()
      after
        updated = Map.new(Process.get(@key), &{&1.id, &1})

        restored =
          Enum.map(previous, fn scope ->
            region = Map.fetch!(updated, scope.id)
            %{region | remaining: if(region.remaining == 0, do: 0, else: scope.remaining)}
          end)

        Process.put(@key, restored)
      end
    end
  end

  @doc "Keeps intrinsic measurement and independent documents from consuming an enclosing line budget."
  @spec suspend((-> term())) :: term()
  def suspend(operation) do
    case Process.delete(@key) do
      nil ->
        operation.()

      previous ->
        try do
          operation.()
        after
          Process.put(@key, previous)
        end
    end
  end

  @doc "Selects complete shaped lines and identifies the paragraph whose last line may need an ellipsis."
  @spec take([list()], number(), (list() -> number())) :: {[list()], [{reference(), reference()}]}
  def take(lines, top, line_height) do
    case Process.get(@key) do
      nil ->
        {lines, []}

      scopes ->
        count = length(lines)

        allowed =
          Enum.map(scopes, fn scope ->
            maximum = if scope.remaining == :none, do: count, else: scope.remaining

            geometry =
              if scope.bottom == nil do
                count
              else
                {fit, _} =
                  Enum.reduce_while(lines, {0, top}, fn line, {fit, y} ->
                    bottom = y - line_height.(line)

                    if bottom >= scope.bottom or (not scope.seen and fit == 0),
                      do: {:cont, {fit + 1, bottom}},
                      else: {:halt, {fit, y}}
                  end)

                fit
              end

            min(maximum, geometry)
          end)

        used = Enum.min([count | allowed])
        candidate = make_ref()

        scopes =
          Enum.zip_with(scopes, allowed, fn scope, allowance ->
            limited? = allowance == used and used < count

            remaining =
              cond do
                limited? -> 0
                scope.remaining == :none -> :none
                true -> scope.remaining - used
              end

            candidate =
              if used > 0 and (scope.bottom != nil or remaining == 0),
                do: candidate,
                else: scope.candidate

            %{
              scope
              | remaining: remaining,
                more: scope.more or limited?,
                candidate: candidate,
                seen: scope.seen or used > 0
            }
          end)

        Process.put(@key, scopes)

        candidates =
          if used == 0,
            do: [],
            else: for(scope <- scopes, scope.candidate == candidate, do: {scope.id, candidate})

        {Enum.take(lines, used), candidates}
    end
  end

  @doc "Places an unbreakable replaced block at a discard boundary, allowing first-fragment progress."
  @spec place_atomic(number(), number()) :: boolean()
  def place_atomic(top, bottom) do
    case Process.get(@key) do
      nil ->
        true

      scopes ->
        fits? =
          Enum.all?(scopes, fn scope ->
            scope.bottom == nil or bottom >= scope.bottom or not scope.seen
          end)

        Process.put(
          @key,
          Enum.map(scopes, fn scope ->
            if fits?,
              do: %{scope | seen: scope.seen or top > bottom},
              else: %{scope | remaining: 0, more: true}
          end)
        )

        fits?
    end
  end

  @doc "Reports whether subsequent in-flow content is beyond the current discard boundary."
  @spec exhausted?() :: boolean()
  def exhausted?, do: Enum.any?(Process.get(@key, []), &(&1.remaining == 0))

  @doc "Records elided following flow without charging whitespace, source anchors or empty collapse-through boxes."
  @spec discard(map()) :: :ok
  def discard(child) do
    empty? =
      case child do
        %{style: %{display: :none}} ->
          true

        %{style: %{_collapses_through: true}} ->
          true

        %{style: %{_static_position_anchor: _}} ->
          true

        %{type: :inline_flow, children: children} ->
          Enum.all?(children, fn child ->
            case child do
              %{style: %{display: :none}} ->
                true

              %{type: :text, text: text, style: style} ->
                Map.get(style, :white_space) not in [:pre, :pre_wrap, :break_spaces] and
                  NativeElixirPdfUtilities.Validators.HtmlValidator.trim_ascii_whitespace(text) ==
                    ""

              _ ->
                false
            end
          end)

        %{type: :text, text: text} ->
          NativeElixirPdfUtilities.Validators.HtmlValidator.trim_ascii_whitespace(text) == ""

        _ ->
          false
      end

    if not empty? do
      Process.put(
        @key,
        Enum.map(Process.get(@key), fn scope ->
          if scope.remaining == 0, do: %{scope | more: true}, else: scope
        end)
      )
    end

    :ok
  end
end
