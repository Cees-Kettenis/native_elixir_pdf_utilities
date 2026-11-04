defmodule NativeElixirPdfUtilities.CodeStyleTest do
  use ExUnit.Case, async: true

  @source_globs ["lib/**/*.{ex,exs}", "test/**/*.{ex,exs}", "scripts/**/*.{ex,exs}"]

  test "function branching is expressed inside a single definition" do
    violations =
      @source_globs
      |> Enum.flat_map(&Path.wildcard/1)
      |> Enum.flat_map(fn path ->
        ast = path |> File.read!() |> Code.string_to_quoted!(file: path)

        {_ast, definitions} =
          Macro.prewalk(ast, [], fn
            {kind, metadata, [head, _body]} = node, definitions when kind in [:def, :defp] ->
              {guarded?, function_head} =
                case head do
                  {:when, _, [function_head | _guards]} -> {true, function_head}
                  function_head -> {false, function_head}
                end

              case function_head do
                {name, _, arguments} when is_atom(name) and is_list(arguments) ->
                  definition = {kind, name, length(arguments), metadata[:line], guarded?}
                  {node, [definition | definitions]}

                {name, _, nil} when is_atom(name) ->
                  definition = {kind, name, 0, metadata[:line], guarded?}
                  {node, [definition | definitions]}

                _ ->
                  {node, definitions}
              end

            node, definitions ->
              {node, definitions}
          end)

        definitions
        |> Enum.group_by(fn {kind, name, arity, _line, _guarded?} -> {kind, name, arity} end)
        |> Enum.flat_map(fn {{kind, name, arity}, clauses} ->
          guarded? = Enum.any?(clauses, &elem(&1, 4))

          case length(clauses) > 1 or guarded? do
            true ->
              lines = clauses |> Enum.map(&elem(&1, 3)) |> Enum.sort()
              [{path, kind, name, arity, lines, guarded?}]

            false ->
              []
          end
        end)
      end)

    assert violations == [], """
    Keep branching inside one function body using case, cond, or with.
    Guarded function definitions and repeated name/arity clauses found:
    #{inspect(violations, pretty: true)}
    """
  end
end
