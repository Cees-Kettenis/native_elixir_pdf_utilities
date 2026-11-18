defmodule NativeElixirPdfUtilities.Validators.CssFunctionValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{
    CssPropertyRegistrationValidator,
    CssReferenceValidator,
    CssSyntaxValidator,
    HtmlValidator,
    PrintMediaValidator
  }

  @doc "Parses a declarative function or mixin prelude in the adopted editor draft."
  @spec definition(String.t(), :function | :mixin) :: {:ok, map()} | :error
  def definition(source, kind) do
    case CssSyntaxValidator.components(source) do
      [%{kind: :block, opener: opener, closed: true, text: text} = block | _] ->
        tail =
          binary_part(source, block.length, byte_size(source) - block.length) |> String.trim()

        arguments = binary_part(text, byte_size(opener), byte_size(text) - byte_size(opener) - 1)

        with {:ok, "--" <> suffix = name, "("} when suffix != "" <-
               HtmlValidator.parse_css_identifier(opener),
             {:ok, parameters} <- parameters(arguments),
             {:ok, type} <- return_type(tail, kind) do
          {:ok, %{name: name, parameters: parameters, type: type}}
        else
          _ -> :error
        end

      _ ->
        with :mixin <- kind,
             {:ok, "--" <> suffix = name, ""} when suffix != "" <-
               HtmlValidator.parse_css_identifier(source) do
          {:ok, %{name: name, parameters: [], type: :any}}
        else
          _ -> :error
        end
    end
  end

  defp parameters(source) do
    items = arguments(source)
    items = if List.last(items) == "", do: Enum.drop(items, -1), else: items

    Enum.reduce_while(items, {:ok, [], MapSet.new()}, fn item, {:ok, parameters, names} ->
      [header | default] = CssSyntaxValidator.split_trimmed(item, ":")
      default = if default == [], do: nil, else: Enum.join(default, ":")

      with {:ok, "--" <> suffix = name, type} when suffix != "" <-
             HtmlValidator.parse_css_identifier(header),
           false <- MapSet.member?(names, name),
           {:ok, type} <- type(String.trim(type)),
           true <-
             is_nil(default) or CssPropertyRegistrationValidator.matches_syntax?(type, default) do
        {:cont, {:ok, [{name, type, default} | parameters], MapSet.put(names, name)}}
      else
        _ -> {:halt, :error}
      end
    end)
    |> case do
      {:ok, parameters, _} -> {:ok, Enum.reverse(parameters)}
      :error -> :error
    end
  end

  defp return_type(tail, kind) do
    if tail == "" do
      {:ok, :any}
    else
      with :function <- kind,
           {:ok, keyword, source} <- HtmlValidator.parse_css_identifier(tail),
           true <- String.downcase(keyword, :ascii) == "returns",
           source when source != "" <- String.trim(source),
           do: type(source),
           else: (_ -> :error)
    end
  end

  defp type(source) do
    case CssSyntaxValidator.components(source) do
      [] ->
        {:ok, :any}

      [%{kind: :block, opener: opener, closed: true, text: text}] ->
        with {:ok, name, "("} <- HtmlValidator.parse_css_identifier(opener),
             true <- String.downcase(name, :ascii) == "type" do
          type = binary_part(text, byte_size(opener), byte_size(text) - byte_size(opener) - 1)
          CssPropertyRegistrationValidator.syntax(~s("#{type}"))
        else
          _ -> :error
        end

      _ ->
        if String.contains?(source, "|"),
          do: :error,
          else: CssPropertyRegistrationValidator.syntax(~s("#{source}"))
    end
  end

  @doc "Evaluates conditional function descriptors and keeps the last definition of each local."
  @spec body(String.t(), map() | nil | :unpositioned, map()) :: {:ok, map()}
  def body(source, media, aliases) do
    with {:ok, selected} <- PrintMediaValidator.select_rules(source, media, aliases) do
      descriptors =
        for %{declaration: {name, value}} <- CssSyntaxValidator.declarations(selected),
            name == "result" or String.starts_with?(name, "--"),
            into: %{},
            do: {name, value}

      {:ok, descriptors}
    end
  end

  @doc "Binds positional and named arguments using the same parameter rules for functions and mixins."
  @spec bind([tuple()], String.t(), (String.t(), term() -> {:ok, String.t()} | :error | tuple())) ::
          {:ok, map()} | :error | {:error, term()}
  def bind(parameters, source, compute) do
    names = Map.new(parameters, fn {name, type, default} -> {name, {type, default}} end)

    supplied_arguments = arguments(source)

    supplied_arguments =
      if List.last(supplied_arguments) == "",
        do: Enum.drop(supplied_arguments, -1),
        else: supplied_arguments

    with {:ok, supplied, _remaining} <-
           Enum.reduce_while(supplied_arguments, {:ok, %{}, parameters}, fn argument,
                                                                            {:ok, values,
                                                                             remaining} ->
             [head | named] = CssSyntaxValidator.split_trimmed(argument, ":")

             choice =
               case {named, remaining} do
                 {[_ | _], _} ->
                   with {:ok, name, ""} <- HtmlValidator.parse_css_identifier(head),
                        {:ok, {type, default}} <- Map.fetch(names, name),
                        do: {name, type, default, Enum.join(named, ":"), remaining}

                 {[], [{name, type, default} | rest]} ->
                   {name, type, default, argument, rest}

                 _ ->
                   :error
               end

             case choice do
               {name, type, default, value, rest} ->
                 if Map.has_key?(values, name) do
                   {:halt, :error}
                 else
                   value =
                     case CssSyntaxValidator.components(value) do
                       [%{kind: :block, opener: "{", closed: true, text: text}] ->
                         text |> binary_part(1, byte_size(text) - 2) |> String.trim()

                       _ ->
                         value
                     end

                   result =
                     case compute.(value, type) do
                       :error when is_binary(default) -> compute.(default, type)
                       result -> result
                     end

                   case result do
                     {:ok, value} -> {:cont, {:ok, Map.put(values, name, value), rest}}
                     error -> {:halt, error}
                   end
                 end

               _ ->
                 {:halt, :error}
             end
           end) do
      Enum.reduce_while(parameters, {:ok, supplied}, fn {name, type, default}, {:ok, values} ->
        cond do
          Map.has_key?(values, name) ->
            {:cont, {:ok, values}}

          is_nil(default) ->
            {:halt, :error}

          true ->
            case compute.(default, type) do
              {:ok, value} -> {:cont, {:ok, Map.put(values, name, value)}}
              error -> {:halt, error}
            end
        end
      end)
    end
  end

  defp arguments(source) do
    if String.trim(source) == "", do: [], else: CssSyntaxValidator.split_trimmed(source, ",")
  end

  @doc "Checks a substituted value against the declaration type."
  @spec check_type(String.t(), term()) :: {:ok, String.t()} | :error
  def check_type(value, type) do
    if CssPropertyRegistrationValidator.matches_syntax?(type, value),
      do: {:ok, value},
      else: :error
  end

  @doc "Checks that a function result can be substituted without changing resource provenance."
  @spec reference_free(String.t()) :: {:ok, String.t()} | :error | {:error, term()}
  def reference_free(value) do
    case CssReferenceValidator.scan(value) do
      {:ok, []} -> {:ok, value}
      {:ok, [_ | _]} -> :error
      error -> error
    end
  end
end
