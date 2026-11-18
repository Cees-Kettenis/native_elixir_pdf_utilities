defmodule NativeElixirPdfUtilities.Validators.CssMixinValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    CssFunctionValidator,
    CssReferenceValidator,
    CssSyntaxValidator,
    HtmlValidator
  }

  @doc "Collects local mixin definitions before applications are expanded."
  @spec definitions(String.t(), (non_neg_integer() -> boolean())) :: map()
  def definitions(source, active?) do
    if candidate?(source, "mixin"),
      do: definitions(source, 0, 0, active?, %{}),
      else: %{}
  end

  defp definitions(source, offset, depth, active?, definitions) do
    HtmlValidator.check_render_resource(:max_depth, depth, :css)

    Enum.reduce(CssSyntaxValidator.rules(source), definitions, fn rule, definitions ->
      prelude = String.trim(rule.selectors)
      rule_offset = offset + rule.offset
      body_offset = rule_offset + byte_size(rule.selectors) + 1

      with true <- rule.block and active?.(body_offset),
           <<"@", tail::binary>> <- prelude,
           {:ok, name, head} <- HtmlValidator.parse_css_identifier(tail) do
        case String.downcase(name, :ascii) do
          "mixin" ->
            case CssFunctionValidator.definition(String.trim(head), :mixin) do
              {:ok, mixin} ->
                Map.put(
                  definitions,
                  mixin.name,
                  Map.merge(mixin, %{source: rule.declarations, offset: body_offset})
                )

              :error ->
                definitions
            end

          "layer" ->
            definitions(rule.declarations, body_offset, depth + 1, active?, definitions)

          _ ->
            definitions
        end
      else
        _ -> definitions
      end
    end)
  end

  @doc "Collects the private properties declared by a style or an applied mixin."
  @spec private_properties([map()], non_neg_integer(), (non_neg_integer() -> boolean())) :: map()
  def private_properties(items, origin, active?) do
    Enum.reduce(items, %{}, fn item, properties ->
      with %{kind: :rule, block: true, selectors: selector} <- item,
           <<"@", tail::binary>> <- String.trim(selector),
           {:ok, name, head} <- HtmlValidator.parse_css_identifier(tail),
           "private" <- String.downcase(name, :ascii),
           "" <- String.trim(head),
           true <- active?.(origin + item.offset + byte_size(selector) + 1) do
        Enum.reduce(CssSyntaxValidator.declarations(item.declarations), properties, fn entry,
                                                                                       properties ->
          case entry.declaration do
            {"--" <> _ = name, value} -> Map.put(properties, name, value)
            _ -> properties
          end
        end)
      else
        _ -> properties
      end
    end)
  end

  @doc "Binds a mixin at its application site and checks expansion recursion."
  @spec application(String.t(), map(), map()) :: {:ok, map(), map()} | :error
  def application(source, rule, context) do
    with {:ok, "--" <> _ = name, tail} <- HtmlValidator.parse_css_identifier(source),
         {:ok, mixin} <- Map.fetch(context.mixins, name),
         {:ok, arguments} <- application_arguments(tail) do
      if name in context.mixin_stack do
        leading = byte_size(rule.selectors) - byte_size(String.trim_leading(rule.selectors))
        location = context.location.(context.origin + rule.offset + leading)

        throw(
          {:render_resource_limit,
           Diagnostics.error(:css, :invalid_css, "circular CSS mixin application",
             module: __MODULE__,
             operation: :expand_css_mixin,
             source: String.trim(rule.source),
             line: location.line,
             column: location.column
           )}
        )
      end

      with {:ok, arguments} <-
             CssFunctionValidator.bind(mixin.parameters, arguments, fn value, type ->
               case CssReferenceValidator.scan(value) do
                 {:ok, []} -> {:ok, {:argument, type, value, context.css_locals}}
                 {:ok, [_ | _]} -> :error
                 error -> error
               end
             end) do
        {:ok, mixin, arguments}
      else
        {:error, _} = error -> throw({:render_resource_limit, error})
        _ -> :error
      end
    else
      _ -> :error
    end
  end

  defp application_arguments(source) do
    case CssSyntaxValidator.components(source) do
      [] ->
        {:ok, ""}

      [%{kind: :block, opener: "(", closed: true, text: text}] ->
        {:ok, binary_part(text, 1, byte_size(text) - 2)}

      _ ->
        :error
    end
  end

  @doc "Checks for a potentially escaped mixin or private-property at-rule."
  @spec candidate?(String.t(), String.t()) :: boolean()
  def candidate?(source, name) do
    String.contains?(source, "@") and
      (String.contains?(source, "\\") or String.contains?(String.downcase(source, :ascii), name))
  end
end
