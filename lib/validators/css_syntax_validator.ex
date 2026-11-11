defmodule NativeElixirPdfUtilities.Validators.CssSyntaxValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @identifier HtmlValidator.css_identifier_pattern()
  @escape HtmlValidator.css_escape_pattern()
  @number HtmlValidator.css_number_pattern()
  @url "(?i:#{HtmlValidator.css_url_name_pattern()})\\([ \\t\\n\\r\\f]*(?![ \\t\\n\\r\\f\"'])(?:#{@escape}|[^\\\\)])*(?:\\)|\\z)"
  @url_body Regex.compile!(
              "\\A(?:#{@escape}|[^ \\t\\n\\r\\f\"'()\\\\\\x00-\\x08\\x0B\\x0E-\\x1F\\x7F])*\\z",
              "u"
            )
  @lexical Regex.compile!(
             "#{@url}|/\\*(?:[^*]|\\*(?!/))*(?:\\*/|\\z)|\"(?:#{@escape}|\\\\(?:\\r\\n|[\\n\\r\\f])|[^\"\\\\\\n\\r\\f])*(?:\"|(?=[\\n\\r\\f])|\\z)|'(?:#{@escape}|\\\\(?:\\r\\n|[\\n\\r\\f])|[^'\\\\\\n\\r\\f])*(?:'|(?=[\\n\\r\\f])|\\z)|<!--|-->|@#{@identifier}|#{@number}(?:#{@identifier}|%)?|#{@identifier}\\(?|#{@escape}|[ \\t\\n\\r\\f]+|.",
             "su"
           )

  @doc "Returns balanced component values with byte spans in the original input."
  @spec components(String.t()) :: [map()]
  def components(source) do
    HtmlValidator.reserve_render_resource(:max_css_work, byte_size(source), :css)

    {stack, offset} =
      @lexical
      |> Regex.scan(source)
      |> Enum.reduce({[{nil, []}], 0}, fn [text], {stack, offset} ->
        token = %{text: text, offset: offset, length: byte_size(text), kind: :token}

        stack =
          cond do
            Regex.match?(~r/\A#{@url}\z/u, text) and
                not match?({:ok, _, "("}, HtmlValidator.parse_css_identifier(text)) ->
              {:ok, _, body} = HtmlValidator.parse_css_identifier(text)
              body = url_body(body)
              valid = Regex.match?(@url_body, body)

              [{open, values} | rest] = stack
              [{open, [%{token | kind: if(valid, do: :url, else: :bad_url)} | values]} | rest]

            String.starts_with?(text, "/*") ->
              [{open, values} | rest] = stack
              [{open, [%{token | kind: :comment} | values]} | rest]

            String.starts_with?(text, ["\"", "'"]) ->
              closed = byte_size(text) > 1 and :binary.first(text) == :binary.last(text)

              kind =
                if closed or offset + byte_size(text) == byte_size(source),
                  do: :string,
                  else: :bad_string

              [{open, values} | rest] = stack
              [{open, [%{token | kind: kind} | values]} | rest]

            text in ["(", "[", "{"] or
                match?({:ok, _, "("}, HtmlValidator.parse_css_identifier(text)) ->
              HtmlValidator.check_render_resource(:max_css_syntax_depth, length(stack), :css)
              [{token, []} | stack]

            text in [")", "]", "}"] and length(stack) > 1 ->
              [{open, values}, {parent, siblings} | rest] = stack

              expected =
                case :binary.last(open.text) do
                  ?( -> ")"
                  ?[ -> "]"
                  ?{ -> "}"
                end

              if text == expected do
                value = block(source, open, values, offset + byte_size(text), true)
                [{parent, [value | siblings]} | rest]
              else
                [{open, [token | values]}, {parent, siblings} | rest]
              end

            true ->
              [{open, values} | rest] = stack
              [{open, [token | values]} | rest]
          end

        {stack, offset + byte_size(text)}
      end)

    finish(source, stack, offset)
  end

  defp url_body(tail) do
    body = binary_part(tail, 1, byte_size(tail) - 1)
    units = HtmlValidator.css_syntax_units(body)
    body = if List.last(units) == ")", do: binary_part(body, 0, byte_size(body) - 1), else: body
    HtmlValidator.trim_css_whitespace(body)
  end

  @doc "Recovers declaration spans, discarding malformed declarations at component boundaries."
  @spec declarations(String.t()) :: [map()]
  def declarations(source) do
    source
    |> segments(";")
    |> Enum.flat_map(fn segment ->
      text = HtmlValidator.trim_css_whitespace(segment.text)

      parsed =
        HtmlValidator.validate_css_declaration(Enum.map_join(segment.components, &serialize/1))

      case parsed do
        {:ok, declaration} ->
          if invalid_value?(segment.components) do
            []
          else
            {at, _} = :binary.match(segment.text, text)

            [
              %{
                text: text,
                offset: segment.offset + at,
                length: byte_size(text),
                declaration: declaration
              }
            ]
          end

        _ ->
          []
      end
    end)
  end

  @doc "Serializes a recovered component with closed blocks and strings and CSS replacement characters."
  @spec serialize(map()) :: String.t()
  def serialize(value) do
    case value do
      %{kind: :block, opener: opener, children: children} ->
        closing =
          case :binary.last(opener) do
            ?( -> ")"
            ?[ -> "]"
            ?{ -> "}"
          end

        opener <> Enum.map_join(children, &serialize/1) <> closing

      %{kind: :string, text: text} ->
        if byte_size(text) > 1 and :binary.first(text) == :binary.last(text),
          do: text,
          else: text <> binary_part(text, 0, 1)

      %{text: text} ->
        String.replace(text, <<0>>, "\uFFFD")
    end
  end

  @doc "Rejects malformed CSS value tokens through the shared syntax recovery rules."
  @spec invalid_value?([map()]) :: boolean()
  def invalid_value?(components) do
    Enum.any?(components, fn value ->
      case value do
        %{kind: :bad_string} ->
          true

        %{kind: :bad_url} ->
          true

        %{kind: :block, opener: opener, children: children} ->
          url =
            case HtmlValidator.parse_css_identifier(opener) do
              {:ok, name, "("} -> String.downcase(name, :ascii) == "url"
              _ -> false
            end

          significant =
            Enum.reject(
              children,
              &(&1.kind == :comment or Regex.match?(~r/\A[ \t\n\r\f]+\z/, &1.text))
            )

          bad_url =
            url and not match?([%{kind: :string}], significant) and
              Enum.any?(children, fn child ->
                child.kind in [:string, :block] or child.text in ["\"", "'"]
              end)

          bad_url or invalid_value?(children)

        %{kind: :token, text: text} ->
          text in [")", "]", "}"]

        _ ->
          false
      end
    end)
  end

  @doc "Masks ignored at-rules without changing source byte positions and collects namespace bindings."
  @spec stylesheet_projection(String.t()) :: {String.t(), map()}
  def stylesheet_projection(source) do
    source
    |> rules()
    |> Enum.reduce({[], %{}, true, 0}, fn rule, {projected, namespaces, prelude, cursor} ->
      selector = HtmlValidator.trim_css_whitespace(rule.selectors)

      at =
        case selector do
          <<"@", name::binary>> ->
            case HtmlValidator.parse_css_identifier(name) do
              {:ok, name, tail} -> {String.downcase(name, :ascii), tail}
              _ -> nil
            end

          _ ->
            nil
        end

      {replacement, namespaces, prelude} =
        case at do
          {"namespace", tail} ->
            namespaces = if prelude, do: namespace_binding(tail, namespaces), else: namespaces
            {String.duplicate(" ", byte_size(rule.source)), namespaces, prelude}

          {name, _} when name in ["charset", "import"] ->
            {String.duplicate(" ", byte_size(rule.source)), namespaces, prelude}

          {name, _} when name in ["media", "font-face", "page"] and not rule.block ->
            {String.duplicate(" ", byte_size(rule.source)), namespaces, false}

          {name, _} when name in ["layer", "scope", "supports"] ->
            {rule.source, namespaces,
             if(name == "layer" and not rule.block, do: prelude, else: false)}

          {"property", _} ->
            {rule.source, namespaces, false}

          {"media", _} ->
            {body, _} = stylesheet_projection(rule.declarations)
            replacement = rule.selectors <> "{" <> body <> if(rule.closed, do: "}", else: "")
            {replacement, namespaces, false}

          {name, _}
          when name in ["font-face", "page", "font-feature-values", "font-palette-values"] ->
            {rule.source, namespaces, false}

          {_unknown, _} ->
            {String.duplicate(" ", byte_size(rule.source)), namespaces, false}

          nil ->
            {rule.source, namespaces, false}
        end

      before = binary_part(source, cursor, rule.offset - cursor)
      after_offset = rule.offset + byte_size(rule.source)
      {[replacement, before | projected], namespaces, prelude, after_offset}
    end)
    |> then(fn {projected, namespaces, _prelude, cursor} ->
      tail = binary_part(source, cursor, byte_size(source) - cursor)
      {IO.iodata_to_binary(Enum.reverse([tail | projected])), namespaces}
    end)
  end

  defp namespace_binding(source, namespaces) do
    values =
      source
      |> components()
      |> Enum.reject(&(&1.kind == :comment or Regex.match?(~r/\A[ \t\n\r\f]+\z/, &1.text)))

    {prefix, uri} =
      case values do
        [uri] -> {"", uri}
        [%{text: prefix}, uri] -> {prefix, uri}
        _ -> {nil, nil}
      end

    uri =
      case uri do
        %{kind: :string, text: text} ->
          HtmlValidator.decode_css_string(text)

        %{kind: :url, text: text} ->
          {:ok, _name, tail} = HtmlValidator.parse_css_identifier(text)
          tail |> url_body() |> HtmlValidator.decode_css_escapes()

        %{kind: :block, opener: opener, children: children} ->
          case HtmlValidator.parse_css_identifier(opener) do
            {:ok, name, "("} ->
              values =
                Enum.reject(
                  children,
                  &(&1.kind == :comment or Regex.match?(~r/\A[ \t\n\r\f]+\z/, &1.text))
                )

              case {String.downcase(name, :ascii), values} do
                {"url", [%{kind: :string} = value]} ->
                  HtmlValidator.decode_css_string(serialize(value))

                _ ->
                  nil
              end

            _ ->
              nil
          end

        _ ->
          nil
      end

    prefix =
      case prefix do
        "" ->
          ""

        prefix when is_binary(prefix) ->
          case HtmlValidator.parse_css_identifier(prefix) do
            {:ok, name, ""} -> name
            _ -> nil
          end

        _ ->
          nil
      end

    if is_binary(prefix) and is_binary(uri),
      do: Map.put(namespaces, prefix, if(uri == "", do: nil, else: uri)),
      else: namespaces
  end

  @doc "Consumes qualified and at-rules, recovering at balanced blocks and semicolons."
  @spec rules(String.t()) :: [map()]
  def rules(source) do
    {rules, start} =
      source
      |> components()
      |> Enum.reduce({[], 0}, fn component, {rules, start} ->
        cond do
          component.kind == :block and component.opener == "{" ->
            selectors = binary_part(source, start, component.offset - start)
            offset = start
            finish = component.offset + component.length

            rule = %{
              source: binary_part(source, offset, finish - offset),
              block: true,
              offset: offset,
              selectors: selectors,
              declarations:
                binary_part(
                  component.text,
                  1,
                  component.length - if(component.closed, do: 2, else: 1)
                ),
              closed: component.closed
            }

            {[rule | rules], finish}

          component.kind == :token and component.text == ";" ->
            selectors = binary_part(source, start, component.offset - start)

            rule = %{
              source: binary_part(source, start, component.offset + 1 - start),
              block: false,
              offset: start,
              selectors: selectors,
              declarations: "",
              closed: true
            }

            if String.starts_with?(HtmlValidator.trim_css_whitespace(selectors), "@"),
              do: {[rule | rules], component.offset + 1},
              else: {rules, start}

          component.kind == :token and component.text in ["<!--", "-->", "}"] ->
            {rules, component.offset + component.length}

          true ->
            {rules, start}
        end
      end)

    remaining = binary_part(source, start, byte_size(source) - start)

    rules =
      if String.starts_with?(HtmlValidator.trim_css_whitespace(remaining), "@"),
        do: [
          %{
            source: remaining,
            offset: start,
            selectors: remaining,
            declarations: "",
            block: false,
            closed: false
          }
          | rules
        ],
        else: rules

    Enum.reverse(rules)
  end

  @doc "Returns authored declarations and nested rules with original byte spans in a rule block."
  @spec block_items(String.t(), boolean()) :: [map()]
  def block_items(source, top_level \\ false) do
    {items, start} =
      source
      |> components()
      |> Enum.reduce({[], 0}, fn component, {items, start} ->
        boundary =
          (component.kind == :block and component.opener == "{") or
            (component.kind == :token and component.text == ";") or
            (top_level and component.kind == :token and component.text in ["<!--", "-->"])

        prelude = if boundary, do: binary_part(source, start, component.offset - start), else: ""
        text = HtmlValidator.trim_css_whitespace(prelude)

        declaration =
          component.kind == :block and
            case HtmlValidator.parse_css_identifier(text) do
              {:ok, "--" <> _, tail} ->
                String.starts_with?(HtmlValidator.trim_css_whitespace(tail), ":")

              _ ->
                false
            end

        cond do
          top_level and component.kind == :token and component.text in ["<!--", "-->"] and
              text == "" ->
            {items, component.offset + component.length}

          component.kind == :block and component.opener == "{" and not declaration ->
            finish = component.offset + component.length

            item = %{
              kind: :rule,
              selectors: prelude,
              offset: start,
              source: binary_part(source, start, finish - start),
              block: true,
              declarations:
                binary_part(
                  component.text,
                  1,
                  component.length - if(component.closed, do: 2, else: 1)
                ),
              closed: component.closed
            }

            {[item | items], finish}

          component.kind == :token and component.text == ";" ->
            finish = component.offset + 1

            item = %{
              kind: if(String.starts_with?(text, "@"), do: :rule, else: :declaration),
              source: binary_part(source, start, finish - start),
              offset: start,
              selectors: prelude,
              declarations: "",
              block: false,
              closed: true
            }

            {[item | items], finish}

          true ->
            {items, start}
        end
      end)

    tail = binary_part(source, start, byte_size(source) - start)

    if HtmlValidator.trim_css_whitespace(tail) == "",
      do: Enum.reverse(items),
      else: Enum.reverse([%{kind: :declaration, source: tail, offset: start} | items])
  end

  defp finish(source, stack, offset) do
    case stack do
      [{nil, values}] ->
        Enum.reverse(values)

      [{open, values}, {parent, siblings} | rest] ->
        finish(
          source,
          [{parent, [block(source, open, values, offset, false) | siblings]} | rest],
          offset
        )
    end
  end

  defp block(source, open, values, ending, closed) do
    Map.merge(open, %{
      kind: :block,
      text: binary_part(source, open.offset, ending - open.offset),
      length: ending - open.offset,
      opener: open.text,
      children: Enum.reverse(values),
      closed: closed
    })
  end

  @doc "Splits at top-level delimiters, keeping strings and nested blocks opaque."
  @spec split(String.t(), String.t()) :: [String.t()]
  def split(source, delimiter) do
    Enum.map(segments(source, delimiter), & &1.text)
  end

  defp segments(source, delimiter) do
    {parts, start, values} =
      source
      |> components()
      |> Enum.reduce({[], 0, []}, fn token, {parts, start, values} ->
        if token.kind == :token and token.text == delimiter do
          part = %{
            text: binary_part(source, start, token.offset - start),
            offset: start,
            components: Enum.reverse(values)
          }

          {[part | parts], token.offset + token.length, []}
        else
          {parts, start, [token | values]}
        end
      end)

    Enum.reverse([
      %{
        text: binary_part(source, start, byte_size(source) - start),
        offset: start,
        components: Enum.reverse(values)
      }
      | parts
    ])
  end

  @doc "Consumes a CSS pseudo-class function without reinterpreting strings or blocks."
  @spec function(String.t(), String.t()) :: {:ok, String.t(), String.t()} | :error | nil
  def function(source, expected) do
    case source do
      <<":", rest::binary>> ->
        case HtmlValidator.parse_css_identifier(rest) do
          {:ok, name, <<"(", _::binary>> = tail} ->
            if String.downcase(name, :ascii) == expected do
              [component | _] = components(tail)

              if component.closed do
                {:ok, binary_part(component.text, 1, component.length - 2),
                 binary_part(tail, component.length, byte_size(tail) - component.length)}
              else
                :error
              end
            end

          {:ok, name, _} ->
            if String.downcase(name, :ascii) == expected, do: :error

          :error ->
            nil
        end

      _ ->
        nil
    end
  end
end
