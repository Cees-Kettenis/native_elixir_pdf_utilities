defmodule NativeElixirPdfUtilities.Validators.SvgParserWorkValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Charges XML attribute and namespace comparisons before entering a tree parser."
  @spec validate(binary()) :: :ok | {:error, {atom(), map()}}
  def validate(source) do
    HtmlValidator.with_render_budget(fn ->
      HtmlValidator.reserve_render_resource(:max_work, byte_size(source), :style)
      # The implicit XML namespace is a protocol constant, not a resource ceiling.
      namespace = %{
        count: 1,
        bytes: 4 + byte_size("http://www.w3.org/XML/1998/namespace"),
        uri: byte_size("http://www.w3.org/XML/1998/namespace")
      }

      scan(source, %{
        scopes: [namespace],
        namespace_bytes: namespace.bytes,
        bindings: MapSet.new([{"xml", "http://www.w3.org/XML/1998/namespace"}])
      })
    end)
    |> case do
      {:error, {reason, diagnostic}} ->
        {:error, {reason, Diagnostics.with_context(diagnostic, source: "SVG")}}

      result ->
        result
    end
  end

  # This scanner estimates work only. XML syntax, entities and namespace validity
  # remain owned by the SAX validator. Stop on malformed lexical input because
  # SAX cannot reach a later start tag through it.
  defp scan(source, state) do
    case :binary.match(source, "<") do
      :nomatch ->
        :ok

      {offset, 1} ->
        rest = binary_part(source, offset + 1, byte_size(source) - offset - 1)

        case rest do
          <<"!--", rest::binary>> ->
            skip_section(rest, "-->", state)

          <<"![CDATA[", rest::binary>> ->
            skip_section(rest, "]]>", state)

          <<"?", rest::binary>> ->
            skip_section(rest, "?>", state)

          <<"!", _::binary>> ->
            :ok

          <<"/", rest::binary>> ->
            parents =
              case state.scopes do
                [_current, parent | ancestors] -> [parent | ancestors]
                _ -> state.scopes
              end

            skip_section(rest, ">", %{state | scopes: parents})

          _ ->
            {name, rest} = name(rest)

            if name == "" do
              :ok
            else
              namespace = hd(state.scopes)
              cost = byte_size(name) + namespace.bytes
              HtmlValidator.reserve_render_resource(:max_work, cost, :style)
              attributes(rest, state, namespace, 0, 0, byte_size(name), cost)
            end
        end
    end
  end

  defp skip_section(source, ending, state) do
    case :binary.match(source, ending) do
      :nomatch ->
        :ok

      {offset, length} ->
        scan(
          binary_part(source, offset + length, byte_size(source) - offset - length),
          state
        )
    end
  end

  defp attributes(source, state, namespace, count, qualified, name_bytes, charged) do
    case whitespace(source) do
      <<"/>", rest::binary>> ->
        scan(rest, state)

      <<">", rest::binary>> ->
        scan(rest, %{state | scopes: [namespace | state.scopes]})

      source ->
        {attribute, rest} = name(source)

        case {attribute, whitespace(rest)} do
          {"", _} ->
            :ok

          {attribute, <<"=", rest::binary>>} ->
            case whitespace(rest) do
              <<quote, value::binary>> when quote in [?", ?'] ->
                case :binary.match(value, <<quote>>) do
                  :nomatch ->
                    :ok

                  {value_bytes, 1} ->
                    declaration? =
                      attribute == "xmlns" or String.starts_with?(attribute, "xmlns:")

                    {namespace, state} =
                      if declaration? do
                        binding_bytes = byte_size(attribute) + value_bytes + 1

                        prefix =
                          if attribute == "xmlns",
                            do: "",
                            else: binary_part(attribute, 6, byte_size(attribute) - 6)

                        binding = {prefix, binary_part(value, 0, value_bytes)}

                        global_bytes =
                          state.namespace_bytes +
                            if(MapSet.member?(state.bindings, binding),
                              do: 0,
                              else: binding_bytes
                            )

                        # The native parser also inserts bindings into a document-wide
                        # sorted namespace vector, including declarations on siblings.
                        HtmlValidator.reserve_render_resource(
                          :max_work,
                          global_bytes,
                          :style
                        )

                        {%{
                           count: namespace.count + 1,
                           bytes: namespace.bytes + binding_bytes,
                           uri: max(namespace.uri, value_bytes)
                         },
                         %{
                           state
                           | namespace_bytes: global_bytes,
                             bindings: MapSet.put(state.bindings, binding)
                         }}
                      else
                        {namespace, state}
                      end

                    count = count + 1

                    qualified =
                      qualified +
                        if(not declaration? and :binary.match(attribute, ":") != :nomatch,
                          do: 1,
                          else: 0
                        )

                    name_bytes = name_bytes + byte_size(attribute) + 1
                    # Duplicate expanded-name checks are quadratic in attributes.
                    # Only prefixed attributes compare namespace URI strings;
                    # default namespaces never apply to unprefixed attributes.
                    # Namespace lookup also scans the bindings in scope. Retaining
                    # shadowed bindings overestimates work without resolving XML.
                    # Merging inherited bindings scans the growing local namespace
                    # list, even if this tag introduces only one declaration.
                    merge_work =
                      if namespace.count > hd(state.scopes).count,
                        do: namespace.count * namespace.bytes,
                        else: 0

                    cost =
                      (count + 1) * (name_bytes + namespace.bytes) +
                        qualified * (qualified + 1) * namespace.uri + merge_work

                    HtmlValidator.reserve_render_resource(
                      :max_work,
                      cost - charged,
                      :style
                    )

                    rest = binary_part(value, value_bytes + 1, byte_size(value) - value_bytes - 1)
                    attributes(rest, state, namespace, count, qualified, name_bytes, cost)
                end

              _ ->
                :ok
            end

          _ ->
            :ok
        end
    end
  end

  defp name(source) do
    case :binary.match(source, [" ", "\t", "\r", "\n", "=", "/", ">", "<", "\"", "'"]) do
      :nomatch ->
        {source, ""}

      {offset, _} ->
        {binary_part(source, 0, offset), binary_part(source, offset, byte_size(source) - offset)}
    end
  end

  defp whitespace(source) do
    case source do
      <<byte, rest::binary>> when byte in [32, 9, 10, 13] -> whitespace(rest)
      _ -> source
    end
  end
end
