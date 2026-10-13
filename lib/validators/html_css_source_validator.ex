defmodule NativeElixirPdfUtilities.Validators.HtmlCssSourceValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlEntities, HtmlParser}
  alias NativeElixirPdfUtilities.Validators.{ForeignContentValidator, HtmlValidator}

  @doc false
  @spec prepare(map()) :: {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(dom) do
    HtmlValidator.with_render_budget(fn ->
      canonical? = is_map(dom) and Map.has_key?(dom, :document_id)

      with :ok <- NativeElixirPdfUtilities.Validators.HtmlTreeValidator.validate_transport(dom),
           {:ok, bound} <- if(canonical?, do: bind_tokens(dom), else: {:ok, %{}}) do
        {inline, embedded} = descriptors(dom.children, bound, canonical?, dom, %{}, [])

        records =
          inline
          |> Enum.sort_by(fn {_id, descriptor} -> descriptor.origin end)
          |> Enum.reduce_while({:ok, %{}}, fn {id, descriptor}, {:ok, records} ->
            case NativeElixirPdfUtilities.HtmlToPdf.CssParser.prepare_source_declarations(
                   descriptor.css,
                   descriptor
                 ) do
              {:ok, declarations} -> {:cont, {:ok, Map.put(records, id, declarations)}}
              error -> {:halt, error}
            end
          end)

        with {:ok, records} <- records,
             do: {:ok, %{inline: inline, embedded: embedded, records: records}}
      end
    end)
  end

  # Validate actual tokenizer ownership, rather than searching for snippets. This
  # reads the original stream once and does not build another tree or count nodes.
  defp bind_tokens(dom) do
    original = dom.source.text
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 4 * byte_size(original), :html)

    with {:ok, original} <- HtmlValidator.validate_html_source(original) do
      lines = [
        0
        | Enum.map(:binary.matches(original, ["\r\n", "\r", "\n"]), fn {at, size} -> at + size end)
      ]

      nodes = source_nodes(dom.children)
      parents = Map.new([dom | nodes], &{&1.node_id, &1.parent_id})

      parents =
        Enum.reduce(nodes, parents, fn node, parents ->
          case Map.get(node, :content) do
            %{node_id: fragment} -> Map.put(parents, fragment, node.node_id)
            _ -> parents
          end
        end)

      authored =
        MapSet.new(
          for %{type: :element, source: %{text: text}} = node <- nodes,
              text != "",
              do: node.node_id
        )

      ownership = %{parents: parents, authored: authored}

      indexed =
        Enum.reduce_while(nodes, {:ok, %{}}, fn node, {:ok, indexed} ->
          source = node.source

          cond do
            node.type == :document_fragment ->
              {:cont, {:ok, indexed}}

            source.text == "" and node.type == :element and
                (node.tag == "style" or Map.has_key?(node.attributes, "style")) ->
              {:halt,
               binding_error("authored CSS owner requires an original opening token", source)}

            source.text == "" ->
              {:cont, {:ok, indexed}}

            true ->
              HtmlValidator.reserve_render_resource(
                :max_html_tree_work,
                byte_size(original),
                :html
              )

              line_start = Enum.at(lines, source.line - 1)

              offset =
                if line_start do
                  tail = binary_part(original, line_start, byte_size(original) - line_start)
                  line = tail |> String.split(~r/\r|\n/, parts: 2) |> hd()

                  if source.column <= String.length(line) + 1 do
                    prefix =
                      line |> String.graphemes() |> Enum.take(source.column - 1) |> Enum.join()

                    line_start + byte_size(prefix)
                  end
                end

              if is_integer(offset) and not Map.has_key?(indexed, offset) and
                   offset + byte_size(source.text) <= byte_size(original) and
                   binary_part(original, offset, byte_size(source.text)) == source.text do
                {:cont, {:ok, Map.put(indexed, offset, node)}}
              else
                {:halt,
                 binding_error("node source does not identify its exact original token", source)}
              end
          end
        end)

      with {:ok, indexed} <- indexed do
        input = HtmlValidator.preprocess_html(original)

        crlf =
          :binary.matches(original, "\r\n")
          |> Enum.with_index()
          |> Enum.map(fn {{at, _}, index} -> at - index end)

        {input, offset} =
          case input do
            <<239, 187, 191, rest::binary>> -> {rest, 3}
            input -> {input, 0}
          end

        if dom.source.line == 1 and dom.source.column == 1,
          do: scan_tokens(input, offset, original, crlf, indexed, [], %{}, ownership),
          else:
            binding_error("original document source must start at line 1, column 1", dom.source)
      end
    end
  end

  defp source_nodes(nodes) do
    Enum.flat_map(nodes, fn node ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
      nested = source_nodes(Map.get(node, :children, []))

      detached =
        case Map.get(node, :content) do
          %{children: children} -> source_nodes(children)
          _ -> []
        end

      content = if Map.has_key?(node, :content), do: [node.content], else: []
      [node | nested ++ content ++ detached]
    end)
  end

  defp scan_tokens(input, offset, original, crlf, indexed, stack, bound, ownership) do
    case input do
      "" ->
        if map_size(indexed) == 0,
          do: {:ok, bound},
          else:
            binding_error(
              "node source is not an original tokenizer boundary",
              hd(Map.values(indexed)).source
            )

      input ->
        HtmlValidator.reserve_render_resource(:max_html_tree_work, 1 + length(crlf), :html)

        context =
          case stack do
            [{prepared, _id} | _] -> ForeignContentValidator.child_context(prepared)
            [] -> :document
          end

        ancestors = Enum.map(stack, fn {prepared, _id} -> prepared.tag end)

        case HtmlParser.read_source_token(input, context, ancestors) do
          {:ok, token, remaining, kind} ->
            finish = offset + byte_size(token)
            first = offset + Enum.count(crlf, &(&1 < offset))
            last = finish + Enum.count(crlf, &(&1 < finish))
            raw = binary_part(original, first, last - first)
            node = Map.get(indexed, first)

            with {:ok, stack, bound} <-
                   bind_token(kind, node, token, raw, first, context, stack, bound, ownership) do
              scan_tokens(
                remaining,
                finish,
                original,
                crlf,
                Map.delete(indexed, first),
                stack,
                bound,
                ownership
              )
            end

          _ ->
            binding_error("original HTML cannot be tokenized in the supplied node context", %{
              text: original
            })
        end
    end
  end

  defp bind_token(kind, node, token, raw, offset, context, stack, bound, ownership) do
    exact? = is_nil(node) or node.source.text == raw

    case {kind, node, exact?} do
      {{:start, prepared}, %{type: :element} = node, true} ->
        ancestors = Enum.map(stack, fn {prior, _id} -> prior.tag end)

        stack =
          Enum.drop_while(stack, fn {prior, _id} ->
            HtmlValidator.omitted_html_end_tag?(
              prior.tag,
              {:start, String.downcase(prepared.tag, :ascii)},
              ancestors
            )
          end)

        actual_parent =
          case stack do
            [{_, id} | _] -> id
            [] -> nil
          end

        if original_parent(node.parent_id, ownership) == actual_parent and
             prepared.tag == node.tag and prepared.attributes == node.attributes and
             prepared.namespace == Map.get(node, :namespace, :html) and
             prepared.attribute_namespaces == Map.get(node, :attribute_namespaces, %{}) do
          stack = if prepared.self_closing?, do: stack, else: [{prepared, node.node_id} | stack]
          {:ok, stack, Map.put(bound, node.node_id, %{offset: offset, raw: raw, token: token})}
        else
          binding_error(
            "original opening token does not match the node's actual parent, namespace, tag or decoded attributes",
            node.source
          )
        end

      {{:end, tag}, nil, true} ->
        rest =
          Enum.drop_while(stack, fn {prepared, _id} ->
            String.downcase(prepared.tag, :ascii) != tag
          end)

        {:ok, Enum.drop(rest, 1), bound}

      {:text, %{type: :text} = node, true} ->
        style_owner =
          case stack do
            [{%{tag: "style"}, id} | _] -> id
            _ -> nil
          end

        decoded =
          cond do
            HtmlValidator.html_text_mode(context) in [:raw_text, :plaintext] ->
              token

            String.starts_with?(token, "<![CDATA[") and is_tuple(context) ->
              binary_part(token, 9, byte_size(token) - 12)

            true ->
              HtmlEntities.decode(token, :text)
          end

        if is_nil(style_owner) or (style_owner == node.parent_id and decoded == node.text) do
          {:ok, stack,
           Map.put(bound, node.node_id, %{
             offset: offset,
             raw: raw,
             token: token,
             context: context
           })}
        else
          binding_error(
            "embedded stylesheet text does not match its actual original owner and decoded content",
            node.source
          )
        end

      {:text, nil, true} ->
        if HtmlValidator.html_text_mode(context) == :data and String.trim(token) == "",
          do: {:ok, stack, bound},
          else:
            binding_error("original text token has no matching canonical source owner", %{
              text: raw
            })

      {:other, nil, true} ->
        {:ok, stack, bound}

      {:other, %{type: :processing_instruction} = node, true} ->
        {:ok, stack, Map.put(bound, node.node_id, %{offset: offset, raw: raw, token: token})}

      _ ->
        binding_error(
          "original token boundary does not match the supplied canonical source owner",
          if(node, do: node.source, else: %{text: raw})
        )
    end
  end

  defp original_parent(id, ownership) do
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

    cond do
      is_nil(id) -> nil
      MapSet.member?(ownership.authored, id) -> id
      true -> original_parent(Map.get(ownership.parents, id), ownership)
    end
  end

  defp descriptors(nodes, bound, canonical?, dom, inline, embedded) do
    Enum.reduce(nodes, {inline, embedded}, fn node, {inline, embedded} ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

      case node do
        %{type: :element, attributes: attributes, children: children} ->
          inline =
            case Map.fetch(attributes, "style") do
              {:ok, css} ->
                descriptor =
                  if canonical?,
                    do: inline_descriptor(node, Map.fetch!(bound, node.node_id), dom),
                    else: local(css)

                Map.put(
                  inline,
                  Map.get(node, :node_id, Map.get(node, :_selector_id, node)),
                  descriptor
                )

              :error ->
                inline
            end

          if node.tag == "style" and Map.get(node, :namespace, :html) in [:html, :svg] and
               NativeElixirPdfUtilities.Validators.HtmlMetadataValidator.stylesheet_type?(
                 attributes
               ) do
            css =
              Enum.map_join(children, "", fn child ->
                if child.type == :text, do: child.text, else: ""
              end)

            descriptor =
              if canonical? do
                {segments, _cursor} =
                  Enum.reduce(children, {[], 0}, fn child, {segments, cursor} ->
                    if child.type == :text do
                      span = Map.fetch!(bound, child.node_id)

                      {decoded, mapping} =
                        decoded_segments(
                          span.raw,
                          span.offset,
                          cond do
                            HtmlValidator.html_text_mode(span.context) == :raw_text ->
                              nil

                            String.starts_with?(span.token, "<![CDATA[") and
                                is_tuple(span.context) ->
                              :cdata

                            true ->
                              :text
                          end
                        )

                      mapping =
                        Enum.map(mapping, fn {at, size, raw_at, raw_size} ->
                          {at + cursor, size, raw_at, raw_size}
                        end)

                      {segments ++ mapping, cursor + byte_size(decoded)}
                    else
                      {segments, cursor}
                    end
                  end)

                span = Map.fetch!(bound, node.node_id)

                descriptor(
                  css,
                  dom.source.text,
                  node,
                  segments,
                  span.offset + byte_size(span.raw)
                )
              else
                local(css)
              end

            {inline, embedded ++ [%{css: css, descriptor: descriptor, node: node}]}
          else
            descriptors(children, bound, canonical?, dom, inline, embedded)
          end

        _ ->
          {inline, embedded}
      end
    end)
  end

  defp inline_descriptor(node, span, dom) do
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 4 * byte_size(span.raw), :html)
    captures = HtmlValidator.html_attribute_captures(span.raw, return: :index)

    capture =
      Enum.find(captures, fn [_all, {at, size} | _] ->
        String.downcase(binary_part(span.raw, at, size), :ascii) == "style"
      end)

    [_all, _name | values] = capture
    {at, size} = List.first(values) || {0, 0}
    value = binary_part(span.raw, at, size)

    {at, value} =
      case value do
        <<quote, _::binary>> when quote in [?", ?'] ->
          {at + 1, binary_part(value, 1, byte_size(value) - 2)}

        value ->
          {at, value}
      end

    {css, segments} = decoded_segments(value, span.offset + at, :attribute)
    descriptor(css, dom.source.text, node, segments, span.offset + at)
  end

  defp decoded_segments(raw, base, context) do
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 4 * byte_size(raw), :html)

    {raw, base, context} =
      if context == :cdata,
        do: {binary_part(raw, 9, byte_size(raw) - 12), base + 9, nil},
        else: {raw, base, context}

    normalized = HtmlValidator.preprocess_html(raw)

    crlf =
      :binary.matches(raw, "\r\n")
      |> Enum.with_index()
      |> Enum.map(fn {{at, _}, index} -> at - index end)

    {decoded, segments} =
      if context,
        do: HtmlEntities.decode_with_spans(normalized, context),
        else: {normalized, for(at <- 0..(byte_size(normalized) - 1)//1, do: {at, 1, at, 1})}

    segments =
      Enum.map(segments, fn {at, size, input_at, input_size} ->
        HtmlValidator.reserve_render_resource(:max_html_tree_work, length(crlf) + 1, :html)
        first = input_at + Enum.count(crlf, &(&1 < input_at))
        last = input_at + input_size + Enum.count(crlf, &(&1 < input_at + input_size))
        {at, size, base + first, last - first}
      end)

    {decoded, segments}
  end

  defp descriptor(css, original, node, segments, origin) do
    with_mapper(%{
      provenance: :original_html,
      css: css,
      original: original,
      owner: Map.take(node, [:node_id, :document_id, :tree_scope_id, :parent_id]),
      segments: List.to_tuple(segments),
      origin: origin
    })
  end

  @doc false
  @spec local(String.t()) :: map()
  def local(css),
    do:
      with_mapper(%{
        provenance: :unbound,
        css: css,
        original: css,
        owner: nil,
        segments: nil,
        origin: 0
      })

  # This pure closure captures validator-checked mapping data. It has no registry,
  # lifecycle or permission effect; edited transport fields cannot alter its map.
  defp with_mapper(mapping) do
    Map.put(mapping, :mapper, fn
      :validate, supplied ->
        HtmlValidator.reserve_render_resource(
          :max_html_tree_work,
          byte_size(mapping.original) +
            if(mapping.segments, do: tuple_size(mapping.segments), else: 0),
          :html
        )

        if supplied == mapping, do: :ok, else: :mismatch

      {:span, offset, length}, _ ->
        map_validated_span(mapping, offset, length)
    end)
  end

  @doc false
  @spec validate_descriptor(term(), term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_descriptor(css, descriptor) do
    valid? =
      case descriptor do
        %{css: ^css, mapper: mapper} when is_function(mapper, 2) ->
          info = :erlang.fun_info(mapper)
          expected = :erlang.fun_info(with_mapper(%{}).mapper)

          info[:module] == __MODULE__ and info[:type] == :local and
            info[:index] == expected[:index] and info[:new_uniq] == expected[:new_uniq] and
            mapper.(:validate, Map.delete(descriptor, :mapper)) == :ok

        _ ->
          false
      end

    if valid?,
      do: :ok,
      else:
        binding_error(
          "CSS source descriptor must match its validator-owned decoded source and original mapping",
          %{text: "css_source_descriptor"}
        )
  end

  @doc false
  @spec map_span(map(), non_neg_integer(), non_neg_integer()) :: map()
  def map_span(descriptor, offset, length) do
    case validate_descriptor(if(is_map(descriptor), do: Map.get(descriptor, :css)), descriptor) do
      :ok -> descriptor.mapper.({:span, offset, length}, nil)
      error -> throw({:render_resource_limit, error})
    end
  end

  defp map_validated_span(descriptor, offset, length) do
    HtmlValidator.reserve_render_resource(
      :max_html_tree_work,
      byte_size(descriptor.original) +
        if(descriptor.segments, do: tuple_size(descriptor.segments), else: 0),
      :html
    )

    # UTF-8 continuation bytes cannot begin even an empty span.
    if not (is_integer(offset) and is_integer(length) and offset >= 0 and length >= 0 and
              offset + length <= byte_size(descriptor.css) and
              (offset == byte_size(descriptor.css) or
                 Bitwise.band(:binary.at(descriptor.css, offset), 0xC0) != 0x80) and
              String.valid?(binary_part(descriptor.css, offset, length))) do
      throw(
        {:render_resource_limit,
         binding_error("CSS span is outside its validated decoded source", %{text: descriptor.css})}
      )
    end

    {first, last} =
      if descriptor.provenance == :original_html and length > 0 do
        overlapping =
          descriptor.segments
          |> Tuple.to_list()
          |> Enum.filter(fn {at, size, _, _} -> at < offset + length and at + size > offset end)

        {_, _, first, _} = hd(overlapping)
        {_, _, last_at, last_size} = List.last(overlapping)
        {first, last_at + last_size}
      else
        if descriptor.provenance == :original_html do
          point =
            case Tuple.to_list(descriptor.segments) do
              [] ->
                descriptor.origin

              segments ->
                case Enum.find(segments, fn {at, size, _, _} ->
                       at <= offset and at + size > offset
                     end) do
                  {_, _, at, _} ->
                    at

                  nil ->
                    {_, _, at, size} = List.last(segments)
                    at + size
                end
            end

          {point, point}
        else
          {offset, offset + length}
        end
      end

    prefix = binary_part(descriptor.original, 0, first)
    lines = String.split(prefix, ~r/\r\n|\r|\n/, trim: false)

    %{
      text: binary_part(descriptor.original, first, last - first),
      offset: first,
      length: last - first,
      line: length(lines),
      column: String.length(List.last(lines)) + 1,
      provenance: descriptor.provenance,
      owner: descriptor.owner
    }
  end

  defp binding_error(message, source) do
    Diagnostics.error(:html, :invalid_document, message,
      module: __MODULE__,
      operation: :source_style_contexts,
      source: source.text,
      line: Map.get(source, :line),
      column: Map.get(source, :column)
    )
  end
end
