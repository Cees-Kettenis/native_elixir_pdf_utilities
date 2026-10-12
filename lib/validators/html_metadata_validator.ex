defmodule NativeElixirPdfUtilities.Validators.HtmlMetadataValidator do
  @moduledoc """
  Validates inert HTML microdata syntax and resolves translation metadata.

  The pinned HTML Standard's `items`, `names:-the-itemprop-attribute` and
  `the-translate-attribute` sections define these attributes. Authored values
  remain unchanged. Microdata identifiers never select assets or PDF links.
  Vocabulary definitions, property association/crawling and extraction APIs
  are outside this preservation contract. Unknown vocabularies are opaque;
  their common-vocabulary and global-identifier rules cannot be inferred from
  URL prefixes or checked by fetching them.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Selects initially enabled HTML style blocks in authored tree order without fetching resources."
  @spec selected_style_nodes(
          [map()],
          NativeElixirPdfUtilities.Validators.PrintMediaValidator.context()
          | nil
          | :unpositioned
        ) :: [map()]
  def selected_style_nodes(nodes, media_context) do
    select_stylesheets(nodes, media_context, [:html])
  end

  @doc "Selects initially enabled HTML and SVG style blocks in one authored document light tree."
  @spec selected_document_style_nodes([map()], map() | nil | :unpositioned) :: [map()]
  def selected_document_style_nodes(nodes, media_context) do
    select_stylesheets(nodes, media_context, [:html, :svg])
  end

  defp select_stylesheets(nodes, media_context, namespaces) do
    scope =
      Enum.find_value(nodes, {nil, nil}, fn node ->
        if Map.has_key?(node, :document_id) and Map.has_key?(node, :tree_scope_id),
          do: {node.document_id, node.tree_scope_id}
      end)

    sources = stylesheet_nodes(nodes, scope, namespaces)

    preferred =
      Enum.reduce(sources, "", fn node, preferred ->
        attrs = node.attributes

        cond do
          node.tag == "meta" and
            String.downcase(Map.get(attrs, "http-equiv", ""), :ascii) == "default-style" and
              Map.get(attrs, "content", "") != "" ->
            attrs["content"]

          node.tag == "style" and stylesheet_type?(attrs) and preferred == "" ->
            Map.get(attrs, "title", "")

          true ->
            preferred
        end
      end)

    Enum.filter(sources, fn node ->
      attrs = node.attributes

      node.tag == "style" and stylesheet_type?(attrs) and
        Map.get(attrs, "title", "") in ["", preferred] and
        NativeElixirPdfUtilities.Validators.PrintMediaValidator.matches_stylesheet?(
          Map.get(attrs, "media", "all"),
          Enum.map_join(node.children, "", fn child -> Map.get(child, :text, "") end),
          media_context
        )
    end)
  end

  defp stylesheet_nodes(nodes, {document, tree} = scope, namespaces) do
    Enum.flat_map(nodes, fn node ->
      case node do
        %{type: :element, tag: tag, attributes: attrs, children: children} ->
          if {Map.get(node, :document_id, document), Map.get(node, :tree_scope_id, tree)} == scope do
            namespace = Map.get(node, :namespace, :html)
            stylesheet? = namespace in namespaces and tag == "style"
            metadata? = namespace == :html and tag == "meta"

            if stylesheet? or metadata?,
              do: HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)

            own =
              if stylesheet? or metadata?,
                do: [
                  %{
                    node
                    | attributes:
                        Map.reject(attrs, fn {name, _} ->
                          not is_nil(Map.get(Map.get(node, :attribute_namespaces, %{}), name))
                        end)
                  }
                ],
                else: []

            if tag in ["template", "style"],
              do: own,
              else: own ++ stylesheet_nodes(children, scope, namespaces)
          else
            []
          end

        _ ->
          []
      end
    end)
  end

  # HTML's style type is an exact ASCII-insensitive value, not a MIME essence.
  # A content attribute named disabled does not set the CSSOM disabled flag.
  defp stylesheet_type?(attrs) do
    String.downcase(Map.get(attrs, "type", ""), :ascii) in ["", "text/css"]
  end

  @doc "Validates applicable, unnamespaced HTML microdata attribute syntax."
  @spec validate_attributes(map(), String.t() | nil) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_attributes(node, source \\ nil) do
    attributes = node.attributes

    if Map.get(node, :namespace, :html) == :html and
         (Map.has_key?(attributes, "itemtype") or Map.has_key?(attributes, "itemid") or
            Map.has_key?(attributes, "itemprop") or Map.has_key?(attributes, "itemref")) do
      namespaces = Map.get(node, :attribute_namespaces, %{})

      attributes =
        node.attributes
        |> Map.take(~w(itemscope itemtype itemid itemprop itemref))
        |> Map.reject(fn {name, _value} -> not is_nil(Map.get(namespaces, name)) end)

      scope? = Map.has_key?(attributes, "itemscope")
      type? = Map.has_key?(attributes, "itemtype")

      Enum.reduce_while(~w(itemtype itemid itemprop itemref), :ok, fn name, :ok ->
        case Map.fetch(attributes, name) do
          {:ok, value} ->
            tokens = HtmlValidator.html_class_tokens(value)

            message =
              cond do
                name in ~w(itemtype itemref) and not scope? ->
                  "#{name} requires itemscope on the same HTML element"

                name == "itemid" and not (scope? and type?) ->
                  "itemid requires both itemscope and itemtype on the same HTML element"

                name in ~w(itemtype itemprop) and tokens == [] ->
                  "#{name} requires at least one ASCII-whitespace-separated token"

                name != "itemid" and length(tokens) != length(Enum.uniq(tokens)) ->
                  "#{name} requires unique, case-sensitive ASCII-whitespace-separated tokens"

                name == "itemtype" and not Enum.all?(tokens, &valid_url?(&1, :absolute)) ->
                  "itemtype requires absolute URL tokens; vocabulary identifiers are preserved without fetching"

                name == "itemid" and
                    not valid_url?(HtmlValidator.trim_ascii_whitespace(value), :relative) ->
                  "itemid requires a valid URL, optionally surrounded by ASCII whitespace; relative identifiers are preserved without resolution"

                name == "itemprop" and
                    not Enum.all?(tokens, fn token ->
                      not String.contains?(token, [".", ":"]) or valid_url?(token, :absolute)
                    end) ->
                  "itemprop names must contain neither a full stop nor a colon unless they are absolute URLs"

                true ->
                  nil
              end

            if message do
              {:halt,
               Diagnostics.error(:html, :unsupported_html, message,
                 module: __MODULE__,
                 source: source || Map.get(node, :tag, "HTML metadata"),
                 line: Map.get(node, :line),
                 column: Map.get(node, :column)
               )}
            else
              {:cont, :ok}
            end

          :error ->
            {:cont, :ok}
        end
      end)
    else
      :ok
    end
  end

  @doc "Checks that applicable itemref tokens name elements in the same authored tree."
  @spec validate_item_references(map(), [map()]) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_item_references(node, tree) do
    attributes = node.attributes

    if Map.get(node, :namespace, :html) == :html and
         is_nil(Map.get(Map.get(node, :attribute_namespaces, %{}), "itemref")) and
         Map.has_key?(attributes, "itemref") do
      case Enum.find(HtmlValidator.html_class_tokens(attributes["itemref"]), fn id ->
             not tree_id?(tree, id)
           end) do
        nil ->
          :ok

        id ->
          Diagnostics.error(
            :html,
            :unsupported_html,
            "itemref=#{inspect(id)} must name an element ID in the same authored tree; template contents have their own tree",
            module: __MODULE__,
            operation: :compute,
            source: node.tag,
            line: Map.get(node, :line),
            column: Map.get(node, :column)
          )
      end
    else
      :ok
    end
  end

  @doc false
  @spec first_tree_ids([map()]) :: %{optional({term(), term(), String.t()}) => map()}
  def first_tree_ids(nodes) do
    scope =
      Enum.find_value(nodes, {nil, nil}, fn node ->
        if Map.has_key?(node, :document_id) and Map.has_key?(node, :tree_scope_id),
          do: {node.document_id, node.tree_scope_id},
          else: nil
      end)

    first_tree_ids(nodes, scope)
  end

  @doc false
  @spec first_tree_ids([map()], {term(), term()}) :: %{
          optional({term(), term(), String.t()}) => map()
        }
  def first_tree_ids(nodes, scope) do
    index_tree_ids(nodes, scope, %{})
  end

  # The template element belongs to this light tree; its content fragment does not.
  # Logical scopes do not authorize resources. Hidden/foreign nodes participate.
  defp index_tree_ids(nodes, {document, tree} = scope, index) do
    Enum.reduce(nodes, index, fn node, index ->
      HtmlValidator.reserve_render_resource(:max_css_work, 1, :html)

      case node do
        %{type: :element, attributes: attributes, children: children} ->
          node_scope =
            {Map.get(node, :document_id, document), Map.get(node, :tree_scope_id, tree)}

          if node_scope == scope do
            namespaces = Map.get(node, :attribute_namespaces, %{})
            id = Map.get(attributes, "id", "")

            index =
              if id != "" and is_nil(Map.get(namespaces, "id")),
                do: Map.put_new(index, {document, tree, id}, node),
                else: index

            if node.tag == "template" and Map.get(node, :namespace, :html) == :html,
              do: index,
              else: index_tree_ids(children, scope, index)
          else
            index
          end

        _ ->
          index
      end
    end)
  end

  # This validates ID references without crawling properties or extracting data.
  # Ordinary children include foreign hosts; inert template contents are separate.
  defp tree_id?(nodes, id) do
    Enum.any?(nodes, fn node ->
      HtmlValidator.reserve_render_resource(:max_css_work, 1, :html)

      case node do
        %{type: :element, attributes: attributes, children: children}
        when is_map(attributes) and is_list(children) ->
          namespaces = Map.get(node, :attribute_namespaces, %{})

          (Map.get(attributes, "id") == id and is_map(namespaces) and
             is_nil(Map.get(namespaces, "id"))) or
            tree_id?(children, id)

        _ ->
          false
      end
    end)
  end

  @doc "Resolves translation mode without changing authored attributes or text."
  @spec translation_mode(map(), boolean()) :: boolean()
  def translation_mode(node, inherited?) do
    if Map.get(node, :namespace, :html) == :html and
         is_nil(Map.get(Map.get(node, :attribute_namespaces, %{}), "translate")) do
      case Map.get(node.attributes, "translate") do
        "" ->
          true

        value when is_binary(value) ->
          case String.downcase(value, :ascii) do
            "yes" -> true
            "no" -> false
            _ -> inherited?
          end

        _ ->
          inherited?
      end
    else
      inherited?
    end
  end

  # URL syntax is distinct from resource authorization and link support. Opaque
  # schemes are allowed here because these identifiers are never dereferenced.
  defp valid_url?(value, kind) do
    case URI.new(URI.encode(value)) do
      {:ok, uri} ->
        scheme = if uri.scheme, do: String.downcase(uri.scheme, :ascii), else: nil

        host_valid? =
          cond do
            is_nil(uri.host) ->
              scheme not in ~w(http https ftp ws wss)

            String.contains?(uri.host, ":") ->
              match?({:ok, _}, :inet.parse_ipv6_address(String.to_charlist(uri.host)))

            true ->
              raw_host = URI.parse(value).host
              normalized_host = :unicode.characters_to_nfkc_binary(raw_host)

              address_valid? =
                if scheme in ~w(http https ftp ws wss) and
                     Regex.match?(~r/(?:\A|\.)[0-9]+\z/, raw_host) do
                  match?({:ok, _}, :inet.parse_ipv4_address(String.to_charlist(raw_host))) and
                    Regex.match?(
                      ~r/\A(?:0|[1-9][0-9]*)\.(?:0|[1-9][0-9]*)\.(?:0|[1-9][0-9]*)\.(?:0|[1-9][0-9]*)\z/,
                      raw_host
                    )
                else
                  true
                end

              (scheme not in ~w(http https ftp ws wss file) or
                 not String.contains?(raw_host, "%")) and address_valid? and
                not Regex.match?(~r/[\x00-\x20\x{7F}-\x{9F}#\/:<>?@\[\]\\^|]/u, normalized_host) and
                (scheme not in ~w(http https ftp ws wss) or raw_host != "")
          end

        # Ports are fixed 16-bit protocol values, not a configurable resource limit.
        # URL code points exclude Unicode noncharacters. These ranges and the
        # plane-ending values are fixed Unicode invariants, not resource limits.
        not Regex.match?(
          ~r/[\x00-\x20\x{7F}-\x{9F}\x{FDD0}-\x{FDEF}<>"`{}|\\^]|%(?![0-9a-fA-F]{2})/u,
          value
        ) and
          not Enum.any?(String.to_charlist(value), &(rem(&1, 65_536) in [65_534, 65_535])) and
          (kind == :relative or is_binary(scheme)) and
          (not is_nil(scheme) or
             not String.contains?(value |> String.split(["/", "?", "#"]) |> hd(), ":")) and
          (is_nil(uri.port) or uri.port in 0..65_535) and host_valid? and
          (scheme != "file" or (is_nil(uri.port) and is_nil(uri.userinfo))) and
          Enum.all?([uri.path, uri.query, uri.fragment, uri.userinfo], fn part ->
            is_nil(part) or not String.contains?(part, ["[", "]"])
          end) and
          (scheme not in ~w(http https ftp ws wss file) or String.contains?(value, "://"))

      {:error, _} ->
        false
    end
  end
end
