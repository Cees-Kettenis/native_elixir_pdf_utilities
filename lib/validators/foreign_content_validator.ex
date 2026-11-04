defmodule NativeElixirPdfUtilities.Validators.ForeignContentValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.HtmlRecoveryValidator

  @svg_names ~w(altGlyph altGlyphDef altGlyphItem animateColor animateMotion animateTransform clipPath feBlend feColorMatrix feComponentTransfer feComposite feConvolveMatrix feDiffuseLighting feDisplacementMap feDistantLight feDropShadow feFlood feFuncA feFuncB feFuncG feFuncR feGaussianBlur feImage feMerge feMergeNode feMorphology feOffset fePointLight feSpecularLighting feSpotLight feTile feTurbulence foreignObject glyphRef linearGradient radialGradient textPath)
  @svg_tags Map.new(@svg_names, &{String.downcase(&1), &1})
  @svg_attribute_names ~w(attributeName attributeType baseFrequency baseProfile calcMode clipPathUnits diffuseConstant edgeMode filterUnits glyphRef gradientTransform gradientUnits kernelMatrix kernelUnitLength keyPoints keySplines keyTimes lengthAdjust limitingConeAngle markerHeight markerUnits markerWidth maskContentUnits maskUnits numOctaves pathLength patternContentUnits patternTransform patternUnits pointsAtX pointsAtY pointsAtZ preserveAlpha preserveAspectRatio primitiveUnits refX refY repeatCount repeatDur requiredExtensions requiredFeatures specularConstant specularExponent spreadMethod startOffset stdDeviation stitchTiles surfaceScale systemLanguage tableValues targetX targetY textLength viewBox viewTarget xChannelSelector yChannelSelector zoomAndPan)
  @svg_attributes Map.new(@svg_attribute_names, &{String.downcase(&1), &1})
  @math_text_points ~w(mi mo mn ms mtext)
  @breakout_tags ~w(b big blockquote body br center code dd div dl dt em embed h1 h2 h3 h4 h5 h6 head hr i img li listing menu meta nobr ol p pre ruby s small span strong strike sub sup table tt u ul var)
  @namespaces %{
    "xlink" => "http://www.w3.org/1999/xlink",
    "xml" => "http://www.w3.org/XML/1998/namespace",
    "xmlns" => "http://www.w3.org/2000/xmlns/"
  }
  @foreign_attributes ~w(xlink:actuate xlink:arcrole xlink:href xlink:role xlink:show xlink:title xlink:type xml:base xml:lang xml:space xmlns xmlns:xlink)

  @doc false
  @spec namespace(term(), String.t()) :: :html | :mathml | :svg
  def namespace(context, tag) do
    html? =
      case context do
        {:foreign, :mathml, parent, attributes} ->
          (parent in @math_text_points and tag not in ~w(mglyph malignmark)) or
            (parent == "annotation-xml" and
               (tag == "svg" or
                  String.downcase(Map.get(attributes, "encoding", "")) in [
                    "text/html",
                    "application/xhtml+xml"
                  ]))

        {:foreign, :svg, parent, _attributes} ->
          parent in ~w(foreignObject desc title)

        _ ->
          true
      end

    if html? do
      case tag do
        "math" -> :mathml
        "svg" -> :svg
        _ -> :html
      end
    else
      elem(context, 1)
    end
  end

  @doc false
  @spec prepare_start_tag(String.t(), String.t(), term()) ::
          {:ok, map()}
          | {:error, {atom(), Diagnostics.diagnostic()}}
          | {:error, :unsupported_html}
  def prepare_start_tag(tag, source, context) do
    namespace = namespace(context, tag)

    with true <- namespace != :html or HtmlValidator.supported_html_tag?(tag),
         {:ok, metadata} <- HtmlValidator.prepare_html_attributes(source, tag, namespace),
         attributes = metadata.attributes,
         {:ok, self_closing?} <-
           HtmlRecoveryValidator.prepare_self_closing(namespace, tag, metadata.self_closing?),
         :ok <- validate_element(namespace, tag, attributes) do
      {tag, attributes} =
        case namespace do
          :svg ->
            {Map.get(@svg_tags, tag, tag),
             Map.new(attributes, fn {name, value} ->
               {Map.get(@svg_attributes, name, name), value}
             end)}

          :mathml ->
            {tag,
             Map.new(attributes, fn {name, value} ->
               {adjust_attribute_name(namespace, name), value}
             end)}

          :html ->
            {tag, attributes}
        end

      namespaced =
        if namespace == :html do
          %{}
        else
          for {name, _value} <- attributes, name in @foreign_attributes, into: %{} do
            {prefix, local} =
              case String.split(name, ":", parts: 2) do
                ["xmlns"] -> {nil, "xmlns"}
                [prefix, local] -> {prefix, local}
              end

            uri = Map.fetch!(@namespaces, prefix || "xmlns")
            {name, %{prefix: prefix, local_name: local, namespace: uri}}
          end
        end

      {:ok,
       %{
         tag: tag,
         attributes: attributes,
         attribute_order:
           Enum.map(metadata.attribute_order, &adjust_attribute_name(namespace, &1)),
         namespace: namespace,
         attribute_namespaces: namespaced,
         self_closing?: self_closing?
       }}
    else
      false -> {:error, :unsupported_html}
      error -> error
    end
  end

  # Source order and the attribute map share the same foreign-name adjustment.
  defp adjust_attribute_name(namespace, name) do
    case namespace do
      :svg -> Map.get(@svg_attributes, name, name)
      :mathml -> if(name == "definitionurl", do: "definitionURL", else: name)
      :html -> name
    end
  end

  defp validate_element(namespace, tag, attributes, inert? \\ false) do
    with :ok <-
           if(namespace != :html or inert?,
             do: validate_static_input(namespace, tag, attributes),
             else: :ok
           ) do
      cond do
        namespace != :html and
            (tag in @breakout_tags or
               (tag == "font" and Enum.any?(~w(color face size), &Map.has_key?(attributes, &1)))) ->
          Diagnostics.error(
            :html,
            :unsupported_html,
            "#{tag} would leave foreign content through malformed-markup recovery; use an HTML integration point",
            module: __MODULE__
          )

        namespace != :html and
            ((Map.has_key?(attributes, "xmlns") and
                attributes["xmlns"] !=
                  if(namespace == :svg,
                    do: "http://www.w3.org/2000/svg",
                    else: "http://www.w3.org/1998/Math/MathML"
                  )) or
               (Map.has_key?(attributes, "xmlns:xlink") and
                  attributes["xmlns:xlink"] != @namespaces["xlink"])) ->
          Diagnostics.error(
            :html,
            :unsupported_html,
            "foreign namespace declarations must match the element namespace",
            module: __MODULE__
          )

        true ->
          :ok
      end
    end
  end

  @doc false
  @spec validate_static_input(
          :html | :svg | :mathml,
          String.t(),
          map() | [{charlist(), charlist(), charlist(), charlist()}]
        ) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_static_input(namespace, tag, attributes) do
    cond do
      Enum.any?(attributes, fn attribute ->
        # DOM maps and namespace-aware SAX attributes retain their own value
        # representation. Event policy needs only the decoded local name.
        name =
            case attribute do
              {name, _value} -> name
              {_uri, _prefix, name, _value} -> name
            end

        case name do
          <<first, second, _rest::binary>> -> first in ~c"oO" and second in ~c"nN"
          [first, second | _rest] -> first in ~c"oO" and second in ~c"nN"
          _ -> false
        end
      end) ->
        Diagnostics.error(
          :html,
          :unsupported_html,
          "event handlers are outside the static input profile",
          module: __MODULE__
        )

      namespace == :svg and
          tag in ~w(animate animateMotion animateTransform animateColor set discard) ->
        Diagnostics.error(
          :html,
          :unsupported_html,
          "SVG animation and timed mutation are outside the static input profile",
          module: __MODULE__
        )

      namespace != :html and tag == "script" ->
        Diagnostics.error(
          :html,
          :unsupported_html,
          "executable scripts are excluded in foreign content",
          module: __MODULE__
        )

      true ->
        :ok
    end
  end

  @doc false
  @spec child_context(map()) :: term()
  def child_context(element) do
    case element.namespace do
      :html -> element.tag
      namespace -> {:foreign, namespace, element.tag, element.attributes}
    end
  end

  @doc false
  @spec context_tag(term()) :: term()
  def context_tag(context) do
    case context do
      {:foreign, _namespace, tag, _attributes} -> String.downcase(tag)
      context -> context
    end
  end

  @doc false
  @spec valid_child?(term(), map(), list()) :: boolean()
  def valid_child?(context, element, ancestors) do
    HtmlValidator.valid_html_child_element?(context, element, ancestors)
  end

  @doc false
  @spec supported_end_tag?(String.t(), term(), list()) :: boolean()
  def supported_end_tag?(tag, context, ancestors) do
    is_tuple(context) or HtmlValidator.supported_html_tag?(tag) or
      Enum.any?(ancestors, &(context_tag(&1) == tag))
  end

  @doc false
  @spec prepare_fragment_context(term()) ::
          {:ok, tuple()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_fragment_context(context) do
    prepared =
      case context do
        "math" ->
          {:foreign, :mathml, "math", %{}}

        "svg" ->
          {:foreign, :svg, "svg", %{}}

        %{namespace: namespace, tag: tag} = element
        when namespace in [:mathml, :svg] and is_binary(tag) ->
          {:foreign, namespace, tag, Map.get(element, :attributes, %{})}

        _ ->
          nil
      end

    case prepared do
      {:foreign, namespace, tag, attributes} when is_map(attributes) ->
        valid? =
          Regex.match?(~r/^[a-zA-Z][a-zA-Z0-9-]*$/, tag) and
            Enum.all?(attributes, fn {name, value} ->
              is_binary(name) and is_binary(value) and String.valid?(value) and
                Regex.match?(~r/^[a-zA-Z][a-zA-Z0-9_:.-]*$/, name) and
                not String.starts_with?(String.downcase(name), "on")
            end)

        if valid? do
          bytes =
            Enum.reduce(attributes, byte_size(tag), fn {name, value}, bytes ->
              bytes + byte_size(name) + byte_size(value)
            end)

          HtmlValidator.check_render_resource(:max_html_source_bytes, bytes, :html)
          HtmlValidator.reserve_render_resource(:max_aggregate_html_source_bytes, bytes, :html)
          tag = String.downcase(tag)
          attributes = Map.new(attributes, fn {name, value} -> {String.downcase(name), value} end)

          with :ok <- validate_element(namespace, tag, attributes) do
            tag = if namespace == :svg, do: Map.get(@svg_tags, tag, tag), else: tag
            {:ok, {:foreign, namespace, tag, attributes}}
          end
        else
          invalid_fragment(context)
        end

      _ ->
        invalid_fragment(context)
    end
  end

  defp invalid_fragment(context) do
    Diagnostics.error(
      :html,
      :unsupported_html,
      "fragment context must name an HTML element or an SVG/MathML element with string attributes",
      module: __MODULE__,
      source: inspect(context)
    )
  end

  @doc false
  @spec valid_element?(map(), list(), term(), list()) :: boolean()
  def valid_element?(element, children, context \\ nil, ancestors \\ []) do
    cond do
      element.namespace != :html ->
        true

      element.tag == "noscript" ->
        HtmlValidator.valid_noscript_element?(
          children,
          if(is_tuple(context), do: "div", else: context),
          ancestors
        )

      true ->
        HtmlValidator.valid_html_element?(element.tag, element.attributes, children)
    end
  end

  @doc false
  @spec validate_render_support(map(), boolean(), :render | :prepare_svg) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_render_support(node, active?, mode \\ :render) do
    case node do
      %{namespace: :svg} when mode == :prepare_svg ->
        # Canonical SVG XML has already resolved namespaces during source
        # admission. HTML's foreign-content recovery rules do not apply to it.
        validate_static_input(:svg, node.tag, node.attributes)

      %{namespace: :mathml} when active? ->
        # Element layout support depends on computed visibility. Unselected
        # semantics/maction annotations still undergo all static validation;
        # MathLayoutValidator prepares every visible element after the cascade.
        validate_inert_element(node)

      %{namespace: namespace, tag: _tag} when namespace in [:mathml, :svg] ->
        if active? and namespace == :svg and mode == :render,
          do: NativeElixirPdfUtilities.Validators.SvgSceneValidator.validate_node(node),
          else: validate_inert_element(node)

      %{tag: "template"} ->
        validate_inert_element(node)

      _ ->
        if active?,
          do: :ok,
          else: validate_inert_element(node)
    end
  end

  defp validate_inert_element(node) do
    case validate_element(Map.get(node, :namespace, :html), node.tag, node.attributes, true) do
      :ok ->
        :ok

      {:error, {reason, diagnostic}} ->
        {:error,
         {reason, Diagnostics.with_context(diagnostic, operation: :compute, source: node.tag)}}
    end
  end
end
