defmodule NativeElixirPdfUtilities.HtmlToPdf.Style do
  @moduledoc """
  Style computation for the native HTML-to-PDF renderer.

  This module applies defaults, inheritance, font resolution, and the CSS
  cascade for the supported text, generated-content, named-counter, selector,
  box, list, link, table, page-break, flexbox, grid, image, static form-control,
  and embedded-font styling subset. Unsupported properties or invalid values
  fail the render instead of being ignored.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.CssParser
  alias NativeElixirPdfUtilities.HtmlToPdf.AssetLoader
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.PngDecoder
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer
  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
  alias NativeElixirPdfUtilities.Validators.EmbeddedViewportValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewport
  alias NativeElixirPdfUtilities.Validators.ResourceValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.FormValidator
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.HtmlLinkValidator
  alias NativeElixirPdfUtilities.Validators.HtmlUrlValidator
  alias NativeElixirPdfUtilities.Validators.HtmlMetadataValidator
  alias NativeElixirPdfUtilities.Validators.PrintMediaValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.LanguageValidator
  alias NativeElixirPdfUtilities.Validators.SvgValidator
  alias NativeElixirPdfUtilities.Validators.SvgStyleValidator
  alias NativeElixirPdfUtilities.Validators.MathLayoutValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.MathLayout

  # Fixed print-theme compatibility entries, not configurable resource limits.
  @native_width_fallback_families [
    "American Typewriter",
    "Arial Hebrew",
    "Chalkboard",
    "Cochin",
    "Corsiva Hebrew",
    "Courier",
    "Euphemia UCAS",
    "Geneva",
    "Gill Sans",
    "Hei",
    "Helvetica",
    "Hoefler Text",
    "InaiMathi",
    "Kai",
    "Lucida Grande",
    "Marker Felt",
    "Monaco",
    "Mshtakan",
    "New Peninim MT",
    "Osaka",
    "Raanana",
    "STHeiti",
    "Symbol",
    "Times",
    "Apple Braille",
    "Apple LiGothic",
    "Apple LiSung",
    "Apple Symbols",
    "AppleGothic",
    "AppleMyungjo",
    "#GungSeo",
    "#HeadLineA",
    "#PCMyungjo",
    "#PilGi"
  ]

  @border_styles [
    :none,
    :hidden,
    :dotted,
    :dashed,
    :solid,
    :double,
    :groove,
    :ridge,
    :inset,
    :outset
  ]
  @medium_border_width 2.25
  @rem_opaque_style_keys [
    :_font_registry,
    :font_face,
    :image,
    :svg_image,
    :_text_decorations,
    :embedded_document,
    :_table_model,
    :_table_cell
  ]

  @type text_node :: %{type: :text, text: String.t(), style: map()}
  @type styled_element :: %{
          type: :element,
          tag: String.t(),
          style: map(),
          children: [text_node() | styled_element()]
        }
  @type styled_tree :: %{type: :document, children: [styled_element()]}
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()
  @type stylesheet_entry :: %{
          css: String.t(),
          base_url: String.t() | nil,
          trusted_local_resources: boolean()
        }

  @doc """
  Computes styles for a parsed HTML document tree.
  """
  @spec compute(term(), [render_option()]) :: {:ok, styled_tree()} | {:error, :invalid_document}
  def compute(dom, opts \\ []) do
    case compute_detailed(dom, opts) do
      {:ok, styled_tree} -> {:ok, styled_tree}
      {:error, {_reason, _detail}} -> {:error, :invalid_document}
    end
  end

  @doc """
  Computes styles and returns detailed CSS/document diagnostics when validation fails.

  Table styles preserve `:_table_model`, and cell styles preserve `:_table_cell`.
  Recoverable HTML table model errors remain in `:_table_model.errors`; success
  means renderable input, not that the source table has no conformance errors.
  Logical spans and associations are independent of visible CSS grid geometry.
  """
  @spec compute_detailed(term(), [render_option()]) ::
          {:ok, styled_tree()}
          | {:error,
             {:invalid_css
              | :invalid_document
              | :invalid_options
              | :unsupported_html
              | :unsupported_glyph
              | :resource_limit_exceeded, map()}}
  def compute_detailed(dom, opts \\ []) do
    compute_detailed(dom, opts, HtmlValidator.new_image_budget())
  end

  @doc false
  @spec compute_detailed(term(), [render_option()], HtmlValidator.image_budget()) ::
          {:ok, styled_tree()}
          | {:error,
             {:invalid_css
              | :invalid_document
              | :invalid_options
              | :unsupported_html
              | :unsupported_glyph
              | :resource_limit_exceeded, map()}}
  def compute_detailed(dom, opts, image_budget) do
    compute_styles(dom, opts, image_budget, :caller)
  end

  @doc false
  @spec compute_child_detailed(
          term(),
          [render_option()],
          HtmlValidator.image_budget(),
          String.t() | nil
        ) ::
          {:ok, styled_tree()} | {:error, {atom(), map()}}
  def compute_child_detailed(dom, opts, image_budget, document_url) do
    with :ok <- ResourceValidator.validate_context(ResourceContext.current()) do
      compute_styles(dom, opts, image_budget, {:document_url, document_url})
    end
  end

  @doc false
  @spec compute_child_detailed(
          term(),
          [render_option()],
          HtmlValidator.image_budget(),
          String.t() | nil,
          {number(), number()}
        ) ::
          {:ok, styled_tree()} | {:error, {atom(), map()}}
  def compute_child_detailed(dom, opts, image_budget, document_url, viewport) do
    compute_child_detailed(dom, opts, image_budget, document_url, viewport, viewport)
  end

  @doc "Styles a child using separate client layout and media viewport dimensions in points."
  @spec compute_child_detailed(
          term(),
          [render_option()],
          HtmlValidator.image_budget(),
          String.t() | nil,
          term(),
          term()
        ) ::
          {:ok, styled_tree()} | {:error, {atom(), map()}}
  def compute_child_detailed(dom, opts, image_budget, document_url, client, media) do
    with :ok <- ResourceValidator.validate_context(ResourceContext.current()),
         {:ok, client, media} <- EmbeddedViewportValidator.prepare_media_viewport(client, media) do
      compute_styles(dom, opts, image_budget, {:document_viewport, document_url, client, media})
    end
  end

  @doc false
  @spec prepare_svg_detailed(term(), [render_option()]) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare_svg_detailed(dom, opts \\ []) do
    compute_styles(dom, opts, HtmlValidator.new_image_budget(), :caller, :prepare_svg)
  end

  defp compute_styles(dom, opts, image_budget, document_url, mode \\ :render) do
    RenderCache.with_stylesheets(fn ->
      HtmlValidator.with_render_budget(fn ->
        FontValidator.with_budget(fn ->
          font_options = Font.normalize_options(opts)

          case HtmlValidator.prepare_style_input(dom, opts, font_options, mode) do
            {:ok, initial_controls?} ->
              {:ok, opts} = font_options
              %{type: :document, children: children} = dom

              url_context =
                case document_url do
                  {:document_viewport, url, _viewport, _media} ->
                    HtmlUrlValidator.document_context_from_url(dom, url)

                  {:document_url, url} ->
                    HtmlUrlValidator.document_context_from_url(dom, url)

                  :caller ->
                    HtmlUrlValidator.document_context(dom, opts)
                end

              with {:ok, url_context} <- url_context,
                   opts <- Keyword.put(opts, :__document_url_context__, url_context),
                   opts <-
                     (case document_url do
                        {:document_viewport, _url, {width, height}, _media} ->
                          Keyword.put(opts, :__client_css_viewport__, %{
                            width: width / 0.75,
                            height: height / 0.75
                          })

                        _ ->
                          opts
                      end),
                   children <-
                     if(initial_controls?,
                       do:
                         NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_initial_nodes(
                           children
                         ),
                       else: HtmlValidator.assign_render_metadata(children)
                     ),
                   {:ok, link_context} <-
                     (case ResourceContext.current() do
                        nil -> HtmlLinkValidator.prepare(children, url_context)
                        context -> HtmlLinkValidator.prepare(children, url_context, context)
                      end),
                   {:ok, media_context} <-
                     (case document_url do
                        {:document_viewport, _url, _viewport, media} ->
                          PrintMediaValidator.client_context(opts, media)

                        {:document_url, _url} ->
                          {:ok, :unpositioned}

                        :caller ->
                          PrintMediaValidator.renderer_context(opts)
                      end),
                   {:ok, entries} <- stylesheet_entries(children, opts, media_context),
                   {:ok, stylesheet_entries} <- prepare_stylesheet_entries(entries, media_context),
                   {:ok, css_fonts} <- stylesheet_fonts(stylesheet_entries, opts),
                   {:ok, font_registry} <- font_registry(opts, css_fonts),
                   {:ok, font_families, font_face} <-
                     resolve_font(
                       Keyword.get(opts, :default_font, "DejaVu Sans"),
                       400,
                       :normal,
                       font_registry
                     ),
                   {:ok, rules} <- stylesheet_rules(stylesheet_entries),
                   {:ok, children} <-
                     NativeElixirPdfUtilities.Validators.HtmlTableValidator.prepare_document(
                       children
                     ) do
                link_metadata =
                  if Enum.any?(link_context.links, fn {_href, destination} ->
                       match?({:target, _}, destination) or match?({:top, _}, destination)
                     end),
                     do: %{link_scope: {link_context.document_id, link_context.tree_scope_id}},
                     else: %{}

                link_metadata =
                  if link_context.document_targets == [],
                    do: link_metadata,
                    else:
                      Map.put(
                        link_metadata,
                        :link_document_targets,
                        link_context.document_targets
                      )

                style_opts =
                  opts
                  |> Keyword.put(:__image_budget__, image_budget)
                  |> Keyword.put(:__link_context__, link_context)

                base_style = %{
                  _custom_properties: %{},
                  _font_registry: font_registry,
                  _root_font_size: 12.0,
                  color: {0, 0, 0},
                  font_face: font_face,
                  font_families: font_families,
                  font_family: font_face.family,
                  font_size: 12.0,
                  font_style: :normal,
                  font_weight: 400,
                  letter_spacing: 0.0,
                  line_height: 14.4,
                  line_height_normal: true,
                  text_align: :left,
                  text_transform: :none,
                  white_space: :normal
                }

                base_style =
                  if mode == :prepare_svg or svg_nodes?(children),
                    do:
                      Map.merge(
                        base_style,
                        SvgStyleValidator.defaults(%{})
                        |> Map.put(:_svg_prepare, true)
                        |> Map.put(:_svg_measurement_only, mode == :prepare_svg)
                      ),
                    else: base_style

                compiled_rules = compile_selector_rules(rules)

                compute_styles = fn cache ->
                  root_id =
                    case ResourceContext.current() do
                      nil -> cache
                      document -> document.tree_scope_id
                    end

                  context = %{rules: compiled_rules, cache: cache, root_selector_id: root_id}

                  context =
                    if Map.get(compiled_rules.selectors, :positional_dependent, false),
                      do: Map.put(context, :root_children, children),
                      else: context

                  style_opts = Keyword.put(style_opts, :__render_cache__, cache)

                  case fragment_root_style(children, base_style, context, style_opts) do
                    {:ok, nil} ->
                      {:ok, Map.merge(%{type: :document, children: []}, link_metadata)}

                    {:ok, root_style} ->
                      with {:ok, styled_children, _counters} <-
                             style_children(
                               children,
                               root_style,
                               context,
                               [],
                               style_opts,
                               [],
                               []
                             ) do
                        {:ok,
                         %{type: :document, children: styled_children}
                         |> Map.merge(link_metadata)
                         |> Map.merge(
                           if MapSet.size(link_context.selected) == 0,
                             do: %{},
                             else: %{
                               link_expected_targets:
                                 HtmlLinkValidator.required_targets(styled_children)
                             }
                         )}
                      end

                    {:error, reason} ->
                      {:error, reason}
                  end
                end

                scoped_compute = fn cache ->
                  if Map.get(compiled_rules.selectors, :language_dependent, false) do
                    LanguageValidator.with_document(children, cache, fn ->
                      compute_styles.(cache)
                    end)
                  else
                    compute_styles.(cache)
                  end
                end

                result =
                  case ResourceContext.current() do
                    nil -> RenderCache.run(scoped_compute)
                    context -> scoped_compute.(context.scope)
                  end

                case result do
                  {:ok, styled_tree} ->
                    {:ok,
                     if(mode == :prepare_svg,
                       do: Map.put(styled_tree, :type, :svg_prepared_document),
                       else: styled_tree
                     )}

                  {:error, :invalid_document} ->
                    {:error, style_error_detail(dom, opts)}

                  {:error, {_reason, _diagnostic}} = error ->
                    error
                end
              else
                {:error, {:font_load_failed, sources}} ->
                  {:error, font_load_error(sources)}

                {:error, {_reason, _diagnostic}} = error ->
                  error

                {:error, :invalid_document} ->
                  {:error, style_error_detail(dom, opts)}
              end

            {:error, {_reason, _diagnostic}} = error ->
              error
          end
        end)
      end)
    end)
  end

  @doc false
  @spec load_stylesheets(term(), [render_option()]) ::
          {:ok, [stylesheet_entry()]}
          | {:error, :invalid_document | :invalid_stylesheet_options | {atom(), map()}}
  def load_stylesheets(dom, opts) do
    font_options = Font.normalize_options(opts)

    case HtmlValidator.validate_style_input(dom, opts, font_options) do
      :ok ->
        {:ok, opts} = font_options
        children = dom.children

        with {:ok, media_context} <-
               PrintMediaValidator.renderer_context(opts),
             do: stylesheet_entries(children, opts, media_context)

      {:error, {:invalid_options, _diagnostic}} ->
        {:error, :invalid_stylesheet_options}

      {:error, {_reason, _diagnostic}} = error ->
        error
    end
  end

  @doc false
  @spec prepare_stylesheets(term(), [render_option()]) ::
          {:ok, [map()]} | {:error, atom() | {atom(), map()}}
  def prepare_stylesheets(dom, opts) do
    with {:ok, entries} <- load_stylesheets(dom, opts),
         {:ok, media_context} <-
           PrintMediaValidator.renderer_context(opts),
         do: prepare_stylesheet_entries(entries, media_context)
  end

  defp prepare_stylesheet_entries(entries, media_context) do
    document_id =
      case ResourceContext.current() do
        nil -> nil
        context -> context.document_id
      end

    RenderCache.fetch_stylesheet(
      {:entries, document_id, entries,
       Enum.map(entries, &PrintMediaValidator.cache_context(&1.css, media_context))},
      fn ->
        Enum.reduce_while(entries, {:ok, []}, fn entry, {:ok, prepared_entries} ->
          case CssParser.prepare_stylesheet(entry.css, media_context) do
            {:ok, prepared} ->
              {:cont, {:ok, [Map.put(entry, :prepared, prepared) | prepared_entries]}}

            {:error, _} = error ->
              {:halt, error}
          end
        end)
        |> case do
          {:ok, entries} -> {:ok, Enum.reverse(entries)}
          {:error, _} = error -> error
        end
      end
    )
  end

  defp style_children(
         children,
         inherited_style,
         rules,
         ancestors,
         opts,
         parent_counters,
         latest_counters
       ) do
    result =
      Enum.reduce_while(
        children,
        {:ok, [], nil, latest_counters},
        fn child, {:ok, acc, previous_sibling_counters, latest_counters} ->
          counters =
            inherit_counters(parent_counters, previous_sibling_counters, latest_counters)

          case style_node(
                 child,
                 inherited_style,
                 rules,
                 ancestors,
                 opts,
                 counters,
                 parent_counters
               ) do
            {:ok, styled_children, nil, latest_counters} ->
              {:cont,
               {:ok, Enum.reverse(styled_children, acc), previous_sibling_counters,
                latest_counters}}

            {:ok, styled_children, element_counters, latest_counters} ->
              {:cont,
               {:ok, Enum.reverse(styled_children, acc), element_counters, latest_counters}}

            {:error, reason} ->
              {:halt, {:error, reason}}
          end
        end
      )

    case result do
      {:ok, styled_children, _previous_sibling_counters, latest_counters} ->
        {:ok, Enum.reverse(styled_children), latest_counters}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp svg_nodes?(nodes) do
    Enum.any?(nodes, fn node ->
      case node do
        %{type: :element, namespace: :svg} -> true
        %{type: :element, children: children} -> svg_nodes?(children)
        _ -> false
      end
    end)
  end

  defp style_node(node, inherited_style, rules, ancestors, opts, counters, parent_counters) do
    case node do
      %{type: :processing_instruction} ->
        {:ok, [], nil, counters}

      %{type: :element, namespace: :svg} ->
        with {:ok, style} <- element_style(node, inherited_style, rules, ancestors, opts),
             {:ok, children, counters} <-
               style_children(
                 node.children,
                 text_style(style),
                 rules,
                 [node | ancestors],
                 opts,
                 counters,
                 parent_counters
               ) do
          prepared = Map.merge(node, %{style: style, children: children})
          outer? = not Enum.any?(ancestors, &(Map.get(&1, :namespace) == :svg))

          if outer? and not Map.get(style, :_svg_measurement_only, false) do
            with {:ok, scene} <-
                   NativeElixirPdfUtilities.HtmlToPdf.SvgScene.prepare(
                     prepared,
                     Keyword.fetch!(opts, :__image_budget__)
                   ) do
              style = ensure_box_style(style)

              style =
                Map.put(
                  style,
                  :display,
                  if(style.display == :inline, do: :inline_block, else: style.display)
                )

              style = Map.put(style, :svg_scene, scene)
              {:ok, [Map.merge(prepared, %{style: style, children: []})], nil, counters}
            end
          else
            {:ok, [prepared], nil, counters}
          end
        end

      %{type: :text, text: text} when is_binary(text) ->
        {:ok,
         [
           Map.merge(node, %{
             type: :text,
             text:
               if(Map.get(inherited_style, :_svg_element, false),
                 do: text,
                 else:
                   transform_text(
                     text,
                     if(
                       Map.get(inherited_style, :_math_text, false) and
                         Map.get(inherited_style, :text_transform) == :math_auto,
                       do: :none,
                       else: Map.get(inherited_style, :text_transform, :none)
                     )
                   )
               ),
             style: text_style(inherited_style)
           })
         ], nil, counters}

      %{type: :element, tag: tag, attributes: attributes, children: children}
      when is_binary(tag) and is_map(attributes) and is_list(children) ->
        case tag do
          tag when tag in ["base", "style", "meta", "title"] ->
            {:ok, [], counters, counters}

          "head" ->
            {:ok, [], counters, counters}

          tag when tag in ["html", "body"] ->
            with {:ok, element_style} <-
                   element_style(node, inherited_style, rules, ancestors, opts) do
              case Map.get(element_style, :display) do
                :none ->
                  {:ok, [], counters, counters}

                _ ->
                  element_counters =
                    apply_counter_operations(
                      element_style,
                      counters,
                      parent_counters,
                      node._selector_id
                    )

                  with {:ok, before, before_counters} <-
                         generated_content_node(
                           node,
                           :before,
                           element_style,
                           rules,
                           ancestors,
                           element_counters
                         ),
                       {:ok, styled_children, counters} <-
                         style_children(
                           children,
                           text_style(element_style),
                           rules,
                           [node | ancestors],
                           opts,
                           before_counters,
                           before_counters
                         ),
                       {:ok, after_content, counters} <-
                         generated_content_node(
                           node,
                           :after,
                           element_style,
                           rules,
                           ancestors,
                           counters
                         ) do
                    sibling_counters =
                      include_counters_created_by(
                        before_counters,
                        counters,
                        {node._selector_id, :after}
                      )

                    rendered_children = before ++ styled_children ++ after_content

                    rendered =
                      case tag do
                        "body" ->
                          [
                            Map.merge(
                              if(Map.get(element_style, :_svg_prepare, false),
                                do: node,
                                else: %{}
                              ),
                              %{
                                type: :element,
                                tag: tag,
                                style: element_style,
                                children: rendered_children
                              }
                            )
                          ]

                        _ ->
                          Enum.map(rendered_children, fn child ->
                            case child do
                              %{tag: "body", style: style} ->
                                %{
                                  child
                                  | style:
                                      Map.put(
                                        style,
                                        :document_overflow,
                                        Map.get(element_style, :overflow, :visible)
                                      )
                                }

                              _ ->
                                child
                            end
                          end)
                      end

                    {:ok, rendered, sibling_counters, counters}
                  end
              end
            end

          "iframe" ->
            with {:ok, style} <- element_style(node, inherited_style, rules, ancestors, opts) do
              if style.display == :none do
                {:ok, [%{type: :element, tag: tag, style: style, children: []}], counters,
                 counters}
              else
                with {:ok, child_parent} <-
                       ResourceContext.rebase(
                         ResourceContext.current(),
                         {:document_url,
                          Keyword.fetch!(opts, :__document_url_context__).resolution_base}
                       ),
                     source <- ResourceValidator.iframe_source(attributes),
                     child_opts <-
                       Keyword.drop(opts, [
                         :__image_budget__,
                         :__link_context__,
                         :__render_cache__,
                         :__document_url_context__,
                         :__client_css_viewport__
                       ]),
                     {:ok, viewport} <-
                       RenderCache.fetch(
                         child_parent.scope,
                         {:iframe_document, child_parent.document_id, child_parent.tree_scope_id,
                          node._document_order, source, child_opts},
                         fn ->
                           EmbeddedViewport.prepare_iframe_document(
                             source,
                             child_parent,
                             child_opts
                           )
                         end
                       ) do
                  style =
                    style
                    |> EmbeddedViewportValidator.prepare_iframe_style()
                    |> Map.put(:embedded_document, viewport)

                  {:ok, [%{type: :element, tag: tag, style: style, children: []}], counters,
                   counters}
                end
              end
            end

          _ ->
            with {:ok, element_style} <-
                   element_style(node, inherited_style, rules, ancestors, opts) do
              case Map.get(element_style, :display) do
                :none ->
                  {:ok, [%{type: :element, tag: tag, style: element_style, children: []}],
                   counters, counters}

                :soft_break ->
                  {:ok, [%{type: :element, tag: tag, style: element_style, children: []}],
                   counters, counters}

                _ ->
                  element_counters =
                    apply_counter_operations(
                      element_style,
                      counters,
                      parent_counters,
                      node._selector_id
                    )

                  element_style =
                    if tag in ["input", "select", "textarea"] and
                         not Map.has_key?(node, :_html_inert) and
                         Map.get(element_style, :form_kind) != :input_button do
                      control = %{
                        tag: tag,
                        attributes: attributes,
                        children: children,
                        id: node._selector_id,
                        order: node._document_order,
                        initial_state: Map.fetch!(node, :_initial_form_state)
                      }

                      element_style
                      |> Map.put(:_form_control, control)
                      |> Map.put(:_form_owner, node._selector_id)
                    else
                      element_style
                    end

                  element_style =
                    Map.merge(element_style, Map.take(node, [:_table_model, :_table_cell]))

                  with rendered_children when is_list(rendered_children) <-
                         if(tag == "details",
                           do: HtmlValidator.disclosure_children(node),
                           else: static_form_children(node, attributes, children)
                         ),
                       {:ok, before, before_counters} <-
                         generated_content_node(
                           node,
                           :before,
                           element_style,
                           rules,
                           ancestors,
                           element_counters
                         ),
                       {:ok, styled_children, counters} <-
                         style_children(
                           rendered_children,
                           if(get_in(node, [:_initial_form_state, :placeholder_shown]),
                             do:
                               Map.put(
                                 text_style(element_style),
                                 :color,
                                 {117 / 255, 117 / 255, 117 / 255}
                               ),
                             else: text_style(element_style)
                           ),
                           rules,
                           [node | ancestors],
                           opts,
                           before_counters,
                           before_counters
                         ),
                       {:ok, after_content, counters} <-
                         generated_content_node(
                           node,
                           :after,
                           element_style,
                           rules,
                           ancestors,
                           counters
                         ) do
                    sibling_counters =
                      include_counters_created_by(
                        before_counters,
                        counters,
                        {node._selector_id, :after}
                      )

                    rendered_children = before ++ styled_children ++ after_content

                    element_style =
                      if tag in ~w(progress meter),
                        do: Map.put(element_style, :_gauge_fallback, children),
                        else: element_style

                    rendered_children =
                      if tag in ~w(ul ol),
                        do:
                          HtmlValidator.resolve_html_list_ordinals(
                            tag,
                            attributes,
                            rendered_children
                          ),
                        else: rendered_children

                    element =
                      Map.merge(
                        if(Map.get(element_style, :_svg_prepare, false), do: node, else: %{}),
                        %{
                          type: :element,
                          tag: tag,
                          style: element_style,
                          children: rendered_children
                        }
                      )

                    element =
                      if Map.get(node, :namespace) == :mathml,
                        do: Map.merge(element, Map.take(node, [:namespace, :attributes])),
                        else: element

                    result =
                      if Map.get(node, :namespace) == :mathml and
                           not Enum.any?(ancestors, &(Map.get(&1, :namespace) == :mathml)) do
                        with {:ok, measured} <-
                               MathLayout.measure(element, %{
                                 resource_context: ResourceContext.current(),
                                 unsupported_glyphs:
                                   Keyword.get(opts, :unsupported_glyphs, :replace)
                               }) do
                          atom_style =
                            element_style
                            |> Map.put(:display, :inline_block)
                            |> Map.put(:_math_box, measured)

                          atom = %{element | style: atom_style, children: []}

                          if Map.get(element_style, :_math_display) == :block do
                            {:ok,
                             [
                               %{
                                 element
                                 | children: [atom],
                                   style:
                                     element_style
                                     |> Map.put(:display, :block)
                                     |> Map.put(:_math_flow, true)
                                     |> Map.put(:text_align, :center)
                               }
                             ]}
                          else
                            {:ok, [atom]}
                          end
                        end
                      else
                        {:ok, [element]}
                      end

                    with {:ok, rendered} <- result do
                      rendered =
                        if tag == "summary",
                          do: HtmlValidator.resolve_html_list_ordinals("ul", %{}, rendered),
                          else: rendered

                      {:ok, rendered, sibling_counters, counters}
                    end
                  end
              end
            end
        end
    end
  end

  defp generated_content_node(node, pseudo_element, inherited_style, rules, ancestors, counters) do
    declarations =
      if Map.get(node, :_default_disclosure_summary, false),
        do: [],
        else: matching_declarations(rules, node, ancestors, pseudo_element)

    case declarations do
      [] ->
        {:ok, [], counters}

      matched_declarations ->
        generated_style =
          inherited_style
          |> text_style()
          |> Map.put(:display, :inline)

        case apply_author_styles(
               generated_style,
               node,
               ancestors,
               rules,
               pseudo_element,
               matched_declarations
             ) do
          {:ok, style} ->
            counters =
              apply_counter_operations(
                style,
                counters,
                counters,
                {node._selector_id, pseudo_element}
              )

            text =
              generated_content_text(Map.get(style, :content, :none), node.attributes, counters)

            case {Map.get(style, :display), text} do
              {:none, _text} ->
                {:ok, [], counters}

              {_display, ""} ->
                {:ok, [], counters}

              {_display, text} ->
                child = %{
                  type: :text,
                  text: transform_text(text, Map.get(style, :text_transform, :none)),
                  style: text_style(style)
                }

                {:ok,
                 [
                   %{
                     type: :element,
                     tag: "::#{pseudo_element}",
                     style: style,
                     children: [child]
                   }
                 ], counters}
            end

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp inherit_counters(parent_counters, previous_sibling_counters, latest_counters) do
    counters =
      case previous_sibling_counters do
        nil ->
          parent_counters

        previous_sibling_counters ->
          Enum.reduce(previous_sibling_counters, parent_counters, fn counter, acc ->
            {name, _creator, _value} = counter

            case Enum.any?(acc, fn {counter_name, _creator, _value} -> counter_name == name end) do
              true -> acc
              false -> acc ++ [counter]
            end
          end)
      end

    Enum.map(counters, fn {name, creator, _value} ->
      {^name, ^creator, latest_value} =
        Enum.find(Enum.reverse(latest_counters), fn
          {counter_name, counter_creator, _value} ->
            counter_name == name and counter_creator == creator
        end)

      {name, creator, latest_value}
    end)
  end

  defp apply_counter_operations(style, counters, parent_counters, creator) do
    counters =
      Enum.reduce(Map.get(style, :counter_reset, []), counters, fn {name, value}, acc ->
        reset_counter(acc, parent_counters, name, value, creator)
      end)

    Enum.reduce(Map.get(style, :counter_increment, []), counters, fn {name, increment}, acc ->
      case innermost_counter_index(acc, name) do
        nil ->
          acc ++ [{name, creator, increment}]

        index ->
          List.update_at(acc, index, fn {^name, counter_creator, value} ->
            {name, counter_creator, value + increment}
          end)
      end
    end)
  end

  defp include_counters_created_by(counters, latest_counters, creator) do
    Enum.reduce(latest_counters, counters, fn
      {_name, ^creator, _value} = counter, acc ->
        acc ++ [counter]

      _counter, acc ->
        acc
    end)
  end

  defp reset_counter(counters, parent_counters, name, value, creator) do
    case innermost_counter_index(counters, name) do
      nil ->
        counters ++ [{name, creator, value}]

      index ->
        {^name, counter_creator, _old_value} = Enum.at(counters, index)

        inherited_from_parent? =
          Enum.any?(parent_counters, fn {parent_name, parent_creator, _value} ->
            parent_name == name and parent_creator == counter_creator
          end)

        case counter_creator == creator or not inherited_from_parent? do
          true -> List.replace_at(counters, index, {name, creator, value})
          false -> counters ++ [{name, creator, value}]
        end
    end
  end

  defp innermost_counter_index(counters, name) do
    counters
    |> Enum.with_index()
    |> Enum.reduce(nil, fn
      {{^name, _creator, _value}, index}, _current -> index
      {_counter, _index}, current -> current
    end)
  end

  defp generated_content_text(content, attributes, counters) do
    Enum.map_join(content_parts(content), "", fn part ->
      text =
        case part do
          {:string, text} ->
            text

          {:attr, name} ->
            Map.get(attributes, name, "")

          {:counter, name} ->
            case innermost_counter_index(counters, name) do
              nil -> "0"
              index -> counters |> Enum.at(index) |> elem(2) |> Integer.to_string()
            end
        end

      HtmlValidator.reserve_render_resource(:max_rendered_text_bytes, byte_size(text), :style)
      text
    end)
  end

  defp content_parts(content) do
    case content do
      parts when is_list(parts) -> parts
      _ -> []
    end
  end

  defp fragment_root_style(children, base_style, rules, opts) do
    case Enum.any?(children, &document_container?/1) do
      true ->
        {:ok, base_style}

      false ->
        body_node = %{
          type: :element,
          tag: "body",
          attributes: %{},
          children: [],
          _selector_classes: MapSet.new(),
          _selector_id: make_ref()
        }

        with {:ok, body_style} <- element_style(body_node, base_style, rules, [], opts) do
          case Map.get(body_style, :display) do
            :none -> {:ok, nil}
            _ -> {:ok, text_style(body_style)}
          end
        end
    end
  end

  defp document_container?(node) do
    case node do
      %{type: :element, tag: tag} when tag in ["html", "body"] -> true
      _ -> false
    end
  end

  defp element_style(
         %{tag: tag, attributes: attributes} = node,
         inherited_style,
         rules,
         ancestors,
         opts
       ) do
    inherited_font_size = Map.fetch!(inherited_style, :font_size)

    popover_state =
      if Map.get(node, :namespace, :html) == :html,
        do: HtmlValidator.html_popover_state(attributes),
        else: :none

    defaults =
      if popover_state != :none and tag != "input" do
        %{display: :none}
      else
        case if(Map.get(node, :namespace) == :svg, do: nil, else: tag) do
          "html" ->
            %{display: :block}

          tag when tag in ["script", "template", "noembed"] ->
            %{display: :none}

          "body" ->
            defaults = block_defaults(inherited_font_size, 400, 0.0)

            case ResourceContext.current() do
              %{depth: depth} when depth > 0 -> Map.put(defaults, :margin, edges(6.0))
              _ -> defaults
            end

          tag
          when tag in ~w(form article aside div footer header main nav section figcaption hgroup search dt details) ->
            block_defaults(inherited_font_size, 400, 0.0)

          "fieldset" ->
            block_defaults(inherited_font_size, 400, 0.0)
            |> Map.merge(%{
              margin: edges(0.0, 1.5, 0.0, 1.5),
              border_widths: edges(1.5),
              border_styles: edges(:groove),
              # ThreeDFace in the pinned Chromium print theme is rgb(239,239,239).
              border_colors: edges({239 / 255, 239 / 255, 239 / 255}),
              _fieldset_default_padding: true,
              _fieldset: true
            })

          "legend" ->
            alignment =
              case String.downcase(Map.get(attributes, "align", ""), :ascii) do
                "center" -> :center
                "right" -> :flex_end
                _ -> :flex_start
              end

            block_defaults(inherited_font_size, 400, 0.0)
            |> Map.merge(%{padding: edges(0.0, 1.5, 0.0, 1.5), justify_self: alignment})

          "summary" ->
            case Map.fetch(node, :_disclosure_open) do
              {:ok, open?} ->
                list_item_defaults(inherited_font_size)
                |> Map.put(:list_style_position, :inside)
                |> Map.put(
                  :list_marker_type,
                  if(open?, do: :disclosure_open, else: :disclosure_closed)
                )

              :error ->
                block_defaults(inherited_font_size, 400, 0.0)
            end

          "address" ->
            Map.put(block_defaults(inherited_font_size, 400, 0.0), :font_style, :italic)

          "center" ->
            Map.put(block_defaults(inherited_font_size, 400, 0.0), :text_align, :center)

          tag when tag in ~w(blockquote figure) ->
            block_defaults(inherited_font_size, 400, inherited_font_size)
            |> Map.put(:margin, edges(inherited_font_size, 30.0, inherited_font_size, 30.0))
            |> Map.put(:_margin_before_em, 1.0)
            |> Map.put(:_margin_after_em, 1.0)

          "dl" ->
            block_defaults(inherited_font_size, 400, inherited_font_size)
            |> Map.put(:_margin_before_em, 1.0)
            |> Map.put(:_margin_after_em, 1.0)

          "dd" ->
            Map.put(
              block_defaults(inherited_font_size, 400, 0.0),
              :margin,
              edges(0.0, 0.0, 0.0, 30.0)
            )

          tag when tag in ~w(pre listing xmp plaintext) ->
            block_defaults(inherited_font_size, 400, inherited_font_size)
            |> Map.put(:white_space, :pre)
            |> Map.put(:font_families, ["DejaVu Sans Mono"])
            |> Map.put(:_margin_before_em, 1.0)
            |> Map.put(:_margin_after_em, 1.0)

          "p" ->
            inherited_font_size
            |> block_defaults(400, inherited_font_size)
            |> Map.put(:_margin_before_em, 1.0)
            |> Map.put(:_margin_after_em, 1.0)

          "h1" ->
            block_defaults(inherited_font_size * 2, 700, 0.0)
            |> Map.put(:_margin_before_em, 0.67)
            |> Map.put(:_margin_after_em, 0.67)

          "h2" ->
            block_defaults(inherited_font_size * 1.5, 700, 0.0)
            |> Map.put(:_margin_before_em, 0.83)
            |> Map.put(:_margin_after_em, 0.83)

          "h3" ->
            block_defaults(inherited_font_size * 1.17, 700, 0.0)
            |> Map.put(:_margin_before_em, 1)
            |> Map.put(:_margin_after_em, 1)

          "h4" ->
            block_defaults(inherited_font_size * 1, 700, 0.0)
            |> Map.put(:_margin_before_em, 1.33)
            |> Map.put(:_margin_after_em, 1.33)

          "h5" ->
            block_defaults(inherited_font_size * 0.83, 700, 0.0)
            |> Map.put(:_margin_before_em, 1.67)
            |> Map.put(:_margin_after_em, 1.67)

          "h6" ->
            block_defaults(inherited_font_size * 0.67, 700, 0.0)
            |> Map.put(:_margin_before_em, 2.33)
            |> Map.put(:_margin_after_em, 2.33)

          tag when tag in ~w(ul ol) ->
            depth = Enum.count(ancestors, &(&1.tag in ~w(ul ol)))

            marker =
              HtmlValidator.html_list_marker_type(tag, attributes) ||
                if(tag == "ol",
                  do: :decimal,
                  else: Enum.at([:disc, :circle, :square], min(depth, 2))
                )

            defaults = list_defaults(marker, inherited_font_size)

            if depth == 0 do
              defaults
            else
              defaults
              |> Map.put(:margin, edges(0.0))
              |> Map.put(:margin_after, 0.0)
              |> Map.delete(:_margin_before_em)
              |> Map.delete(:_margin_after_em)
            end

          "li" ->
            defaults =
              list_item_defaults(inherited_font_size)
              |> Map.put(
                :_list_value,
                HtmlValidator.parse_html_integer(Map.get(attributes, "value"))
              )

            case HtmlValidator.html_list_marker_type(tag, attributes) do
              nil -> defaults
              marker -> Map.put(defaults, :list_marker_type, marker)
            end

          "table" ->
            table_defaults(inherited_font_size)

          "caption" ->
            table_caption_defaults(inherited_font_size)

          "colgroup" ->
            table_column_defaults(:group, node._table_spans.span)

          "col" ->
            table_column_defaults(:column, node._table_spans.span)

          "thead" ->
            table_row_group_defaults(:head)

          "tbody" ->
            table_row_group_defaults(:body)

          "tfoot" ->
            table_row_group_defaults(:foot)

          "tr" ->
            table_row_defaults()

          "th" ->
            table_cell_defaults(:header, node._table_spans, inherited_font_size)

          "td" ->
            table_cell_defaults(:data, node._table_spans, inherited_font_size)

          "strong" ->
            %{display: :inline, font_weight: 700}

          "b" ->
            %{display: :inline, font_weight: 700}

          "em" ->
            %{display: :inline, font_style: :italic}

          "i" ->
            %{display: :inline, font_style: :italic}

          tag when tag in ["u", "ins"] ->
            %{display: :inline, text_decoration_line: [:underline]}

          tag when tag in ["s", "del", "strike"] ->
            %{display: :inline, text_decoration_line: [:line_through]}

          "mark" ->
            %{display: :inline, background_color: {1, 1, 0}, color: {0, 0, 0}}

          tag when tag in ~w(cite dfn var) ->
            %{display: :inline, font_style: :italic}

          tag when tag in ~w(code samp kbd tt) ->
            %{display: :inline, font_families: ["DejaVu Sans Mono"]}

          "big" ->
            %{display: :inline, font_size: inherited_font_size * 1.2}

          "small" ->
            %{display: :inline, font_size: inherited_font_size / 1.2}

          tag when tag in ~w(span abbr data time noscript label) ->
            %{display: :inline}

          "a" ->
            link_defaults(attributes, opts)

          "nobr" ->
            %{display: :inline, white_space: :nowrap}

          "wbr" ->
            if Enum.any?(ancestors, &(&1.tag == "nobr")),
              do: %{display: :soft_break, white_space: :normal},
              else: %{display: :soft_break}

          "br" ->
            %{display: :line_break}

          "hr" ->
            with {:ok, hints} <- HtmlValidator.horizontal_rule_hints(attributes),
                 {:ok, gray} <- HtmlValidator.parse_css_color("gray", nil) do
              block_defaults(inherited_font_size, 400, 0.0)
              |> Map.merge(%{
                color: gray,
                border_styles: edges(:inset),
                border_widths: edges(0.75),
                margin: %{top: 0.0, right: :auto, bottom: 0.0, left: :auto},
                _margin_before_em: {:css_layout_em, 0.5},
                _margin_after_em: {:css_layout_em, 0.5},
                overflow: :hidden
              })
              |> Map.merge(hints)
            end

          "picture" ->
            %{display: :inline}

          "source" ->
            %{display: :none}

          "iframe" ->
            with {:ok, hints} <-
                   HtmlValidator.html_dimension_hints(
                     attributes,
                     [{"width", :width}, {"height", :height}],
                     "iframe"
                   ) do
              block_defaults(inherited_font_size, 400, 0.0)
              |> Map.merge(%{
                display: :inline_block,
                width: 225.0,
                height: 112.5,
                overflow: :hidden,
                margin: edges(0.0),
                padding: edges(0.0),
                border_widths: edges(1.5),
                border_styles: edges(:inset)
              })
              |> Map.merge(hints)
            end

          "img" ->
            image_defaults(node, ancestors, opts, inherited_font_size)

          "input" ->
            form_control_defaults(
              HtmlValidator.input_type(Map.get(attributes, "type", "text")),
              inherited_font_size
            )
            |> then(fn defaults ->
              case Map.get(node._initial_form_state, :size) do
                nil ->
                  defaults

                size ->
                  Map.merge(defaults, %{
                    width: {:control_columns, size},
                    _control_column_count: size,
                    white_space: :pre,
                    overflow: :hidden
                  })
              end
            end)

          "select" ->
            form_control_defaults("select", inherited_font_size)

          "textarea" ->
            form_control_defaults("textarea", inherited_font_size)
            |> Map.merge(%{
              width: {:control_columns, node._initial_form_state.cols},
              height: {:control_rows, node._initial_form_state.rows},
              _control_column_count: node._initial_form_state.cols,
              _control_row_count: node._initial_form_state.rows,
              white_space: if(node._initial_form_state.wrap == :off, do: :pre, else: :pre_wrap),
              overflow_wrap:
                if(node._initial_form_state.wrap == :off, do: :normal, else: :break_word),
              overflow: :auto
            })

          "button" ->
            form_control_defaults(:button, inherited_font_size)

          tag when tag in ~w(progress meter) ->
            %{
              display: :inline_block,
              width: {:gauge_em, if(tag == "progress", do: 10, else: 5)},
              height: {:gauge_em, 1},
              vertical_align: {:gauge_em, -0.2},
              appearance: :auto,
              _gauge: Map.fetch!(node, :_initial_gauge_state)
            }

          _ ->
            %{display: :inline}
        end
      end

    defaults =
      cond do
        Map.get(node, :namespace) == :mathml ->
          block_defaults(inherited_font_size, 400, 0.0)
          |> Map.merge(MathLayoutValidator.defaults(node, inherited_style))
          |> Map.merge(
            MathLayoutValidator.schema_child_defaults(node, inherited_style, ancestors)
          )

        Map.get(node, :namespace) == :svg ->
          Map.merge(SvgStyleValidator.defaults(inherited_style), %{
            display: :inline,
            overflow: if(tag == "svg", do: :hidden, else: :visible)
          })

        true ->
          defaults
      end

    defaults =
      if popover_state == :none do
        defaults
      else
        Map.merge(defaults, %{display: :none, _popover_state: popover_state})
      end

    defaults =
      case defaults do
        {:ok, defaults} -> defaults
        defaults -> defaults
      end

    case defaults do
      defaults when is_map(defaults) ->
        defaults =
          if Map.get(defaults, :font_weight) == 400 and
               tag not in ["input", "select", "textarea", "button"] do
            Map.delete(defaults, :font_weight)
          else
            defaults
          end

        # Canonical render validation permits only active HTML element defaults here.
        defaults =
          if Map.get(node, :namespace, :html) == :html and
               HtmlValidator.html_hidden_state(attributes) == :hidden do
            Map.put(defaults, :display, :none)
          else
            defaults
          end

        style =
          inherited_style
          |> text_style()
          |> Map.merge(defaults)
          |> Map.put(:_inherited_color, Map.fetch!(inherited_style, :color))
          |> Map.put(:_root_element, tag in ["html", "body"] and ancestors == [])

        style =
          if Map.get(style, :_svg_prepare, false),
            do: Map.put(style, :_svg_element, Map.get(node, :namespace) == :svg),
            else: style

        style =
          if Map.has_key?(node, :_html_inert),
            do: Map.drop(style, [:link_url, :link_destination]),
            else: style

        style =
          case Map.fetch(node, :_heading_level) do
            {:ok, level} -> Map.put(style, :heading_level, level)
            :error -> style
          end

        target = HtmlLinkValidator.target(node, Keyword.fetch!(opts, :__link_context__))

        style =
          if target == nil,
            do: style,
            else:
              Map.merge(style, %{
                html_target: target,
                html_target_source:
                  Map.get(
                    node.attributes,
                    "id",
                    Map.get(node.attributes, "name", "HTML fragment target")
                  )
              })

        result =
          tag
          |> finalize_element_style(
            if(Map.get(node, :_default_disclosure_summary, false),
              do: apply_author_styles(style, node, ancestors, rules, nil, []),
              else: apply_author_styles(style, node, ancestors, rules)
            ),
            Keyword.fetch!(opts, :__image_budget__),
            opts
          )

        result =
          case result do
            {:ok, style} ->
              if tag == "fieldset" or Map.get(style, :_fieldset_precision, false) do
                # LayoutNG's used line heights retain fixed 1/64 CSS-pixel precision.
                height = Float.floor(style.line_height / 0.75 * 64) / 64 * 0.75

                {:ok,
                 style |> Map.put(:line_height, height) |> Map.put(:_fieldset_precision, true)}
              else
                result
              end

            _ ->
              result
          end

        case {tag, ancestors, result} do
          {tag, [], {:ok, style}} when tag in ["html", "body"] ->
            {:ok, Map.put(style, :_root_font_size, Map.fetch!(style, :font_size))}

          _ ->
            result
        end

      {:error, {_reason, _diagnostic}} = error ->
        error
    end
  end

  defp finalize_element_style(tag, result, image_budget, opts) do
    case {tag, result} do
      {tag, {:ok, style}} when tag in ~w(progress meter) ->
        style =
          Enum.reduce([:width, :height, :vertical_align], style, fn property, style ->
            case Map.get(style, property) do
              {:gauge_em, factor} -> Map.put(style, property, factor * style.font_size)
              _ -> style
            end
          end)

        style =
          if style.display == :inline, do: Map.put(style, :display, :inline_block), else: style

        finalize_background_image_style(
          NativeElixirPdfUtilities.Validators.HtmlGaugeValidator.prepare_style(style),
          opts,
          image_budget
        )

      {tag, {:ok, style}}
      when tag in ~w(input textarea) and not is_map_key(style, :_popover_state) and
             style.form_kind != :hidden ->
        dimensions =
          Enum.reduce_while([:width, :height], {:ok, style}, fn property, {:ok, style} ->
            dimension =
              case {Map.get(style, property), property} do
                {:auto, :width} when is_map_key(style, :_control_column_count) ->
                  {:control_columns, style._control_column_count}

                {:auto, :height} when is_map_key(style, :_control_row_count) ->
                  {:control_rows, style._control_row_count}

                {value, _} ->
                  value
              end

            extent =
              case dimension do
                {:control_columns, count} ->
                  initial_control_width(style, count)

                {:control_rows, count} ->
                  Float.floor(style.line_height / 0.75 * 64) / 64 * 0.75 * count

                _ ->
                  nil
              end

            if is_nil(extent) do
              {:cont, {:ok, style}}
            else
              extent =
                if Map.get(style, :box_sizing, :content_box) == :border_box,
                  do:
                    extent +
                      if(property == :width,
                        do:
                          style.padding.left + style.padding.right + style.border_widths.left +
                            style.border_widths.right,
                        else:
                          style.padding.top + style.padding.bottom + style.border_widths.top +
                            style.border_widths.bottom
                      ),
                  else: extent

              case NativeElixirPdfUtilities.Validators.HtmlFormValidator.validate_initial_control_dimension(
                     extent,
                     property
                   ) do
                {:ok, extent} -> {:cont, {:ok, Map.put(style, property, extent)}}
                error -> {:halt, error}
              end
            end
          end)

        with {:ok, dimensions} <- dimensions,
             do:
               finalize_background_image_style(
                 Map.drop(dimensions, [:_control_column_count, :_control_row_count]),
                 opts,
                 image_budget
               )

      {"input", {:ok, %{form_kind: :hidden} = style}} ->
        # HTML's UA !important rule wins over author display declarations.
        {:ok, style |> Map.put(:display, :none) |> Map.delete(:_popover_state)}

      {tag, {:ok, %{_popover_state: state} = style}} ->
        with :ok <- HtmlValidator.validate_initial_popover_style(state, style, tag) do
          {:ok, Map.drop(style, [:_popover_state, :background_image_source])}
        end

      {"source", {:ok, style}} ->
        {:ok, style |> Map.put(:display, :none) |> Map.delete(:background_image_source)}

      {"fieldset", {:ok, style}} ->
        # HTML gives the fieldset an outer formatting context, and routes the
        # computed flex/grid display to its anonymous content box.
        display = style.display

        used_display =
          case display do
            :none ->
              :none

            value when value in [:inline, :inline_block, :inline_flex, :inline_grid] ->
              :inline_block

            _ ->
              :block
          end

        content_display =
          case display do
            value when value in [:flex, :inline_flex] -> :flex
            value when value in [:grid, :inline_grid] -> :grid
            _ -> :block
          end

        style =
          style
          |> Map.put(:display, used_display)
          |> Map.put(:_fieldset_content_display, content_display)

        finalize_background_image_style(style, opts, image_budget)

      {"wbr", {:ok, style}} ->
        # The element creates a word-break object rather than an authored CSS box.
        style = if style.display == :none, do: style, else: Map.put(style, :display, :soft_break)
        {:ok, Map.delete(style, :background_image_source)}

      {"noembed", {:ok, style}} ->
        {:ok, HtmlValidator.noembed_render_style(style)}

      {"img", {:ok, %{display: :none} = style}} ->
        {:ok, style}

      {"img", {:ok, style}} ->
        with {:ok, style} <-
               finalize_image_style(
                 style
                 |> Map.put(
                   :image_display,
                   if(style.display in [:inline, :inline_block], do: :inline, else: :block)
                 )
                 |> Map.put(:display, :image),
                 image_budget,
                 opts
               ) do
          finalize_background_image_style(style, opts, image_budget)
        end

      {"select", {:ok, style}} ->
        # Native menu lists reserve internal inline space in addition to CSS padding.
        # Chromium's default theme uses 4px at the start and 16px for the arrow.
        padding = style.padding

        style = %{
          style
          | padding: %{padding | left: padding.left + 3.0, right: padding.right + 12.0}
        }

        finalize_background_image_style(style, opts, image_budget)

      {_tag, {:ok, style}} ->
        finalize_background_image_style(style, opts, image_budget)

      {_tag, result} ->
        result
    end
  end

  defp finalize_image_style(style, image_budget, opts) do
    case {Map.get(style, :display), Map.get(style, :image), Map.get(style, :svg_image)} do
      {:image, nil, svg} when is_binary(svg) ->
        with {:ok, image} <-
               rasterize_svg_image(svg, svg_raster_options(style), image_budget, opts),
             {:ok, image} <-
               NativeElixirPdfUtilities.Validators.ResponsiveImageValidator.correct_natural_dimensions(
                 image,
                 Map.fetch!(style, :_image_selection)
               ) do
          {:ok, style |> Map.put(:image, image) |> Map.delete(:svg_image)}
        else
          {:error, {reason, diagnostic}} ->
            {:error,
             {reason, Diagnostics.with_context(diagnostic, source: style._image_selection.url)}}
        end

      _ ->
        {:ok, Map.delete(style, :svg_image)}
    end
  end

  defp finalize_background_image_style(style, opts, image_budget) do
    style =
      style
      |> Map.put_new(:background_position, {{:percent, 0.0}, {:percent, 0.0}})
      |> Map.put_new(:background_repeat, :repeat)
      |> Map.put_new(:background_size, {:auto, :auto})

    case Map.get(style, :background_image_source) do
      nil ->
        {:ok, style}

      source ->
        case load_image_style(source, opts, :background_image) do
          {:ok, %{image: image}} ->
            {:ok,
             style
             |> Map.put(:background_image, image)
             |> Map.delete(:background_image_source)}

          {:ok, %{svg_image: svg}} ->
            with {:ok, image} <- rasterize_svg_image(svg, [], image_budget, opts) do
              {:ok,
               style
               |> Map.put(:background_image, image)
               |> Map.delete(:background_image_source)}
            end

          {:error, {_reason, _diagnostic}} = error ->
            error
        end
    end
  end

  defp put_line_height(style) do
    case Map.get(style, :line_height_multiplier) do
      multiplier when is_number(multiplier) ->
        style
        |> Map.put(:line_height, Map.fetch!(style, :font_size) * multiplier)
        |> Map.delete(:line_height_explicit)

      _ ->
        case Map.get(style, :line_height_normal, false) do
          true ->
            style
            |> Map.put(:line_height, normal_line_height(style))
            |> Map.delete(:line_height_explicit)

          false ->
            Map.delete(style, :line_height_explicit)
        end
    end
  end

  defp normal_line_height(style) do
    font_size = Map.fetch!(style, :font_size)

    %{
      type: :embedded,
      ascent: ascent,
      descent: descent,
      line_gap: line_gap,
      units_per_em: units_per_em
    } =
      Map.fetch!(style, :font_face)

    ascent_pixels = round(ascent / units_per_em * font_size / 0.75)
    descent_pixels = round(-descent / units_per_em * font_size / 0.75)
    line_gap_pixels = round(max(line_gap, 0) / units_per_em * font_size / 0.75)
    max((ascent_pixels + descent_pixels + line_gap_pixels) * 0.75, font_size)
  end

  defp text_style(style) do
    inherited =
      Map.take(style, [
        :_math_text,
        :math_style,
        :math_shift,
        :math_depth,
        :_fieldset_precision,
        :_form_owner,
        :_text_decorations,
        :_custom_properties,
        :_font_registry,
        :_root_font_size,
        :color,
        :accent_color,
        :font_face,
        :font_families,
        :font_family,
        :font_size,
        :font_style,
        :font_weight,
        :letter_spacing,
        :line_break,
        :overflow_wrap,
        :word_break,
        :line_height,
        :line_height_multiplier,
        :line_height_normal,
        :list_marker_type,
        :list_style_position,
        :link_url,
        :link_destination,
        :text_align,
        :text_transform,
        :white_space
      ])

    if Map.get(style, :_svg_prepare, false) do
      inherited
      |> Map.merge(SvgStyleValidator.inherit(style))
      |> Map.put(:_svg_inherited, SvgStyleValidator.parent_properties(style))
      |> Map.put(:_svg_prepare, true)
      |> Map.put(:_svg_measurement_only, Map.get(style, :_svg_measurement_only, false))
      |> Map.put(:_svg_element, Map.get(style, :_svg_element, false))
    else
      inherited
    end
  end

  defp block_defaults(font_size, font_weight, margin_bottom) do
    %{
      background_color: nil,
      background_image: nil,
      background_position: {{:percent, 0.0}, {:percent, 0.0}},
      background_repeat: :repeat,
      background_size: {:auto, :auto},
      break_inside: :auto,
      border_color: :current_color,
      border_colors: edges(:current_color),
      border_radius: 0.0,
      border_styles: edges(:none),
      border_widths: edges(@medium_border_width),
      box_sizing: :content_box,
      display: :block,
      font_size: font_size,
      font_weight: font_weight,
      margin: edges(0.0, 0.0, margin_bottom, 0.0),
      margin_after: margin_bottom,
      padding: edges(0.0),
      position: :static,
      offsets: edges(:auto),
      z_index: :auto
    }
  end

  defp list_defaults(marker_type, font_size) do
    block_defaults(font_size, 400, font_size)
    |> Map.merge(%{
      _margin_before_em: 1.0,
      _margin_after_em: 1.0,
      display: :list,
      list_marker_type: marker_type,
      list_style_position: :outside,
      margin: edges(font_size, 0.0, font_size, 0.0),
      padding: edges(0.0, 0.0, 0.0, 30.0)
    })
  end

  defp list_item_defaults(font_size) do
    Map.put(block_defaults(font_size, 400, 0.0), :display, :list_item)
  end

  defp table_defaults(font_size) do
    block_defaults(font_size, 400, 0.0)
    |> Map.merge(%{
      display: :table,
      border_collapse: :separate,
      border_spacing: {1.5, 1.5},
      margin: edges(0.0),
      padding: edges(0.0),
      table_layout: :auto
    })
  end

  defp table_caption_defaults(font_size) do
    block_defaults(font_size, 400, 0.0)
    |> Map.merge(%{
      display: :table_caption,
      text_align: :center
    })
  end

  defp table_row_group_defaults(section) do
    %{
      background_color: nil,
      border_color: :current_color,
      border_colors: edges(:current_color),
      border_radius: 0.0,
      border_styles: edges(:none),
      border_widths: edges(@medium_border_width),
      box_sizing: :content_box,
      display: :table_row_group,
      padding: edges(0.0),
      table_section: section
    }
  end

  defp table_row_defaults do
    %{
      background_color: nil,
      border_color: :current_color,
      border_colors: edges(:current_color),
      border_radius: 0.0,
      border_styles: edges(:none),
      border_widths: edges(@medium_border_width),
      box_sizing: :content_box,
      display: :table_row,
      padding: edges(0.0)
    }
  end

  defp table_column_defaults(kind, span) do
    %{
      background_color: nil,
      display: if(kind == :group, do: :table_column_group, else: :table_column),
      span: span
    }
  end

  defp table_cell_defaults(kind, spans, font_size) do
    base = %{
      background_color: nil,
      border_color: :current_color,
      border_colors: edges(:current_color),
      border_radius: 0.0,
      border_styles: edges(:none),
      border_widths: edges(0.0),
      box_sizing: :content_box,
      colspan: 1,
      display: :table_cell,
      font_size: font_size,
      font_weight: 400,
      margin: edges(0.0),
      padding: edges(0.75),
      rowspan: 1,
      vertical_align: :middle
    }

    cell = Map.merge(base, spans)

    case kind do
      :header -> Map.merge(cell, %{font_weight: 700, text_align: :center})
      :data -> cell
    end
  end

  defp link_defaults(attributes, opts) do
    case Map.get(attributes, "href") do
      nil ->
        {:ok, %{display: :inline}}

      href ->
        case Map.fetch!(Keyword.fetch!(opts, :__link_context__).links, href) do
          {:uri, url} ->
            {:ok,
             %{
               display: :inline,
               color: {0, 0, 238 / 255},
               text_decoration_line: [:underline],
               link_url: url
             }}

          destination ->
            {:ok,
             %{
               display: :inline,
               color: {0, 0, 238 / 255},
               text_decoration_line: [:underline],
               link_destination: destination
             }}
        end
    end
  end

  defp image_defaults(node, ancestors, opts, font_size) do
    with {:ok, selected} <-
           NativeElixirPdfUtilities.Validators.ResponsiveImageValidator.select(
             node,
             ancestors,
             opts
           ),
         {:ok, image_style} <-
           (case load_image_style(selected.url, opts, :image) do
              :error ->
                Diagnostics.error(
                  :style,
                  :invalid_document,
                  "selected image could not be decoded; supply a supported PNG, JPEG or static SVG",
                  source: selected.url
                )

              {:error, {reason, diagnostic}} ->
                {:error, {reason, Diagnostics.with_context(diagnostic, source: selected.url)}}

              result ->
                result
            end) do
      image_style = Map.put(image_style, :_image_selection, selected)

      corrected =
        case Map.fetch(image_style, :image) do
          {:ok, image} ->
            with {:ok, image} <-
                   NativeElixirPdfUtilities.Validators.ResponsiveImageValidator.correct_natural_dimensions(
                     image,
                     selected
                   ),
                 do: {:ok, Map.put(image_style, :image, image)}

          :error ->
            {:ok, image_style}
        end

      with {:ok, image_style} <- corrected do
        {:ok,
         block_defaults(font_size, 400, 0.0)
         |> Map.merge(%{
           display: :inline,
           margin: edges(0.0),
           object_fit: :fill,
           object_position: {{:percent, 0.5}, {:percent, 0.5}},
           padding: edges(0.0)
         })
         |> Map.merge(image_style)
         |> Map.merge(selected.dimensions)}
      end
    else
      {:error, {_reason, _diagnostic}} = error -> error
    end
  end

  # Fixed native appearance sizing combines font metrics with HTML character
  # counts. This calculation is shared by text inputs and textarea columns.
  defp initial_control_width(style, count) do
    zero_width = Font.text_width("0", style.font_face, style.font_size)

    {average, maximum} =
      case FontValidator.prepare_text_metrics(style.font_face) do
        {:ok, %{average_width: average, max_width: maximum, units_per_em: units}}
        when is_number(average) ->
          average = average / units * style.font_size
          # Blink's fixed native-width heuristics retain legacy font
          # compatibility and round averages up at a half CSS pixel.
          if style.font_family in @native_width_fallback_families or
               average > zero_width * 1.7 do
            {zero_width, zero_width}
          else
            {max(average, round(average / 0.75) * 0.75), maximum / units * style.font_size}
          end

        _ ->
          {zero_width, zero_width}
      end

    extent =
      if style.form_kind == :textarea,
        do: average * count,
        else: average * count + max(maximum - average, 0.0)

    extent = Float.ceil(extent / 0.75) * 0.75
    # Pinned Linux print theme reserves a 15px gutter for textarea
    # overflow:auto. Scrollbar widgets are absent from print artwork.
    if style.form_kind == :textarea and Map.get(style, :overflow) == :auto,
      do: extent + 11.25,
      else: extent
  end

  defp form_control_defaults(kind, font_size) do
    button_input? = is_binary(kind) and HtmlValidator.button_input_type?(kind)
    kind = if button_input? or kind == :button, do: "button", else: kind

    base =
      block_defaults(font_size, 400, 0.0)
      |> Map.merge(%{
        background_color: {1.0, 1.0, 1.0},
        border_color: {0.47, 0.47, 0.47},
        border_colors: edges({0.47, 0.47, 0.47}),
        border_styles: edges(:solid),
        border_widths: edges(0.75),
        form_kind:
          if(HtmlValidator.text_input_type?(kind), do: :text, else: String.to_atom(kind)),
        margin: edges(0.0),
        padding: edges(1.5, 2.25, 1.5, 2.25)
      })

    case kind do
      kind when kind in ["checkbox", "radio"] ->
        base
        |> Map.merge(%{
          background_color: nil,
          border_colors: edges(nil),
          border_styles: edges(:none),
          border_widths: edges(0.0),
          padding: edges(0.0),
          width: font_size
        })

      "textarea" ->
        base
        |> Map.put(:height, font_size * 3.6)
        |> Map.put(:white_space, :pre_line)
        |> Map.put(:width, font_size * 15.0)

      "button" ->
        base = if button_input?, do: Map.put(base, :white_space, :pre), else: base

        base
        |> Map.put(:form_kind, if(button_input?, do: :input_button, else: :button))
        |> Map.put(:background_color, {0.94, 0.94, 0.94})
        |> Map.put(:box_sizing, :border_box)
        |> Map.put(:font_families, ["Arial", "sans-serif"])
        |> Map.put(:font_size, 10.0)
        |> Map.put(:font_style, :normal)
        |> Map.put(:line_height_multiplier, nil)
        |> Map.put(:line_height_normal, true)
        |> Map.put(:text_align, :center)
        |> Map.put(:width, 105.0)

      _ ->
        Map.put(base, :width, font_size * 10.5)
    end
  end

  defp static_form_children(node, attributes, children) do
    case node.tag do
      "input" ->
        kind = HtmlValidator.input_type(Map.get(attributes, "type", "text"))

        text =
          cond do
            kind == "checkbox" ->
              {:ok, if(node._initial_form_state.checked, do: "☒", else: "☐")}

            kind == "radio" ->
              {:ok, if(node._initial_form_state.checked, do: "●", else: "○")}

            Map.get(node._initial_form_state, :placeholder_shown, false) ->
              {:ok, node._initial_form_state.placeholder}

            kind == "password" ->
              FormValidator.password_display_text(node._initial_form_state.value, :style)

            HtmlValidator.text_input_type?(kind) ->
              {:ok, node._initial_form_state.value}

            HtmlValidator.button_input_type?(kind) ->
              {:ok, node._initial_form_state.value}
          end

        with {:ok, text} <- text, do: [%{type: :text, text: text}]

      tag when tag in ~w(progress meter) ->
        []

      "select" ->
        node._initial_form_state.selection.children

      "textarea" ->
        state = node._initial_form_state

        [
          %{
            type: :text,
            text: if(state.placeholder_shown, do: state.placeholder, else: state.value)
          }
        ]

      "button" ->
        children

      _ ->
        children
    end
  end

  defp edges(value) do
    edges(value, value, value, value)
  end

  defp edges(top, right, bottom, left) do
    %{top: top, right: right, bottom: bottom, left: left}
  end

  defp stylesheet_rules(entries) do
    {rules, _next_order} =
      Enum.reduce(entries, {[], 0}, fn entry, state ->
        Enum.reduce(entry.prepared.rules, state, fn rule, {rules, order} ->
          {[%{rule | order: order} | rules], order + 1}
        end)
      end)

    {:ok, Enum.reverse(rules)}
  end

  defp read_stylesheet(path, opts \\ []) do
    case ResourceContext.current() do
      %{depth: depth} = context when depth > 0 ->
        AssetLoader.resolve_local(path, :stylesheet, context, opts)

      _ ->
        RenderCache.fetch_stylesheet({:file, path}, fn ->
          NativeElixirPdfUtilities.FileReader.read(path, Limits.get(:max_css_source_bytes))
        end)
    end
  end

  defp stylesheet_source(stylesheet) do
    case stylesheet do
      {:css, css} when is_binary(css) ->
        {:ok, css}

      {:file, path} when is_binary(path) ->
        case read_stylesheet(path) do
          {:ok, css} -> {:ok, css}
          {:error, _reason} -> {:error, :invalid_document}
        end
    end
  end

  defp stylesheet_entries(children, opts, media_context) do
    stylesheets = Keyword.get(opts, :stylesheets, [])
    base_url = Keyword.get(opts, :base_url)

    with {:ok, configured_entries} <- configured_stylesheet_entries(stylesheets, base_url, opts) do
      embedded_entries =
        children
        |> style_sources(media_context)
        |> Enum.map(&%{css: &1, base_url: base_url, trusted_local_resources: false})

      {:ok, configured_entries ++ embedded_entries}
    end
  end

  defp configured_stylesheet_entries(stylesheets, base_url, opts) do
    Enum.reduce_while(stylesheets, {:ok, []}, fn stylesheet, {:ok, acc} ->
      case stylesheet do
        {:css, css} when is_binary(css) ->
          entry = %{css: css, base_url: base_url, trusted_local_resources: false}
          {:cont, {:ok, acc ++ [entry]}}

        {:file, path} when is_binary(path) ->
          case read_stylesheet(path, opts) do
            {:ok, css} ->
              entry = %{
                css: css,
                base_url:
                  case ResourceContext.current() do
                    %{depth: depth} = context when depth > 0 ->
                      {:ok, authorized} = ResourceValidator.authorize_reference(path, context)
                      Path.dirname(authorized)

                    _ ->
                      path |> Path.expand() |> Path.dirname()
                  end,
                trusted_local_resources: true
              }

              {:cont, {:ok, acc ++ [entry]}}

            {:error, {_reason, _diagnostic}} = error ->
              {:halt, error}

            {:error, _reason} ->
              {:halt, {:error, :invalid_document}}
          end
      end
    end)
  end

  defp stylesheet_fonts(entries, opts) do
    Enum.reduce_while(entries, {:ok, []}, fn entry, {:ok, acc} ->
      font_faces = entry.prepared.font_faces

      resolved =
        Enum.reduce_while(font_faces, {:ok, []}, fn font_face, {:ok, fonts} ->
          {sources, source_error} =
            Enum.reduce_while(
              font_face.sources,
              {[], {:error, {:font_load_failed, font_face.sources}}},
              fn source, {sources, source_error} ->
                case font_asset_source(source, entry, opts) do
                  {:ok, config} ->
                    {:cont, {sources ++ [config], source_error}}

                  {:error, {reason, _diagnostic}} = error
                  when reason in [:resource_denied, :resource_limit_exceeded] ->
                    {:halt, {[], error}}

                  {:error, _} = error ->
                    {:cont, {sources, error}}
                end
              end
            )

          case sources do
            [%{path: _path} | _remaining] ->
              paths = Enum.map(sources, &Map.fetch!(&1, :path))

              font = font_face |> Map.delete(:sources) |> Map.put(:path, paths)

              {:cont, {:ok, fonts ++ [font]}}

            [%{data: _data} | _remaining] ->
              candidates = Enum.map(sources, &Map.fetch!(&1, :data))

              font = font_face |> Map.delete(:sources) |> Map.put(:data, candidates)

              {:cont, {:ok, fonts ++ [font]}}

            [] ->
              {:halt, source_error}
          end
        end)

      case resolved do
        {:ok, fonts} -> {:cont, {:ok, acc ++ fonts}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp font_asset_source(source, entry, opts) do
    case {ResourceContext.current(), entry} do
      {%{depth: depth} = context, entry} when depth > 0 ->
        base =
          if entry.trusted_local_resources,
            do: entry.base_url,
            else: {:document_url, entry.base_url}

        with {:ok, context} <- ResourceContext.rebase(context, base),
             {:ok, data} <- AssetLoader.resolve_local(source, :font, context, opts),
             do: {:ok, %{data: data}}

      {_, %{trusted_local_resources: true, base_url: stylesheet_base_url}} ->
        path =
          case Path.type(source) do
            :absolute -> source
            _ -> Path.expand(source, stylesheet_base_url)
          end

        {:ok, %{path: path}}

      _ ->
        with {:ok, data} <- AssetLoader.resolve(source, :font, opts), do: {:ok, %{data: data}}
    end
  end

  defp style_sources(children, media_context) do
    children
    |> HtmlMetadataValidator.selected_document_style_nodes(media_context)
    |> Enum.map(fn node -> Enum.map_join(node.children, "", & &1.text) end)
  end

  defp font_registry(opts, css_fonts) do
    fonts = Keyword.get(opts, :fonts, [])

    case Font.load_registry(Keyword.put(opts, :fonts, fonts ++ css_fonts)) do
      {:ok, registry} ->
        {:ok, registry}

      :error when css_fonts != [] ->
        sources = Enum.flat_map(css_fonts, &Map.get(&1, :path, []))
        {:error, {:font_load_failed, sources}}

      :error ->
        {:error, :invalid_document}

      {:error, {_reason, _diagnostic}} = error ->
        error
    end
  end

  defp font_load_error(sources) do
    source = Enum.join(sources, ", ")

    {:invalid_document,
     Diagnostics.diagnostic(
       :style,
       :invalid_document,
       "CSS font sources could not be resolved or loaded: #{source}",
       source: source
     )}
  end

  defp style_error_detail(dom, opts) do
    case first_author_css_error(dom, opts) do
      nil ->
        {:invalid_document,
         %{
           stage: :style,
           reason: :invalid_document,
           message:
             "document style validation failed; check fonts, images, attributes, and supported CSS values"
         }}

      css_error ->
        css_error
    end
  end

  defp first_author_css_error(dom, opts) do
    stylesheet_diagnostic_error(dom, opts) || inline_style_diagnostic_error(dom, opts)
  end

  defp stylesheet_diagnostic_error(dom, opts) do
    case stylesheet_diagnostic_sources(dom, opts) do
      {:ok, sources} ->
        Enum.find_value(sources, fn {_label, source} ->
          invalid_declaration_detail(source, stylesheet_declaration_sources(source))
        end)

      {:error, error} ->
        error
    end
  end

  defp stylesheet_diagnostic_sources(dom, opts) do
    with {:ok, configured} <- configured_stylesheet_sources(Keyword.get(opts, :stylesheets, [])),
         {:ok, embedded} <- embedded_stylesheet_sources(dom, opts) do
      {:ok, configured ++ embedded}
    end
  end

  defp configured_stylesheet_sources(stylesheets) do
    case stylesheets do
      stylesheets when is_list(stylesheets) ->
        Enum.reduce_while(stylesheets, {:ok, []}, fn stylesheet, {:ok, acc} ->
          case stylesheet_source(stylesheet) do
            {:ok, source} ->
              {:cont, {:ok, acc ++ [{:configured, source}]}}

            {:error, :invalid_document} ->
              {:halt,
               {:error,
                {:invalid_document,
                 %{
                   stage: :style,
                   reason: :invalid_document,
                   message: "configured stylesheet file could not be read"
                 }}}}
          end
        end)
    end
  end

  defp embedded_stylesheet_sources(dom, opts) do
    %{type: :document, children: children} = dom

    with {:ok, media_context} <- PrintMediaValidator.renderer_context(opts),
         do: {:ok, Enum.map(style_sources(children, media_context), &{:embedded, &1})}
  end

  defp stylesheet_declaration_sources(css) do
    css
    |> then(&Regex.replace(~r/@font-face\s*\{[^{}]*\}/ui, &1, ""))
    |> then(&Regex.scan(~r/[^{}]+\{(?<declarations>[^{}]*)\}/u, &1, capture: ["declarations"]))
    |> List.flatten()
    |> Enum.flat_map(&CssParser.declaration_sources/1)
  end

  defp inline_style_diagnostic_error(dom, opts) do
    Enum.find_value(inline_style_sources(dom), &inline_style_error(&1, opts))
  end

  defp inline_style_error(style_source, _opts) do
    case CssParser.parse_declarations_detailed(style_source) do
      {:ok, _declarations} ->
        invalid_declaration_detail(style_source, CssParser.declaration_sources(style_source))

      {:error, error} ->
        error
    end
  end

  defp inline_style_sources(node) do
    case node do
      %{type: :document, children: children} ->
        Enum.flat_map(children, &inline_style_sources/1)

      %{type: :element, attributes: attributes, children: children} ->
        own =
          case Map.get(attributes, "style") do
            style when is_binary(style) -> [style]
            _ -> []
          end

        own ++ Enum.flat_map(children, &inline_style_sources/1)

      _ ->
        []
    end
  end

  defp invalid_declaration_detail(css_source, declaration_sources) do
    Enum.find_value(declaration_sources, fn declaration_source ->
      with {:ok, [declaration]} <- CssParser.parse_declarations(declaration_source),
           {:ok, style} <- diagnostic_style(),
           {:ok, _style} <- apply_declaration(style, declaration) do
        nil
      else
        _ -> {:invalid_css, declaration_detail(css_source, declaration_source)}
      end
    end)
  end

  defp diagnostic_style do
    with {:ok, registry} <- font_registry([], []),
         {:ok, families, font_face} <- resolve_font("DejaVu Sans", 400, :normal, registry) do
      {:ok,
       12.0
       |> block_defaults(400, 0.0)
       |> flex_container_defaults()
       |> grid_container_defaults()
       |> Map.merge(%{
         _custom_properties: %{},
         _font_registry: registry,
         _root_font_size: 12.0,
         color: {0, 0, 0},
         font_face: font_face,
         font_families: families,
         font_family: font_face.family,
         font_size: 12.0,
         font_style: :normal,
         font_weight: 400,
         letter_spacing: 0.0,
         line_height: 14.4,
         line_height_normal: true,
         text_align: :left,
         text_transform: :none
       })}
    end
  end

  defp declaration_detail(css_source, declaration_source) do
    source = HtmlValidator.trim_css_whitespace(declaration_source)
    {line, column} = source_location(css_source, source)

    %{
      stage: :css,
      reason: :invalid_css,
      message: ~s(line #{line}: declaration "#{source}" is invalid or unsupported),
      line: line,
      column: column,
      source: source
    }
  end

  defp source_location(source, snippet) do
    {index, _length} = :binary.match(source, snippet)
    prefix = binary_part(source, 0, index)
    lines = String.split(prefix, "\n", trim: false)
    line = length(lines)
    column = String.length(List.last(lines) || "") + 1
    {line, column}
  end

  defp resolve_font(family_value, weight, style, registry, cache \\ nil) do
    result =
      case cache do
        nil ->
          Font.resolve(family_value, weight, style, registry)

        cache ->
          RenderCache.fetch(cache, {:font, family_value, weight, style}, fn ->
            Font.resolve(family_value, weight, style, registry)
          end)
      end

    {:ok, _families, _font_face} = result
    result
  end

  defp compile_selector_rules(rules) do
    Enum.reduce(
      rules,
      %{selectors: %{nil => %{}, :before => %{}, :after => %{}}, declarations: %{}},
      fn rule, compiled ->
        declarations =
          rule.declarations
          |> Enum.with_index()
          |> Enum.map(fn {declaration, declaration_index} ->
            %{
              declaration: declaration,
              important: important_declaration?(declaration),
              inline: false,
              order: rule.order,
              declaration_index: declaration_index
            }
          end)

        selectors =
          Enum.reduce(rule.selectors, compiled.selectors, fn selector, indexes ->
            indexes =
              case selector do
                %{language_dependent: true} -> Map.put(indexes, :language_dependent, true)
                _ -> indexes
              end

            # Retain source roots only when a validated selector uses positions.
            indexes =
              if not Map.get(indexes, :positional_dependent, false) and
                   Enum.any?(selector.parts, fn part ->
                     Enum.any?([part | part.negations], fn compound ->
                       Enum.any?(compound.pseudo_classes, fn pseudo_class ->
                         case pseudo_class do
                           kind
                           when kind in [
                                  :first_child,
                                  :last_child,
                                  :first_of_type,
                                  :last_of_type,
                                  :only_child,
                                  :only_of_type
                                ] ->
                             true

                           {kind, _index}
                           when kind in [
                                  :nth_child,
                                  :nth_last_child,
                                  :nth_of_type,
                                  :nth_last_of_type
                                ] ->
                             true

                           _ ->
                             false
                         end
                       end)
                     end)
                   end),
                 do: Map.put(indexes, :positional_dependent, true),
                 else: indexes

            [rightmost | _] = parts = Enum.reverse(selector.parts)

            # Index only a positive requirement of the rightmost compound selector.
            # Attribute-only and negation-only selectors must remain general candidates.
            key =
              cond do
                rightmost.id != nil -> {:id, rightmost.id}
                rightmost.classes != [] -> {:class, hd(rightmost.classes)}
                rightmost.tag != nil -> {:tag, rightmost.tag}
                true -> :universal
              end

            entry = {rule.order, %{selector | parts: parts}}

            Map.update!(indexes, rightmost.pseudo_element, fn index ->
              Map.update(index, key, [entry], &[entry | &1])
            end)
          end)

        %{
          selectors: selectors,
          declarations: Map.put(compiled.declarations, rule.order, declarations)
        }
      end
    )
  end

  defp apply_author_styles(
         style,
         node,
         ancestors,
         rules,
         pseudo_element \\ nil,
         matched_declarations \\ nil
       ) do
    inline_style =
      case pseudo_element do
        nil -> Map.get(node.attributes, "style", "")
        _pseudo_element -> ""
      end

    parsed =
      RenderCache.fetch(rules.cache, {:inline, inline_style}, fn ->
        CssParser.parse_declarations(inline_style)
      end)

    case parsed do
      {:ok, inline_declarations} ->
        presentation =
          if Map.get(node, :namespace) == :svg and is_nil(pseudo_element),
            do: SvgStyleValidator.presentation_declarations(node),
            else: {:ok, []}

        with {:ok, presentation} <- presentation do
          declarations =
            (matched_declarations || matching_declarations(rules, node, ancestors, pseudo_element))
            |> Kernel.++(
              inline_declarations
              |> Enum.with_index()
              |> Enum.map(fn {declaration, declaration_index} ->
                %{
                  declaration: declaration,
                  important: important_declaration?(declaration),
                  inline: true,
                  specificity: {0, 0, 0},
                  order: 0,
                  declaration_index: declaration_index
                }
              end)
            )
            |> Enum.sort_by(fn matched ->
              {
                matched.important,
                matched.inline,
                matched.specificity,
                matched.order,
                matched.declaration_index
              }
            end)
            |> Enum.map(& &1.declaration)
            |> then(&(presentation ++ &1))

          hints = if pseudo_element == nil, do: MathLayoutValidator.style_hints(node), else: []

          with {:ok, style} <- apply_declarations(style, hints ++ declarations, rules.cache),
               # Defaults contain no rem values; inherited lengths are already resolved.
               style = if(declarations == [], do: style, else: resolve_rem_values(style)),
               style =
                 style
                 |> resolve_font_relative_defaults()
                 |> Map.delete(:_inherited_color)
                 |> Map.delete(:_root_element)
                 |> resolve_current_color_values(),
               style = style |> apply_border_styles() |> resolve_auto_margin_values(),
               {:ok, style} <- put_font_face(style, rules.cache),
               {:ok, style} <- MathLayoutValidator.compute_style(style) do
            # Native dropdown appearance uses the font's normal line height.
            # HTML's select rendering rules reset authored line-height here.
            # Native dropdown text includes one CSS pixel of internal leading
            # above and below the font line, independent of authored padding.
            style =
              case style do
                %{form_kind: :select} ->
                  style
                  |> Map.put(:line_height_normal, false)
                  |> Map.delete(:line_height_multiplier)
                  |> Map.delete(:line_height_explicit)
                  |> Map.put(:line_height, normal_line_height(style) + 1.5)

                _ ->
                  put_line_height(style)
              end

            case style do
              %{text_decoration_line: [_ | _]} -> HtmlValidator.resolve_text_decorations(style)
              %{_text_decorations: _} -> HtmlValidator.resolve_text_decorations(style)
              _ -> {:ok, style}
            end
          end
        end

      {:error, _reason} ->
        {:error, :invalid_document}
    end
  end

  defp matching_declarations(context, node, ancestors, pseudo_element) do
    index = Map.fetch!(context.rules.selectors, pseudo_element)

    case map_size(index) do
      0 ->
        []

      _ ->
        attributes = node.attributes
        classes = node._selector_classes

        keys = [
          :universal,
          {:id, Map.get(attributes, "id")},
          {:tag, node.tag} | Enum.map(classes, &{:class, &1})
        ]

        keys
        |> Enum.flat_map(&Map.get(index, &1, []))
        |> Enum.reduce(%{}, fn {order, selector}, matched ->
          case match_selector_parts(selector.parts, node, ancestors, context) do
            true ->
              Map.update(matched, order, selector.specificity, &max(&1, selector.specificity))

            false ->
              matched
          end
        end)
        |> Enum.flat_map(fn {order, specificity} ->
          context.rules.declarations
          |> Map.fetch!(order)
          |> Enum.map(&Map.put(&1, :specificity, specificity))
        end)
    end
  end

  defp match_selector_parts(parts, node, ancestors, context) do
    case parts do
      [part] ->
        matches_simple_selector?(part, node, ancestors, context)

      [part, left_part | remaining] ->
        case matches_simple_selector?(part, node, ancestors, context) do
          true ->
            matches_ancestor_selector?(part.combinator, left_part, remaining, ancestors, context)

          false ->
            false
        end
    end
  end

  defp matches_ancestor_selector?(combinator, left_part, remaining, ancestors, context) do
    case {combinator, ancestors} do
      {:child, [parent | rest]} ->
        match_selector_parts([left_part | remaining], parent, rest, context)

      {:descendant, ancestors} ->
        ancestors
        |> Enum.with_index()
        |> Enum.any?(fn {ancestor, index} ->
          rest = Enum.drop(ancestors, index + 1)
          match_selector_parts([left_part | remaining], ancestor, rest, context)
        end)

      _ ->
        false
    end
  end

  defp matches_simple_selector?(part, node, ancestors, context) do
    HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)
    attributes = Map.get(node, :attributes, %{})
    classes = node._selector_classes

    tag_matches? = is_nil(part.tag) or Map.get(node, :tag) == part.tag
    id_matches? = is_nil(part.id) or Map.get(attributes, "id") == part.id
    classes_match? = Enum.all?(part.classes, &MapSet.member?(classes, &1))

    # Resolve context only for compounds whose positive requirements match.
    # An impossible compound needs neither sibling positions nor negation work.
    tag_matches? and id_matches? and classes_match? and
      Enum.all?(part.attributes, &HtmlValidator.attribute_selector_matches?(&1, node)) and
      Enum.all?(part.pseudo_classes, &pseudo_class_matches?(&1, node, ancestors, context)) and
      Enum.all?(part.negations, &(not matches_simple_selector?(&1, node, ancestors, context)))
  end

  defp pseudo_class_matches?(pseudo_class, node, ancestors, context) do
    case pseudo_class do
      {:lang, ranges} ->
        language = LanguageValidator.resolved_language(node, ancestors)
        Enum.any?(ranges, &LanguageValidator.matches?(&1, language))

      state
      when state in [
             :enabled,
             :disabled,
             :checked,
             :required,
             :optional,
             :popover_open,
             :indeterminate
           ] ->
        HtmlValidator.control_state_matches?(state, node)

      state when state in [:read_only, :read_write] ->
        HtmlValidator.editing_state_matches?(state, node, ancestors, context.cache)

      :root ->
        Map.get(node, :tag) == "html" and ancestors == []

      kind
      when kind in [
             :first_child,
             :last_child,
             :first_of_type,
             :last_of_type,
             :only_child,
             :only_of_type
           ] ->
        case Map.fetch(selector_positions(ancestors, context), node._selector_id) do
          {:ok, {position, count, type_position, type_count}} ->
            case kind do
              :first_child -> position == 1
              :last_child -> position == count
              :first_of_type -> type_position == 1
              :last_of_type -> type_position == type_count
              :only_child -> count == 1
              :only_of_type -> type_count == 1
            end

          :error ->
            # The virtual body supplying fragment defaults is not a source node.
            false
        end

      {kind, index} when kind in [:nth_child, :nth_last_child, :nth_of_type, :nth_last_of_type] ->
        case Map.fetch(selector_positions(ancestors, context), node._selector_id) do
          {:ok, {position, count, type_position, type_count}} ->
            position =
              case kind do
                :nth_child -> position
                :nth_last_child -> count - position + 1
                :nth_of_type -> type_position
                :nth_last_of_type -> type_count - type_position + 1
              end

            case index do
              :odd ->
                rem(position, 2) == 1

              :even ->
                rem(position, 2) == 0

              {0, offset} ->
                position == offset

              {step, offset} ->
                rem(position - offset, step) == 0 and div(position - offset, step) >= 0

              index ->
                position == index
            end

          :error ->
            false
        end
    end
  end

  # The stylesheet cache lives only within this render. Fresh selector identities
  # prevent reuse across style computations, documents or changed child lists.
  defp selector_positions(ancestors, context) do
    parent =
      case ancestors do
        [parent | _rest] -> parent
        [] -> %{_selector_id: context.root_selector_id, children: context.root_children}
      end

    RenderCache.fetch_stylesheet({:selector_positions, parent._selector_id}, fn ->
      elements = Enum.filter(parent.children, &match?(%{type: :element}, &1))
      HtmlValidator.reserve_render_resource(:max_css_work, length(elements), :css)
      type_counts = Enum.frequencies_by(elements, &{Map.get(&1, :namespace, :html), &1.tag})
      count = length(elements)

      {positions, _seen} =
        elements
        |> Enum.with_index(1)
        |> Enum.reduce({%{}, %{}}, fn {child, index}, {positions, seen} ->
          type = {Map.get(child, :namespace, :html), child.tag}
          type_position = Map.get(seen, type, 0) + 1
          value = {index, count, type_position, Map.fetch!(type_counts, type)}
          {Map.put(positions, child._selector_id, value), Map.put(seen, type, type_position)}
        end)

      positions
    end)
  end

  defp apply_declarations(style, declarations, cache) do
    {custom_property_declarations, ordinary_declarations} =
      Enum.split_with(declarations, &custom_property_declaration?/1)

    {foundational_declarations, dependent_declarations} =
      Enum.split_with(ordinary_declarations, fn declaration ->
        case declaration do
          {property, _value} -> property in ["color", "font-size"]
          {property, _value, :important} -> property in ["color", "font-size"]
        end
      end)

    with {:ok, style} <- apply_declaration_list(style, custom_property_declarations, cache),
         {:ok, custom_properties} <-
           HtmlValidator.compute_custom_properties(style._custom_properties, css_budget(cache)),
         style = Map.put(style, :_custom_properties, custom_properties),
         {:ok, style} <- apply_declaration_list(style, foundational_declarations, cache) do
      style =
        case Map.get(style, :_root_element, false) do
          true -> Map.put(style, :_root_font_size, Map.fetch!(style, :font_size))
          false -> style
        end

      style =
        if Map.get(style, :_fieldset_default_padding, false) do
          font_size = style.font_size
          padding = edges(font_size * 0.35, font_size * 0.75, font_size * 0.625, font_size * 0.75)

          padding =
            Map.new(padding, fn {side, value} ->
              {side, Float.floor(value / 0.75 * 64) / 64 * 0.75}
            end)

          style |> Map.put(:padding, padding) |> Map.delete(:_fieldset_default_padding)
        else
          style
        end

      apply_declaration_list(style, dependent_declarations, cache)
    end
  end

  defp custom_property_declaration?(declaration) do
    case declaration do
      {"--" <> _custom_property, _value} -> true
      {"--" <> _custom_property, _value, :important} -> true
      _ -> false
    end
  end

  defp apply_declaration_list(style, declarations, cache) do
    Enum.reduce_while(declarations, {:ok, style}, fn declaration, {:ok, acc} ->
      case apply_declaration(acc, declaration, cache) do
        {:ok, style} -> {:cont, {:ok, style}}
        {:error, reason} -> {:halt, {:error, reason}}
        _ -> {:halt, {:error, :invalid_document}}
      end
    end)
  end

  defp important_declaration?(declaration) do
    case declaration do
      {_property, _value, :important} -> true
      _ -> false
    end
  end

  defp apply_declaration(style, declaration, cache \\ nil) do
    case declaration do
      {property, value} -> apply_declaration_value(style, property, value, cache)
      {property, value, :important} -> apply_declaration_value(style, property, value, cache)
    end
  end

  defp apply_declaration_value(style, property, value, cache) do
    case property do
      "--" <> _custom_property when is_binary(value) ->
        custom_properties =
          style
          |> Map.get(:_custom_properties, %{})
          |> Map.put(property, value)

        {:ok, Map.put(style, :_custom_properties, custom_properties)}

      _ ->
        with {:ok, value} <- resolved_css_value(style, value, cache) do
          apply_resolved_declaration_value(style, property, value, cache)
        else
          :error -> {:error, :invalid_document}
          {:error, _detail} = error -> error
        end
    end
  end

  defp apply_resolved_declaration_value(style, property, value, cache) do
    svg_property =
      if Map.get(style, :_svg_prepare, false) and
           (Map.get(style, :_svg_element, false) or
              property not in ~w(width height opacity visibility transform)),
         do: SvgStyleValidator.apply(style, property, value),
         else: :unhandled

    case svg_property do
      :unhandled -> apply_host_declaration_value(style, property, value, cache)
      result -> result
    end
  end

  defp apply_host_declaration_value(style, property, value, cache) do
    case property do
      "color" ->
        with {:ok, color} <-
               HtmlValidator.parse_css_color_declaration(
                 value,
                 Map.get(style, :_inherited_color, Map.fetch!(style, :color))
               ) do
          case color do
            :transparent -> {:ok, Map.put(style, :color, {0, 0, 0, 0.0})}
            color -> {:ok, Map.put(style, :color, color)}
          end
        end

      "background-color" ->
        with {:ok, color} <- HtmlValidator.parse_css_color(value, Map.fetch!(style, :color)) do
          {:ok, Map.put(style, :background_color, transparent_color_to_nil(color))}
        end

      "background" ->
        put_background(style, value)

      "background-image" ->
        put_background_image(style, value)

      "background-size" ->
        put_background_size(style, value)

      "background-position" ->
        put_two_axis_position(style, :background_position, value)

      "background-repeat" ->
        put_background_repeat(style, value)

      "box-sizing" ->
        put_box_sizing(style, value)

      "margin" ->
        with {:ok, lengths} <- HtmlValidator.parse_css_box_lengths(value, :margin),
             do: {:ok, put_box_lengths(style, "margin", lengths)}

      "padding" ->
        with {:ok, lengths} <- parse_box_lengths(value),
             do: {:ok, put_box_lengths(style, "padding", lengths)}

      "border-width" ->
        with {:ok, widths} <- parse_border_widths(value),
             do: {:ok, put_box_lengths(style, "border-width", widths)}

      property when property in ["margin-top", "margin-right", "margin-bottom", "margin-left"] ->
        with {:ok, length} <- HtmlValidator.parse_css_margin(value),
             do: {:ok, put_edge_length(style, property, length)}

      property
      when property in ["padding-top", "padding-right", "padding-bottom", "padding-left"] ->
        with {:ok, length} <- parse_length(value),
             do: {:ok, put_edge_length(style, property, length)}

      "border-color" ->
        with {:ok, border_colors} <-
               parse_border_colors(value, Map.fetch!(style, :color)) do
          border_color = border_colors.top

          {:ok,
           style
           |> Map.put(:border_color, border_color)
           |> Map.put(:border_colors, border_colors)}
        end

      property when property in ["border-top", "border-right", "border-bottom", "border-left"] ->
        with {:ok, width, color, border_style} <-
               parse_border_edge(value, Map.fetch!(style, :color)) do
          {:ok,
           style
           |> put_border_edge_width(property, width)
           |> put_border_edge_color(property, color)
           |> put_border_edge_style(property, border_style)}
        end

      property
      when property in [
             "border-top-width",
             "border-right-width",
             "border-bottom-width",
             "border-left-width"
           ] ->
        with {:ok, width} <- parse_border_width(value) do
          {:ok, put_border_edge_width(style, border_edge_property(property), width)}
        end

      property
      when property in [
             "border-top-color",
             "border-right-color",
             "border-bottom-color",
             "border-left-color"
           ] ->
        with {:ok, color} <- HtmlValidator.parse_css_color(value, Map.fetch!(style, :color)) do
          {:ok,
           put_border_edge_color(
             style,
             border_edge_property(property),
             transparent_color_to_nil(color)
           )}
        end

      property
      when property in [
             "border-style",
             "border-top-style",
             "border-right-style",
             "border-bottom-style",
             "border-left-style"
           ] ->
        put_border_style(style, property, value)

      "border-radius" ->
        with {:ok, length} <- parse_length(value),
             do: {:ok, Map.put(style, :border_radius, length)}

      "text-align" ->
        put_text_align(style, value)

      "text-transform" ->
        put_text_transform(style, value)

      "content" ->
        put_generated_content(style, value)

      "counter-reset" ->
        put_counter_values(style, :counter_reset, value, 0)

      "counter-increment" ->
        put_counter_values(style, :counter_increment, value, 1)

      "letter-spacing" ->
        put_letter_spacing(style, value)

      property
      when property in ["white-space", "word-break", "word-wrap", "overflow-wrap", "line-break"] ->
        put_text_wrapping(style, property, value)

      property when property in ["break-before", "page-break-before"] ->
        with {:ok, break_value} <- page_break_value(value),
             do: {:ok, Map.put(style, :break_before, break_value)}

      property when property in ["break-after", "page-break-after"] ->
        with {:ok, break_value} <- page_break_value(value),
             do: {:ok, Map.put(style, :break_after, break_value)}

      property when property in ["break-inside", "page-break-inside"] ->
        put_break_inside(style, value)

      "vertical-align" ->
        with {:ok, align} <- HtmlValidator.parse_vertical_align(value, style.font_size),
             do: {:ok, Map.put(style, :vertical_align, align)}

      "appearance" ->
        with {:ok, appearance} <- HtmlValidator.parse_gauge_appearance(value),
             do: {:ok, Map.put(style, :appearance, appearance)}

      "accent-color" ->
        case HtmlValidator.parse_css_accent_color(value, style.color) do
          {:ok, :auto} -> {:ok, Map.delete(style, :accent_color)}
          {:ok, color} -> {:ok, Map.put(style, :accent_color, color)}
          :error -> {:error, :invalid_document}
        end

      "min-height" ->
        put_size(style, :min_height, value)

      property when property in ["min-width", "max-width", "max-height"] ->
        put_size(style, length_property(property), value)

      "border" ->
        with {:ok, border_widths, border_color, border_styles} <-
               parse_border(value, Map.fetch!(style, :color)) do
          style =
            style
            |> Map.put(:border_widths, border_widths)
            |> Map.put(:border_color, border_color)
            |> Map.put(:border_colors, edges(border_color))
            |> Map.put(:border_styles, border_styles)

          {:ok, style}
        end

      "font-size" ->
        with {:ok, properties} <- MathLayoutValidator.parse_property("font-size", value, style),
             do: {:ok, Map.merge(style, properties)}

      property when property in ["math-style", "math-shift", "math-depth"] ->
        with {:ok, properties} <- MathLayoutValidator.parse_property(property, value, style),
             do: {:ok, Map.merge(style, properties)}

      "line-height" ->
        case parse_line_height(value) do
          {:ok, {:multiplier, multiplier}} ->
            {:ok,
             style
             |> Map.put(:line_height_multiplier, multiplier)
             |> Map.delete(:line_height_normal)
             |> Map.put(:line_height_explicit, true)}

          {:ok, :normal} ->
            {:ok,
             style
             |> Map.put(:line_height_normal, true)
             |> Map.delete(:line_height_multiplier)
             |> Map.put(:line_height_explicit, true)}

          {:ok, line_height} ->
            {:ok,
             style
             |> Map.put(:line_height, line_height)
             |> Map.delete(:line_height_multiplier)
             |> Map.delete(:line_height_normal)
             |> Map.put(:line_height_explicit, true)}

          :error ->
            :error
        end

      "font-weight" ->
        put_font_weight(style, value)

      "font-style" ->
        put_font_style(style, value)

      "font-family" ->
        put_font_family(style, value, cache)

      property
      when property in ~w(text-decoration text-decoration-line text-decoration-color text-decoration-thickness text-decoration-style) ->
        case HtmlValidator.parse_css_text_decoration(
               property,
               value,
               style.font_size,
               style.color
             ) do
          {:ok, properties} -> {:ok, Map.merge(style, properties)}
          :error -> {:error, :invalid_document}
        end

      "display" ->
        put_display(style, value)

      property when property in ~w(list-style list-style-type list-style-position) ->
        case HtmlValidator.parse_css_list_style(property, value) do
          {:ok, properties} -> {:ok, Map.merge(style, properties)}
          :error -> {:error, :invalid_document}
        end

      property when property in ["width", "height"] ->
        put_size(style, length_property(property), value)

      "position" ->
        put_position(style, value)

      property when property in ["top", "right", "bottom", "left"] ->
        put_position_offset(style, property, value)

      "z-index" ->
        put_z_index(style, value)

      "object-fit" ->
        put_object_fit(style, value)

      "object-position" ->
        put_two_axis_position(style, :object_position, value)

      "overflow" ->
        put_overflow(style, value)

      "aspect-ratio" ->
        with {:ok, aspect_ratio} <- HtmlValidator.parse_aspect_ratio(value),
             do: {:ok, Map.put(style, :aspect_ratio, aspect_ratio)}

      "border-collapse" ->
        put_border_collapse(style, value)

      "border-spacing" ->
        with {:ok, horizontal, vertical} <- parse_border_spacing(value) do
          {:ok, Map.put(style, :border_spacing, {horizontal, vertical})}
        end

      "table-layout" ->
        put_table_layout(style, value)

      "flex-direction" ->
        put_flex_direction(style, value)

      "flex-wrap" ->
        put_flex_wrap(style, value)

      "gap" ->
        with {:ok, row_gap, column_gap} <- parse_gap(value) do
          {:ok, style |> Map.put(:row_gap, row_gap) |> Map.put(:column_gap, column_gap)}
        end

      property when property in ["row-gap", "column-gap"] ->
        with {:ok, length} <- parse_length(value),
             do: {:ok, Map.put(style, gap_property(property), length)}

      "justify-content" ->
        put_justify_content(style, value)

      "align-items" ->
        put_align_items(style, value)

      "align-self" ->
        put_align_self(style, value)

      "justify-self" ->
        put_grid_alignment(style, :justify_self, value)

      "order" ->
        put_order(style, value)

      "flex-grow" ->
        put_nonnegative_number(style, :flex_grow, value)

      "flex-shrink" ->
        put_nonnegative_number(style, :flex_shrink, value)

      "flex-basis" ->
        put_flex_basis(style, value)

      "flex" ->
        put_flex(style, value)

      property when property in ["grid-template-columns", "grid-template-rows"] ->
        with {:ok, tracks} <- parse_grid_tracks(value) do
          {:ok, Map.put(style, grid_template_property(property), tracks)}
        end

      property when property in ["grid-auto-columns", "grid-auto-rows"] ->
        with {:ok, track} <- parse_grid_track(value) do
          {:ok, Map.put(style, grid_auto_property(property), track)}
        end

      property
      when property in [
             "grid-column",
             "grid-row",
             "grid-column-start",
             "grid-column-end",
             "grid-row-start",
             "grid-row-end",
             "grid-area"
           ] ->
        put_grid_placement(style, property, value)

      "justify-items" ->
        put_grid_alignment(style, :justify_items, value)

      "align-content" ->
        put_justify_content(style, value, :align_content)

      _ ->
        {:error, :invalid_document}
    end
  end

  defp put_display(style, value) do
    case value |> String.trim() |> String.downcase() do
      value when value in ["math", "inline math", "block math"] ->
        with {:ok, properties} <- MathLayoutValidator.parse_property("display", value, style),
             do: {:ok, Map.merge(style, properties)}

      "block" ->
        style =
          if Map.has_key?(style, :_math_context),
            do: Map.merge(style, %{_math_display: :block, _math_formatting: false}),
            else: style

        {:ok, style |> ensure_box_style() |> Map.put(:display, :block)}

      "inline" ->
        style =
          if Map.has_key?(style, :_math_context),
            do: Map.merge(style, %{_math_display: :inline, _math_formatting: false}),
            else: style

        {:ok, Map.put(style, :display, :inline)}

      "inline-block" ->
        style =
          if Map.has_key?(style, :_math_context),
            do: Map.merge(style, %{_math_display: :inline, _math_formatting: false}),
            else: style

        {:ok, style |> ensure_box_style() |> Map.put(:display, :inline_block)}

      "none" ->
        {:ok, Map.put(style, :display, :none)}

      "flex" ->
        {:ok,
         style |> ensure_box_style() |> flex_container_defaults() |> Map.put(:display, :flex)}

      "inline-flex" ->
        {:ok,
         style
         |> ensure_box_style()
         |> flex_container_defaults()
         |> Map.put(:display, :inline_flex)}

      "grid" ->
        {:ok,
         style |> ensure_box_style() |> grid_container_defaults() |> Map.put(:display, :grid)}

      "inline-grid" ->
        {:ok,
         style
         |> ensure_box_style()
         |> grid_container_defaults()
         |> Map.put(:display, :inline_grid)}

      _ ->
        {:error, :invalid_document}
    end
  end

  defp ensure_box_style(style) do
    style
    |> Map.put_new(:background_color, nil)
    |> Map.put_new(:background_image, nil)
    |> Map.put_new(:background_position, {{:percent, 0.0}, {:percent, 0.0}})
    |> Map.put_new(:background_repeat, :repeat)
    |> Map.put_new(:background_size, {:auto, :auto})
    |> Map.put_new(:border_color, :current_color)
    |> Map.put_new(:border_colors, edges(Map.get(style, :border_color, :current_color)))
    |> Map.put_new(:border_radius, 0.0)
    |> Map.put_new(:border_styles, edges(:none))
    |> Map.put_new(:border_widths, edges(@medium_border_width))
    |> Map.put_new(:box_sizing, :content_box)
    |> Map.put_new(:margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    |> Map.put_new(:margin_after, 0.0)
    |> Map.put_new(:padding, edges(0.0))
    |> Map.put_new(:position, :static)
    |> Map.put_new(:offsets, edges(:auto))
    |> Map.put_new(:z_index, :auto)
  end

  defp put_size(style, property, value) do
    with {:ok, size} <- HtmlValidator.parse_css_size(value, property),
         do: {:ok, Map.put(style, property, size)}
  end

  defp put_background(style, value) do
    style =
      style
      |> Map.put(:background_color, nil)
      |> Map.put(:background_image, nil)
      |> Map.delete(:background_image_source)
      |> Map.put(:background_position, {{:percent, 0.0}, {:percent, 0.0}})
      |> Map.put(:background_repeat, :repeat)
      |> Map.put(:background_size, {:auto, :auto})

    with {:ok, color} <-
           HtmlValidator.parse_css_background_color(value, Map.fetch!(style, :color)) do
      {:ok, Map.put(style, :background_color, transparent_color_to_nil(color))}
    end
  end

  defp put_background_image(style, value) do
    case HtmlValidator.parse_background_image_reference(value) do
      {:ok, nil} ->
        {:ok, style |> Map.put(:background_image, nil) |> Map.delete(:background_image_source)}

      {:ok, source} ->
        {:ok, Map.put(style, :background_image_source, source)}

      error ->
        error
    end
  end

  defp put_background_size(style, value) do
    case value |> String.trim() |> String.downcase() do
      "contain" ->
        {:ok, Map.put(style, :background_size, :contain)}

      "cover" ->
        {:ok, Map.put(style, :background_size, :cover)}

      value ->
        parts = HtmlValidator.split_css_tokens(value)

        parsed =
          Enum.map(parts, fn part ->
            case part do
              "auto" -> {:ok, :auto}
              part -> parse_size(part)
            end
          end)

        case parsed do
          [{:ok, size}] ->
            {:ok, Map.put(style, :background_size, {size, :auto})}

          [{:ok, width}, {:ok, height}] ->
            {:ok, Map.put(style, :background_size, {width, height})}

          _ ->
            {:error, :invalid_document}
        end
    end
  end

  defp put_background_repeat(style, value) do
    case value |> String.trim() |> String.downcase() do
      "repeat" -> {:ok, Map.put(style, :background_repeat, :repeat)}
      "no-repeat" -> {:ok, Map.put(style, :background_repeat, :no_repeat)}
      "repeat-x" -> {:ok, Map.put(style, :background_repeat, :repeat_x)}
      "repeat-y" -> {:ok, Map.put(style, :background_repeat, :repeat_y)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_position(style, value) do
    case value |> String.trim() |> String.downcase() do
      "static" -> {:ok, Map.put(style, :position, :static)}
      "relative" -> {:ok, Map.put(style, :position, :relative)}
      "absolute" -> {:ok, Map.put(style, :position, :absolute)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_position_offset(style, property, value) do
    parsed =
      case value |> String.trim() |> String.downcase() do
        "auto" -> {:ok, :auto}
        value -> parse_position_component(value)
      end

    with {:ok, offset} <- parsed do
      side = String.to_existing_atom(property)
      offsets = style |> Map.get(:offsets, edges(:auto)) |> Map.put(side, offset)
      {:ok, Map.put(style, :offsets, offsets)}
    end
  end

  defp put_z_index(style, value) do
    case value |> String.trim() |> String.downcase() do
      "auto" ->
        {:ok, Map.put(style, :z_index, :auto)}

      value ->
        case Integer.parse(value) do
          {z_index, ""} -> {:ok, Map.put(style, :z_index, z_index)}
          _ -> {:error, :invalid_document}
        end
    end
  end

  defp put_object_fit(style, value) do
    case value |> String.trim() |> String.downcase() do
      "fill" -> {:ok, Map.put(style, :object_fit, :fill)}
      "contain" -> {:ok, Map.put(style, :object_fit, :contain)}
      "cover" -> {:ok, Map.put(style, :object_fit, :cover)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_two_axis_position(style, property, value) do
    with {:ok, position} <- parse_two_axis_position(value) do
      {:ok, Map.put(style, property, position)}
    end
  end

  defp put_overflow(style, value) do
    case value |> String.trim() |> String.downcase() do
      "visible" -> {:ok, Map.put(style, :overflow, :visible)}
      "hidden" -> {:ok, Map.put(style, :overflow, :hidden)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_border_style(style, property, value) do
    case property do
      "border-style" ->
        with {:ok, border_styles} <- parse_border_styles(value) do
          {:ok, Map.put(style, :border_styles, border_styles)}
        end

      _ ->
        with {:ok, border_style} <- parse_border_style(value) do
          {:ok, put_border_edge_style(style, border_edge_property(property), border_style)}
        end
    end
  end

  defp apply_border_styles(style) do
    border_styles = Map.get(style, :border_styles, edges(:none))

    border_widths =
      style
      |> Map.get(:border_widths, edges(@medium_border_width))
      |> Enum.map(fn {side, width} ->
        case Map.fetch!(border_styles, side) in [:none, :hidden] do
          true ->
            {side, 0.0}

          false ->
            {side, if(width > 0, do: max(0.75, Float.floor(width / 0.75) * 0.75), else: 0.0)}
        end
      end)
      |> Map.new()

    style
    |> Map.put(:border_styles, border_styles)
    |> Map.put(:border_widths, border_widths)
  end

  defp resolve_current_color_values(style) do
    color = Map.fetch!(style, :color)

    style =
      case Map.get(style, :border_color) do
        :current_color -> Map.put(style, :border_color, color)
        _ -> style
      end

    case Map.get(style, :border_colors) do
      border_colors when is_map(border_colors) ->
        resolved =
          border_colors
          |> Enum.map(fn {side, border_color} ->
            case border_color do
              :current_color -> {side, color}
              _ -> {side, border_color}
            end
          end)
          |> Map.new()

        Map.put(style, :border_colors, resolved)

      _ ->
        style
    end
  end

  defp resolve_rem_values(style) do
    root_font_size = Map.fetch!(style, :_root_font_size)

    style
    |> Enum.map(fn {key, value} ->
      case key in @rem_opaque_style_keys do
        true -> {key, value}
        false -> {key, resolve_rem_term(value, root_font_size)}
      end
    end)
    |> Map.new()
  end

  defp resolve_rem_term(term, root_font_size) do
    cond do
      match?({:rem, multiplier} when is_number(multiplier), term) ->
        {:rem, multiplier} = term
        multiplier * root_font_size

      is_map(term) ->
        term
        |> Enum.map(fn {key, value} -> {key, resolve_rem_term(value, root_font_size)} end)
        |> Map.new()

      is_list(term) ->
        Enum.map(term, &resolve_rem_term(&1, root_font_size))

      is_tuple(term) ->
        term
        |> Tuple.to_list()
        |> Enum.map(&resolve_rem_term(&1, root_font_size))
        |> List.to_tuple()

      true ->
        term
    end
  end

  defp resolve_font_relative_defaults(style) do
    style =
      case Map.pop(style, :_margin_before_em) do
        {multiplier, style}
        when is_number(multiplier) or multiplier == {:css_layout_em, 0.5} ->
          Map.update!(
            style,
            :margin,
            &Map.put(&1, :top, font_relative_margin(Map.fetch!(style, :font_size), multiplier))
          )

        {_value, style} ->
          style
      end

    case Map.pop(style, :_margin_after_em) do
      {multiplier, style}
      when is_number(multiplier) or multiplier == {:css_layout_em, 0.5} ->
        margin_after = font_relative_margin(Map.fetch!(style, :font_size), multiplier)

        style
        |> Map.update!(:margin, &Map.put(&1, :bottom, margin_after))
        |> Map.put(:margin_after, margin_after)

      {_value, style} ->
        style
    end
  end

  # HR's fractional default em margins use fixed browser layout precision.
  # Existing defaults retain their original multiplication until their own audit.
  defp font_relative_margin(font_size, multiplier) do
    case multiplier do
      {:css_layout_em, em} -> Float.floor(font_size * em / 0.75 * 64) / 64 * 0.75
      em -> font_size * em
    end
  end

  defp put_box_sizing(style, value) do
    case value |> String.trim() |> String.downcase() do
      "border-box" -> {:ok, Map.put(style, :box_sizing, :border_box)}
      "content-box" -> {:ok, Map.put(style, :box_sizing, :content_box)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_text_transform(style, value) do
    case value |> String.trim() |> String.downcase() do
      "math-auto" ->
        with {:ok, properties} <-
               MathLayoutValidator.parse_property("text-transform", value, style),
             do: {:ok, Map.merge(style, properties)}

      "none" ->
        {:ok, Map.put(style, :text_transform, :none)}

      "uppercase" ->
        {:ok, Map.put(style, :text_transform, :uppercase)}

      "lowercase" ->
        {:ok, Map.put(style, :text_transform, :lowercase)}

      "capitalize" ->
        {:ok, Map.put(style, :text_transform, :capitalize)}

      _ ->
        {:error, :invalid_document}
    end
  end

  defp put_generated_content(style, value) do
    normalized = String.trim(value)

    case String.downcase(normalized) do
      keyword when keyword in ["none", "normal"] ->
        {:ok, Map.put(style, :content, :none)}

      _ ->
        token_regex =
          ~r/\s*("(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*'|attr\(\s*[a-zA-Z_][a-zA-Z0-9_:-]*\s*\)|counter\(\s*[a-zA-Z_][a-zA-Z0-9_-]*\s*\))/u

        tokens = Regex.scan(token_regex, normalized)
        parsed_source = Enum.map_join(tokens, "", &List.first/1) |> String.trim()

        case tokens != [] and parsed_source == normalized do
          true ->
            parts =
              Enum.map(tokens, fn [_, token] ->
                cond do
                  String.starts_with?(token, ["\"", "'"]) ->
                    text = String.slice(token, 1, String.length(token) - 2)
                    {:string, String.replace(text, ~r/\\([\\"'])/u, "\\1")}

                  String.starts_with?(token, "attr(") ->
                    [_, name] = Regex.run(~r/^attr\(\s*([a-zA-Z_][a-zA-Z0-9_:-]*)\s*\)$/u, token)
                    {:attr, String.downcase(name)}

                  String.starts_with?(token, "counter(") ->
                    [_, name] =
                      Regex.run(~r/^counter\(\s*([a-zA-Z_][a-zA-Z0-9_-]*)\s*\)$/u, token)

                    {:counter, name}
                end
              end)

            {:ok, Map.put(style, :content, parts)}

          false ->
            {:error, :invalid_document}
        end
    end
  end

  defp put_counter_values(style, property, value, default_value) do
    tokens = value |> String.trim() |> String.split(~r/\s+/u, trim: true)

    case tokens do
      ["none"] ->
        {:ok, Map.put(style, property, [])}

      tokens ->
        result =
          Enum.reduce_while(tokens, {:ok, [], false}, fn token, {:ok, values, integer_allowed?} ->
            identifier? =
              token != "none" and Regex.match?(~r/^[a-zA-Z_][a-zA-Z0-9_-]*$/u, token)

            cond do
              identifier? ->
                {:cont, {:ok, values ++ [{token, default_value}], true}}

              integer_allowed? ->
                case Integer.parse(token) do
                  {integer, ""} ->
                    {name, _old_value} = List.last(values)
                    {:cont, {:ok, List.replace_at(values, -1, {name, integer}), false}}

                  _ ->
                    {:halt, :error}
                end

              true ->
                {:halt, :error}
            end
          end)

        case result do
          {:ok, values, _integer_allowed?} -> {:ok, Map.put(style, property, values)}
          :error -> {:error, :invalid_document}
        end
    end
  end

  defp put_letter_spacing(style, value) do
    case value |> String.trim() |> String.downcase() do
      "normal" ->
        {:ok, Map.put(style, :letter_spacing, 0.0)}

      value ->
        with {:ok, length} <-
               HtmlValidator.parse_css_length(value, :margin, Map.fetch!(style, :font_size)) do
          {:ok, Map.put(style, :letter_spacing, length)}
        else
          :error -> {:error, :invalid_document}
        end
    end
  end

  defp put_text_wrapping(style, property, value) do
    case HtmlValidator.parse_css_text_wrapping(property, value) do
      {:ok, key, value} -> {:ok, Map.put(style, key, value)}
      :error -> {:error, :invalid_document}
    end
  end

  defp put_break_inside(style, value) do
    case value |> String.trim() |> String.downcase() do
      "auto" -> {:ok, Map.put(style, :break_inside, :auto)}
      "avoid" -> {:ok, Map.put(style, :break_inside, :avoid)}
      _ -> {:error, :invalid_document}
    end
  end

  defp transform_text(text, transform) do
    HtmlValidator.reserve_render_resource(:max_rendered_text_bytes, byte_size(text), :style)

    transformed =
      case transform do
        :math_auto -> MathLayoutValidator.math_auto_text(text)
        :uppercase -> String.upcase(text)
        :lowercase -> String.downcase(text)
        :capitalize -> Regex.replace(~r/\b\p{L}/u, text, &String.upcase/1)
        _ -> text
      end

    HtmlValidator.reserve_render_resource(
      :max_rendered_text_bytes,
      max(byte_size(transformed) - byte_size(text), 0),
      :style
    )

    transformed
  end

  defp css_budget(cache) do
    case cache do
      nil -> HtmlValidator.new_css_budget()
      cache -> RenderCache.fetch(cache, :css_variable_budget, &HtmlValidator.new_css_budget/0)
    end
  end

  defp resolved_css_value(style, value, cache) do
    custom_properties = Map.get(style, :_custom_properties, %{})
    HtmlValidator.resolve_css_variables(value, custom_properties, %{}, css_budget(cache))
  end

  defp flex_container_defaults(style) do
    style
    |> Map.put_new(:flex_direction, :row)
    |> Map.put_new(:flex_wrap, :nowrap)
    |> Map.put_new(:row_gap, 0.0)
    |> Map.put_new(:column_gap, 0.0)
    |> Map.put_new(:justify_content, :flex_start)
    |> Map.put_new(:align_items, :stretch)
  end

  defp grid_container_defaults(style) do
    style
    |> Map.put_new(:grid_template_columns, [])
    |> Map.put_new(:grid_template_rows, [])
    |> Map.put_new(:grid_auto_columns, :auto)
    |> Map.put_new(:grid_auto_rows, :auto)
    |> Map.put_new(:row_gap, 0.0)
    |> Map.put_new(:column_gap, 0.0)
    |> Map.put_new(:justify_items, :stretch)
    |> Map.put_new(:align_items, :stretch)
    |> Map.put_new(:justify_content, :flex_start)
    |> Map.put_new(:align_content, :stretch)
  end

  defp put_flex_direction(style, value) do
    case value |> String.trim() |> String.downcase() do
      "row" -> {:ok, Map.put(style, :flex_direction, :row)}
      "row-reverse" -> {:ok, Map.put(style, :flex_direction, :row_reverse)}
      "column" -> {:ok, Map.put(style, :flex_direction, :column)}
      "column-reverse" -> {:ok, Map.put(style, :flex_direction, :column_reverse)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_flex_wrap(style, value) do
    case value |> String.trim() |> String.downcase() do
      "nowrap" -> {:ok, Map.put(style, :flex_wrap, :nowrap)}
      "wrap" -> {:ok, Map.put(style, :flex_wrap, :wrap)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_justify_content(style, value) do
    put_justify_content(style, value, :justify_content)
  end

  defp put_justify_content(style, value, property) do
    case value |> String.trim() |> String.downcase() do
      value when value in ["start", "flex-start"] -> {:ok, Map.put(style, property, :flex_start)}
      value when value in ["end", "flex-end"] -> {:ok, Map.put(style, property, :flex_end)}
      "center" -> {:ok, Map.put(style, property, :center)}
      "stretch" -> {:ok, Map.put(style, property, :stretch)}
      "space-between" -> {:ok, Map.put(style, property, :space_between)}
      "space-around" -> {:ok, Map.put(style, property, :space_around)}
      "space-evenly" -> {:ok, Map.put(style, property, :space_evenly)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_align_items(style, value) do
    case value |> String.trim() |> String.downcase() do
      "stretch" ->
        {:ok, Map.put(style, :align_items, :stretch)}

      value when value in ["start", "flex-start"] ->
        {:ok, Map.put(style, :align_items, :flex_start)}

      value when value in ["end", "flex-end"] ->
        {:ok, Map.put(style, :align_items, :flex_end)}

      "center" ->
        {:ok, Map.put(style, :align_items, :center)}

      _ ->
        {:error, :invalid_document}
    end
  end

  defp put_align_self(style, value) do
    case value |> String.trim() |> String.downcase() do
      "auto" ->
        {:ok, Map.put(style, :align_self, :auto)}

      "stretch" ->
        {:ok, Map.put(style, :align_self, :stretch)}

      value when value in ["start", "flex-start"] ->
        {:ok, Map.put(style, :align_self, :flex_start)}

      value when value in ["end", "flex-end"] ->
        {:ok, Map.put(style, :align_self, :flex_end)}

      "center" ->
        {:ok, Map.put(style, :align_self, :center)}

      _ ->
        {:error, :invalid_document}
    end
  end

  defp put_grid_alignment(style, property, value) do
    case value |> String.trim() |> String.downcase() do
      "stretch" -> {:ok, Map.put(style, property, :stretch)}
      value when value in ["start", "flex-start"] -> {:ok, Map.put(style, property, :flex_start)}
      value when value in ["end", "flex-end"] -> {:ok, Map.put(style, property, :flex_end)}
      "center" -> {:ok, Map.put(style, property, :center)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_border_collapse(style, value) do
    case value |> String.trim() |> String.downcase() do
      "collapse" -> {:ok, Map.put(style, :border_collapse, :collapse)}
      "separate" -> {:ok, Map.put(style, :border_collapse, :separate)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_table_layout(style, value) do
    case value |> String.trim() |> String.downcase() do
      "auto" -> {:ok, Map.put(style, :table_layout, :auto)}
      "fixed" -> {:ok, Map.put(style, :table_layout, :fixed)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_order(style, value) do
    case Integer.parse(String.trim(value)) do
      {order, ""} -> {:ok, Map.put(style, :order, order)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_nonnegative_number(style, key, value) do
    case parse_nonnegative_number(value) do
      {:ok, number} -> {:ok, Map.put(style, key, number)}
      :error -> {:error, :invalid_document}
    end
  end

  defp put_flex_basis(style, value) do
    case value |> String.trim() |> String.downcase() do
      "auto" ->
        {:ok, Map.put(style, :flex_basis, :auto)}

      _ ->
        with {:ok, length} <- parse_length(value), do: {:ok, Map.put(style, :flex_basis, length)}
    end
  end

  defp put_flex(style, value) do
    tokens = value |> String.trim() |> String.downcase() |> String.split(~r/\s+/u, trim: true)

    case tokens do
      ["none"] ->
        {:ok,
         style
         |> Map.put(:flex_grow, 0.0)
         |> Map.put(:flex_shrink, 0.0)
         |> Map.put(:flex_basis, :auto)}

      ["auto"] ->
        {:ok,
         style
         |> Map.put(:flex_grow, 1.0)
         |> Map.put(:flex_shrink, 1.0)
         |> Map.put(:flex_basis, :auto)}

      ["initial"] ->
        {:ok,
         style
         |> Map.put(:flex_grow, 0.0)
         |> Map.put(:flex_shrink, 1.0)
         |> Map.put(:flex_basis, :auto)}

      [grow] ->
        with {:ok, grow} <- parse_nonnegative_number(grow) do
          {:ok,
           style
           |> Map.put(:flex_grow, grow)
           |> Map.put_new(:flex_shrink, 1.0)
           |> Map.put_new(:flex_basis, 0.0)}
        end

      [grow, shrink] ->
        with {:ok, grow} <- parse_nonnegative_number(grow),
             {:ok, shrink} <- parse_nonnegative_number(shrink) do
          {:ok,
           style
           |> Map.put(:flex_grow, grow)
           |> Map.put(:flex_shrink, shrink)
           |> Map.put_new(:flex_basis, 0.0)}
        end

      [grow, shrink, basis] ->
        with {:ok, grow} <- parse_nonnegative_number(grow),
             {:ok, shrink} <- parse_nonnegative_number(shrink),
             {:ok, basis} <- flex_basis_value(basis) do
          {:ok,
           style
           |> Map.put(:flex_grow, grow)
           |> Map.put(:flex_shrink, shrink)
           |> Map.put(:flex_basis, basis)}
        end

      _ ->
        {:error, :invalid_document}
    end
  end

  defp length_property(property) do
    case property do
      "width" -> :width
      "height" -> :height
      "min-width" -> :min_width
      "max-width" -> :max_width
      "max-height" -> :max_height
    end
  end

  defp gap_property(property) do
    case property do
      "row-gap" -> :row_gap
      "column-gap" -> :column_gap
    end
  end

  defp flex_basis_value(value) do
    case value do
      "auto" -> {:ok, :auto}
      value -> parse_length(value)
    end
  end

  defp parse_grid_tracks(value) do
    result =
      value
      |> String.trim()
      |> String.downcase()
      |> HtmlValidator.split_css_tokens()
      |> Enum.reduce_while({:ok, [], 0}, fn source, {:ok, chunks, track_count} ->
        case parse_grid_track_source(source) do
          {:ok, tracks} ->
            track_count = track_count + length(tracks)

            case HtmlValidator.validate_layout_cardinality(:grid_tracks, track_count) do
              :ok -> {:cont, {:ok, [tracks | chunks], track_count}}
              {:error, {_reason, _diagnostic}} = error -> {:halt, error}
            end

          :error ->
            {:halt, :error}

          {:error, {_reason, _diagnostic}} = error ->
            {:halt, error}
        end
      end)

    case result do
      {:ok, chunks, _track_count} -> {:ok, chunks |> Enum.reverse() |> List.flatten()}
      result -> result
    end
  end

  defp parse_grid_track_source(value) do
    normalized = value |> String.trim() |> String.downcase()

    case Regex.named_captures(~r/^repeat\((?<count>[1-9]\d*)\s*,\s*(?<track>.+)\)$/u, normalized) do
      %{"count" => count, "track" => track} ->
        with {count, ""} <- Integer.parse(count),
             :ok <- HtmlValidator.validate_layout_cardinality(:grid_tracks, count),
             {:ok, parsed_track} <- parse_grid_track(track) do
          {:ok, List.duplicate(parsed_track, count)}
        else
          {:error, {_reason, _diagnostic}} = error -> error
          _ -> :error
        end

      _ ->
        with {:ok, track} <- parse_grid_track(normalized), do: {:ok, [track]}
    end
  end

  defp parse_grid_track(value) do
    normalized = value |> String.trim() |> String.downcase()

    cond do
      String.starts_with?(normalized, "minmax(") ->
        parse_minmax_grid_track(normalized)

      normalized == "auto" ->
        {:ok, :auto}

      Regex.match?(~r/^\d+(?:\.\d+)?fr$/u, normalized) ->
        with {number, "fr"} <- HtmlValidator.parse_css_number(normalized),
             do: {:ok, {:fr, number}}

      true ->
        with {:ok, length} <- parse_length(normalized), do: {:ok, {:length, length}}
    end
  end

  defp parse_minmax_grid_track(value) do
    case Regex.named_captures(~r/^minmax\((?<min>.+)\s*,\s*(?<max>.+)\)$/u, value) do
      %{"min" => min_track, "max" => max_track} ->
        with {:ok, minimum} <- parse_grid_track(min_track),
             {:ok, maximum} <- parse_grid_track(max_track) do
          minimum_valid = minimum == :auto or match?({:length, _length}, minimum)

          maximum_valid =
            maximum == :auto or
              match?({:length, _length}, maximum) or
              match?({:fr, _fraction}, maximum)

          case minimum_valid and maximum_valid do
            true -> {:ok, {:minmax, minimum, maximum}}
            false -> :error
          end
        end

      _ ->
        :error
    end
  end

  defp put_grid_placement(style, property, value) do
    case property do
      property when property in ["grid-column-start", "grid-column-end"] ->
        with {:ok, placement} <- parse_grid_line(value) do
          {:ok, Map.put(style, grid_line_property(property), placement)}
        end

      property when property in ["grid-row-start", "grid-row-end"] ->
        with {:ok, placement} <- parse_grid_line(value) do
          {:ok, Map.put(style, grid_line_property(property), placement)}
        end

      "grid-column" ->
        with {:ok, start_line, end_line} <- parse_grid_axis(value) do
          {:ok,
           style
           |> Map.put(:grid_column_start, start_line)
           |> Map.put(:grid_column_end, end_line)}
        end

      "grid-row" ->
        with {:ok, start_line, end_line} <- parse_grid_axis(value) do
          {:ok,
           style
           |> Map.put(:grid_row_start, start_line)
           |> Map.put(:grid_row_end, end_line)}
        end

      "grid-area" ->
        with {:ok, row_start, column_start, row_end, column_end} <- parse_grid_area(value) do
          {:ok,
           style
           |> Map.put(:grid_row_start, row_start)
           |> Map.put(:grid_column_start, column_start)
           |> Map.put(:grid_row_end, row_end)
           |> Map.put(:grid_column_end, column_end)}
        end
    end
  end

  defp parse_grid_axis(value) do
    case value |> String.trim() |> String.downcase() |> String.split("/", trim: true) do
      [start_line] ->
        with {:ok, start_line} <- parse_grid_line(start_line), do: {:ok, start_line, :auto}

      [start_line, end_line] ->
        with {:ok, start_line} <- parse_grid_line(start_line),
             {:ok, end_line} <- parse_grid_line(end_line) do
          {:ok, start_line, end_line}
        end

      _ ->
        :error
    end
  end

  defp parse_grid_area(value) do
    case value |> String.trim() |> String.downcase() |> String.split("/", trim: true) do
      [row_start, column_start, row_end, column_end] ->
        with {:ok, row_start} <- parse_grid_line(row_start),
             {:ok, column_start} <- parse_grid_line(column_start),
             {:ok, row_end} <- parse_grid_line(row_end),
             {:ok, column_end} <- parse_grid_line(column_end) do
          {:ok, row_start, column_start, row_end, column_end}
        end

      _ ->
        :error
    end
  end

  defp parse_grid_line(value) do
    normalized = value |> String.trim() |> String.downcase()

    case normalized do
      "auto" ->
        {:ok, :auto}

      _ ->
        parse_grid_number_or_span(normalized)
    end
  end

  defp parse_grid_number_or_span(value) do
    cond do
      Regex.match?(~r/^span\s+[1-9]\d*$/u, value) ->
        ["span", count] = String.split(value, ~r/\s+/u, trim: true)
        {count, ""} = Integer.parse(count)

        with :ok <- HtmlValidator.validate_layout_cardinality(:grid_placement, count) do
          {:ok, {:span, count}}
        end

      Regex.match?(~r/^[1-9]\d*$/u, value) ->
        {line, ""} = Integer.parse(value)

        with :ok <- HtmlValidator.validate_layout_cardinality(:grid_placement, line) do
          {:ok, line}
        end

      true ->
        :error
    end
  end

  defp grid_template_property(property) do
    case property do
      "grid-template-columns" -> :grid_template_columns
      "grid-template-rows" -> :grid_template_rows
    end
  end

  defp grid_auto_property(property) do
    case property do
      "grid-auto-columns" -> :grid_auto_columns
      "grid-auto-rows" -> :grid_auto_rows
    end
  end

  defp grid_line_property(property) do
    case property do
      "grid-column-start" -> :grid_column_start
      "grid-column-end" -> :grid_column_end
      "grid-row-start" -> :grid_row_start
      "grid-row-end" -> :grid_row_end
    end
  end

  defp put_text_align(style, value) do
    case String.downcase(String.trim(value)) do
      "left" -> {:ok, Map.put(style, :text_align, :left)}
      "center" -> {:ok, Map.put(style, :text_align, :center)}
      "right" -> {:ok, Map.put(style, :text_align, :right)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_font_weight(style, value) do
    normalized = value |> String.trim() |> String.downcase()

    cond do
      normalized == "normal" ->
        {:ok, Map.put(style, :font_weight, 400)}

      normalized == "bold" ->
        {:ok, Map.put(style, :font_weight, 700)}

      Regex.match?(~r/^[1-9]00$/u, normalized) ->
        {weight, ""} = Integer.parse(normalized)
        {:ok, Map.put(style, :font_weight, weight)}

      true ->
        {:error, :invalid_document}
    end
  end

  defp put_font_style(style, value) do
    case String.downcase(String.trim(value)) do
      "normal" -> {:ok, Map.put(style, :font_style, :normal)}
      "italic" -> {:ok, Map.put(style, :font_style, :italic)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_font_family(style, value, cache) do
    with {:ok, families, font_face} <-
           resolve_font(
             value,
             Map.fetch!(style, :font_weight),
             Map.fetch!(style, :font_style),
             Map.fetch!(style, :_font_registry),
             cache
           ) do
      {:ok,
       style
       |> Map.put(:font_families, families)
       |> Map.put(:font_family, font_face.family)
       |> Map.put(:font_face, font_face)}
    end
  end

  defp put_font_face(style, cache) do
    with {:ok, families, font_face} <-
           resolve_font(
             Map.fetch!(style, :font_families),
             Map.fetch!(style, :font_weight),
             Map.fetch!(style, :font_style),
             Map.fetch!(style, :_font_registry),
             cache
           ) do
      {:ok,
       style
       |> Map.put(:font_families, families)
       |> Map.put(:font_family, font_face.family)
       |> Map.put(:font_face, font_face)}
    end
  end

  defp parse_box_lengths(value) do
    parse_box_lengths(value, :nonnegative)
  end

  defp parse_box_lengths(value, length_context) do
    HtmlValidator.parse_css_box_lengths(value, length_context)
  end

  defp parse_gap(value) do
    gaps =
      value
      |> String.split(~r/\s+/u, trim: true)
      |> Enum.map(&parse_length/1)

    case gaps do
      [{:ok, both}] -> {:ok, both, both}
      [{:ok, row_gap}, {:ok, column_gap}] -> {:ok, row_gap, column_gap}
      _ -> :error
    end
  end

  defp parse_border_spacing(value) do
    lengths =
      value
      |> String.split(~r/\s+/u, trim: true)
      |> Enum.map(&parse_length/1)

    case lengths do
      [{:ok, both}] -> {:ok, both, both}
      [{:ok, horizontal}, {:ok, vertical}] -> {:ok, horizontal, vertical}
      _ -> :error
    end
  end

  defp parse_length(value) do
    parse_length(value, :nonnegative)
  end

  defp parse_size(value) do
    HtmlValidator.parse_css_size(value)
  end

  defp parse_position_component(value) do
    normalized = String.trim(value)

    case Regex.run(~r/^(-?\d+(?:\.\d+)?)%$/u, normalized) do
      [_, percentage] ->
        with {number, ""} <- HtmlValidator.parse_css_number(percentage),
             do: {:ok, {:percent, number / 100.0}}

      _ ->
        parse_length(normalized, :margin)
    end
  end

  defp parse_two_axis_position(value) do
    tokens = value |> String.trim() |> String.downcase() |> String.split(~r/\s+/u, trim: true)

    case tokens do
      [token] ->
        case position_token(token) do
          {:ok, :horizontal, component} -> {:ok, {component, {:percent, 0.5}}}
          {:ok, :vertical, component} -> {:ok, {{:percent, 0.5}, component}}
          {:ok, :both, component} -> {:ok, {component, component}}
          {:ok, :component, component} -> {:ok, {component, {:percent, 0.5}}}
          _ -> :error
        end

      [first, second] ->
        with {:ok, first_axis, first_component} <- position_token(first),
             {:ok, second_axis, second_component} <- position_token(second),
             {:ok, position} <-
               order_position_components(
                 first_axis,
                 first_component,
                 second_axis,
                 second_component
               ) do
          {:ok, position}
        end

      _ ->
        :error
    end
  end

  defp position_token(token) do
    case token do
      "left" ->
        {:ok, :horizontal, {:percent, 0.0}}

      "right" ->
        {:ok, :horizontal, {:percent, 1.0}}

      "top" ->
        {:ok, :vertical, {:percent, 0.0}}

      "bottom" ->
        {:ok, :vertical, {:percent, 1.0}}

      "center" ->
        {:ok, :both, {:percent, 0.5}}

      token ->
        case parse_position_component(token) do
          {:ok, component} -> {:ok, :component, component}
          _ -> :error
        end
    end
  end

  defp order_position_components(first_axis, first, second_axis, second) do
    cond do
      first_axis == :vertical and second_axis in [:horizontal, :component, :both] ->
        {:ok, {second, first}}

      first_axis in [:horizontal, :component, :both] and
          second_axis in [:vertical, :component, :both] ->
        {:ok, {first, second}}

      first_axis == :both and second_axis == :horizontal ->
        {:ok, {second, first}}

      true ->
        :error
    end
  end

  defp parse_length(value, length_context) do
    HtmlValidator.parse_css_length(value, length_context)
  end

  defp parse_line_height(value) do
    normalized = value |> String.trim() |> String.downcase()

    cond do
      normalized == "normal" ->
        {:ok, :normal}

      match?({:ok, _length}, parse_length(normalized)) ->
        parse_length(normalized)

      true ->
        with {:ok, multiplier} <- parse_nonnegative_number(normalized) do
          {:ok, {:multiplier, multiplier}}
        end
    end
  end

  defp parse_nonnegative_number(value) do
    case HtmlValidator.parse_css_number(String.trim(value)) do
      {number, ""} when number >= 0 -> {:ok, number}
      _ -> :error
    end
  end

  # Keep flow arithmetic numeric while retaining automatic inline-margin
  # intent. Author edge/shorthand overrides have already replaced the raw
  # values, so this records the final cascade rather than stale UA hints.
  defp resolve_auto_margin_values(style) do
    case Map.get(style, :margin) do
      %{top: top, right: right, bottom: bottom, left: left} = margin
      when top == :auto or right == :auto or bottom == :auto or left == :auto ->
        margin =
          Map.new(margin, fn {side, value} -> {side, if(value == :auto, do: 0.0, else: value)} end)

        style = style |> Map.put(:margin, margin) |> Map.put(:margin_after, margin.bottom)

        if left == :auto or right == :auto,
          do: Map.put(style, :margin_inline_auto, %{left: left == :auto, right: right == :auto}),
          else: style

      _ ->
        style
    end
  end

  defp put_box_lengths(style, property, lengths) do
    case property do
      "margin" ->
        style
        |> Map.put(:margin, lengths)
        |> Map.put(:margin_after, lengths.bottom)
        |> Map.delete(:_margin_before_em)
        |> Map.delete(:_margin_after_em)

      "padding" ->
        Map.put(style, :padding, lengths)

      "border-width" ->
        Map.put(style, :border_widths, lengths)
    end
  end

  defp put_edge_length(style, property, length) do
    [box_property, edge] = String.split(property, "-", parts: 2)
    key = String.to_existing_atom(edge)
    map_key = String.to_existing_atom(box_property)
    lengths = style |> Map.fetch!(map_key) |> Map.put(key, length)
    style = Map.put(style, map_key, lengths)

    case property do
      "margin-top" ->
        Map.delete(style, :_margin_before_em)

      "margin-bottom" ->
        style
        |> Map.put(:margin_after, length)
        |> Map.delete(:_margin_after_em)

      _ ->
        style
    end
  end

  defp put_border_edge_width(style, property, width) do
    edge =
      property
      |> String.replace_prefix("border-", "")
      |> String.to_existing_atom()

    Map.update!(style, :border_widths, &Map.put(&1, edge, width))
  end

  defp put_border_edge_color(style, property, color) do
    edge =
      property
      |> String.replace_prefix("border-", "")
      |> String.to_existing_atom()

    Map.update(
      style,
      :border_colors,
      edges(Map.get(style, :border_color, :current_color)),
      &Map.put(&1, edge, color)
    )
  end

  defp put_border_edge_style(style, property, border_style) do
    edge =
      property
      |> String.replace_prefix("border-", "")
      |> String.to_existing_atom()

    Map.update(style, :border_styles, edges(:none), &Map.put(&1, edge, border_style))
  end

  defp border_edge_property(property) do
    property
    |> String.replace_suffix("-width", "")
    |> String.replace_suffix("-color", "")
    |> String.replace_suffix("-style", "")
  end

  defp parse_border(value, current_color) do
    tokens = HtmlValidator.split_css_tokens(value)

    parsed =
      Enum.reduce_while(tokens, %{width: nil, style: nil, color: nil}, fn token, acc ->
        cond do
          is_nil(acc.style) and match?({:ok, _}, parse_border_style(token)) ->
            {:ok, border_style} = parse_border_style(token)
            {:cont, %{acc | style: border_style}}

          is_nil(acc.width) and match?({:ok, _}, parse_border_width(token)) ->
            {:ok, width} = parse_border_width(token)
            {:cont, %{acc | width: width}}

          is_nil(acc.color) and
              match?({:ok, _}, HtmlValidator.parse_css_color(token, current_color)) ->
            {:ok, color} = HtmlValidator.parse_css_color(token, current_color)
            {:cont, %{acc | color: transparent_color_to_nil(color)}}

          true ->
            {:halt, :error}
        end
      end)

    case parsed do
      %{width: width, style: border_style, color: color} ->
        width = width || @medium_border_width
        border_style = border_style || :none

        color =
          if is_nil(color) and not Enum.any?(tokens, &transparent_token?/1),
            do: current_color,
            else: color

        {:ok, edges(width), color, edges(border_style)}

      :error ->
        :error
    end
  end

  defp parse_border_edge(value, current_color) do
    with {:ok, border_widths, border_color, border_styles} <-
           parse_border(value, current_color) do
      {:ok, border_widths.top, border_color, border_styles.top}
    end
  end

  defp parse_border_styles(value) do
    styles =
      value
      |> String.split(~r/\s+/u, trim: true)
      |> Enum.map(&parse_border_style/1)

    case styles do
      [{:ok, all}] ->
        {:ok, edges(all)}

      [{:ok, vertical}, {:ok, horizontal}] ->
        {:ok, edges(vertical, horizontal, vertical, horizontal)}

      [{:ok, top}, {:ok, horizontal}, {:ok, bottom}] ->
        {:ok, edges(top, horizontal, bottom, horizontal)}

      [{:ok, top}, {:ok, right}, {:ok, bottom}, {:ok, left}] ->
        {:ok, edges(top, right, bottom, left)}

      _ ->
        :error
    end
  end

  defp parse_border_colors(value, current_color) do
    colors =
      value
      |> HtmlValidator.split_css_tokens()
      |> Enum.map(fn color ->
        case HtmlValidator.parse_css_color(color, current_color) do
          {:ok, color} -> {:ok, transparent_color_to_nil(color)}
          :error -> :error
        end
      end)

    case colors do
      [{:ok, all}] ->
        {:ok, edges(all)}

      [{:ok, vertical}, {:ok, horizontal}] ->
        {:ok, edges(vertical, horizontal, vertical, horizontal)}

      [{:ok, top}, {:ok, horizontal}, {:ok, bottom}] ->
        {:ok, edges(top, horizontal, bottom, horizontal)}

      [{:ok, top}, {:ok, right}, {:ok, bottom}, {:ok, left}] ->
        {:ok, edges(top, right, bottom, left)}

      _ ->
        :error
    end
  end

  defp parse_border_style(value) do
    normalized = value |> String.trim() |> String.downcase()

    case Enum.find(@border_styles, &(Atom.to_string(&1) == normalized)) do
      nil -> :error
      border_style -> {:ok, border_style}
    end
  end

  defp parse_border_widths(value) do
    widths =
      value
      |> String.split(~r/\s+/u, trim: true)
      |> Enum.map(&parse_border_width/1)

    case widths do
      [{:ok, all}] ->
        {:ok, edges(all)}

      [{:ok, vertical}, {:ok, horizontal}] ->
        {:ok, edges(vertical, horizontal, vertical, horizontal)}

      [{:ok, top}, {:ok, horizontal}, {:ok, bottom}] ->
        {:ok, edges(top, horizontal, bottom, horizontal)}

      [{:ok, top}, {:ok, right}, {:ok, bottom}, {:ok, left}] ->
        {:ok, edges(top, right, bottom, left)}

      _ ->
        :error
    end
  end

  defp parse_border_width(value) do
    case value |> String.trim() |> String.downcase() do
      "thin" -> {:ok, 0.75}
      "medium" -> {:ok, @medium_border_width}
      "thick" -> {:ok, 3.75}
      value -> parse_length(value)
    end
  end

  defp transparent_token?(token) do
    String.downcase(String.trim(token)) == "transparent"
  end

  defp transparent_color_to_nil(color) do
    case color do
      :transparent -> nil
      color -> color
    end
  end

  defp page_break_value(value) do
    case String.downcase(String.trim(value)) do
      "auto" -> {:ok, :auto}
      "always" -> {:ok, :page}
      "page" -> {:ok, :page}
      _ -> :error
    end
  end

  defp load_image_style(src, opts, kind) do
    image_budget = Keyword.fetch!(opts, :__image_budget__)

    case image_source(src, opts, image_budget, kind) do
      {:ok, svg, :svg} ->
        {:ok, %{svg_image: svg}}

      {:ok, data, expected_format} ->
        decoded =
          RenderCache.fetch(
            Keyword.fetch!(opts, :__render_cache__),
            {:decoded_image, data},
            fn -> decode_image(data, image_budget) end,
            fn
              {:ok, %{format: :png} = image} ->
                HtmlValidator.reserve_decoded_image(
                  image_budget,
                  image.width_px,
                  image.height_px,
                  image.decoded_stride
                )

              _ ->
                :ok
            end
          )

        with {:ok, image} <- decoded do
          case is_nil(expected_format) or image.format == expected_format do
            true -> {:ok, %{image: image}}
            false -> :error
          end
        end

      {:error, {_reason, _diagnostic}} = error ->
        error

      _ ->
        :error
    end
  end

  defp rasterize_svg_image(svg, raster_options, image_budget, opts) do
    loader = fn ->
      with {:ok, png} <- SvgRasterizer.rasterize(svg, raster_options, image_budget) do
        PngDecoder.decode(png, image_budget, false)
      end
    end

    RenderCache.fetch(
      Keyword.fetch!(opts, :__render_cache__),
      {:rasterized_svg, svg, raster_options},
      loader,
      fn _image ->
        case SvgValidator.validate(svg, raster_options, image_budget) do
          {:ok, _dimensions} -> :ok
          {:error, _reason} = error -> error
        end
      end
    )
  end

  defp data_uri_image_source(src, image_budget) do
    case Regex.run(~r/^data:image\/svg\+xml;base64,([A-Za-z0-9+\/=\r\n]+)$/u, src) do
      [_, encoded] ->
        with :ok <- HtmlValidator.reserve_image_source(image_budget, byte_size(encoded)),
             {:ok, svg} <- Base.decode64(String.replace(encoded, ~r/\s/u, "")) do
          {:ok, svg, :svg}
        end

      _ ->
        case Regex.run(~r/^data:(image\/(?:png|jpeg));base64,([A-Za-z0-9+\/=\r\n]+)$/u, src) do
          [_, mime_type, encoded] ->
            with :ok <- HtmlValidator.reserve_image_source(image_budget, byte_size(encoded)),
                 {:ok, data} <- Base.decode64(String.replace(encoded, ~r/\s/u, "")) do
              {:ok, data, data_uri_format(mime_type)}
            end

          _ ->
            :error
        end
    end
  end

  defp svg_raster_options(style) do
    case Map.get(style, :object_fit, :fill) do
      fit when fit in [:contain, :cover] ->
        []

      _ ->
        [
          width: style |> target_image_width() |> points_to_pixels(),
          height: style |> target_image_height() |> points_to_pixels()
        ]
        |> Enum.reject(fn {_key, value} -> is_nil(value) end)
    end
  end

  defp target_image_width(style) do
    case {Map.get(style, :width), Map.get(style, :height), Map.get(style, :aspect_ratio)} do
      {width, _height, _ratio} when is_number(width) ->
        width

      {_width, height, ratio} when is_number(height) and is_number(ratio) ->
        HtmlValidator.aspect_ratio_dimension(height, ratio, :width, :css)

      _ ->
        nil
    end
  end

  defp target_image_height(style) do
    case {Map.get(style, :height), Map.get(style, :width), Map.get(style, :aspect_ratio)} do
      {height, _width, _ratio} when is_number(height) ->
        height

      {_height, width, ratio} when is_number(width) and is_number(ratio) ->
        HtmlValidator.aspect_ratio_dimension(width, ratio, :height, :css)

      _ ->
        nil
    end
  end

  defp points_to_pixels(points) do
    case points do
      points when is_number(points) -> max(round(points / 0.75), 1)
      _ -> nil
    end
  end

  defp image_source(src, opts, image_budget, kind) do
    case String.starts_with?(src, "data:") do
      true ->
        data_uri_image_source(src, image_budget)

      false ->
        result =
          case ResourceContext.current() do
            %{depth: depth} = context when depth > 0 ->
              AssetLoader.resolve_local(src, kind, context, opts)

            _ ->
              AssetLoader.resolve(src, kind, opts, image_budget)
          end

        with {:ok, data} <- result, do: {:ok, data, nil}
    end
  end

  defp data_uri_format(mime_type) do
    case mime_type do
      "image/png" -> :png
      "image/jpeg" -> :jpeg
    end
  end

  defp decode_image(data, image_budget) do
    cond do
      String.starts_with?(data, <<137, 80, 78, 71, 13, 10, 26, 10>>) ->
        PngDecoder.decode(data, image_budget, true)

      String.starts_with?(data, <<255, 216>>) ->
        decode_jpeg(data)

      true ->
        :error
    end
  end

  defp decode_jpeg(data) do
    with {:ok, metadata} <- HtmlValidator.jpeg_metadata(data) do
      {:ok,
       Map.merge(metadata, %{
         format: :jpeg,
         data: data,
         width: metadata.width_px * 0.75,
         height: metadata.height_px * 0.75,
         bits_per_component: 8
       })}
    end
  end
end
