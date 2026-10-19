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
  alias NativeElixirPdfUtilities.Validators.ObjectValidator
  alias NativeElixirPdfUtilities.Validators.ResourceValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.CssSourceContextValidator
  alias NativeElixirPdfUtilities.Validators.CssValueValidator
  alias NativeElixirPdfUtilities.Validators.FormValidator
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.HtmlLinkValidator
  alias NativeElixirPdfUtilities.Validators.HtmlUrlValidator
  alias NativeElixirPdfUtilities.Validators.HtmlMetadataValidator
  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator
  alias NativeElixirPdfUtilities.Validators.PrintMediaValidator
  alias NativeElixirPdfUtilities.Validators.HtmlCssSourceValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.LanguageValidator
  alias NativeElixirPdfUtilities.Validators.SvgValidator
  alias NativeElixirPdfUtilities.Validators.SvgStyleValidator
  alias NativeElixirPdfUtilities.Validators.MathLayoutValidator
  alias NativeElixirPdfUtilities.Validators.MathTableValidator
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

  @medium_border_width 2.25
  # Fixed style values share literal edge maps across the styled document.
  @zero_edges CssValueValidator.edges(0.0)
  @none_edges CssValueValidator.edges(:none)
  @current_color_edges CssValueValidator.edges(:current_color)
  @black_edges CssValueValidator.edges({0, 0, 0})
  @medium_border_edges CssValueValidator.edges(@medium_border_width)
  @cell_padding_edges CssValueValidator.edges(0.75)
  @control_padding_edges CssValueValidator.edges(1.5)
  @rem_opaque_style_keys [
    :_font_registry,
    :font_face,
    :_root_font_face,
    :_inherited_font,
    :image,
    :svg_image,
    :_text_decorations,
    :embedded_document,
    :_frameset,
    :_frameset_parent,
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
  # Styled surfaces retain source/link/canvas metadata and may have direct text
  # children, as in the validated source editor. Required rendering fields stay typed.
  @type styled_tree :: %{
          required(:type) => :document,
          required(:children) => [text_node() | styled_element()],
          optional(atom()) => term()
        }
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()
  @type stylesheet_entry :: %{
          css: String.t(),
          source_kind: :configured | :embedded,
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
    budget =
      case ResourceContext.current() do
        nil -> HtmlValidator.new_image_budget()
        context -> context.image_budget
      end

    compute_detailed(dom, opts, budget)
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
          term(),
          term()
        ) :: {:ok, styled_tree()} | {:error, {atom(), map()}}
  @spec compute_child_detailed(
          term(),
          [render_option()],
          HtmlValidator.image_budget(),
          String.t() | nil,
          term(),
          term()
        ) ::
          {:ok, styled_tree()} | {:error, {atom(), map()}}
  def compute_child_detailed(
        dom,
        opts,
        image_budget,
        document_url,
        client,
        media,
        frame_margins \\ nil
      ) do
    with :ok <- ResourceValidator.validate_context(ResourceContext.current()),
         {:ok, client, media} <- EmbeddedViewportValidator.prepare_media_viewport(client, media),
         {:ok, frame_margins} <-
           EmbeddedViewportValidator.prepare_body_margins(frame_margins) do
      compute_styles(
        dom,
        opts,
        image_budget,
        {:document_viewport, document_url, client, media},
        :render,
        frame_margins
      )
    end
  end

  @doc false
  @spec prepare_svg_detailed(term(), [render_option()]) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare_svg_detailed(dom, opts \\ []) do
    budget =
      case ResourceContext.current() do
        nil -> HtmlValidator.new_image_budget()
        context -> context.image_budget
      end

    compute_styles(dom, opts, budget, :caller, :prepare_svg)
  end

  defp compute_styles(
         dom,
         opts,
         image_budget,
         document_url,
         mode \\ :render,
         frame_margins \\ nil
       ) do
    RenderCache.with_stylesheets(fn ->
      HtmlValidator.with_render_budget(fn ->
        FontValidator.with_budget(fn ->
          font_options = Font.normalize_options(opts)

          case HtmlValidator.prepare_style_input(dom, opts, font_options, mode) do
            {:ok, initial_controls?} ->
              {:ok, opts} = font_options

              opts =
                if is_nil(frame_margins),
                  do: opts,
                  else: Keyword.put(opts, :__frame_body_margins__, frame_margins)

              %{type: :document} = dom

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
                   {:ok, composition, children} <-
                     NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.prepare_composition(
                       dom,
                       initial_controls?
                     ),
                   {:ok, link_context} <-
                     HtmlLinkValidator.prepare_composition(
                       children,
                       url_context,
                       composition,
                       ResourceContext.current()
                     ),
                   {:ok, image_maps} <-
                     NativeElixirPdfUtilities.Validators.ImageMapValidator.prepare(
                       children,
                       link_context
                     ),
                   {:ok, media_context} <-
                     (case document_url do
                        {:document_viewport, _url, _viewport, media} ->
                          PrintMediaValidator.client_context(opts, media)

                        {:document_url, _url} ->
                          {:ok, :unpositioned}

                        :caller ->
                          PrintMediaValidator.renderer_context(opts)
                      end),
                   {:ok, entries} <- stylesheet_entries(dom.children, opts, media_context),
                   shadow_entries <-
                     composition.roots
                     |> Enum.sort_by(fn {_scope, root} -> root.order end)
                     |> Enum.flat_map(fn {scope, root} ->
                       root.children
                       |> HtmlMetadataValidator.selected_shadow_style_nodes(media_context)
                       |> Enum.map(fn style ->
                         %{
                           css: Enum.map_join(style.children, "", & &1.text),
                           base_url: Keyword.get(opts, :base_url),
                           trusted_local_resources: false,
                           shadow_scope_id: scope,
                           shadow_parent_scope_id: root.host_tree_scope_id,
                           shadow_host_id: root.host_id,
                           shadow_depth: root.depth
                         }
                       end)
                     end),
                   entries <- entries ++ shadow_entries,
                   {:ok, stylesheet_entries} <- prepare_stylesheet_entries(entries, media_context),
                   {:ok, css_fonts} <- stylesheet_fonts(stylesheet_entries, opts),
                   {:ok, font_registry} <-
                     font_registry(
                       opts,
                       css_fonts,
                       Map.new(composition.roots, fn {scope, root} ->
                         {scope, root.host_tree_scope_id}
                       end)
                     ),
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

                base_style = base_style(font_registry, font_families, font_face)

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

                  context = %{
                    rules: compiled_rules,
                    cache: cache,
                    root_selector_id: root_id,
                    root_canvas_key: make_ref(),
                    image_maps: image_maps,
                    position_viewport: media_context
                  }

                  context =
                    if Map.get(compiled_rules.selectors, :positional_dependent, false) do
                      context = Map.put(context, :root_children, children)

                      if map_size(composition.roots) > 0,
                        do:
                          Map.merge(context, %{
                            logical_nodes:
                              Map.merge(
                                NativeElixirPdfUtilities.Validators.HtmlSelectedContentValidator.logical_index(
                                  dom
                                ),
                                Map.get(composition, :clone_logical_nodes, %{})
                              ),
                            consumed_shadow_roots: composition.roots
                          }),
                        else: context
                    else
                      context
                    end

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
                        root_canvas_style =
                          RenderCache.fetch(cache, {:root_canvas, context.root_canvas_key}, fn ->
                            nil
                          end)

                        body_style =
                          Enum.find_value(styled_children, fn node ->
                            case node do
                              %{tag: "body", style: style} -> style
                              _ -> nil
                            end
                          end)

                        canvas =
                          HtmlValidator.prepare_document_canvas(root_canvas_style, body_style)

                        {:ok,
                         %{
                           type: :document,
                           children: styled_children,
                           document_canvas: canvas,
                           document_root_sizing:
                             NativeElixirPdfUtilities.Validators.CssSizingValidator.prepare_root(
                               root_canvas_style
                             )
                         }
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
                  # Native temporal formats also depend on inherited language,
                  # even when the author stylesheet has no :lang selector.
                  LanguageValidator.with_composed_document(
                    dom,
                    composition.roots,
                    Map.get(composition, :clone_logical_nodes, %{}),
                    cache,
                    fn ->
                      compute_styles.(cache)
                    end
                  )
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

  @doc """
  Visits original HTML declaration contexts without reading fonts, files or assets.

  This internal execution path shares normal DOM validation, defaults, inheritance,
  selectors and cascade ordering. The three-argument consumer receives the original
  node with added render metadata, a pseudo-element or nil, and a limited context.
  `:resolved` means declaration/context policy only, never a render-ready style.
  Detached template contents retain their unresolved authored scope. MathML
  contexts need selected MATH metrics and remain unresolved without font reads.

  `source_obligations.declaration_contexts` records each exact authored inline or
  rule declaration and the original node/pseudo contexts where its shared value
  validation completed. Declarations without such a context retain an explicit
  unresolved reason, including detached template and inactive stylesheet sources.
  Element-independent literal checks carry `:checked_without_element_context`
  with empty contexts and retain their authored source ownership.
  Unapplied stylesheet values carry `:checked_symbolically` with their exact
  source grammar and dependency types, empty element contexts and the actual
  original selection basis. They grant no measured geometry, selected font or
  resource permission. A matching rule awaiting real Math metrics stays pending.
  Checked contexts do not certify any future insertion.

  The result preserves source obligations for unmatched rules, inactive media,
  references and detached insertion contexts. Font selection, metrics, decorations
  and resource authorization/contents remain unchecked runtime obligations.
  Canonical inline and embedded CSS records bind verified original HTML tokens,
  including newline preprocessing and attribute references. Metadata-free trees
  retain local records with `provenance: :unbound`. Record owners carry unchanged
  logical identities, never resource permissions.

  CSS sources are strings, never filenames to load. No certificate, receipt,
  permission or parser mutation is produced. Keep the enclosing render budget and
  the same CSS budget across the original invocation.
  """
  @spec with_source_style_contexts(term(), term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec with_source_style_contexts(term(), term(), term(), term(), keyword()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def with_source_style_contexts(dom, sources, budget, consumer, opts \\ []) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, controls?} <-
             HtmlValidator.validate_source_style_input(dom, sources, budget, consumer, opts),
           children =
             if(controls?,
               do:
                 NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_initial_nodes(
                   dom.children
                 ),
               else: HtmlValidator.assign_render_metadata(dom.children)
             ),
           {:ok, provenance} <- HtmlCssSourceValidator.prepare(%{dom | children: children}),
           entries = Enum.map(sources, &%{css: &1}),
           {:ok, media_context} <- PrintMediaValidator.renderer_context(opts),
           selected =
             children
             |> HtmlMetadataValidator.selected_document_style_nodes(media_context)
             |> MapSet.new(& &1._selector_id),
           embedded =
             Enum.map(provenance.embedded, fn entry ->
               Map.put(
                 entry,
                 :applicable,
                 MapSet.member?(selected, Map.get(entry.node, :_selector_id))
               )
             end),
           {:ok, prepared} <- prepare_stylesheet_entries(entries ++ embedded, media_context, true) do
        with {:ok, result, obligations} <-
               CssSourceContextValidator.with_context_obligations(
                 prepared,
                 provenance,
                 fn context_entries, inline, observe, select ->
                   {:ok, rules} = stylesheet_rules(context_entries)
                   compiled = compile_selector_rules(rules)

                   RenderCache.run(fn cache ->
                     context = %{
                       rules: compiled,
                       cache: cache,
                       root_selector_id: cache,
                       root_children: children,
                       source_budget: budget,
                       inline_records: inline,
                       source_context_observer: observe,
                       source_selection_observer: select
                     }

                     traverse = fn ->
                       source_selection_children(children, context, [])

                       with {:ok, inherited} <-
                              fragment_root_style(
                                children,
                                source_base_style(children),
                                context,
                                opts
                              ),
                            :ok <-
                              source_context_children(children, inherited, context, [], consumer) do
                         {:ok,
                          %{
                            source_provenance: provenance,
                            font_source_entries: context_entries,
                            source_obligations: %{
                              stylesheets: Enum.map(entries ++ embedded, & &1.css),
                              inline: provenance.records,
                              unmatched_rules: :unresolved,
                              inactive_media: :unresolved,
                              resource_references: :unresolved,
                              template_insertion_contexts: :unresolved,
                              math_font_contexts: :unresolved,
                              authored:
                                Enum.map(
                                  prepared,
                                  &Map.take(
                                    &1.prepared,
                                    [
                                      :authored_rules,
                                      :authored_fonts,
                                      :authored_pages,
                                      :authored_media,
                                      :media_provider,
                                      :media_policy,
                                      :stylesheet_applicable
                                    ]
                                  )
                                )
                            },
                            runtime_obligations: [
                              :font_selection,
                              :font_metrics,
                              :text_decorations,
                              :resources
                            ]
                          }}
                       end
                     end

                     if Map.get(compiled.selectors, :language_dependent, false),
                       do: LanguageValidator.with_document(children, cache, traverse),
                       else: traverse.()
                   end)
                 end,
                 budget
               ) do
          {:ok, put_in(result.source_obligations[:declaration_contexts], obligations)}
        end
      end
    end)
  end

  defp base_style(registry, families, face) do
    %{
      _custom_properties: %{},
      _font_registry: registry,
      _root_font_size: 12.0,
      _root_font_face: face,
      _root_line_height: 14.4,
      color: {0, 0, 0},
      font_face: face,
      font_families: families,
      font_family: if(is_nil(face), do: nil, else: face.family),
      font_size: 12.0,
      font_style: :normal,
      font_weight: 400,
      letter_spacing: 0.0,
      line_height: 14.4,
      line_height_normal: true,
      text_align: :left,
      _text_align_initial: true,
      caption_side: :top,
      text_transform: :none,
      quotes: :auto,
      white_space: :normal
    }
  end

  defp source_base_style(children) do
    style = base_style(nil, [], nil)

    if svg_nodes?(children),
      do: Map.merge(style, SvgStyleValidator.defaults(%{}) |> Map.put(:_svg_prepare, true)),
      else: style
  end

  # Record actual selector applicability before declaration execution. In
  # particular, a matching Math rule is still applied even while its selected
  # metrics are pending. Detached fragment contents use their own later scope.
  defp source_selection_children(children, context, ancestors) do
    Enum.each(children, fn node ->
      if node.type == :element do
        Enum.each(context.rules.selectors, fn {pseudo, index} ->
          if is_map(index) do
            Enum.each(matching_declarations(context, node, ancestors, pseudo), fn matched ->
              record = Map.get(matched, :source_record, matched)
              :ok = context.source_selection_observer.(record, node, pseudo)
            end)
          end
        end)

        source_selection_children(source_children(node), context, [node | ancestors])
      end
    end)
  end

  # Source policy walks authored children even when painting selects a subset.
  # Detached template contents have no final insertion context to execute here.
  defp source_context_children(children, inherited, rules, ancestors, consumer) do
    # Detached template source enters this phase after the ordinary document
    # table preparation. Reuse canonical span/model preparation for that source.
    with {:ok, children} <- HtmlTableValidator.prepare_document(children) do
      Enum.reduce_while(children, :ok, fn node, :ok ->
        HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

        result =
          case node do
            %{type: :element, namespace: :mathml} ->
              unresolved_source_context(node, consumer, :math_font_context)

            %{type: :element} ->
              with {:ok, style} <- element_style(node, inherited, rules, ancestors, []),
                   style <-
                     NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_initial_control_box(
                       node,
                       style
                     ),
                   :ok <-
                     HtmlValidator.consume_source_style_context(consumer, node, nil, %{
                       status: :resolved,
                       style: style
                     }),
                   :ok <- source_pseudo_contexts(node, style, rules, ancestors, consumer),
                   :ok <-
                     source_context_children(
                       source_children(node),
                       style,
                       rules,
                       [node | ancestors],
                       consumer
                     ) do
                case Map.get(node, :content) do
                  %{type: :document_fragment} = fragment ->
                    unresolved_source_context(fragment, consumer, :template_insertion_context)

                  _ ->
                    :ok
                end
              end

            _ ->
              :ok
          end

        case result do
          :ok -> {:cont, :ok}
          error -> {:halt, error}
        end
      end)
    end
  end

  defp source_children(node) do
    if node.tag == "details" and Map.get(node, :namespace, :html) == :html do
      HtmlValidator.source_disclosure_children(node)
    else
      node.children
    end
  end

  defp source_pseudo_contexts(node, style, rules, ancestors, consumer) do
    Enum.reduce_while([:before, :after], :ok, fn pseudo, :ok ->
      declarations = matching_declarations(rules, node, ancestors, pseudo)

      result =
        case declarations do
          [] ->
            :ok

          _ ->
            with {:ok, context} <-
                   apply_author_styles(
                     text_style(style) |> Map.put(:display, :inline),
                     node,
                     ancestors,
                     rules,
                     pseudo,
                     declarations
                   ),
                 do:
                   HtmlValidator.consume_source_style_context(consumer, node, pseudo, %{
                     status: :resolved,
                     style: context
                   })
        end

      case result do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp unresolved_source_context(node, consumer, reason) do
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

    with :ok <-
           HtmlValidator.consume_source_style_context(consumer, node, nil, %{
             status: :unresolved,
             reason: reason
           }),
         :ok <-
           Enum.reduce_while(Map.get(node, :children, []), :ok, fn child, :ok ->
             case unresolved_source_context(child, consumer, reason) do
               :ok -> {:cont, :ok}
               error -> {:halt, error}
             end
           end) do
      case Map.get(node, :content) do
        %{type: :document_fragment} = fragment ->
          unresolved_source_context(fragment, consumer, :template_insertion_context)

        _ ->
          :ok
      end
    end
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

  defp prepare_stylesheet_entries(entries, media_context, source? \\ false) do
    document_id =
      case ResourceContext.current() do
        nil -> nil
        context -> context.document_id
      end

    RenderCache.fetch_stylesheet(
      {:entries, source?, document_id, entries, media_context, Limits.effective()},
      fn ->
        Enum.reduce_while(entries, {:ok, []}, fn entry, {:ok, prepared_entries} ->
          prepared =
            if source?,
              do:
                CssParser.prepare_source_stylesheet(
                  entry.css,
                  Map.get(entry, :descriptor),
                  media_context
                ),
              else: CssParser.prepare_stylesheet(entry.css, media_context)

          with {:ok, prepared} <- prepared,
               prepared =
                 if(source?,
                   do:
                     if(Map.get(entry, :applicable, true),
                       do: Map.put(prepared, :stylesheet_applicable, true),
                       else:
                         Map.merge(prepared, %{
                           stylesheet_applicable: false,
                           rules: [],
                           font_faces: [],
                           page_options: []
                         })
                     ),
                   else: prepared
                 ),
               {:ok, prepared} <-
                 if(source?,
                   do: {:ok, prepared},
                   else:
                     NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.bind_stylesheet(
                       entry,
                       prepared,
                       media_context
                     )
                 ) do
            {:cont, {:ok, [Map.put(entry, :prepared, prepared) | prepared_entries]}}
          else
            {:error, _} = error -> {:halt, error}
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

  # Keep the authored or cloned identity on the box-owning styled node. Pseudo
  # nodes remain generated nodes and never impersonate an authored element.
  defp style_node(node, inherited_style, rules, ancestors, opts, counters, parent_counters) do
    case style_prepared_node(
           node,
           inherited_style,
           rules,
           ancestors,
           opts,
           counters,
           parent_counters
         ) do
      {:ok, rendered, sibling_counters, counters} ->
        rendered =
          NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.prepare_source_completion_paint(
            node,
            rendered
          )

        rendered =
          Enum.map(rendered, fn styled ->
            if Map.get(node, :type) == :element and Map.get(styled, :tag) == Map.get(node, :tag),
              do:
                Map.merge(
                  :maps.with(
                    [
                      :attributes,
                      :namespace,
                      :attribute_namespaces,
                      :attribute_order,
                      :node_id,
                      :document_id,
                      :tree_scope_id,
                      :parent_id,
                      :composed_parent_id,
                      :source,
                      :cloned_from_node_id,
                      :_html_translate,
                      :_html_inert,
                      :_html_editable
                    ],
                    node
                  ),
                  styled
                ),
              else: styled
          end)

        {:ok, rendered, sibling_counters, counters}

      error ->
        error
    end
  end

  defp style_prepared_node(
         node,
         inherited_style,
         rules,
         ancestors,
         opts,
         counters,
         parent_counters
       ) do
    case node do
      %{type: :processing_instruction} ->
        {:ok, [], nil, counters}

      %{type: :element, namespace: :svg} ->
        svg_ancestors =
          for ancestor <- ancestors, Map.get(ancestor, :namespace) == :svg, do: ancestor.tag

        applicable? =
          NativeElixirPdfUtilities.Validators.SvgForeignObjectValidator.layout_applicable?(
            node,
            svg_ancestors
          )

        with {:ok, style} <- element_style(node, inherited_style, rules, ancestors, opts),
             {:ok, children, counters} <-
               style_children(
                 if(applicable?, do: node.children, else: []),
                 text_style(style),
                 rules,
                 [node | ancestors],
                 opts,
                 counters,
                 parent_counters
               ) do
          prepared =
            Map.merge(node, %{
              style: style,
              children: if(applicable?, do: children, else: node.children)
            })

          outer? = not Enum.any?(ancestors, &(Map.get(&1, :namespace) == :svg))

          if outer? and not Map.get(style, :_svg_measurement_only, false) do
            with {:ok, scene} <-
                   NativeElixirPdfUtilities.HtmlToPdf.SvgScene.prepare(
                     prepared,
                     Keyword.fetch!(opts, :__image_budget__),
                     opts
                   ) do
              style =
                style |> CssValueValidator.ensure_box_style() |> resolve_current_color_values()

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

          "noframes" ->
            with {:ok, style} <- element_style(node, inherited_style, rules, ancestors, opts),
                 :ok <-
                   NativeElixirPdfUtilities.Validators.FramesetValidator.validate_fallback_style(
                     style
                   ),
                 do: {:ok, [], counters, counters}

          "frameset" ->
            with {:ok, style} <- element_style(node, inherited_style, rules, ancestors, opts),
                 {:ok, style} <-
                   NativeElixirPdfUtilities.Validators.FramesetValidator.prepare_initial_style(
                     style
                   ) do
              with :ok <-
                     NativeElixirPdfUtilities.Validators.FramesetValidator.validate_style(style),
                   {:ok, plan} <-
                     NativeElixirPdfUtilities.Validators.FramesetValidator.prepare(
                       attributes,
                       Map.get(inherited_style, :_frameset_parent),
                       style
                     ),
                   style <- Map.put(style, :_frameset, plan),
                   {:ok, children, sibling_counters} <-
                     style_children(
                       children,
                       text_style(style),
                       rules,
                       [node | ancestors],
                       opts,
                       counters,
                       counters
                     ),
                   {:ok, plan} <-
                     NativeElixirPdfUtilities.Validators.FramesetValidator.prepare_children(
                       plan,
                       children
                     ) do
                {:ok,
                 [
                   %{
                     type: :element,
                     tag: tag,
                     style: Map.put(style, :_frameset, plan),
                     children: children
                   }
                 ], sibling_counters, sibling_counters}
              end
            end

          tag when tag in ["html", "body"] ->
            with {:ok, element_style} <-
                   element_style(node, inherited_style, rules, ancestors, opts) do
              children =
                if Map.get(element_style, :_html_content_skipped, false), do: [], else: children

              case Map.get(element_style, :display) do
                :none ->
                  {:ok, [], counters, counters}

                _ ->
                  if tag == "html" and ancestors == [] do
                    RenderCache.fetch(rules.cache, {:root_canvas, rules.root_canvas_key}, fn ->
                      element_style
                    end)
                  end

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

          tag when tag in ["iframe", "frame"] ->
            with {:ok, style} <- element_style(node, inherited_style, rules, ancestors, opts),
                 {:ok, style} <-
                   if(tag == "frame",
                     do:
                       NativeElixirPdfUtilities.Validators.FramesetValidator.prepare_initial_style(
                         style
                       ),
                     else: {:ok, style}
                   ) do
              if style.display == :none do
                {:ok, [%{type: :element, tag: tag, style: style, children: []}], counters,
                 counters}
              else
                with :ok <-
                       if(tag == "frame",
                         do:
                           NativeElixirPdfUtilities.Validators.FramesetValidator.validate_style(
                             style
                           ),
                         else: :ok
                       ),
                     {:ok, frame_request} <-
                       if(tag == "frame",
                         do:
                           NativeElixirPdfUtilities.Validators.FramesetValidator.prepare_frame(
                             attributes,
                             Map.get(inherited_style, :_frameset_parent)
                           ),
                         else:
                           with {:ok, initial} <-
                                  EmbeddedViewportValidator.prepare_initialization(
                                    attributes,
                                    :iframe
                                  ) do
                             {:ok,
                              Map.put(
                                initial,
                                :source,
                                ResourceValidator.iframe_source(attributes)
                              )}
                           end
                       ),
                     {:ok, child_parent} <-
                       ResourceContext.rebase(
                         ResourceContext.current(),
                         {:document_url,
                          Keyword.fetch!(opts, :__document_url_context__).resolution_base}
                       ),
                     source <- frame_request.source,
                     child_opts <-
                       Keyword.drop(opts, [
                         :__image_budget__,
                         :__template_source_owner__,
                         :__link_context__,
                         :__render_cache__,
                         :__document_url_context__,
                         :__client_css_viewport__,
                         :__frame_body_margins__
                       ]),
                     {:ok, viewport} <-
                       RenderCache.fetch(
                         child_parent.scope,
                         {:iframe_document, tag, child_parent.document_id,
                          child_parent.tree_scope_id, node._document_order, source,
                          frame_request.margins, child_opts},
                         fn ->
                           EmbeddedViewport.prepare_iframe_document(
                             source,
                             child_parent,
                             child_opts
                           )
                         end
                       ),
                     :ok <-
                       if(tag == "frame",
                         do:
                           NativeElixirPdfUtilities.Validators.FramesetValidator.validate_document_navigation(
                             viewport.prepared_dom
                           ),
                         else: :ok
                       ) do
                  style =
                    if tag == "frame" do
                      Map.put(
                        style,
                        :embedded_document,
                        Map.merge(viewport, %{
                          host: :frame,
                          frame_border: frame_request.border_enabled,
                          frame_scrollbars: frame_request.scrollbars,
                          frame_margins: frame_request.margins,
                          frame_name: frame_request.name
                        })
                      )
                    else
                      style
                      |> EmbeddedViewportValidator.prepare_iframe_style()
                      |> Map.put(
                        :embedded_document,
                        Map.merge(viewport, %{
                          frame_scrollbars: frame_request.scrollbars,
                          frame_margins: frame_request.margins
                        })
                      )
                    end

                  {:ok, [%{type: :element, tag: tag, style: style, children: []}], counters,
                   counters}
                end
              end
            end

          _ ->
            with {:ok, element_style} <-
                   element_style(node, inherited_style, rules, ancestors, opts),
                 element_style <-
                   NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_initial_control_box(
                     node,
                     element_style
                   ),
                 {:ok, element_style} <-
                   NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_initial_auxiliary_style(
                     node,
                     element_style
                   ),
                 {:ok, element_style} <-
                   NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_special_input_style(
                     node,
                     element_style,
                     opts
                   ),
                 {:ok, element_style} <-
                   NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_initial_toggle_style(
                     node,
                     element_style
                   ),
                 {:ok, element_style} <-
                   NativeElixirPdfUtilities.Validators.HtmlFileValidator.prepare_render_style(
                     node,
                     element_style,
                     ancestors,
                     opts
                   ),
                 {:ok, element_style} <-
                   NativeElixirPdfUtilities.Validators.HtmlTemporalValidator.prepare_render_style(
                     node,
                     element_style,
                     ancestors,
                     opts
                   ),
                 {:ok, element_style} <-
                   if(get_in(element_style, [:_temporal_display, :picker_visible]),
                     do:
                       with {:ok, image} <-
                              rasterize_svg_image(
                                NativeElixirPdfUtilities.HtmlToPdf.TemporalArtwork.picker_source(
                                  element_style._temporal_display.kind
                                ),
                                [
                                  width:
                                    ceil(
                                      element_style._temporal_display.indicator_content_size /
                                        0.75 *
                                        NativeElixirPdfUtilities.Limits.get(:svg_raster_scale)
                                    )
                                ],
                                Keyword.fetch!(opts, :__image_budget__),
                                opts
                              ) do
                         {:ok, Map.put(element_style, :_temporal_picker_image, image)}
                       end,
                     else: {:ok, element_style}
                   ),
                 {:ok, selected_style, selected_children} <-
                   prepare_embedded_element(node, element_style, children, opts),
                 {element_style, children} <-
                   if(
                     NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.source_completion?(
                       node
                     ) and tag in ~w(object embed),
                     do: {element_style, children},
                     else: {selected_style, selected_children}
                   ) do
              case Map.get(element_style, :display) do
                _ when is_map_key(element_style, :_html_content_skipped) ->
                  skipped =
                    Map.merge(
                      Map.take(node, [:_initial_form_state, :_initial_gauge_state]),
                      %{type: :element, tag: tag, style: element_style, children: []}
                    )

                  {:ok, [skipped], counters, counters}

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
                         (not Map.has_key?(node, :_html_inert) or
                            Map.has_key?(element_style, :_range_state) or
                            Map.has_key?(element_style, :_color_state) or
                            Map.has_key?(element_style, :_temporal_display)) and
                         Map.get(element_style, :form_kind) != :input_button and
                         not Map.has_key?(element_style, :_image_input_state) and
                         not Map.has_key?(element_style, :_file_display) do
                      control = %{
                        tag: tag,
                        attributes:
                          NativeElixirPdfUtilities.Validators.HtmlFormValidator.initial_control_attributes(
                            node
                          ),
                        children: Map.get(node, :_initial_form_children, children),
                        id: node._selector_id,
                        order: node._document_order,
                        initial_state: Map.fetch!(node, :_initial_form_state),
                        editor_style:
                          Map.take(element_style, [
                            :font_face,
                            :font_size,
                            :font_weight,
                            :font_style,
                            :font_families,
                            :color,
                            :letter_spacing,
                            :line_height,
                            :text_align,
                            :white_space,
                            :word_break,
                            :overflow_wrap,
                            :line_break,
                            :padding,
                            :border_widths,
                            :form_kind,
                            :_number_state,
                            :_select_state
                          ])
                      }

                      control =
                        if Map.has_key?(element_style, :_temporal_display),
                          do:
                            Map.put(
                              control,
                              :temporal_display_mode,
                              element_style._temporal_display.forms_mode
                            ),
                          else: control

                      if Map.has_key?(node, :_html_inert) do
                        Map.put(
                          element_style,
                          if(Map.has_key?(element_style, :_temporal_display),
                            do: :_temporal_control,
                            else:
                              if(Map.has_key?(element_style, :_color_state),
                                do: :_color_control,
                                else: :_range_control
                              )
                          ),
                          control
                        )
                      else
                        element_style
                        |> Map.put(:_form_control, control)
                        |> Map.put(:_form_owner, node._selector_id)
                      end
                    else
                      element_style
                    end

                  element_style =
                    Map.merge(element_style, Map.take(node, [:_table_model, :_table_cell]))

                  with rendered_children when is_list(rendered_children) <-
                         if(
                           NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.source_completion?(
                             node
                           ) and tag in ~w(details select option optgroup textarea),
                           do: children,
                           else:
                             if(tag == "details",
                               do: HtmlValidator.disclosure_children(node),
                               else:
                                 if(
                                   tag == "select" and
                                     Map.get(element_style, :form_kind) == :base_select,
                                   do:
                                     case Enum.find(children, &(Map.get(&1, :tag) == "button")) do
                                       nil -> node._initial_form_state.selection.children
                                       button -> [button]
                                     end,
                                   else:
                                     if(
                                       tag == "option" and
                                         Map.get(element_style, :_base_select, false),
                                       do: children,
                                       else: static_form_children(node, children)
                                     )
                                 )
                             )
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
                         ),
                       sibling_counters =
                         include_counters_created_by(
                           before_counters,
                           counters,
                           {node._selector_id, :after}
                         ),
                       {:ok, picker, counters} <-
                         if(
                           tag == "select" and Map.get(element_style, :form_kind) == :base_select,
                           do:
                             generated_content_node(
                               node,
                               :picker_icon,
                               element_style,
                               rules,
                               ancestors,
                               counters
                             ),
                           else: {:ok, [], counters}
                         ) do
                    rendered_children = before ++ styled_children ++ picker ++ after_content

                    rendered_children =
                      if tag == "optgroup" do
                        label_style =
                          block_defaults(element_style.font_size, element_style.font_weight, 0.0)
                          |> Map.merge(text_style(element_style))
                          |> Map.put(:padding, CssValueValidator.edges(0.0, 1.5, 0.75, 1.5))
                          |> Map.put(:border_widths, @zero_edges)
                          |> Map.put(:min_height, element_style.font_size * 1.2)
                          |> Map.put(:line_height, normal_line_height(element_style))

                        [
                          %{
                            type: :element,
                            tag: "optgroup-label",
                            style: label_style,
                            children: [
                              %{
                                type: :text,
                                text: attributes["label"],
                                style: text_style(label_style)
                              }
                            ]
                          }
                          | rendered_children
                        ]
                      else
                        rendered_children
                      end

                    element_style =
                      if tag == "select" and Map.get(element_style, :appearance) != :base_select do
                        state = element_style._select_state

                        rows =
                          Enum.flat_map(rendered_children, fn row ->
                            cond do
                              get_in(row, [:style, :display]) == :none -> []
                              Map.get(row, :tag) == "optgroup" -> row.children
                              true -> [row]
                            end
                          end)
                          |> Enum.filter(
                            &(Map.get(&1, :type) == :element and
                                get_in(&1, [:style, :display]) != :none)
                          )

                        intrinsic_width =
                          if state.listbox do
                            Enum.reduce(rows, 0.0, fn row, width ->
                              text =
                                Enum.map_join(
                                  Map.get(row, :children, []),
                                  &Map.get(&1, :text, "")
                                )

                              row_style = row.style

                              max(
                                width,
                                Font.text_width(text, row_style.font_face, row_style.font_size) +
                                  row_style.padding.left + row_style.padding.right
                              )
                            end) + 11.25
                          else
                            Enum.reduce(state.choices, 0.0, fn choice, width ->
                              max(
                                width,
                                Font.text_width(
                                  choice.label,
                                  element_style.font_face,
                                  element_style.font_size
                                )
                              )
                            end)
                            |> max(
                              Font.text_width(
                                hd(state.children).text,
                                element_style.font_face,
                                element_style.font_size
                              )
                            )
                          end

                        # Menus ceil text to CSS pixels; listboxes retain ordinary
                        # Blink LayoutUnit precision (fixed 1/64 CSS pixel) until paint.
                        intrinsic_width =
                          if(state.listbox,
                            do: Float.ceil(intrinsic_width / (0.75 / 64)) * (0.75 / 64),
                            else: Float.ceil(intrinsic_width / 0.75) * 0.75
                          ) + element_style.padding.left + element_style.padding.right +
                            element_style.border_widths.left + element_style.border_widths.right

                        row_height =
                          Enum.reduce(rows, normal_line_height(element_style) + 0.75, fn row,
                                                                                         height ->
                            row_style = row.style

                            max(
                              height,
                              max(
                                case Map.get(row_style, :height) do
                                  height when is_number(height) -> height
                                  _ -> 0.0
                                end,
                                max(
                                  row_style.line_height,
                                  case Map.get(row_style, :min_height) do
                                    height when is_number(height) -> height
                                    _ -> 0.0
                                  end
                                )
                              ) +
                                row_style.padding.top + row_style.padding.bottom
                            )
                          end)

                        intrinsic_height =
                          if(state.listbox,
                            do: row_height * state.rows,
                            else: element_style.line_height
                          ) +
                            element_style.padding.top + element_style.padding.bottom +
                            element_style.border_widths.top + element_style.border_widths.bottom

                        dimensions = %{width: intrinsic_width, height: intrinsic_height}

                        Enum.reduce(dimensions, element_style, fn {property, extent}, style ->
                          if Map.get(style, property) in [nil, :auto] do
                            case NativeElixirPdfUtilities.Validators.HtmlFormValidator.validate_initial_control_dimension(
                                   extent,
                                   property
                                 ) do
                              {:ok, extent} ->
                                extent =
                                  if style.box_sizing == :content_box,
                                    do:
                                      extent -
                                        if(property == :width,
                                          do:
                                            style.padding.left + style.padding.right +
                                              style.border_widths.left + style.border_widths.right,
                                          else:
                                            style.padding.top + style.padding.bottom +
                                              style.border_widths.top + style.border_widths.bottom
                                        ),
                                    else: extent

                                Map.put(style, property, extent)

                              {:error, _} = error ->
                                throw({:render_resource_limit, error})
                            end
                          else
                            style
                          end
                        end)
                      else
                        element_style
                      end

                    element_style =
                      if tag in ~w(progress meter),
                        do: Map.put(element_style, :_gauge_fallback, children),
                        else: element_style

                    rendered_children =
                      HtmlValidator.resolve_html_list_ordinals(tag, attributes, rendered_children)

                    element_style =
                      HtmlTableValidator.prepare_atomic_flow_style(
                        element_style,
                        rendered_children
                      )

                    element =
                      Map.merge(
                        if(
                          Map.get(element_style, :_svg_prepare, false) or
                            Map.get(node, :namespace) == :mathml,
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

                    result =
                      if element_style.display == :contents do
                        {:ok, rendered_children}
                      else
                        if Map.get(node, :namespace) == :mathml and
                             MathLayoutValidator.independent_math_context?(ancestors) do
                          with {:ok, length_context} <-
                                 MathLayoutValidator.prepare_css_context(opts),
                               math_context =
                                 Map.merge(length_context, %{
                                   resource_context: ResourceContext.current(),
                                   unsupported_glyphs:
                                     Keyword.get(opts, :unsupported_glyphs, :replace)
                                 }),
                               {:ok, measured} <- MathLayout.measure(element, math_context) do
                            owner_style = MathLayoutValidator.owner_atom_style(element)

                            atom_style =
                              owner_style
                              |> Map.put(:display, :inline_block)
                              |> Map.put(:_math_box, measured)

                            atom = %{
                              element
                              | style: atom_style,
                                children: Map.get(measured, :positioned_nodes, [])
                            }

                            if Map.get(element_style, :_math_display) == :block do
                              block_style =
                                if tag == "math", do: element_style, else: owner_style

                              with {:ok, dependent} <-
                                     MathTableValidator.prepare_block_flow_dependency(element) do
                                atom =
                                  if dependent do
                                    atom
                                    |> Map.put(:_math_layout_source, element)
                                    |> Map.put(:_math_layout_context, math_context)
                                  else
                                    atom
                                  end

                                {:ok,
                                 [
                                   %{
                                     element
                                     | children: [atom],
                                       style:
                                         block_style
                                         |> Map.put(:display, :block)
                                         |> Map.put(:_math_flow, true)
                                         |> Map.put(:_math_flow_requires_inline_size, dependent)
                                         |> Map.put(:text_align, :center)
                                   }
                                 ]}
                              end
                            else
                              {:ok, [atom]}
                            end
                          end
                        else
                          {:ok, [element]}
                        end
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

  defp prepare_embedded_element(node, style, children, opts) do
    if node.tag in ["object", "embed"] and style.display != :none do
      host = if node.tag == "embed", do: :embed, else: :object

      with {:ok, parent} <-
             ResourceContext.rebase(
               ResourceContext.current(),
               {:document_url, Keyword.fetch!(opts, :__document_url_context__).resolution_base}
             ),
           {:ok, selected} <- ObjectValidator.select(node.attributes, parent, host) do
        case selected do
          :fallback ->
            {:ok, style, children}

          :empty ->
            {:ok, Map.put(style, :display, :none), []}

          selected ->
            child_opts =
              Keyword.drop(opts, [
                :__image_budget__,
                :__link_context__,
                :__render_cache__,
                :__document_url_context__,
                :__client_css_viewport__,
                :__frame_body_margins__
              ])

            with {:ok, prepared} <-
                   RenderCache.fetch(
                     parent.scope,
                     {:embedded_source, host, parent.document_id, parent.tree_scope_id,
                      node._document_order, selected, child_opts},
                     fn ->
                       EmbeddedViewport.prepare_selected(selected, parent, child_opts, host)
                     end
                   ) do
              case prepared do
                {:viewport, viewport} ->
                  {:ok,
                   style
                   |> EmbeddedViewportValidator.prepare_iframe_style()
                   |> Map.put(:embedded_document, viewport), []}

                {:image, image_style} ->
                  {:ok,
                   style
                   |> Map.put(
                     :image_display,
                     if(style.display in [:inline, :inline_block], do: :inline, else: :block)
                   )
                   |> Map.put(:display, :image)
                   |> Map.put(:image, Map.fetch!(image_style, :image))
                   |> Map.merge(Map.get(image_style, :svg_document_fitting, %{})), []}
              end
            end
        end
      end
    else
      {:ok, style, children}
    end
  end

  defp generated_content_node(node, pseudo_element, inherited_style, rules, ancestors, counters) do
    if Map.get(inherited_style, :_html_content_skipped, false) do
      {:ok, [], counters}
    else
      declarations =
        if Map.get(node, :_default_disclosure_summary, false),
          do: [],
          else: matching_declarations(rules, node, ancestors, pseudo_element)

      case {declarations, node.tag} do
        {[], tag} when tag != "q" and pseudo_element != :picker_icon ->
          {:ok, [], counters}

        {matched_declarations, _tag} ->
          generated_style =
            inherited_style
            |> text_style()
            |> Map.put(:display, if(pseudo_element == :picker_icon, do: :block, else: :inline))
            |> Map.put(:_inherited_quotes, Map.get(inherited_style, :quotes, :auto))
            |> Map.put(
              :content,
              if(node.tag == "q",
                do: [{:quote, if(pseudo_element == :before, do: :open_quote, else: :close_quote)}],
                else: :none
              )
            )
            |> then(fn style ->
              if pseudo_element == :picker_icon,
                do:
                  style
                  |> Map.put(:content, [
                    {:string,
                     NativeElixirPdfUtilities.HtmlToPdf.CounterStyle.marker(:disclosure_open, 0)}
                  ])
                  |> Map.put(:margin, CssValueValidator.edges(0.0, 0.0, 0.0, :auto)),
                else: style
            end)

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

              {text, counters} =
                if style.display == :none do
                  {"", counters}
                else
                  generated_content_text(
                    Map.get(style, :content, :none),
                    node.attributes,
                    counters,
                    NativeElixirPdfUtilities.Validators.HtmlQuoteValidator.pairs(
                      style,
                      node,
                      ancestors
                    )
                  )
                end

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

    # Quote depth belongs to the document, including across containing element
    # boundaries. Ordinary documents retain the empty-counter fast path.
    counters =
      case latest_counters do
        [] ->
          counters

        _ ->
          case List.keyfind(latest_counters, :_quote_depth, 0) do
            nil ->
              counters

            quote ->
              if List.keymember?(counters, :_quote_depth, 0),
                do: counters,
                else: counters ++ [quote]
          end
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

  defp generated_content_text(content, attributes, counters, quote_pairs) do
    {parts, counters} =
      Enum.reduce(content_parts(content), {[], counters}, fn part, {parts, counters} ->
        {text, counters} =
          case part do
            {:string, text} ->
              {text, counters}

            {:attr, name} ->
              {Map.get(attributes, name, ""), counters}

            {:counter, name} ->
              text =
                case innermost_counter_index(counters, name) do
                  nil -> "0"
                  index -> counters |> Enum.at(index) |> elem(2) |> Integer.to_string()
                end

              {text, counters}

            {:quote, kind} ->
              index = innermost_counter_index(counters, :_quote_depth)
              depth = if index == nil, do: 0, else: counters |> Enum.at(index) |> elem(2)

              {text, depth} =
                NativeElixirPdfUtilities.Validators.HtmlQuoteValidator.token(
                  kind,
                  quote_pairs,
                  depth
                )

              counters =
                if index == nil,
                  do: counters ++ [{:_quote_depth, :document, depth}],
                  else: List.replace_at(counters, index, {:_quote_depth, :document, depth})

              {text, counters}
          end

        HtmlValidator.reserve_render_resource(:max_rendered_text_bytes, byte_size(text), :style)
        {[text | parts], counters}
      end)

    {parts |> Enum.reverse() |> IO.iodata_to_binary(), counters}
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
            :none ->
              if Map.has_key?(rules, :source_budget),
                do: {:ok, text_style(body_style)},
                else: {:ok, nil}

            _ ->
              {:ok, text_style(body_style)}
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
    opts =
      if Map.has_key?(rules, :source_budget),
        do: opts,
        else:
          NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.source_owner(
            node,
            opts
          )

    attributes =
      if Map.get(node, :namespace, :html) == :html,
        do: HtmlMetadataValidator.unqualified_attributes(node),
        else: attributes

    inherited_font_size = Map.fetch!(inherited_style, :font_size)

    popover_state =
      if Map.get(node, :namespace, :html) == :html,
        do: HtmlValidator.html_popover_state(attributes),
        else: :none

    defaults =
      case if(Map.get(node, :namespace) == :svg, do: nil, else: tag) do
        "slot" ->
          %{display: :contents}

        "selectedcontent" ->
          %{display: :inline}

        "html" ->
          %{display: :block}

        tag
        when tag in [
               "script",
               "template",
               "noembed",
               "noframes",
               "datalist",
               "basefont",
               "bgsound"
             ] ->
          %{display: :none}

        "body" ->
          defaults = block_defaults(inherited_font_size, 400, 0.0)

          case ResourceContext.current() do
            %{depth: depth} when depth > 0 ->
              Map.put(
                defaults,
                :margin,
                Keyword.get(opts, :__frame_body_margins__, CssValueValidator.edges(6.0))
              )

            _ ->
              defaults
          end

        "frameset" ->
          with {:ok, hints} <-
                 NativeElixirPdfUtilities.Validators.FramesetValidator.prepare_color_hints(
                   attributes,
                   Map.get(inherited_style, :_frameset_parent)
                 ) do
            block_defaults(inherited_font_size, 400, 0.0) |> Map.merge(hints)
          end

        tag
        when tag in ~w(form article aside div footer header main nav section figcaption hgroup search dt details frame) ->
          block_defaults(inherited_font_size, 400, 0.0)

        "dialog" ->
          block_defaults(inherited_font_size, 400, 0.0)
          |> Map.merge(
            NativeElixirPdfUtilities.Validators.HtmlDialogValidator.defaults(
              attributes,
              inherited_font_size
            )
          )

        "fieldset" ->
          block_defaults(inherited_font_size, 400, 0.0)
          |> Map.merge(%{
            margin: CssValueValidator.edges(0.0, 1.5, 0.0, 1.5),
            border_widths: @control_padding_edges,
            border_styles: CssValueValidator.edges(:groove),
            # ThreeDFace in the pinned Chromium print theme is rgb(239,239,239).
            border_colors: CssValueValidator.edges({239 / 255, 239 / 255, 239 / 255}),
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
          |> Map.merge(%{
            padding: CssValueValidator.edges(0.0, 1.5, 0.0, 1.5),
            justify_self: alignment
          })

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
          |> Map.put(
            :margin,
            CssValueValidator.edges(inherited_font_size, 30.0, inherited_font_size, 30.0)
          )
          |> Map.put(:_margin_before_em, 1.0)
          |> Map.put(:_margin_after_em, 1.0)

        "dl" ->
          defaults = block_defaults(inherited_font_size, 400, inherited_font_size)

          if Enum.any?(ancestors, &(&1.tag == "dl")) do
            defaults
            |> Map.put(:margin, @zero_edges)
            |> Map.put(:margin_after, 0.0)
          else
            defaults |> Map.put(:_margin_before_em, 1.0) |> Map.put(:_margin_after_em, 1.0)
          end

        "rp" ->
          %{display: :none}

        "rt" ->
          if match?([%{tag: "ruby"} | _], ancestors) do
            %{
              display: :inline,
              font_size: inherited_font_size * 0.5,
              line_height_normal: true,
              _ruby_annotation: true
            }
          else
            %{display: :inline}
          end

        tag when tag in ~w(ruby rb rtc) ->
          %{display: :inline}

        "dd" ->
          Map.put(
            block_defaults(inherited_font_size, 400, 0.0),
            :margin,
            CssValueValidator.edges(0.0, 0.0, 0.0, 30.0)
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

        tag when tag in ~w(ul ol menu dir) ->
          depth = Enum.count(ancestors, &(&1.tag in ~w(ul ol menu dir)))

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
            |> Map.put(:margin, @zero_edges)
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

        "q" ->
          %{display: :inline, _quote_shaping: true}

        tag when tag in ~w(abbr acronym) ->
          if Map.has_key?(attributes, "title"),
            do: %{
              display: :inline,
              text_decoration_line: [:underline],
              text_decoration_style: :dotted
            },
            else: %{display: :inline}

        tag when tag in ~w(span font blink keygen data time noscript label) ->
          %{display: :inline}

        "a" ->
          link_defaults(
            attributes,
            opts,
            Map.has_key?(rules, :source_budget),
            Map.get(inherited_style, :_body_link_color)
          )

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
              border_styles: CssValueValidator.edges(:inset),
              border_widths: @cell_padding_edges,
              margin: %{top: 0.0, right: :auto, bottom: 0.0, left: :auto},
              _margin_before_em: {:css_layout_em, 0.5},
              _margin_after_em: {:css_layout_em, 0.5},
              overflow: :hidden
            })
            |> Map.merge(hints)
          end

        "picture" ->
          %{display: :inline}

        tag when tag in ["source", "area"] ->
          %{display: :none}

        tag when tag in ["object", "embed"] ->
          with {:ok, hints} <-
                 HtmlValidator.html_dimension_hints(
                   attributes,
                   [{"width", :width}, {"height", :height}],
                   tag
                 ) do
            block_defaults(inherited_font_size, 400, 0.0)
            |> Map.put(:display, :inline)
            |> Map.merge(hints)
          end

        "iframe" ->
          with {:ok, hints} <- HtmlValidator.iframe_hints(attributes) do
            block_defaults(inherited_font_size, 400, 0.0)
            |> Map.merge(%{
              display: :inline_block,
              width: 225.0,
              height: 112.5,
              overflow: :hidden,
              margin: @zero_edges,
              padding: @zero_edges,
              border_widths: @control_padding_edges,
              border_styles: CssValueValidator.edges(:inset)
            })
            |> Map.merge(hints)
          end

        "img" ->
          if Map.has_key?(rules, :source_budget) or popover_state != :none do
            image_box_defaults(attributes, inherited_font_size)
          else
            image_defaults(node, ancestors, opts, inherited_font_size)
          end

        "input" ->
          input_attributes =
            NativeElixirPdfUtilities.Validators.HtmlFormValidator.initial_control_attributes(node)

          if HtmlValidator.input_type(Map.get(input_attributes, "type", "text")) == "image" do
            with {:ok, defaults} <-
                   if(Map.has_key?(rules, :source_budget) or popover_state != :none,
                     do: image_box_defaults(input_attributes, inherited_font_size),
                     else:
                       image_defaults(
                         %{node | attributes: input_attributes},
                         ancestors,
                         opts,
                         inherited_font_size
                       )
                   ) do
              {:ok, Map.put(defaults, :_image_input_state, node._initial_form_state)}
            end
          else
            form_control_defaults(
              HtmlValidator.input_type(Map.get(input_attributes, "type", "text")),
              inherited_font_size
            )
            |> then(fn defaults ->
              defaults =
                if Map.has_key?(node._initial_form_state, :temporal),
                  do:
                    defaults
                    |> Map.put(:_temporal_state, node._initial_form_state.temporal)
                    |> Map.put(:_temporal_disabled, node._initial_form_state.disabled)
                    |> Map.put(
                      :background_color,
                      if(node._initial_form_state.disabled,
                        do: {239 / 255, 239 / 255, 239 / 255, 0.3},
                        else: {1.0, 1.0, 1.0}
                      )
                    )
                    |> Map.put(
                      :color,
                      if(node._initial_form_state.disabled,
                        do: {84 / 255, 84 / 255, 84 / 255},
                        else: {0.0, 0.0, 0.0}
                      )
                    ),
                  else: defaults

              defaults =
                if Map.has_key?(node._initial_form_state, :files) and
                     node._initial_form_state.disabled,
                   do: Map.put(defaults, :color, {84 / 255, 84 / 255, 84 / 255}),
                   else: defaults

              defaults =
                if Map.has_key?(node._initial_form_state, :color_input) do
                  Map.merge(defaults, %{
                    _color_state: node._initial_form_state.color_input,
                    _color_disabled: node._initial_form_state.disabled,
                    background_color:
                      if(node._initial_form_state.disabled,
                        do: {239 / 255, 239 / 255, 239 / 255, 0.3},
                        else: {239 / 255, 239 / 255, 239 / 255}
                      )
                  })
                else
                  defaults
                end

              defaults =
                if Map.has_key?(node._initial_form_state, :range),
                  do:
                    defaults
                    |> Map.put(:_range_state, node._initial_form_state.range)
                    |> Map.put(:_range_disabled, node._initial_form_state.disabled)
                    |> Map.put(
                      :height,
                      if(Map.get(node._initial_form_state, :datalist), do: 16.5, else: 12.0)
                    )
                    |> Map.put(
                      :color,
                      if(node._initial_form_state.disabled,
                        do: {197 / 255, 197 / 255, 197 / 255},
                        else: {16 / 255, 16 / 255, 16 / 255}
                      )
                    ),
                  else: defaults

              case Map.get(node._initial_form_state, :number) do
                nil ->
                  defaults

                number ->
                  Map.merge(defaults, %{
                    width: {:control_columns, number.columns},
                    _control_column_count: number.columns,
                    _number_state: number,
                    white_space: :pre,
                    overflow: :hidden
                  })
              end
              |> then(fn defaults ->
                defaults =
                  if Map.has_key?(node._initial_form_state, :number) and
                       node._initial_form_state.disabled do
                    color = {118 / 255, 118 / 255, 118 / 255, 0.3}

                    defaults
                    |> Map.put(:border_color, color)
                    |> Map.put(:border_colors, CssValueValidator.edges(color))
                  else
                    defaults
                  end

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
            end)
          end

        "select" ->
          selection = node._initial_form_state.selection

          form_control_defaults("select", inherited_font_size)
          |> Map.merge(%{
            width: :auto,
            height: :auto,
            box_sizing: :border_box,
            border_radius: if(selection.listbox, do: 1.5, else: 0.0),
            border_color: {118 / 255, 118 / 255, 118 / 255},
            border_colors: CssValueValidator.edges({118 / 255, 118 / 255, 118 / 255}),
            white_space: :pre,
            padding: @zero_edges,
            # Chromium's pinned Linux sheet gives popup selects ButtonFace.
            # The native unstyled theme supplies its own field background.
            background_color:
              if(selection.listbox,
                do: {1.0, 1.0, 1.0},
                else: {239 / 255, 239 / 255, 239 / 255}
              ),
            overflow: :hidden,
            color:
              if(node._initial_form_state.disabled,
                do: {128 / 255, 128 / 255, 128 / 255},
                else: {0, 0, 0}
              ),
            _select_state: selection,
            form_kind: if(selection.listbox, do: :listbox, else: :select),
            vertical_align: if(selection.listbox, do: :text_bottom, else: :baseline)
          })

        "option" ->
          block_defaults(inherited_font_size, 400, 0.0)
          |> Map.merge(%{
            _initial_option_id: node._selector_id,
            _native_select_option:
              NativeElixirPdfUtilities.Validators.HtmlFormValidator.native_select_option?(
                ancestors
              ),
            white_space: :pre,
            padding:
              CssValueValidator.edges(
                0.0,
                1.5,
                0.75,
                if(List.first(ancestors).tag == "optgroup", do: 15.0, else: 1.5)
              ),
            min_height: {:css_layout_em, 1.2},
            background_color:
              if(node._initial_selected, do: {206 / 255, 206 / 255, 206 / 255}, else: nil)
          })
          |> then(fn style ->
            if node._initial_option_disabled or
                 Enum.any?(ancestors, &(get_in(&1, [:_initial_form_state, :disabled]) == true)),
               do: Map.put(style, :color, {128 / 255, 128 / 255, 128 / 255}),
               else:
                 if(node._initial_selected,
                   do: Map.put(style, :color, {16 / 255, 16 / 255, 16 / 255}),
                   else: style
                 )
          end)

        "optgroup" ->
          block_defaults(inherited_font_size, 700, 0.0)

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
          if Map.get(inherited_style, :_base_select, false),
            do: %{display: :contents},
            else: form_control_defaults(:button, inherited_font_size)

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
            overflow: if(tag in ~w(svg symbol marker), do: :hidden, else: :visible)
          })

        true ->
          defaults
      end

    defaults =
      case defaults do
        {:ok, defaults} -> defaults
        defaults -> defaults
      end

    defaults =
      if popover_state == :none or not is_map(defaults) do
        defaults
      else
        defaults
        |> Map.drop([:_margin_before_em, :_margin_after_em, :_dialog_default_padding])
        |> NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_popover_defaults(
          HtmlValidator.html_popover_defaults(attributes, tag, inherited_font_size)
        )
        |> Map.put(:_popover_state, popover_state)
      end

    defaults =
      if is_map(defaults) and tag in ~w(input textarea select button) do
        Map.update!(defaults, :display, fn display ->
          NativeElixirPdfUtilities.Validators.HtmlFormValidator.initial_control_display(
            node,
            display,
            ancestors
          )
        end)
      else
        defaults
      end

    defaults =
      with defaults when is_map(defaults) <- defaults,
           {:ok, hints} <- HtmlTableValidator.presentation_hints(node, ancestors, inherited_style) do
        defaults
        |> Map.merge(hints)
        |> Map.merge(
          NativeElixirPdfUtilities.Validators.HtmlLegacyMarkupValidator.presentation_hints(
            node,
            case ResourceContext.current() do
              %{depth: depth} when depth > 0 ->
                Map.get(defaults, :margin, CssValueValidator.edges(6.0))

              _ ->
                CssValueValidator.edges(6.0)
            end
          )
        )
        |> Map.merge(NativeElixirPdfUtilities.Validators.HtmlVisibilityValidator.defaults(node))
      end

    case defaults do
      defaults when is_map(defaults) ->
        defaults =
          Map.merge(defaults, MathLayoutValidator.child_selection_defaults(node, ancestors))

        defaults =
          if Map.get(defaults, :font_weight) == 400 and
               tag not in ["input", "select", "textarea", "button", "option"] do
            Map.delete(defaults, :font_weight)
          else
            defaults
          end

        style =
          inherited_style
          |> text_style()
          |> Map.merge(defaults)
          |> Map.merge(%{
            _inherited_font:
              Map.take(inherited_style, [
                :font_families,
                :font_size,
                :font_style,
                :font_weight,
                :font_face,
                :_font_family_scope
              ]),
            orphans: Map.get(inherited_style, :orphans, 2),
            widows: Map.get(inherited_style, :widows, 2),
            _inherited_orphans: Map.get(inherited_style, :orphans, 2),
            _inherited_widows: Map.get(inherited_style, :widows, 2),
            _inherited_quotes: Map.get(inherited_style, :quotes, :auto),
            _inherited_color: Map.fetch!(inherited_style, :color),
            _inherited_caption_side: Map.get(inherited_style, :caption_side, :top),
            _inherited_css_transform: Map.get(inherited_style, :css_transform, :none),
            _inherited_css_transform_metrics:
              NativeElixirPdfUtilities.Validators.CssTransformValidator.inherited_metric_style(
                inherited_style,
                "transform"
              ),
            _inherited_transform_origin_metrics:
              NativeElixirPdfUtilities.Validators.CssTransformValidator.inherited_metric_style(
                inherited_style,
                "transform-origin"
              ),
            _inherited_transform_origin:
              Map.get(inherited_style, :transform_origin, {{:percent, 0.5}, {:percent, 0.5}}),
            _root_element: tag in ["html", "body"] and ancestors == []
          })

        style =
          Map.put(
            style,
            :_ua_font,
            Map.take(style, [
              :font_families,
              :font_size,
              :font_style,
              :font_weight,
              :_font_family_scope
            ])
          )

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

        target =
          if Map.has_key?(rules, :source_budget),
            do: nil,
            else: HtmlLinkValidator.target(node, Keyword.fetch!(opts, :__link_context__))

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

        style =
          if tag == "select",
            do: Map.put(style, :_select_inherited_style, inherited_style),
            else: style

        authored =
          if(Map.get(node, :_default_disclosure_summary, false),
            do: apply_author_styles(style, node, ancestors, rules, nil, []),
            else: apply_author_styles(style, node, ancestors, rules)
          )

        authored =
          with {:ok, style} <- authored,
               {:ok, style} <- HtmlTableValidator.prepare_presentation_style(node, style),
               do:
                 {:ok,
                  NativeElixirPdfUtilities.Validators.HtmlVisibilityValidator.prepare_style(style)}

        authored =
          with {:ok, style} <- authored do
            {:ok,
             if(Map.has_key?(rules, :source_budget),
               do: style,
               else:
                 NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.prepare_source_completion_style(
                   node,
                   style
                 )
             )}
          end

        authored =
          case authored do
            {:ok, %{_popover_state: _, display: display} = style}
            when display != :none and tag in ["img", "input"] and
                   not is_map_key(rules, :source_budget) ->
              if tag == "img" or Map.has_key?(style, :_image_input_state) do
                with {:ok, image_defaults} <-
                       image_defaults(node, ancestors, opts, inherited_font_size) do
                  style = Map.merge(image_defaults, style)

                  {:ok, style}
                end
              else
                authored
              end

            _ ->
              authored
          end

        authored =
          case authored do
            {:ok, %{display: display} = style}
            when tag == "img" and display != :none and not is_map_key(rules, :source_budget) ->
              model =
                NativeElixirPdfUtilities.Validators.ImageMapValidator.associate(
                  node,
                  Map.fetch!(rules, :image_maps)
                )

              {:ok, if(model == nil, do: style, else: Map.put(style, :_image_map, model))}

            _ ->
              authored
          end

        result =
          if Map.has_key?(rules, :source_budget),
            do: authored,
            else:
              finalize_element_style(tag, authored, Keyword.fetch!(opts, :__image_budget__), opts)

        result =
          with {:ok, style} <- result do
            style =
              if tag == "noembed" and not Map.has_key?(rules, :source_budget),
                do:
                  NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.prepare_source_completion_style(
                    node,
                    style
                  ),
                else: style

            {:ok,
             NativeElixirPdfUtilities.Validators.HtmlVisibilityValidator.prepare_style(style)}
          end

        result =
          case result do
            {:ok, style} ->
              if not Map.has_key?(rules, :source_budget) and
                   (tag == "fieldset" or Map.get(style, :_fieldset_precision, false)) do
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

        result =
          case {tag, ancestors, result} do
            {tag, [], {:ok, style}} when tag in ["html", "body"] ->
              {:ok,
               style
               |> Map.put(:_root_font_size, Map.fetch!(style, :font_size))
               |> Map.put(:_root_font_face, Map.get(style, :font_face))
               |> Map.put(:_root_line_height, Map.fetch!(style, :line_height))}

            _ ->
              result
          end

        case result do
          {:ok, style} ->
            with :ok <-
                   if(Map.has_key?(rules, :source_budget),
                     do: :ok,
                     else:
                       NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.observe(
                         node,
                         style
                       )
                   ),
                 do: result

          _ ->
            result
        end

      {:error, {_reason, _diagnostic}} = error ->
        error
    end
  end

  defp finalize_element_style(tag, result, image_budget, opts) do
    result =
      case result do
        {:ok, style} ->
          {:ok,
           style
           |> Map.delete(:_popover_state)
           |> NativeElixirPdfUtilities.Validators.HtmlFormValidator.resolve_popover_dimensions()}

        error ->
          error
      end

    paint_tag =
      case result do
        {:ok, style} -> if(Map.has_key?(style, :_image_input_state), do: "img", else: tag)
        _ -> tag
      end

    case {paint_tag, result} do
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
      when tag in ~w(input textarea) and style.form_kind != :hidden ->
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

        style =
          if style.form_kind == :select and Map.get(style, :appearance, :auto) != :none,
            do: %{
              style
              | padding: %{padding | left: padding.left + 3.0, right: padding.right + 12.0}
            },
            else: style

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
               rasterize_svg_image(
                 svg,
                 svg_raster_options(style),
                 image_budget,
                 opts,
                 Limits.get(:svg_raster_scale)
               ),
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

    style =
      if Map.get(style, :display) == :none,
        do: Map.drop(style, [:background_image_source, :list_style_image_source]),
        else: style

    Enum.reduce_while(
      [
        {:background_image, :background_image_source},
        {:list_style_image, :list_style_image_source}
      ],
      {:ok, style},
      fn {property, source_property}, {:ok, style} ->
        case Map.get(style, source_property) do
          nil ->
            {:cont, {:ok, style}}

          source ->
            kind = if property == :list_style_image, do: :image, else: property

            result =
              case load_image_style(source, opts, kind) do
                {:ok, %{image: image}} ->
                  {:ok, image}

                {:ok, %{svg_image: svg}} ->
                  rasterize_svg_image(svg, [], image_budget, opts, Limits.get(:svg_raster_scale))

                {:error, _} = error ->
                  error
              end

            case result do
              {:ok, image} ->
                {:cont, {:ok, style |> Map.put(property, image) |> Map.delete(source_property)}}

              {:error, _} = error ->
                {:halt, error}
            end
        end
      end
    )
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

  @doc "Returns the normal line height of an already validated embedded-font style in points."
  @spec normal_line_height(map()) :: number()
  def normal_line_height(style), do: HtmlValidator.normal_line_height(style)

  @doc "Builds an anonymous formatting box with inherited text and initial box properties."
  @spec anonymous_box_style(map(), atom()) :: map()
  def anonymous_box_style(parent, display) do
    block_defaults(Map.get(parent, :font_size, 12.0), 400, 0.0)
    |> Map.merge(text_style(parent))
    |> Map.merge(%{
      display: display,
      border_widths: @zero_edges,
      border_color: Map.get(parent, :color, {0, 0, 0}),
      border_colors: CssValueValidator.edges(Map.get(parent, :color, {0, 0, 0})),
      vertical_align: :baseline
    })
  end

  defp text_style(style) do
    inherited =
      :maps.with(
        [
          :_math_text,
          :_body_link_color,
          :_quote_shaping,
          :math_style,
          :math_shift,
          :math_depth,
          :_fieldset_precision,
          :_table_atomic_context,
          :_form_owner,
          :_initial_option_id,
          :_text_decorations,
          :_custom_properties,
          :_font_registry,
          :_font_family_scope,
          :_base_select,
          :_root_font_size,
          :_root_font_face,
          :_root_line_height,
          :_position_viewport,
          :_frameset_parent,
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
          :orphans,
          :widows,
          :list_marker_type,
          :list_style_position,
          :list_style_image,
          :link_url,
          :link_destination,
          :text_align,
          :_text_align_initial,
          :caption_side,
          :text_transform,
          :quotes,
          :white_space
        ],
        style
      )

    inherited =
      if Map.get(style, :display) in [:table_row_group, :table_row],
        do: Map.put(inherited, :_table_vertical_align, Map.get(style, :vertical_align, :middle)),
        else: inherited

    inherited =
      if Map.get(style, :display) in [:table, :table_row_group, :table_row],
        do:
          Map.put(
            inherited,
            :_table_border_colors,
            Map.get(
              style,
              :border_colors,
              CssValueValidator.edges(Map.get(style, :color, {0, 0, 0}))
            )
          ),
        else: inherited

    inherited =
      if Map.has_key?(style, :_frameset),
        do: Map.put(inherited, :_frameset_parent, style._frameset),
        else: inherited

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
      # CSS fragmentation's fixed initial values, inherited by block content.
      orphans: 2,
      widows: 2,
      border_color: :current_color,
      border_colors: @current_color_edges,
      border_radius: 0.0,
      border_styles: @none_edges,
      border_widths: @medium_border_edges,
      box_sizing: :content_box,
      display: :block,
      font_size: font_size,
      font_weight: font_weight,
      margin: CssValueValidator.edges(0.0, 0.0, margin_bottom, 0.0),
      margin_after: margin_bottom,
      padding: @zero_edges,
      position: :static,
      offsets: CssValueValidator.edges(:auto),
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
      margin: CssValueValidator.edges(font_size, 0.0, font_size, 0.0),
      padding: CssValueValidator.edges(0.0, 0.0, 0.0, 30.0)
    })
  end

  defp list_item_defaults(font_size) do
    Map.put(block_defaults(font_size, 400, 0.0), :display, :list_item)
  end

  defp table_defaults(font_size) do
    block_defaults(font_size, 400, 0.0)
    |> Map.merge(%{
      display: :table,
      box_sizing: :border_box,
      border_collapse: :separate,
      border_spacing: {1.5, 1.5},
      margin: @zero_edges,
      padding: @zero_edges,
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
      border_colors: @current_color_edges,
      border_radius: 0.0,
      border_styles: @none_edges,
      border_widths: @medium_border_edges,
      box_sizing: :content_box,
      display: :table_row_group,
      padding: @zero_edges,
      table_section: section,
      vertical_align: :middle
    }
  end

  defp table_row_defaults do
    %{
      background_color: nil,
      border_color: :current_color,
      border_colors: @current_color_edges,
      border_radius: 0.0,
      border_styles: @none_edges,
      border_widths: @medium_border_edges,
      box_sizing: :content_box,
      display: :table_row,
      padding: @zero_edges
    }
  end

  defp table_column_defaults(kind, span) do
    table_row_defaults()
    |> Map.take([
      :background_color,
      :border_color,
      :border_colors,
      :border_radius,
      :border_styles,
      :border_widths,
      :box_sizing
    ])
    |> Map.merge(%{
      display: if(kind == :group, do: :table_column_group, else: :table_column),
      span: span
    })
  end

  defp table_cell_defaults(kind, spans, font_size) do
    base = %{
      background_color: nil,
      border_color: :current_color,
      border_colors: @current_color_edges,
      border_radius: 0.0,
      border_styles: @none_edges,
      border_widths: @zero_edges,
      box_sizing: :content_box,
      colspan: 1,
      display: :table_cell,
      font_size: font_size,
      font_weight: 400,
      margin: @zero_edges,
      padding: @cell_padding_edges,
      rowspan: 1,
      vertical_align: :middle
    }

    cell = Map.merge(base, spans)

    case kind do
      :header -> Map.merge(cell, %{font_weight: 700, text_align: :center})
      :data -> cell
    end
  end

  defp link_defaults(attributes, opts, source?, body_link_color) do
    case Map.get(attributes, "href") do
      nil ->
        {:ok, %{display: :inline}}

      href ->
        defaults = %{
          display: :inline,
          color: body_link_color || {0, 0, 238 / 255},
          text_decoration_line: [:underline]
        }

        if source? do
          {:ok, defaults}
        else
          case Map.fetch!(Keyword.fetch!(opts, :__link_context__).links, href) do
            {:uri, url} -> {:ok, Map.put(defaults, :link_url, url)}
            destination -> {:ok, Map.put(defaults, :link_destination, destination)}
          end
        end
    end
  end

  defp image_defaults(node, ancestors, opts, font_size) do
    validator = NativeElixirPdfUtilities.Validators.ResponsiveImageValidator

    case validator.prepare_deferred(node, ancestors, opts) do
      {:ok, request, dimensions} ->
        {:ok,
         image_box_style(font_size)
         |> Map.merge(dimensions)
         |> Map.put(:_responsive_image, request)}

      :eager ->
        with {:ok, selected} <- validator.select(node, ancestors, opts),
             {:ok, image_style} <- selected_image_style(selected, opts) do
          {:ok,
           image_box_style(font_size) |> Map.merge(image_style) |> Map.merge(selected.dimensions)}
        end

      {:error, _} = error ->
        error
    end
  end

  @doc false
  @spec resolve_image(map(), number()) :: {:ok, map()} | {:error, {atom(), map()}}
  def resolve_image(style, width) do
    NativeElixirPdfUtilities.Validators.ResponsiveImageValidator.resolve_request(
      style._responsive_image,
      width / 0.75,
      fn selected, opts ->
        with {:ok, resource} <- selected_image_style(selected, opts) do
          style = style |> Map.delete(:_responsive_image) |> Map.merge(resource)
          finalize_image_style(style, Keyword.fetch!(opts, :__image_budget__), opts)
        end
      end
    )
  end

  defp selected_image_style(selected, opts) do
    with {:ok, image_style} <-
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
        {:ok, image_style}
      end
    else
      {:error, {_reason, _diagnostic}} = error -> error
    end
  end

  defp image_box_defaults(attributes, font_size) do
    with {:ok, hints} <- HtmlValidator.image_dimension_hints(attributes),
         do: {:ok, Map.merge(image_box_style(font_size), hints)}
  end

  defp image_box_style(font_size) do
    block_defaults(font_size, 400, 0.0)
    |> Map.merge(%{
      display: :inline,
      margin: @zero_edges,
      object_fit: :fill,
      object_position: {{:percent, 0.5}, {:percent, 0.5}},
      padding: @zero_edges
    })
  end

  # Fixed native appearance sizing combines font metrics with HTML character
  # counts. This calculation is shared by text inputs and textarea columns.
  defp initial_control_width(style, count) do
    zero_width = Font.text_width("0", style.font_face, style.font_size)

    {average, maximum} =
      case FontValidator.prepare_text_metrics(style.font_face) do
        {:ok, %{average_width: average, units_per_em: units}}
        when is_number(average) ->
          average = average / units * style.font_size
          # Blink's fixed native-width heuristics retain legacy font
          # compatibility and round averages up at a half CSS pixel.
          if style.font_family in @native_width_fallback_families or
               average > zero_width * 1.7 do
            {zero_width, zero_width}
          else
            # Linux SimpleFontData::PlatformInit uses the rounded selected
            # typeface ink span for every native input's maximum character
            # width, independently of its control type.
            {left, _bottom, right, _top} = style.font_face.bbox
            maximum = round((right - left) / units * style.font_size / 0.75) * 0.75

            {max(average, round(average / 0.75) * 0.75), maximum}
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
    cond do
      style.form_kind == :textarea and Map.get(style, :overflow) == :auto -> extent + 11.25
      get_in(style, [:_number_state, :decoration]) -> extent + 11.25
      true -> extent
    end
  end

  defp form_control_defaults(kind, font_size) do
    button_input? = is_binary(kind) and HtmlValidator.button_input_type?(kind)
    kind = if button_input? or kind == :button, do: "button", else: kind

    base =
      block_defaults(font_size, 400, 0.0)
      |> Map.merge(%{
        background_color: {1.0, 1.0, 1.0},
        display: :inline_block,
        font_families: ["Arial", "sans-serif"],
        font_size: 10.0,
        font_style: :normal,
        font_weight: 400,
        line_height_multiplier: nil,
        line_height_normal: true,
        border_color: {0.47, 0.47, 0.47},
        border_colors: CssValueValidator.edges({0.47, 0.47, 0.47}),
        border_styles: CssValueValidator.edges(:solid),
        border_widths: @cell_padding_edges,
        form_kind:
          if(HtmlValidator.readonly_input_type?(kind), do: :text, else: String.to_atom(kind)),
        margin: @zero_edges,
        padding: CssValueValidator.edges(1.5, 2.25, 1.5, 2.25)
      })

    case kind do
      "hidden" ->
        Map.put(base, :display, :none)

      kind when kind in ~w(date month week time datetime-local) ->
        Map.merge(base, %{
          display: :inline_block,
          appearance: :auto,
          font_families: ["monospace"],
          font_size: 10.0,
          font_style: :normal,
          font_weight: 400,
          line_height_multiplier: nil,
          line_height_normal: true,
          width: :auto,
          height: :auto,
          overflow: :hidden,
          padding: CssValueValidator.edges(0.0, 0.0, 0.0, 0.75),
          border_widths: @control_padding_edges,
          border_styles: CssValueValidator.edges(:inset),
          border_color: {118 / 255, 118 / 255, 118 / 255},
          border_colors: CssValueValidator.edges({118 / 255, 118 / 255, 118 / 255})
        })

      "file" ->
        Map.merge(base, %{
          display: :inline_block,
          appearance: :none,
          font_families: ["Arial", "sans-serif"],
          font_size: 10.0,
          font_style: :normal,
          font_weight: 400,
          line_height_multiplier: nil,
          line_height_normal: true,
          width: :auto,
          height: :auto,
          overflow: :hidden,
          white_space: :pre,
          background_color: nil,
          padding: @zero_edges,
          border_widths: @zero_edges,
          border_styles: @none_edges
        })

      "color" ->
        Map.merge(base, %{
          display: :inline_block,
          form_kind: :color,
          appearance: :auto,
          width: 37.5,
          height: 20.25,
          box_sizing: :border_box,
          border_widths: @cell_padding_edges,
          border_styles: CssValueValidator.edges(:solid),
          border_color: {0, 0, 0},
          border_colors: CssValueValidator.edges({0, 0, 0}),
          padding: CssValueValidator.edges(0.75, 1.5, 0.75, 1.5)
        })

      "range" ->
        Map.merge(base, %{
          display: :inline_block,
          appearance: :auto,
          width: 96.75,
          height: 12.0,
          background_color: {1.0, 1.0, 1.0},
          border_widths: @zero_edges,
          border_styles: @none_edges,
          padding: @zero_edges,
          margin: @control_padding_edges,
          color: {16 / 255, 16 / 255, 16 / 255}
        })

      kind when kind in ["checkbox", "radio"] ->
        base
        |> Map.merge(%{
          background_color: nil,
          border_colors: CssValueValidator.edges(nil),
          border_styles: @none_edges,
          border_widths: @zero_edges,
          padding: @zero_edges,
          width: 9.75,
          height: 9.75
        })

      "number" ->
        base
        |> Map.merge(%{
          display: :inline_block,
          appearance: :auto,
          font_families: ["Arial", "sans-serif"],
          font_size: 10.0,
          font_style: :normal,
          font_weight: 400,
          line_height_multiplier: nil,
          line_height_normal: true,
          padding: CssValueValidator.edges(0.75, 1.5, 0.75, 1.5),
          border_widths: @control_padding_edges,
          border_styles: CssValueValidator.edges(:inset),
          border_color: {118 / 255, 118 / 255, 118 / 255},
          border_colors: CssValueValidator.edges({118 / 255, 118 / 255, 118 / 255})
        })

      "textarea" ->
        base
        |> Map.put(:font_families, ["monospace"])
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
        base =
          if HtmlValidator.text_input_type?(kind) do
            Map.merge(base, %{
              display: :inline_block,
              appearance: :auto,
              padding: CssValueValidator.edges(0.75, 1.5, 0.75, 1.5),
              border_widths: @control_padding_edges,
              border_styles: CssValueValidator.edges(:inset),
              border_color: {118 / 255, 118 / 255, 118 / 255},
              border_colors: CssValueValidator.edges({118 / 255, 118 / 255, 118 / 255})
            })
          else
            base
          end

        Map.put(base, :width, font_size * 10.5)
    end
  end

  defp static_form_children(node, children) do
    case node.tag do
      "input" ->
        kind =
          HtmlValidator.input_type(
            Map.get(
              NativeElixirPdfUtilities.Validators.HtmlFormValidator.initial_control_attributes(
                node
              ),
              "type",
              "text"
            )
          )

        text =
          cond do
            kind in ["range", "color", "image", "file"] or
                Map.has_key?(node._initial_form_state, :temporal) ->
              {:ok, ""}

            kind == "checkbox" ->
              {:ok, if(node._initial_form_state.checked, do: "☒", else: "☐")}

            kind == "radio" ->
              {:ok, if(node._initial_form_state.checked, do: "●", else: "○")}

            Map.get(node._initial_form_state, :placeholder_shown, false) ->
              {:ok, node._initial_form_state.placeholder}

            kind == "password" ->
              FormValidator.password_display_text(node._initial_form_state.value, :style)

            kind == "number" ->
              {:ok, node._initial_form_state.display}

            HtmlValidator.text_input_type?(kind) ->
              {:ok, node._initial_form_state.value}

            HtmlValidator.button_input_type?(kind) ->
              {:ok, node._initial_form_state.value}
          end

        with {:ok, text} <- text do
          if kind in ["range", "color", "image", "file"] or
               Map.has_key?(node._initial_form_state, :temporal),
             do: [],
             else: [%{type: :text, text: text}]
        end

      tag when tag in ~w(progress meter) ->
        []

      "select" ->
        if node._initial_form_state.selection.listbox,
          do: children,
          else: node._initial_form_state.selection.children

      "option" ->
        [%{type: :text, text: node._initial_option_label}]

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

  defp stylesheet_rules(entries) do
    {rules, _next_order} =
      Enum.reduce(entries, {[], 0}, fn entry, state ->
        Enum.reduce(entry.prepared.rules, state, fn rule, {rules, order} ->
          rule = %{rule | order: order}

          rule =
            Map.merge(rule, Map.take(entry, [:shadow_scope_id, :shadow_host_id, :shadow_depth]))

          {[rule | rules], order + 1}
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
        |> HtmlMetadataValidator.selected_document_style_nodes(media_context)
        |> Enum.map(fn node ->
          NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.stylesheet_owner(
            node,
            %{
              css: Enum.map_join(node.children, "", & &1.text),
              source_kind: :embedded,
              base_url: base_url,
              trusted_local_resources: false
            }
          )
        end)

      {:ok, configured_entries ++ embedded_entries}
    end
  end

  defp configured_stylesheet_entries(stylesheets, base_url, opts) do
    Enum.reduce_while(stylesheets, {:ok, []}, fn stylesheet, {:ok, acc} ->
      case stylesheet do
        {:css, css} when is_binary(css) ->
          entry = %{
            css: css,
            source_kind: :configured,
            base_url: base_url,
            trusted_local_resources: false
          }

          {:cont, {:ok, acc ++ [entry]}}

        {:file, path} when is_binary(path) ->
          case read_stylesheet(path, opts) do
            {:ok, css} ->
              entry = %{
                css: css,
                source_kind: :configured,
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

  @doc false
  @spec prepare_font_resources([map()], keyword()) ::
          {:ok, Font.registry()} | {:error, atom() | {atom(), map()}}
  def prepare_font_resources(entries, opts) do
    # Entries and options are already validator-owned. This is ordinary loading,
    # separate from the source-only cascade and from the active style registry.
    with {:ok, css_fonts} <- stylesheet_fonts(entries, opts),
         do: font_registry(opts, css_fonts)
  end

  defp stylesheet_fonts(entries, opts) do
    Enum.reduce_while(entries, {:ok, []}, fn entry, {:ok, acc} ->
      font_faces = entry.prepared.font_faces

      resolved =
        font_faces
        |> Enum.with_index()
        |> Enum.reduce_while({:ok, []}, fn {font_face, face_index}, {:ok, fonts} ->
          provider = Enum.at(Map.get(entry.prepared, :font_source_tickets, []), face_index)

          {sources, source_error} =
            Enum.reduce_while(
              Enum.with_index(font_face.sources),
              {[], {:error, {:font_load_failed, font_face.sources}}},
              fn {source, ordinal}, {sources, source_error} ->
                case NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.with_font_candidate(
                       provider,
                       ordinal,
                       fn -> font_asset_source(source, entry, opts) end
                     ) do
                  {:ok, {config, ticket}} ->
                    {:cont, {sources ++ [%{config: config, ticket: ticket}], source_error}}

                  {:error, {reason, _diagnostic}} = error
                  when reason in [:resource_denied, :resource_limit_exceeded] ->
                    {:halt, {[], error}}

                  {:error, _} = error ->
                    {:cont, {sources, error}}
                end
              end
            )

          case sources do
            [%{config: %{path: _path}} | _remaining] ->
              paths = Enum.map(sources, &Map.fetch!(&1.config, :path))

              font =
                font_face
                |> Map.delete(:sources)
                |> Map.put(:path, paths)
                |> Map.merge(Map.take(entry, [:shadow_scope_id, :shadow_parent_scope_id]))

              NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.record_font_recipe(
                provider,
                Enum.map(sources, & &1.ticket),
                font
              )

              {:cont, {:ok, fonts ++ [font]}}

            [%{config: %{data: _data}} | _remaining] ->
              candidates = Enum.map(sources, &Map.fetch!(&1.config, :data))

              font =
                font_face
                |> Map.delete(:sources)
                |> Map.put(:data, candidates)
                |> Map.merge(Map.take(entry, [:shadow_scope_id, :shadow_parent_scope_id]))

              NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.record_font_recipe(
                provider,
                Enum.map(sources, & &1.ticket),
                font
              )

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

  defp font_registry(opts, css_fonts, scope_parents \\ %{}) do
    fonts = Keyword.get(opts, :fonts, [])

    result =
      NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.with_font_registry(
        opts,
        css_fonts,
        fn -> Font.load_registry(Keyword.put(opts, :fonts, fonts ++ css_fonts)) end
      )

    case result do
      {:ok, registry} when map_size(scope_parents) == 0 ->
        {:ok, registry}

      {:ok, registry} ->
        # Loading and source charges happen once per declaration. Lookup keeps
        # each declaration scope separate even when decoded bytes are shared.
        {configured, rest} = Enum.split(registry.fallback, length(fonts))
        {loaded_css, bundled} = Enum.split(rest, length(css_fonts))
        declarations = Enum.zip(css_fonts, loaded_css)
        scoped = Enum.group_by(declarations, fn {config, _face} -> config[:shadow_scope_id] end)
        parents = scope_parents
        global = configured ++ Enum.map(Map.get(scoped, nil, []), &elem(&1, 1)) ++ bundled

        scopes =
          Map.new(parents, fn {scope, _parent} ->
            chain =
              Stream.unfold(scope, fn
                nil ->
                  nil

                current ->
                  HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :font)
                  {current, Map.get(parents, current)}
              end)
              |> Enum.map(fn current ->
                Enum.map(Map.get(scoped, current, []), &elem(&1, 1))
              end)

            {faces, _families} =
              Enum.reduce(chain ++ [global], {[], MapSet.new()}, fn layer, {faces, seen} ->
                selected = Enum.reject(layer, &MapSet.member?(seen, String.downcase(&1.family)))
                families = MapSet.new(layer, &String.downcase(&1.family))
                {faces ++ selected, MapSet.union(seen, families)}
              end)

            {scope,
             %{
               embedded: Enum.uniq_by(faces, &{String.downcase(&1.family), &1.weight, &1.style}),
               fallback: faces
             }}
          end)

        {:ok,
         %{
           registry
           | embedded: Enum.uniq_by(global, &{String.downcase(&1.family), &1.weight, &1.style}),
             fallback: global
         }
         |> Map.put(:scoped, scopes)}

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
    with {:ok, sources} <- stylesheet_diagnostic_sources(dom, opts),
         {:ok, media_context} <- PrintMediaValidator.renderer_context(opts) do
      Enum.find_value(sources, fn {_label, source} ->
        CssValueValidator.declaration_error(
          source,
          CssValueValidator.stylesheet_declaration_sources(source, media_context)
        )
      end)
    else
      {:error, error} -> error
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

  defp inline_style_diagnostic_error(dom, opts) do
    Enum.find_value(inline_style_sources(dom), &inline_style_error(&1, opts))
  end

  defp inline_style_error(style_source, _opts) do
    case CssParser.parse_declarations_detailed(style_source) do
      {:ok, _declarations} ->
        CssValueValidator.declaration_error(
          style_source,
          CssParser.declaration_sources(style_source)
        )

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

  defp resolve_font(family_value, weight, style, registry, cache \\ nil) do
    result =
      case cache do
        nil ->
          Font.resolve(family_value, weight, style, registry)

        cache ->
          RenderCache.fetch(cache, {:font, registry, family_value, weight, style}, fn ->
            Font.resolve(family_value, weight, style, registry)
          end)
      end

    {:ok, _families, _font_face} = result
    result
  end

  defp compile_selector_rules(rules) do
    Enum.reduce(
      rules,
      %{
        selectors: %{nil => %{}, :before => %{}, :after => %{}, :picker_icon => %{}},
        declarations: %{}
      },
      fn rule, compiled ->
        declarations =
          rule.declarations
          |> Enum.with_index()
          |> Enum.map(fn {declaration, declaration_index} ->
            %{
              declaration: declaration,
              important: CssValueValidator.important_declaration?(declaration),
              inline: false,
              order: rule.order,
              declaration_index: declaration_index,
              definition_scope: Map.get(rule, :shadow_scope_id),
              encapsulation: -Map.get(rule, :shadow_depth, 0)
            }
            |> then(fn matched ->
              case Map.get(rule, :source_declarations) do
                nil ->
                  matched

                records ->
                  record = Enum.at(records, declaration_index)

                  matched
                  |> Map.put(:source_record, record)
                  |> Map.put(:source, record.source)
                  |> Map.merge(Map.take(record, [:context_id]))
              end
            end)
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
                     compounds =
                       case Map.get(part, :shadow_selector) do
                         {_kind, compound} when is_map(compound) ->
                           [compound | compound.negations]

                         _ ->
                           []
                       end

                     Enum.any?([part | part.negations] ++ compounds, fn compound ->
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
                match?({:slotted, _}, Map.get(rightmost, :shadow_selector)) -> :universal
                rightmost.id != nil -> {:id, rightmost.id}
                rightmost.classes != [] -> {:class, hd(rightmost.classes)}
                rightmost.tag != nil -> {:tag, rightmost.tag}
                true -> :universal
              end

            selector = %{selector | parts: parts}

            selector =
              Map.merge(
                selector,
                Map.take(rule, [:shadow_scope_id, :shadow_host_id, :shadow_depth])
              )

            entry = {rule.order, selector}

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
      if Map.has_key?(rules, :source_budget) do
        if pseudo_element == nil,
          do:
            {:ok,
             Map.get(
               rules.inline_records,
               Map.get(node, :node_id, Map.get(node, :_selector_id, node)),
               []
             )},
          else: {:ok, []}
      else
        RenderCache.fetch(rules.cache, {:inline, inline_style}, fn ->
          CssParser.parse_declarations(inline_style)
        end)
      end

    case parsed do
      {:ok, inline_declarations} ->
        presentation =
          if Map.get(node, :namespace) == :svg and is_nil(pseudo_element),
            do: SvgStyleValidator.presentation_declarations(node),
            else: {:ok, []}

        with {:ok, presentation} <- presentation do
          matched =
            (matched_declarations || matching_declarations(rules, node, ancestors, pseudo_element))
            |> Kernel.++(
              inline_declarations
              |> Enum.with_index()
              |> Enum.map(fn {declaration, declaration_index} ->
                matched = %{
                  declaration:
                    case declaration do
                      %{declaration: normalized} -> normalized
                      normalized -> normalized
                    end,
                  important: CssValueValidator.important_declaration?(declaration),
                  inline: true,
                  specificity: {0, 0, 0},
                  order: 0,
                  declaration_index: declaration_index,
                  definition_scope: Map.get(node, :tree_scope_id),
                  encapsulation: 0
                }

                case declaration do
                  %{source: source} ->
                    matched
                    |> Map.put(:source, source)
                    |> Map.merge(Map.take(declaration, [:context_id]))

                  _ ->
                    matched
                end
              end)
            )
            |> Enum.sort_by(fn matched ->
              encapsulation = Map.get(matched, :encapsulation, 0)

              {matched.important, if(matched.important, do: -encapsulation, else: encapsulation),
               matched.inline, matched.specificity, matched.order, matched.declaration_index}
            end)

          style =
            Enum.reduce(matched, style, fn matched, style ->
              case matched.declaration do
                {"font-family", _value} ->
                  Map.put(style, :_font_family_scope, matched.definition_scope)

                {"font-family", _value, :important} ->
                  Map.put(style, :_font_family_scope, matched.definition_scope)

                _ ->
                  style
              end
            end)

          declarations =
            matched
            |> Enum.map(fn matched ->
              if Map.has_key?(rules, :source_budget) and Map.has_key?(matched, :source),
                do: Map.take(matched, [:declaration, :source, :context_id]),
                else: matched.declaration
            end)
            |> then(&(presentation ++ &1))

          style =
            if pseudo_element == nil,
              do:
                NativeElixirPdfUtilities.Validators.HtmlSelectedContentValidator.prepare_select_defaults(
                  style,
                  declarations
                ),
              else: style

          hints = if pseudo_element == nil, do: MathLayoutValidator.style_hints(node), else: []

          if Map.has_key?(rules, :source_budget) do
            with {:ok, validated} <-
                   CssValueValidator.validate_source_cascade(
                     style,
                     hints ++ declarations,
                     rules.source_budget,
                     node
                   ) do
              Enum.each(hints ++ declarations, fn declaration ->
                :ok = rules.source_context_observer.(declaration, node, pseudo_element)
              end)

              {:ok,
               validated
               |> resolve_rem_values()
               |> resolve_font_relative_defaults()
               |> Map.delete(:_inherited_color)
               |> Map.delete(:_inherited_caption_side)
               |> Map.delete(:_inherited_orphans)
               |> Map.delete(:_inherited_widows)
               |> Map.delete(:_root_element)
               |> resolve_current_color_values()}
            end
          else
            with {:ok, style} <- apply_declarations(style, hints ++ declarations, rules.cache),
                 # Defaults contain no rem values; inherited lengths are already resolved.
                 style = if(declarations == [], do: style, else: resolve_rem_values(style)),
                 style =
                   style
                   |> resolve_font_relative_defaults()
                   # Cascade inputs are no longer needed once declarations resolve.
                   # Keep the computed properties and captured transform metrics.
                   |> Map.drop([
                     :_inherited_color,
                     :_inherited_caption_side,
                     :_inherited_orphans,
                     :_inherited_widows,
                     :_root_element,
                     :_inherited_font,
                     :_inherited_quotes,
                     :_inherited_css_transform,
                     :_inherited_css_transform_metrics,
                     :_inherited_transform_origin_metrics,
                     :_inherited_transform_origin,
                     :_ua_font
                   ])
                   |> resolve_current_color_values(),
                 style = style |> apply_border_styles() |> resolve_auto_margin_values(),
                 {:ok, style} <- put_font_face(style, rules.cache),
                 {:ok, style} <- MathLayoutValidator.compute_style(style) do
              # Native dropdown appearance uses the font's normal line height.
              # HTML's select rendering rules reset authored line-height here.
              # Native dropdown text includes one CSS pixel of internal leading
              # above and below the font line, independent of authored padding.
              style =
                if node.tag == "select" and Map.get(style, :appearance) == :base_select and
                     not style._select_state.listbox,
                   do:
                     style
                     |> Map.put(:form_kind, :base_select)
                     |> then(fn style ->
                       if node._initial_form_state.disabled and
                            not Enum.any?(declarations, fn {property, _} ->
                              property == "color"
                            end), do: Map.put(style, :color, {0, 0, 0}), else: style
                     end),
                   else: style

              style =
                case style do
                  %{form_kind: :listbox} ->
                    style
                    |> Map.put(:line_height_normal, true)
                    |> Map.delete(:line_height_multiplier)
                    |> put_line_height()

                  %{_initial_option_id: _, _native_select_option: true} ->
                    style =
                      if style.min_height == {:css_layout_em, 1.2},
                        do:
                          Map.put(
                            style,
                            :min_height,
                            Float.floor(style.font_size * 1.2 / 0.75 * 64) / 64 * 0.75
                          ),
                        else: style

                    style
                    |> Map.put(:line_height_normal, true)
                    |> Map.delete(:line_height_multiplier)
                    |> put_line_height()

                  %{_number_state: _} ->
                    style =
                      style
                      |> put_line_height()
                      |> NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_initial_number_style(
                        declarations
                      )

                    if NativeElixirPdfUtilities.Validators.HtmlFormValidator.number_uses_normal_line_height?(
                         style
                       ) do
                      style
                      |> Map.put(:line_height_normal, true)
                      |> Map.delete(:line_height_multiplier)
                      |> put_line_height()
                    else
                      style
                    end

                  %{_color_state: _} ->
                    style
                    |> NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_initial_textfield_style(
                      declarations
                    )
                    |> put_line_height()

                  %{_temporal_state: _} ->
                    style
                    |> NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_initial_temporal_style(
                      declarations
                    )
                    |> put_line_height()

                  %{form_kind: :text} ->
                    style
                    |> NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_initial_textfield_style(
                      declarations
                    )
                    |> put_line_height()

                  %{form_kind: :select} ->
                    style
                    |> Map.put(
                      :_native_menu,
                      Map.get(style, :appearance, :auto) != :none and
                        not Enum.any?(declarations, fn {property, _value} ->
                          property in ["background", "background-color", "background-image"] or
                            (String.starts_with?(property, "border") and
                               property != "border-radius")
                        end)
                    )
                    |> Map.put(:line_height_normal, false)
                    |> Map.delete(:line_height_multiplier)
                    |> Map.delete(:line_height_explicit)
                    |> Map.put(
                      :line_height,
                      normal_line_height(style) +
                        if(Map.get(style, :appearance, :auto) == :none, do: 0.0, else: 1.5)
                    )

                  _ ->
                    put_line_height(style)
                end

              with {:ok, style} <-
                     (case style do
                        %{text_decoration_line: [_ | _]} ->
                          HtmlValidator.resolve_text_decorations(style)

                        %{_text_decorations: _} ->
                          HtmlValidator.resolve_text_decorations(style)

                        _ ->
                          {:ok, style}
                      end),
                   :ok <-
                     NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.observe_cascade(
                       node,
                       pseudo_element,
                       matched
                     ),
                   do:
                     {:ok,
                      style
                      |> Map.put(:_position_viewport, Map.get(rules, :position_viewport))
                      |> then(
                        &NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_skipped_input_style(
                          node,
                          &1,
                          declarations
                        )
                      )}
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
          {:tag, String.downcase(node.tag, :ascii)} | Enum.map(classes, &{:class, &1})
        ]

        keys
        |> Enum.flat_map(&Map.get(index, &1, []))
        |> Enum.reduce(%{}, fn {order, selector}, matched ->
          scope = Map.get(selector, :shadow_scope_id)
          host = Map.get(selector, :shadow_host_id)
          rightmost = hd(selector.parts)
          boundary = Map.get(rightmost, :shadow_selector)

          applicable? =
            case {scope, boundary} do
              {nil, nil} -> not Map.has_key?(node, :shadow_scope_id)
              {nil, _} -> false
              {_scope, {:host, _}} -> Map.get(node, :node_id) == host
              {scope, {:slotted, _}} -> Map.get(node, :assigned_shadow_scope_id) == scope
              {scope, nil} -> Map.get(node, :tree_scope_id) == scope
            end

          local_ancestors =
            if is_nil(scope),
              do: Enum.filter(ancestors, &(not Map.has_key?(&1, :shadow_scope_id))),
              else:
                Enum.filter(ancestors, fn ancestor ->
                  Map.get(ancestor, :tree_scope_id) == scope or
                    Map.get(ancestor, :node_id) == host
                end)

          local_ancestors =
            if match?({:slotted, _}, boundary),
              do:
                Enum.reject(
                  local_ancestors,
                  &(Map.get(&1, :node_id) == Map.get(node, :assigned_slot_id))
                ),
              else: local_ancestors

          selector_context = Map.merge(context, %{shadow_scope_id: scope, shadow_host_id: host})

          case applicable? and
                 match_selector_parts(selector.parts, node, local_ancestors, selector_context) do
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

    target =
      case Map.get(part, :shadow_selector) do
        {:slotted, _} -> Map.get(node, :assigned_slot, node)
        _ -> node
      end

    attributes = Map.get(target, :attributes, %{})

    classes =
      Map.get_lazy(target, :_selector_classes, fn ->
        target
        |> Map.get(:attributes, %{})
        |> Map.get("class", "")
        |> HtmlValidator.html_class_tokens()
        |> MapSet.new()
      end)

    expected_tag =
      if Map.get(node, :namespace, :html) == :html,
        do: part.tag,
        else: Map.get(part, :source_tag, part.tag)

    tag_matches? = is_nil(part.tag) or Map.get(target, :tag) == expected_tag
    id_matches? = is_nil(part.id) or Map.get(attributes, "id") == part.id
    classes_match? = Enum.all?(part.classes, &MapSet.member?(classes, &1))

    # Resolve context only for compounds whose positive requirements match.
    # An impossible compound needs neither sibling positions nor negation work.
    shadow_matches? =
      case Map.get(part, :shadow_selector) do
        {:host, compound} ->
          Map.get(node, :node_id) == Map.get(context, :shadow_host_id) and
            (is_nil(compound) or
               matches_simple_selector?(
                 compound,
                 node,
                 ancestors,
                 Map.put(context, :shadow_argument, true)
               ))

        {:slotted, compound} ->
          Map.get(node, :assigned_shadow_scope_id) == Map.get(context, :shadow_scope_id) and
            matches_simple_selector?(
              compound,
              node,
              ancestors,
              Map.put(context, :shadow_argument, true)
            )

        nil ->
          Map.get(context, :shadow_argument, false) or is_nil(Map.get(context, :shadow_scope_id)) or
            Map.get(node, :node_id) != Map.get(context, :shadow_host_id)
      end

    shadow_matches? and tag_matches? and id_matches? and classes_match? and
      Enum.all?(part.attributes, &HtmlValidator.attribute_selector_matches?(&1, target)) and
      Enum.all?(part.pseudo_classes, &pseudo_class_matches?(&1, target, ancestors, context)) and
      Enum.all?(part.negations, &(not matches_simple_selector?(&1, target, ancestors, context)))
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
             :valid,
             :invalid,
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
        case Map.fetch(
               selector_positions(ancestors, context, node),
               Map.get(node, :node_id, node._selector_id)
             ) do
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
        case Map.fetch(
               selector_positions(ancestors, context, node),
               Map.get(node, :node_id, node._selector_id)
             ) do
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
  defp selector_positions(ancestors, context, node) do
    parent =
      case Map.get(context, :logical_nodes, %{})[Map.get(node, :parent_id)] do
        parent when is_map(parent) ->
          parent

        _ ->
          case ancestors do
            [parent | _rest] -> parent
            [] -> %{_selector_id: context.root_selector_id, children: context.root_children}
          end
      end

    key = Map.get(parent, :node_id, Map.get(parent, :_selector_id))

    RenderCache.fetch_stylesheet({:selector_positions, context.cache, key}, fn ->
      elements =
        Enum.filter(parent.children, fn child ->
          match?(%{type: :element}, child) and
            not (Map.get(child, :tag) == "template" and
                   Map.has_key?(
                     Map.get(context, :consumed_shadow_roots, %{}),
                     get_in(child, [:content, :node_id])
                   ))
        end)

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

          {Map.put(positions, Map.get(child, :node_id, Map.get(child, :_selector_id)), value),
           Map.put(seen, type, type_position)}
        end)

      positions
    end)
  end

  defp apply_declarations(style, declarations, cache) do
    CssValueValidator.validate_cascade(
      style,
      declarations,
      css_budget(cache),
      fn declaration, validated ->
        if elem(declaration, 0) == "font-family",
          do: put_font_face(validated, cache),
          else: {:ok, validated}
      end
    )
  end

  defp apply_border_styles(style) do
    border_styles = Map.get(style, :border_styles, @none_edges)

    border_widths =
      if border_styles == @none_edges do
        @zero_edges
      else
        :maps.map(
          fn side, width ->
            if Map.fetch!(border_styles, side) in [:none, :hidden],
              do: 0.0,
              else: if(width > 0, do: max(0.75, Float.floor(width / 0.75) * 0.75), else: 0.0)
          end,
          Map.get(style, :border_widths, @medium_border_edges)
        )
      end

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
          case {border_colors, color} do
            {@current_color_edges, {0, 0, 0}} ->
              @black_edges

            _ ->
              :maps.fold(
                fn side, value, resolved ->
                  if value == :current_color, do: Map.put(resolved, side, color), else: resolved
                end,
                border_colors,
                border_colors
              )
          end

        Map.put(style, :border_colors, resolved)

      _ ->
        style
    end
  end

  defp resolve_rem_values(style) do
    root_font_size = Map.fetch!(style, :_root_font_size)

    :maps.fold(
      fn key, value, resolved ->
        replacement =
          if key in @rem_opaque_style_keys,
            do: value,
            else: resolve_rem_term(value, root_font_size)

        if replacement === value, do: resolved, else: Map.put(resolved, key, replacement)
      end,
      style,
      style
    )
  end

  defp resolve_rem_term(term, root_font_size) do
    case term do
      {:rem, multiplier} when is_number(multiplier) ->
        multiplier * root_font_size

      {red, green, blue} when is_number(red) and is_number(green) and is_number(blue) ->
        term

      {red, green, blue, alpha}
      when is_number(red) and is_number(green) and is_number(blue) and is_number(alpha) ->
        term

      term when is_map(term) ->
        :maps.fold(
          fn key, value, resolved ->
            replacement = resolve_rem_term(value, root_font_size)
            if replacement === value, do: resolved, else: Map.put(resolved, key, replacement)
          end,
          term,
          term
        )

      term when is_list(term) ->
        Enum.map(term, &resolve_rem_term(&1, root_font_size))

      term when is_tuple(term) ->
        term
        |> Tuple.to_list()
        |> Enum.map(&resolve_rem_term(&1, root_font_size))
        |> List.to_tuple()

      _ ->
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
    NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.original_css_budget() ||
      RenderCache.fetch(cache, :css_variable_budget, &HtmlValidator.new_css_budget/0)
  end

  defp put_font_face(style, cache) do
    registry = Map.fetch!(style, :_font_registry)

    registry =
      if Map.has_key?(registry, :scoped),
        do: Map.put(registry, :active_scope, Map.get(style, :_font_family_scope)),
        else: registry

    with {:ok, families, font_face} <-
           resolve_font(
             Map.fetch!(style, :font_families),
             Map.fetch!(style, :font_weight),
             Map.fetch!(style, :font_style),
             registry,
             cache
           ) do
      {:ok,
       style
       |> Map.put(:_font_registry, registry)
       |> Map.put(:font_families, families)
       |> Map.put(:font_family, font_face.family)
       |> Map.put(:font_face, font_face)}
    end
  end

  defp resolve_auto_margin_values(style) do
    NativeElixirPdfUtilities.Validators.CssSizingValidator.prepare_auto_margins(style)
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

  defp rasterize_svg_image(svg, raster_options, image_budget, opts, scale \\ 1) do
    raster_options = Keyword.put(raster_options, :scale, scale)
    fonts = Keyword.get(opts, :fonts, [])
    font_options = Keyword.take(opts, [:system_font_discovery])

    loader = fn ->
      with {:ok, png} <-
             SvgRasterizer.rasterize(
               svg,
               raster_options,
               image_budget,
               fonts,
               %{mode: :image},
               font_options
             ),
           {:ok, image} <- PngDecoder.decode(png, image_budget, false) do
        # Raster density changes pixel storage, not the replaced image's
        # natural CSS dimensions or the background tile's physical size.
        {:ok, %{image | width: image.width / scale, height: image.height / scale}}
      end
    end

    RenderCache.fetch(
      Keyword.fetch!(opts, :__render_cache__),
      {:rasterized_svg, svg, raster_options, fonts, font_options},
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
    NativeElixirPdfUtilities.Validators.SvgImageValidator.data_uri(src, image_budget)
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

        with {:ok, data} <- result,
             {:ok, mime} <-
               NativeElixirPdfUtilities.Validators.MimeValidator.prepare(data, src, nil) do
          {:ok, data, if(mime == "image/svg+xml", do: :svg, else: nil)}
        end
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
