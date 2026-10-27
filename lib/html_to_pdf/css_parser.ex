defmodule NativeElixirPdfUtilities.HtmlToPdf.CssParser do
  @moduledoc """
  Strict CSS parser for the native HTML-to-PDF renderer.

  The parser accepts the document-oriented selector subset used by the style
  cascade: element, class, id, attribute operators and case flags, descendant, child,
  supported positional, initial `:enabled`/`:disabled`/`:checked`/`:popover-open` and negation pseudo-classes, `::before`, `::after`, and
  comma groups. Bare `@page { ... }` and simple `@font-face` rules are accepted
  outside the style cascade, and `@media print` rules are included in the
  active print cascade. Page selectors, named-page preludes, and misspelled
  `@page` at-rules are rejected. Declarations are kept as normalized
  property/value pairs so the style layer can validate values against the
  renderer's supported property set.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlCssSourceValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.PrintMediaValidator
  alias NativeElixirPdfUtilities.Validators.CssReferenceValidator
  alias NativeElixirPdfUtilities.Validators.CssPageValidator
  alias NativeElixirPdfUtilities.Validators.CssFontFaceValidator

  @type declaration :: {String.t(), String.t()} | {String.t(), String.t(), :important}
  @type selector_part :: %{
          required(:tag) => String.t() | nil,
          required(:id) => String.t() | nil,
          required(:classes) => [String.t()],
          required(:attributes) => [
            {:present, String.t()}
            | {attribute_operator(), String.t(), String.t()}
            | {attribute_operator(), String.t(), String.t(), :sensitive | :insensitive}
          ],
          required(:pseudo_classes) => [
            :first_child
            | :last_child
            | :first_of_type
            | :last_of_type
            | :only_child
            | :only_of_type
            | :root
            | :enabled
            | :disabled
            | :checked
            | :required
            | :optional
            | :read_only
            | :read_write
            | :popover_open
            | {:lang, [[String.t()]]}
            | {:nth_child | :nth_last_child | :nth_of_type | :nth_last_of_type,
               integer() | :odd | :even | {integer(), integer()}}
          ],
          required(:negations) => [selector_part()],
          required(:pseudo_element) => nil | :before | :after | :picker_icon,
          required(:combinator) => nil | :descendant | :child,
          optional(:language_dependent) => true
        }
  @type selector :: %{
          required(:parts) => [selector_part()],
          required(:specificity) => {non_neg_integer(), non_neg_integer(), non_neg_integer()},
          optional(:language_dependent) => true
        }
  @type attribute_operator :: :equals | :includes | :dash_match | :prefix | :suffix | :substring
  @type rule :: %{
          selectors: [selector()],
          declarations: [declaration()],
          order: non_neg_integer()
        }
  @type stylesheet :: [rule()]
  @type font_face :: %{
          family: String.t(),
          sources: [String.t()],
          weight: 100..900,
          style: :normal | :italic
        }
  @type page_option ::
          {:page_size, PageGeometry.page_size_input()}
          | {:margin, PageGeometry.margin_input()}

  @css_opaque_pattern HtmlValidator.css_opaque_pattern()
  @page_rule_regex Regex.compile!(
                     "#{@css_opaque_pattern}(*SKIP)(*F)|@page\\s*\\{(?<declarations>(?:#{@css_opaque_pattern}|[^{}])*)\\}",
                     "ui"
                   )
  @page_rule_candidate_regex Regex.compile!(
                               "#{@css_opaque_pattern}(*SKIP)(*F)|@page[^{;]*(?=\\{|;|$)",
                               "ui"
                             )
  @font_face_rule_regex Regex.compile!(
                          "#{@css_opaque_pattern}(*SKIP)(*F)|@font-face\\s*\\{(?<declarations>(?:#{@css_opaque_pattern}|[^{}])*)\\}",
                          "ui"
                        )
  @css_block_regex Regex.compile!(
                     "#{@css_opaque_pattern}(*SKIP)(*F)|\\{(?:#{@css_opaque_pattern}|[^{}])*\\}",
                     "u"
                   )

  @doc """
  Parses a CSS stylesheet into strict renderer rules.
  """
  @spec parse(String.t()) ::
          {:ok, stylesheet()}
          | {:error, :invalid_css | :not_implemented | :resource_limit_exceeded}
  def parse(css) do
    case parse_detailed(css) do
      {:ok, stylesheet} -> {:ok, stylesheet}
      {:error, {reason, _detail}} -> {:error, reason}
    end
  end

  @doc """
  Parses a CSS stylesheet and returns source-location details when parsing fails.
  """
  @spec parse_detailed(String.t()) ::
          {:ok, stylesheet()}
          | {:error, {:invalid_css | :not_implemented | :resource_limit_exceeded, map()}}
  def parse_detailed(css) do
    with {:ok, prepared} <- prepare_stylesheet(css), do: {:ok, prepared.rules}
  end

  @typedoc false
  @type prepared_stylesheet :: %{
          rules: stylesheet(),
          font_faces: [font_face()],
          page_options: [page_option()]
        }

  @doc false
  @spec prepare_stylesheet(
          String.t(),
          PrintMediaValidator.context() | nil | :unpositioned
        ) ::
          {:ok, prepared_stylesheet()}
          | {:error, {:invalid_css | :not_implemented | :resource_limit_exceeded, map()}}
  def prepare_stylesheet(css, media_context \\ nil) do
    HtmlValidator.with_render_budget(fn ->
      case HtmlValidator.validate_css_source(css) do
        {:ok, css} ->
          HtmlValidator.reserve_render_resource(
            :max_aggregate_css_source_bytes,
            byte_size(css),
            :css
          )

          RenderCache.fetch_stylesheet(
            {:parsed, css, PrintMediaValidator.cache_context(css, media_context)},
            fn ->
              with {:ok, active_css} <-
                     css |> strip_comments() |> PrintMediaValidator.select_rules(media_context),
                   {:ok, font_faces} <- parse_font_faces(active_css, css),
                   {:ok, page_options} <- parse_page_rules(active_css, css) do
                parsed_css = active_css |> strip_font_face_rules() |> strip_page_rules()

                case parse_rules(parsed_css) do
                  {:ok, rules} ->
                    {:ok, %{rules: rules, font_faces: font_faces, page_options: page_options}}

                  {:error, :invalid_css} ->
                    {:error, {:invalid_css, css_error_detail(css, parsed_css)}}
                end
              else
                {:error, {:invalid_css, detail}} ->
                  {:error, {:invalid_css, detail}}

                {:error, :invalid_css} ->
                  {:error, {:invalid_css, css_error_detail(css, css)}}
              end
            end,
            fn result ->
              case result do
                {:ok, prepared} ->
                  HtmlValidator.reserve_render_resource(
                    :max_css_rules,
                    length(prepared.rules),
                    :css
                  )

                {:error, _} ->
                  :ok
              end
            end
          )

        {:error, {:invalid_css, diagnostic}} ->
          {:error, {:invalid_css, diagnostic}}
      end
    end)
  end

  @doc false
  @spec prepare_source_stylesheet(
          String.t(),
          map() | nil,
          PrintMediaValidator.context() | nil | :unpositioned
        ) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare_source_stylesheet(css, descriptor \\ nil, media_context \\ nil) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, css} <- HtmlValidator.validate_css_source(css),
           :ok <-
             if(is_nil(descriptor),
               do: :ok,
               else: HtmlCssSourceValidator.validate_descriptor(css, descriptor)
             ) do
        HtmlValidator.reserve_render_resource(
          :max_aggregate_css_source_bytes,
          byte_size(css),
          :css
        )

        HtmlValidator.reserve_render_resource(:max_css_work, byte_size(css), :css)
        projection = source_projection(css) |> Map.put(:descriptor, descriptor)

        with {:ok, projected, media} <- source_media_projection(projection, media_context),
             {:ok, fonts} <- source_font_records(projected, projection, media),
             {:ok, pages} <- source_page_records(projected, projection, media),
             rules_css = mask_source_blocks(projected, @font_face_rule_regex),
             rules_css = mask_source_blocks(rules_css, @page_rule_regex),
             {:ok, sources} <- rule_sources(rules_css),
             {:ok, rules} <- source_rules(sources, rules_css, projection, media) do
          active_rules =
            rules
            |> Enum.filter(& &1.active)
            |> Enum.with_index()
            |> Enum.map(fn {rule, order} -> %{rule | order: order} end)

          {:ok,
           %{
             rules: active_rules,
             font_faces: fonts |> Enum.filter(& &1.active) |> Enum.map(& &1.face),
             page_options:
               Enum.reduce(pages, [], fn page, options ->
                 if page.active,
                   do: PageGeometry.merge_page_options(options, page.options),
                   else: options
               end),
             authored_rules: rules,
             authored_fonts: fonts,
             authored_pages: pages,
             authored_media: media,
             media_provider: PrintMediaValidator,
             media_policy: :unresolved_provider_context
           }}
        else
          {:error, {reason, detail}} ->
            {:error, {reason, detail}}

          {:error, _} ->
            source_parse_failure(
              projection,
              projection.text,
              0,
              :invalid_css,
              css_error_detail(css, projection.text)
            )
        end
      end
    end)
  end

  defp source_projection(css) do
    {text, gaps} = HtmlValidator.css_comment_projection(css)
    %{original: css, text: text, gaps: gaps}
  end

  defp source_record(projection, offset, length) do
    HtmlValidator.reserve_render_resource(
      :max_css_work,
      length(projection.gaps) + offset + length,
      :css
    )

    original_offset =
      offset +
        Enum.reduce(projection.gaps, 0, fn {at, bytes}, total ->
          if at <= offset, do: total + bytes, else: total
        end)

    finish = offset + length

    original_finish =
      finish +
        Enum.reduce(projection.gaps, 0, fn {at, bytes}, total ->
          if at < finish, do: total + bytes, else: total
        end)

    case Map.get(projection, :descriptor) do
      nil ->
        prefix = binary_part(projection.original, 0, original_offset)
        lines = String.split(prefix, "\n", trim: false)

        %{
          text:
            binary_part(projection.original, original_offset, original_finish - original_offset),
          offset: original_offset,
          length: original_finish - original_offset,
          line: length(lines),
          column: String.length(List.last(lines)) + 1,
          provenance: :unbound,
          owner: nil
        }

      descriptor ->
        HtmlCssSourceValidator.map_span(
          descriptor,
          original_offset,
          original_finish - original_offset
        )
    end
  end

  @doc false
  @spec style_declaration_records(String.t()) :: {:ok, [map()]} | {:error, {atom(), map()}}
  @spec style_declaration_records(String.t(), PrintMediaValidator.context() | nil | :unpositioned) ::
          {:ok, [map()]} | {:error, {atom(), map()}}
  def style_declaration_records(css, media_context \\ nil) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, css} <- HtmlValidator.validate_css_source(css),
           projection = source_projection(css),
           {:ok, projected, media} <- source_media_projection(projection, media_context),
           rules_css = mask_source_blocks(projected, @font_face_rule_regex),
           rules_css = mask_source_blocks(rules_css, @page_rule_regex),
           {:ok, sources} <- rule_sources(rules_css) do
        {records, _cursor} =
          Enum.map_reduce(sources, 0, fn rule, cursor ->
            {offset, length} =
              :binary.match(rules_css, rule.source,
                scope: {cursor, byte_size(rules_css) - cursor}
              )

            base = offset + byte_size(rule.selectors) + 1

            records =
              if source_active?(base, media) do
                Enum.map(declaration_spans(rule.declarations), fn span ->
                  %{source: source_record(projection, base + span.offset, span.length)}
                end)
              else
                []
              end

            {records, offset + length}
          end)

        {:ok, List.flatten(records)}
      else
        {:error, {_reason, _diagnostic}} = error ->
          error

        {:error, _source} ->
          {:error, {:invalid_css, css_error_detail(css, css)}}
      end
    end)
  end

  @doc false
  @spec prepare_source_declarations(String.t(), map()) ::
          {:ok, [map()]} | {:error, {atom(), map()}}
  def prepare_source_declarations(css, descriptor) do
    HtmlValidator.with_render_budget(fn ->
      projection = %{original: css, text: css, gaps: [], descriptor: descriptor}

      with :ok <- HtmlCssSourceValidator.validate_descriptor(css, descriptor) do
        case parse_declarations_input(css, true) do
          {:ok, declarations} ->
            {:ok, source_declarations(css, declarations, 0, projection)}

          {:error, {reason, detail, offset}} ->
            source_parse_failure(projection, css, 0, reason, detail, offset)

          {:error, _} = error ->
            error
        end
      end
    end)
  end

  defp source_media_projection(projection, media_context) do
    with {:ok, projected, media} <-
           PrintMediaValidator.project_source_rules(projection.text, media_context) do
      records =
        Enum.map(media, fn record ->
          record
          |> Map.put(:source, source_record(projection, record.offset, record.length))
          |> Map.delete(:offset)
          |> Map.delete(:length)
          |> Map.put(:policy, :unresolved_provider_context)
          |> Map.put(:provider, PrintMediaValidator)
        end)

      {:ok, projected, records}
    end
  end

  defp source_active?(offset, media) do
    Enum.all?(media, fn record ->
      offset < record.body_offset or offset >= record.body_offset + record.body_length or
        record.active
    end)
  end

  defp mask_source_blocks(css, regex) do
    Regex.replace(regex, css, fn block, _body -> String.duplicate(" ", byte_size(block)) end)
  end

  defp source_declarations(block, declarations, base, projection, references? \\ true) do
    declaration_spans(block)
    |> Enum.zip(declarations)
    |> Enum.map(fn {%{text: snippet, offset: offset, length: length}, declaration} ->
      %{
        declaration: declaration,
        projected_offset: base + offset,
        projected_length: length,
        source: source_record(projection, base + offset, length),
        context_policy: :requires_applicable_original_context,
        references:
          if(references?,
            do: source_reference_records(snippet, base + offset, projection),
            else: []
          )
      }
    end)
  end

  defp source_reference_records(snippet, base, projection) do
    case CssReferenceValidator.scan(snippet, fn offset, length ->
           source_record(projection, base + offset, length)
         end) do
      {:ok, references} ->
        Enum.map(references, fn reference ->
          source = source_record(projection, base + reference.offset, reference.length)
          reference |> Map.drop([:offset, :length]) |> Map.put(:source, source)
        end)

      {:error, {_reason, _diagnostic}} = error ->
        throw({:render_resource_limit, error})
    end
  end

  defp source_rules(sources, rules_css, projection, media) do
    sources
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, [], 0}, fn {source, order}, {:ok, rules, cursor} ->
      {offset, length} =
        :binary.match(rules_css, source.source,
          scope: {cursor, byte_size(projection.text) - cursor}
        )

      case parse_rule(source, order, true) do
        {:ok, rule} ->
          base = offset + byte_size(source.selectors) + 1
          records = source_declarations(source.declarations, rule.declarations, base, projection)
          selector = HtmlValidator.trim_css_whitespace(source.selectors)

          selector_offset =
            offset + byte_size(source.selectors) -
              byte_size(String.trim_leading(source.selectors))

          rule =
            rule
            |> Map.put(:source_declarations, records)
            |> Map.put(:source, source_record(projection, selector_offset, byte_size(selector)))
            |> Map.put(:active, source_active?(base, media))

          {:cont, {:ok, [rule | rules], offset + length}}

        {:error, {reason, detail, declaration_offset}} ->
          {:halt,
           source_parse_failure(
             projection,
             source.declarations,
             offset + byte_size(source.selectors) + 1,
             reason,
             detail,
             declaration_offset
           )}

        {:error, _} ->
          {:halt,
           source_parse_failure(
             projection,
             source.source,
             offset,
             :invalid_css,
             css_error_detail(source.source, source.source)
           )}
      end
    end)
    |> case do
      {:ok, rules, _cursor} -> {:ok, Enum.reverse(rules)}
      error -> error
    end
  end

  defp source_parse_failure(projection, block, base, reason, detail, exact_offset \\ nil) do
    {offset, length} =
      if is_nil(exact_offset),
        do: :binary.match(block, detail.source),
        else: {exact_offset, byte_size(detail.source)}

    source = source_record(projection, base + offset, length)

    HtmlValidator.reserve_render_resource(:max_css_work, byte_size(detail.message), :css)

    message =
      detail.message
      |> String.replace(~r/\Aline [0-9]+:/, "line #{source.line}:")
      |> String.replace(~s("#{detail.source}"), ~s("#{source.text}"))

    NativeElixirPdfUtilities.Diagnostics.error(:css, reason, message,
      module: __MODULE__,
      source: source.text,
      line: source.line,
      column: source.column
    )
  end

  defp source_font_records(projected, projection, media) do
    @font_face_rule_regex
    |> Regex.scan(projected, return: :index)
    |> Enum.reduce_while({:ok, []}, fn [{offset, length}, {base, bytes}], {:ok, fonts} ->
      block = binary_part(projected, base, bytes)

      case parse_declarations_input(block, true) do
        {:ok, declarations} ->
          records = source_declarations(block, declarations, base, projection, false)

          case CssFontFaceValidator.validate_authored(declarations) do
            {:ok, face} ->
              font = %{
                source: source_record(projection, offset, length),
                declarations: records,
                candidates: source_font_candidates(records, block, base, projection),
                face: face,
                active: source_active?(offset, media),
                policy: :authored_descriptor_values_and_selection,
                runtime: [:authorization, :font_bytes, :font_metrics]
              }

              {:cont, {:ok, [font | fonts]}}

            {:error, %{property: property, index: index}} ->
              record = if is_nil(index), do: nil, else: Enum.at(records, index)

              source =
                if record, do: record.source, else: source_record(projection, offset, length)

              message =
                if record,
                  do: "@font-face declaration is invalid or unsupported",
                  else: "@font-face is missing required #{inspect(property)} descriptor"

              {:halt,
               NativeElixirPdfUtilities.Diagnostics.error(:css, :invalid_css, message,
                 module: __MODULE__,
                 source: source.text,
                 line: source.line,
                 column: source.column
               )}
          end

        {:error, {reason, detail, offset}} ->
          {:halt, source_parse_failure(projection, block, base, reason, detail, offset)}
      end
    end)
    |> case do
      {:ok, fonts} -> {:ok, Enum.reverse(fonts)}
      error -> error
    end
  end

  defp source_font_candidates(records, block, base, projection) do
    selected_src =
      records
      |> Enum.with_index()
      |> Enum.reduce(nil, fn {record, index}, selected ->
        if elem(record.declaration, 0) == "src", do: index, else: selected
      end)

    {candidates, _cursor} =
      records
      |> Enum.with_index()
      |> Enum.reduce({[], 0}, fn {record, index}, {acc, cursor} ->
        {property, _value} = record.declaration

        if property == "src" do
          cursor = record.projected_offset - base
          finish = cursor + record.projected_length

          [_property, value] =
            block |> binary_part(cursor, record.projected_length) |> String.split(":", parts: 2)

          {items, cursor} =
            CssFontFaceValidator.source_candidates(value)
            |> Enum.with_index()
            |> Enum.reduce({[], cursor}, fn {candidate, candidate_index}, {items, cursor} ->
              {at, bytes} =
                :binary.match(block, candidate, scope: {cursor, finish - cursor})

              original = source_record(projection, base + at, bytes)

              {normalized, gaps} = HtmlValidator.css_comment_projection(candidate, :value)
              candidate_projection = %{original: candidate, text: normalized, gaps: gaps}

              map_span = fn offset, length ->
                local = source_record(candidate_projection, offset, length)
                source_record(projection, base + at + local.offset, local.length)
              end

              info =
                case CssFontFaceValidator.validate_source_candidate(normalized, map_span) do
                  {:ok, info} ->
                    info

                  {:error, {_reason, _diagnostic}} = error ->
                    throw({:render_resource_limit, error})
                end

              references =
                Enum.map(info.source_references, fn reference ->
                  source = map_span.(reference.offset, reference.length)

                  reference |> Map.drop([:offset, :length]) |> Map.put(:source, source)
                end)

              info = %{info | source_references: references}

              role =
                cond do
                  index != selected_src -> :superseded
                  not info.supported -> :unsupported
                  candidate_index == 0 -> :first_candidate
                  true -> :fallback_candidate
                end

              item =
                Map.merge(info, %{
                  source: original,
                  declaration_source: record.source,
                  declaration_index: index,
                  candidate_index: candidate_index,
                  role: role,
                  authorization: :unresolved,
                  contents: :unresolved
                })

              {[item | items], at + bytes}
            end)

          {items ++ acc, cursor}
        else
          {acc, cursor}
        end
      end)

    Enum.reverse(candidates)
  end

  defp source_page_records(projected, projection, media) do
    @page_rule_regex
    |> Regex.scan(projected, return: :index)
    |> Enum.reduce_while({:ok, []}, fn [{offset, length}, {base, bytes}], {:ok, pages} ->
      block = binary_part(projected, base, bytes)

      with {:ok, declarations} <- parse_declarations_input(block, true) do
        records = source_declarations(block, declarations, base, projection)

        result =
          Enum.reduce_while(records, {:ok, [], []}, fn record, {:ok, options, checked} ->
            HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)

            case CssPageValidator.validate_declaration(record.declaration) do
              {:ok, validated} ->
                record =
                  record
                  |> Map.put(:value_policy, validated.policy)
                  |> Map.put(:context_policy, :page_context)

                {:cont, {:ok, merge_page_option(options, validated.option), [record | checked]}}

              :error ->
                source = record.source

                {:halt,
                 NativeElixirPdfUtilities.Diagnostics.error(
                   :css,
                   :invalid_css,
                   "@page declaration is invalid or unsupported",
                   module: CssPageValidator,
                   source: source.text,
                   line: source.line,
                   column: source.column
                 )}
            end
          end)

        case result do
          {:ok, options, checked} ->
            page = %{
              source: source_record(projection, offset, length),
              declarations: Enum.reverse(checked),
              options: options,
              active: source_active?(offset, media),
              policy: :validator_owned_page_declarations,
              runtime: [:page_context, :resources]
            }

            {:cont, {:ok, [page | pages]}}

          error ->
            {:halt, error}
        end
      else
        {:error, {reason, detail, failed_offset}} ->
          {:halt, source_parse_failure(projection, block, base, reason, detail, failed_offset)}
      end
    end)
    |> case do
      {:ok, pages} -> {:ok, Enum.reverse(pages)}
      error -> error
    end
  end

  @doc """
  Extracts active local font declarations from `@font-face` rules.

  Sources must use `url(...)` with a TrueType or OpenType source. Remote URLs,
  data URIs, WOFF/WOFF2 sources, and unsupported descriptors are rejected.
  Relative paths are returned unchanged for the style layer to resolve against
  the stylesheet location or renderer `:base_url`. Supported sources retain
  their declared order so loading can fall back when an earlier file is
  unavailable or invalid.
  """
  @spec font_faces(String.t()) ::
          {:ok, [font_face()]}
          | {:error, :invalid_css}
          | {:error, {:resource_limit_exceeded, map()}}
  def font_faces(css) do
    HtmlValidator.with_render_budget(fn ->
      case HtmlValidator.validate_css_source(css) do
        {:ok, css} ->
          HtmlValidator.reserve_render_resource(
            :max_aggregate_css_source_bytes,
            byte_size(css),
            :css
          )

          case css |> strip_comments() |> PrintMediaValidator.select_rules(nil) do
            {:ok, active_css} ->
              case parse_font_faces(active_css, active_css) do
                {:ok, font_faces} -> {:ok, font_faces}
                {:error, {reason, _detail}} -> {:error, reason}
              end

            {:error, :invalid_css} ->
              {:error, :invalid_css}
          end

        {:error, {:invalid_css, _diagnostic}} ->
          {:error, :invalid_css}
      end
    end)
  end

  @doc """
  Extracts renderer page defaults from bare `@page { ... }` rules.

  Valid page-context properties are accepted even when the renderer does not
  apply them yet. Rendering consumes accepted named and explicit two-length
  `size` values, portrait and landscape orientations, one-to-four-value
  `margin` shorthands, and the four margin longhands. Malformed declarations,
  unknown properties, invalid paged-media descriptor values, page selectors,
  named-page preludes, and misspelled `@page` at-rules return
  `{:error, :invalid_css}`.
  """
  @spec page_options(String.t()) ::
          {:ok, [page_option()]}
          | {:error, :invalid_css}
          | {:error, {:resource_limit_exceeded, map()}}
  def page_options(css) do
    HtmlValidator.with_render_budget(fn ->
      case HtmlValidator.validate_css_source(css) do
        {:ok, css} ->
          HtmlValidator.reserve_render_resource(
            :max_aggregate_css_source_bytes,
            byte_size(css),
            :css
          )

          with {:ok, active_css} <-
                 css |> strip_comments() |> PrintMediaValidator.select_rules(nil) do
            case parse_page_rules(active_css, css) do
              {:ok, page_options} -> {:ok, page_options}
              {:error, {reason, _detail}} -> {:error, reason}
            end
          end

        {:error, {:invalid_css, _diagnostic}} ->
          {:error, :invalid_css}
      end
    end)
  end

  @doc """
  Parses a CSS declaration block into normalized property/value pairs.

  This is used for both stylesheet blocks and inline `style` attributes.
  """
  @spec parse_declarations(String.t()) ::
          {:ok, [declaration()]}
          | {:error, :invalid_css | :not_implemented | :resource_limit_exceeded}
  def parse_declarations(css) do
    case parse_declarations_detailed(css) do
      {:ok, declarations} -> {:ok, declarations}
      {:error, {reason, _detail}} -> {:error, reason}
    end
  end

  @doc """
  Parses a CSS declaration block and returns source-location details on failure.
  """
  @spec parse_declarations_detailed(String.t()) ::
          {:ok, [declaration()]}
          | {:error, {:invalid_css | :not_implemented | :resource_limit_exceeded, map()}}
  def parse_declarations_detailed(css) do
    case parse_declarations_input(css, false) do
      {:error, {reason, detail, _offset}} -> {:error, {reason, detail}}
      result -> result
    end
  end

  defp parse_declarations_input(css, source?) do
    HtmlValidator.with_render_budget(fn ->
      case HtmlValidator.validate_css_source(css, :declarations) do
        {:ok, css} ->
          HtmlValidator.reserve_render_resource(
            :max_aggregate_css_source_bytes,
            byte_size(css),
            :css
          )

          sources = if source?, do: declaration_spans(css), else: declaration_sources(css)

          Enum.reduce_while(sources, {:ok, []}, fn source, {:ok, acc} ->
            {declaration, offset} =
              case source do
                %{text: text, offset: offset} -> {text, offset}
                text -> {text, nil}
              end

            case HtmlValidator.validate_css_declaration(declaration) do
              {:ok, parsed} ->
                {:cont, {:ok, [parsed | acc]}}

              {:error, :invalid_css} ->
                {:halt,
                 {:error,
                  {:invalid_css, declaration_error_detail(css, declaration, offset), offset}}}
            end
          end)
          |> then(fn result ->
            case result do
              {:ok, items} -> {:ok, Enum.reverse(items)}
              error -> error
            end
          end)

        {:error, {:invalid_css, diagnostic}} ->
          {:error, {:invalid_css, diagnostic}}
      end
    end)
  end

  @doc "Removes CSS comments while preserving token boundaries, quoted strings and escapes."
  @spec strip_comments(String.t()) :: String.t()
  def strip_comments(css) do
    HtmlValidator.normalize_css_comments(css)
  end

  @doc "Removes CSS comments using an explicit separator, preserving quoted strings and escapes."
  @spec strip_comments(String.t(), String.t()) :: String.t()
  def strip_comments(css, separator) do
    strip_comments(css, nil, false, [], separator)
  end

  defp strip_comments(css, quote, escaped?, acc, separator) do
    case {css, quote, escaped?} do
      {"", _quote, _escaped?} ->
        acc |> Enum.reverse() |> IO.iodata_to_binary()

      {<<"/*", rest::binary>>, nil, false} ->
        case :binary.match(rest, "*/") do
          {index, 2} ->
            remaining = binary_part(rest, index + 2, byte_size(rest) - index - 2)
            strip_comments(remaining, nil, false, [separator | acc], separator)

          :nomatch ->
            strip_comments("", nil, false, [<<"/*", rest::binary>> | acc], separator)
        end

      {<<character::utf8, rest::binary>>, quote, true} ->
        strip_comments(rest, quote, false, [<<character::utf8>> | acc], separator)

      {<<character::utf8, rest::binary>>, quote, false}
      when character == ?\\ ->
        strip_comments(rest, quote, true, [<<character::utf8>> | acc], separator)

      {<<character::utf8, rest::binary>>, quote, false}
      when not is_nil(quote) and character == quote ->
        strip_comments(rest, nil, false, [<<character::utf8>> | acc], separator)

      {<<character::utf8, rest::binary>>, nil, false} when character in [?", ?'] ->
        strip_comments(rest, character, false, [<<character::utf8>> | acc], separator)

      {<<character::utf8, rest::binary>>, quote, false} ->
        strip_comments(rest, quote, false, [<<character::utf8>> | acc], separator)
    end
  end

  defp strip_page_rules(css) do
    Regex.replace(@page_rule_regex, css, "")
  end

  defp strip_font_face_rules(css) do
    Regex.replace(@font_face_rule_regex, css, "")
  end

  defp parse_font_faces(css, diagnostic_css) do
    @font_face_rule_regex
    |> Regex.scan(css, capture: ["declarations"])
    |> List.flatten()
    |> Enum.reduce_while({:ok, []}, fn block, {:ok, acc} ->
      case parse_font_face(block) do
        {:ok, font_face} ->
          {:cont, {:ok, [font_face | acc]}}

        {:error, {message, source}} ->
          {:halt,
           {:error, {:invalid_css, font_face_error_detail(diagnostic_css, message, source)}}}
      end
    end)
    |> then(fn result ->
      case result do
        {:ok, items} -> {:ok, Enum.reverse(items)}
        error -> error
      end
    end)
  end

  defp parse_font_face(block) do
    case parse_declarations_detailed(block) do
      {:ok, declarations} ->
        case CssFontFaceValidator.validate(declarations) do
          {:ok, face} ->
            {:ok, face}

          {:error, %{property: property, index: index}} ->
            {:error, font_face_descriptor_error(block, property, index)}
        end

      {:error, {:invalid_css, detail}} ->
        source = Map.get(detail, :source, "@font-face")

        {:error, {~s(@font-face declaration "#{source}" is invalid or unsupported), source}}
    end
  end

  defp font_face_descriptor_error(block, property, index) do
    source = if is_nil(index), do: nil, else: Enum.at(declaration_sources(block), index)

    case source do
      nil ->
        {~s(@font-face is missing required "#{property}" descriptor), "@font-face"}

      source ->
        {~s(@font-face declaration "#{source}" is invalid or unsupported), source}
    end
  end

  defp font_face_error_detail(css, message, source) do
    {line, column} = source_location(css, source)

    %{
      stage: :css,
      reason: :invalid_css,
      message: "line #{line}: #{message}",
      line: line,
      column: column,
      source: source
    }
  end

  defp page_rule_blocks(css) do
    @page_rule_regex
    |> Regex.scan(css, capture: ["declarations"])
    |> List.flatten()
  end

  defp parse_page_rules(css, diagnostic_css) do
    case invalid_page_rule_source(css) do
      nil ->
        css
        |> page_rule_blocks()
        |> Enum.reduce_while({:ok, []}, fn block, {:ok, acc} ->
          case page_options_from(block) do
            {:ok, page_options} ->
              {:cont, {:ok, PageGeometry.merge_page_options(acc, page_options)}}

            {:error, source} ->
              {:halt, {:error, {:invalid_css, declaration_error_detail(diagnostic_css, source)}}}
          end
        end)

      source ->
        {:error, {:invalid_css, page_rule_error_detail(diagnostic_css, source)}}
    end
  end

  defp invalid_page_rule_source(css) do
    css
    |> strip_page_rules()
    |> then(&Regex.replace(@css_block_regex, &1, "{}"))
    |> then(&Regex.run(@page_rule_candidate_regex, &1))
    |> case do
      [source] -> String.trim(source)
      nil -> nil
    end
  end

  defp page_options_from(block) do
    block
    |> declaration_sources()
    |> Enum.reduce_while({:ok, []}, fn source, {:ok, acc} ->
      case HtmlValidator.validate_css_declaration(source) do
        {:ok, {property, value}} ->
          put_page_option(acc, property, value, source)

        {:ok, {property, value, :important}} ->
          put_page_option(acc, property, value, source)

        {:error, :invalid_css} ->
          {:halt, {:error, source}}
      end
    end)
  end

  defp put_page_option(options, property, value, source) do
    case CssPageValidator.declaration_option(property, value) do
      {:ok, option} -> {:cont, {:ok, merge_page_option(options, option)}}
      :error -> {:halt, {:error, source}}
    end
  end

  defp merge_page_option(options, option) do
    case option do
      nil ->
        options

      {:margin_side, side, margin} ->
        margin =
          options
          |> Keyword.get(:margin)
          |> PageGeometry.merge_margin_defaults(%{side => margin})

        Keyword.put(options, :margin, margin)

      {key, value} ->
        Keyword.put(options, key, value)
    end
  end

  defp parse_rules(css) do
    case rule_sources(css) do
      {:ok, rule_sources} -> sources_to_rules(rule_sources)
      {:error, _source} -> {:error, :invalid_css}
    end
  end

  defp quoted_character_state(character, quote, _escaped?) do
    cond do
      byte_size(character) > 1 and String.starts_with?(character, ["\"", "'", "/*"]) ->
        {:quoted, quote, false}

      match?(<<"\\", _, _::binary>>, character) ->
        {:quoted, quote, false}

      character == quote ->
        {:quoted, nil, false}

      not is_nil(quote) ->
        {:quoted, quote, false}

      character in ["\"", "'"] ->
        {:quoted, character, false}

      true ->
        :syntax
    end
  end

  defp rule_sources(css) do
    HtmlValidator.reserve_render_resource(:max_css_work, byte_size(css), :css)

    initial = %{
      rules: [],
      selector: nil,
      current: [],
      mode: :selector,
      quote: nil,
      escaped?: false
    }

    result =
      css
      |> HtmlValidator.css_syntax_units()
      |> Enum.reduce_while({:ok, initial}, fn character, {:ok, state} ->
        case quoted_character_state(character, state.quote, state.escaped?) do
          {:quoted, quote, escaped?} ->
            {:cont,
             {:ok,
              %{
                state
                | current: [character | state.current],
                  quote: quote,
                  escaped?: escaped?
              }}}

          :syntax ->
            cond do
              state.mode == :selector and character == "{" ->
                selector = state.current |> Enum.reverse() |> Enum.join()

                case HtmlValidator.trim_css_whitespace(selector) do
                  "" ->
                    {:halt, {:error, css}}

                  _selector ->
                    {:cont,
                     {:ok, %{state | selector: selector, current: [], mode: :declarations}}}
                end

              state.mode == :declarations and character == "}" ->
                declarations = state.current |> Enum.reverse() |> Enum.join()

                HtmlValidator.reserve_render_resource(:max_css_rules, 1, :css)

                rule = %{
                  source: state.selector <> "{" <> declarations <> "}",
                  selectors: state.selector,
                  declarations: declarations
                }

                next = %{
                  state
                  | rules: [rule | state.rules],
                    selector: nil,
                    current: [],
                    mode: :selector
                }

                {:cont, {:ok, next}}

              character in ["{", "}"] ->
                {:halt, {:error, css}}

              true ->
                {:cont, {:ok, %{state | current: [character | state.current]}}}
            end
        end
      end)

    case result do
      {:ok, %{mode: :selector, quote: nil, escaped?: false} = state} ->
        case state.current
             |> Enum.reverse()
             |> Enum.join()
             |> HtmlValidator.trim_css_whitespace() do
          "" -> {:ok, Enum.reverse(state.rules)}
          _unparsed -> {:error, css}
        end

      {:ok, _state} ->
        {:error, css}

      {:error, source} ->
        {:error, source}
    end
  end

  defp sources_to_rules(rule_sources) do
    rule_sources
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {source, order}, {:ok, acc} ->
      case parse_rule(source, order) do
        {:ok, rule} -> {:cont, {:ok, [rule | acc]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
    |> then(fn result ->
      case result do
        {:ok, items} -> {:ok, Enum.reverse(items)}
        error -> error
      end
    end)
  end

  defp parse_rule(
         %{selectors: selector_source, declarations: declaration_source},
         order,
         source? \\ false
       ) do
    with {:ok, selectors} <- parse_selectors(selector_source),
         true <- selectors != [],
         {:ok, declarations} <-
           if(source?,
             do: parse_declarations_input(declaration_source, true),
             else: parse_declarations(declaration_source)
           ),
         true <- declarations != [] do
      {:ok, %{selectors: selectors, declarations: declarations, order: order}}
    else
      {:error, {_, _, _}} = error -> error
      _ -> {:error, :invalid_css}
    end
  end

  defp parse_selectors(selector_source) do
    selector_source
    |> split_selector_list()
    |> Enum.reduce_while({:ok, []}, fn selector, {:ok, acc} ->
      case parse_selector(HtmlValidator.trim_css_whitespace(selector)) do
        {:ok, parsed} -> {:cont, {:ok, [parsed | acc]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
    |> then(fn result ->
      case result do
        {:ok, items} -> {:ok, Enum.reverse(items)}
        error -> error
      end
    end)
  end

  defp split_selector_list(selector_source) do
    result =
      selector_source
      |> HtmlValidator.css_syntax_units()
      |> Enum.reduce({[], [], nil, false, 0}, fn character,
                                                 {parts, current, quote, escaped?, depth} ->
        case quoted_character_state(character, quote, escaped?) do
          {:quoted, quote, escaped?} ->
            {parts, [character | current], quote, escaped?, depth}

          :syntax ->
            case character do
              "," when depth == 0 ->
                part = current |> Enum.reverse() |> Enum.join()
                {[part | parts], [], nil, false, depth}

              "(" ->
                {parts, [character | current], nil, false, depth + 1}

              ")" ->
                {parts, [character | current], nil, false, depth - 1}

              _ ->
                {parts, [character | current], nil, false, depth}
            end
        end
      end)

    {parts, current, _quote, _escaped?, _depth} = result
    final = current |> Enum.reverse() |> Enum.join()
    Enum.reverse([final | parts])
  end

  defp parse_selector(selector) do
    HtmlValidator.reserve_render_resource(:max_css_work, byte_size(selector), :css)

    case selector_tokens(selector) do
      {:ok, []} -> {:error, :invalid_css}
      {:ok, tokens} -> selector_tokens_to_parts(tokens)
      {:error, :invalid_css} -> {:error, :invalid_css}
    end
  end

  defp selector_tokens(selector) do
    result =
      selector
      |> HtmlValidator.css_syntax_units()
      |> Enum.reduce_while(
        {:ok, [], [], nil, false, 0, 0},
        fn character, {:ok, tokens, current, quote, escaped?, bracket_depth, parenthesis_depth} ->
          case quoted_character_state(character, quote, escaped?) do
            {:quoted, quote, escaped?} ->
              {:cont,
               {:ok, tokens, [character | current], quote, escaped?, bracket_depth,
                parenthesis_depth}}

            :syntax ->
              cond do
                character == "[" ->
                  {:cont,
                   {:ok, tokens, [character | current], nil, false, bracket_depth + 1,
                    parenthesis_depth}}

                character == "]" and bracket_depth > 0 ->
                  {:cont,
                   {:ok, tokens, [character | current], nil, false, bracket_depth - 1,
                    parenthesis_depth}}

                character == "(" ->
                  {:cont,
                   {:ok, tokens, [character | current], nil, false, bracket_depth,
                    parenthesis_depth + 1}}

                character == ")" and parenthesis_depth > 0 ->
                  {:cont,
                   {:ok, tokens, [character | current], nil, false, bracket_depth,
                    parenthesis_depth - 1}}

                character in ["]", ")"] ->
                  {:halt, {:error, :invalid_css}}

                bracket_depth == 0 and parenthesis_depth == 0 and character == ">" ->
                  tokens = push_selector_token(tokens, current)
                  {:cont, {:ok, [">" | tokens], [], nil, false, 0, 0}}

                bracket_depth == 0 and parenthesis_depth == 0 and
                    character in [" ", "\t", "\n", "\r", "\f"] ->
                  tokens = push_selector_token(tokens, current)
                  {:cont, {:ok, tokens, [], nil, false, 0, 0}}

                true ->
                  {:cont,
                   {:ok, tokens, [character | current], nil, false, bracket_depth,
                    parenthesis_depth}}
              end
          end
        end
      )

    case result do
      {:ok, tokens, current, nil, false, 0, 0} ->
        {:ok, tokens |> push_selector_token(current) |> Enum.reverse()}

      {:ok, _tokens, _current, _quote, _escaped?, _bracket_depth, _parenthesis_depth} ->
        {:error, :invalid_css}

      {:error, :invalid_css} ->
        {:error, :invalid_css}
    end
  end

  defp push_selector_token(tokens, current) do
    case current |> Enum.reverse() |> Enum.join() |> HtmlValidator.trim_css_whitespace() do
      "" -> tokens
      token -> [token | tokens]
    end
  end

  defp selector_tokens_to_parts(tokens) do
    parsed =
      Enum.reduce_while(tokens, {:ok, [], nil, false}, fn token,
                                                          {:ok, parts, pending_combinator,
                                                           language?} ->
        cond do
          token == ">" and (parts == [] or not is_nil(pending_combinator)) ->
            {:halt, {:error, :invalid_css}}

          token == ">" ->
            {:cont, {:ok, parts, :child, language?}}

          true ->
            case parse_simple_selector(token) do
              {:ok, part} ->
                combinator =
                  case parts do
                    [] -> nil
                    _ -> pending_combinator || :descendant
                  end

                {:cont,
                 {:ok, [Map.put(part, :combinator, combinator) | parts], nil,
                  language? or Map.get(part, :language_dependent, false)}}

              {:error, reason} ->
                {:halt, {:error, reason}}
            end
        end
      end)

    case parsed do
      {:ok, _parts, pending_combinator, _language?} when not is_nil(pending_combinator) ->
        {:error, :invalid_css}

      {:ok, parts, nil, language?} ->
        parts = Enum.reverse(parts)

        case Enum.drop(parts, -1) |> Enum.any?(&(not is_nil(&1.pseudo_element))) do
          true ->
            {:error, :invalid_css}

          false ->
            selector = %{parts: parts, specificity: specificity(parts)}
            {:ok, if(language?, do: Map.put(selector, :language_dependent, true), else: selector)}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp parse_simple_selector(selector) do
    parsed =
      case selector do
        <<"*", rest::binary>> -> {:ok, nil, rest}
        <<prefix, _::binary>> when prefix in [?#, ?., ?[, ?:] -> {:ok, nil, selector}
        _ -> HtmlValidator.parse_css_identifier(selector)
      end

    case parsed do
      {:ok, tag, modifiers} ->
        part = %{
          tag: if(is_nil(tag), do: nil, else: String.downcase(tag, :ascii)),
          id: nil,
          classes: [],
          attributes: [],
          pseudo_classes: [],
          negations: [],
          pseudo_element: nil,
          combinator: nil
        }

        part =
          if is_binary(tag) and tag != part.tag, do: Map.put(part, :source_tag, tag), else: part

        case parse_selector_modifiers(modifiers, part) do
          {:ok, part} ->
            {:ok,
             %{
               part
               | classes: Enum.reverse(part.classes),
                 attributes: Enum.reverse(part.attributes),
                 pseudo_classes: Enum.reverse(part.pseudo_classes),
                 negations: Enum.reverse(part.negations)
             }}

          error ->
            error
        end

      :error ->
        {:error, :invalid_css}
    end
  end

  defp parse_selector_modifiers(modifiers, part) do
    HtmlValidator.reserve_render_resource(:max_css_work, byte_size(modifiers), :css)

    case modifiers do
      "" ->
        {:ok, part}

      _ ->
        cond do
          String.starts_with?(modifiers, ["#", "."]) ->
            <<prefix, name::binary>> = modifiers

            case HtmlValidator.parse_css_identifier(name) do
              {:ok, name, rest} ->
                case {prefix, part.id} do
                  {?#, nil} ->
                    parse_selector_modifiers(rest, %{part | id: name})

                  {?#, _id} ->
                    {:error, :invalid_css}

                  {?., _id} ->
                    parse_selector_modifiers(rest, %{part | classes: [name | part.classes]})
                end

              :error ->
                {:error, :invalid_css}
            end

          parsed = HtmlValidator.parse_css_shadow_selector(modifiers) ->
            case parsed do
              {:ok, kind, compound, rest} ->
                parsed_compound =
                  if is_nil(compound),
                    do: {:ok, nil},
                    else: parse_simple_selector(HtmlValidator.trim_css_whitespace(compound))

                case parsed_compound do
                  {:ok, compound} ->
                    if is_nil(compound) or
                         (is_nil(compound.pseudo_element) and
                            not Map.has_key?(compound, :shadow_selector)) do
                      part =
                        if is_map(compound) and Map.get(compound, :language_dependent, false),
                          do: Map.put(part, :language_dependent, true),
                          else: part

                      parse_selector_modifiers(
                        rest,
                        Map.put(part, :shadow_selector, {kind, compound})
                      )
                    else
                      {:error, :invalid_css}
                    end

                  _ ->
                    {:error, :invalid_css}
                end

              _ ->
                {:error, :invalid_css}
            end

          String.starts_with?(modifiers, "::") ->
            case HtmlValidator.parse_css_pseudo_element(modifiers) do
              {:ok, kind} -> {:ok, %{part | pseudo_element: kind}}
              :error -> {:error, :invalid_css}
            end

          String.starts_with?(modifiers, "[") ->
            case HtmlValidator.parse_css_attribute_selector(modifiers) do
              {:ok, attribute, rest} ->
                parse_selector_modifiers(rest, %{
                  part
                  | attributes: [attribute | part.attributes]
                })

              :error ->
                {:error, :invalid_css}
            end

          parsed = HtmlValidator.parse_css_simple_pseudo_class(modifiers) ->
            {:ok, pseudo_class, rest} = parsed

            parse_selector_modifiers(rest, %{
              part
              | pseudo_classes: [pseudo_class | part.pseudo_classes]
            })

          parsed = HtmlValidator.parse_css_nth_pseudo_class(modifiers) ->
            case parsed do
              {:ok, pseudo_class, rest} ->
                parse_selector_modifiers(rest, %{
                  part
                  | pseudo_classes: [pseudo_class | part.pseudo_classes]
                })

              :error ->
                {:error, :invalid_css}
            end

          parsed = HtmlValidator.parse_css_language_selector(modifiers) ->
            case parsed do
              {:ok, pseudo_class, rest} ->
                part = Map.put(part, :language_dependent, true)

                parse_selector_modifiers(rest, %{
                  part
                  | pseudo_classes: [pseudo_class | part.pseudo_classes]
                })

              :error ->
                {:error, :invalid_css}
            end

          parsed = HtmlValidator.parse_css_negated_selector(modifiers) ->
            case parsed do
              {:ok, selector, rest} ->
                case parse_simple_selector(HtmlValidator.trim_css_whitespace(selector)) do
                  {:ok, %{pseudo_element: nil} = negated} ->
                    part =
                      if Map.get(negated, :language_dependent, false),
                        do: Map.put(part, :language_dependent, true),
                        else: part

                    parse_selector_modifiers(rest, %{
                      part
                      | negations: [negated | part.negations]
                    })

                  _ ->
                    {:error, :invalid_css}
                end

              :error ->
                {:error, :invalid_css}
            end

          true ->
            {:error, :invalid_css}
        end
    end
  end

  defp specificity(parts) do
    Enum.reduce(parts, {0, 0, 0}, fn part, {ids, classes, elements} ->
      id_count = if is_nil(part.id), do: 0, else: 1
      element_count = if is_nil(part.tag), do: 0, else: 1
      pseudo_element_count = if is_nil(part.pseudo_element), do: 0, else: 1
      class_count = length(part.classes) + length(part.attributes) + length(part.pseudo_classes)

      {shadow_ids, shadow_classes, shadow_elements} =
        case Map.get(part, :shadow_selector) do
          {:host, nil} ->
            {0, 1, 0}

          {:host, compound} ->
            {ids, classes, elements} = specificity([compound])
            {ids, classes + 1, elements}

          {:slotted, compound} ->
            {ids, classes, elements} = specificity([compound])
            {ids, classes, elements + 1}

          nil ->
            {0, 0, 0}
        end

      {negation_ids, negation_classes, negation_elements} = specificity(part.negations)

      {
        ids + id_count + negation_ids + shadow_ids,
        classes + class_count + negation_classes + shadow_classes,
        elements + element_count + pseudo_element_count + negation_elements + shadow_elements
      }
    end)
  end

  defp css_error_detail(original_css, parsed_css) do
    {kind, snippet} = first_css_issue(parsed_css)
    projection = source_projection(original_css)

    case if(snippet == "", do: :nomatch, else: :binary.match(projection.text, snippet)) do
      {offset, length} ->
        source = source_record(projection, offset, length)
        detail = css_issue_to_detail({kind, source.text}, original_css)

        %{
          detail
          | line: source.line,
            column: source.column,
            message: css_issue_message(kind, source.line, source.text)
        }

      :nomatch ->
        css_issue_to_detail({kind, snippet}, original_css)
    end
  end

  defp first_css_issue(css) do
    case rule_sources(css) do
      {:ok, rule_sources} ->
        Enum.find_value(rule_sources, {:stylesheet, String.trim(css)}, &rule_issue/1)

      {:error, source} ->
        {:stylesheet, String.trim(source)}
    end
  end

  defp rule_issue(%{selectors: selectors, declarations: declarations}) do
    cond do
      invalid_selector(selectors) ->
        {:selector, invalid_selector(selectors)}

      invalid_declaration(declarations) ->
        {:declaration, invalid_declaration(declarations)}

      true ->
        nil
    end
  end

  defp invalid_selector(selectors) do
    selectors
    |> split_selector_list()
    |> Enum.map(&HtmlValidator.trim_css_whitespace/1)
    |> Enum.find(fn selector ->
      selector == "" or match?({:error, :invalid_css}, parse_selector(selector))
    end)
  end

  defp invalid_declaration(declarations) do
    declarations
    |> declaration_sources()
    |> Enum.find(fn declaration ->
      match?({:error, :invalid_css}, HtmlValidator.validate_css_declaration(declaration))
    end)
    |> case do
      nil ->
        case String.trim(declarations) do
          "" -> declarations
          _ -> nil
        end

      declaration ->
        declaration
    end
  end

  @doc false
  @spec declaration_sources(String.t()) :: [String.t()]
  def declaration_sources(source) do
    source
    |> declaration_parts()
    |> Enum.map(&HtmlValidator.trim_css_whitespace/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp declaration_spans(source) do
    {spans, _cursor} =
      source
      |> declaration_parts()
      |> Enum.reduce({[], 0}, fn part, {spans, cursor} ->
        text = HtmlValidator.trim_css_whitespace(part)

        spans =
          if text == "",
            do: spans,
            else:
              then(:binary.match(part, text), fn {at, bytes} ->
                [%{text: text, offset: cursor + at, length: bytes} | spans]
              end)

        {spans, cursor + byte_size(part) + 1}
      end)

    Enum.reverse(spans)
  end

  defp declaration_parts(source) do
    {parts, current, _quote, _depth, _escaped?} =
      source
      |> HtmlValidator.css_syntax_units()
      |> Enum.reduce({[], [], nil, 0, false}, fn character,
                                                 {parts, current, quote, depth, escaped?} ->
        case quoted_character_state(character, quote, escaped?) do
          {:quoted, quote, escaped?} ->
            {parts, [character | current], quote, depth, escaped?}

          :syntax ->
            cond do
              character == "(" ->
                {parts, [character | current], nil, depth + 1, false}

              character == ")" ->
                {parts, [character | current], nil, max(depth - 1, 0), false}

              depth == 0 and character == ";" ->
                part = current |> Enum.reverse() |> Enum.join()
                {[part | parts], [], nil, depth, false}

              true ->
                {parts, [character | current], nil, depth, false}
            end
        end
      end)

    final = current |> Enum.reverse() |> Enum.join()

    [final | parts]
    |> Enum.reverse()
  end

  defp css_issue_to_detail({kind, source}, css) do
    source = HtmlValidator.trim_css_whitespace(source)
    {line, column} = source_location(css, source)

    %{
      stage: :css,
      reason: :invalid_css,
      message: css_issue_message(kind, line, source),
      line: line,
      column: column,
      source: source
    }
  end

  defp css_issue_message(kind, line, source) do
    case kind do
      :selector -> ~s(line #{line}: selector "#{source}" is invalid or unsupported)
      :declaration -> ~s(line #{line}: declaration "#{source}" is invalid or unsupported)
      :stylesheet -> ~s(line #{line}: CSS source "#{source}" is invalid)
    end
  end

  defp declaration_error_detail(css, declaration, offset \\ nil) do
    source = HtmlValidator.trim_css_whitespace(declaration)
    {line, column} = source_location(css, source, offset)

    %{
      stage: :css,
      reason: :invalid_css,
      message: ~s(line #{line}: declaration "#{source}" is invalid or unsupported),
      line: line,
      column: column,
      source: source
    }
  end

  defp page_rule_error_detail(css, page_rule) do
    source = String.trim(page_rule)
    {line, column} = source_location(css, source)

    %{
      stage: :css,
      reason: :invalid_css,
      message: ~s(line #{line}: page rule "#{source}" is invalid or unsupported),
      line: line,
      column: column,
      source: source
    }
  end

  defp source_location(source, snippet, offset \\ nil) do
    case snippet == "" do
      true ->
        {1, 1}

      false ->
        case if(is_nil(offset),
               do: :binary.match(source, snippet),
               else: {offset, byte_size(snippet)}
             ) do
          {index, _length} ->
            prefix = binary_part(source, 0, index)
            lines = String.split(prefix, "\n", trim: false)
            line = length(lines)
            column = String.length(List.last(lines) || "") + 1
            {line, column}

          :nomatch ->
            {1, 1}
        end
    end
  end
end
