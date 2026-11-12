defmodule NativeElixirPdfUtilities.HtmlToPdf.CssParser do
  @moduledoc """
  CSS parsing for the native HTML-to-PDF renderer.

  Balanced component values preserve authored byte spans while invalid selector
  rules and malformed declarations recover at CSS syntax boundaries. Unknown
  at-rules and imports are ignored. Supported declarations still pass the shared
  value and resource validators.

  Selectors include namespaces, attributes, tree and sibling relationships,
  logical and relational functions, filtered positions and initial document
  states. Static pseudo-element ownership can be parsed before its layout and
  painting implementation is available; rendering reports that gate explicitly.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlCssSourceValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.PrintMediaValidator
  alias NativeElixirPdfUtilities.Validators.CssReferenceValidator
  alias NativeElixirPdfUtilities.Validators.CssPageValidator
  alias NativeElixirPdfUtilities.Validators.CssFontFaceValidator
  alias NativeElixirPdfUtilities.Validators.CssSyntaxValidator
  alias NativeElixirPdfUtilities.Validators.CssStylesheetValidator
  alias NativeElixirPdfUtilities.Validators.CssPropertyRegistrationValidator

  @type declaration :: {String.t(), String.t()} | {String.t(), String.t(), :important}
  @type selector_part :: %{
          required(:tag) => String.t() | nil,
          required(:id) => String.t() | nil,
          required(:classes) => [String.t()],
          required(:attributes) => [tuple()],
          required(:pseudo_classes) => [atom() | tuple()],
          required(:negations) => [selector_part()],
          required(:pseudo_element) => atom() | tuple() | nil,
          required(:combinator) => nil | :descendant | :child | :adjacent | :sibling,
          optional(:namespace) => String.t() | nil,
          optional(:extra_ids) => [String.t()],
          optional(:source_tag) => String.t(),
          optional(:host_eligible) => true,
          optional(:explicit_type) => true,
          optional(:shadow_selector) => tuple(),
          optional(:language_dependent) => true
        }
  @type selector :: %{
          required(:parts) => [selector_part()],
          required(:specificity) => {non_neg_integer(), non_neg_integer(), non_neg_integer()},
          optional(:language_dependent) => true,
          optional(:tree_dependent) => true
        }
  @type attribute_operator :: :equals | :includes | :dash_match | :prefix | :suffix | :substring
  @type rule :: %{
          selectors: [selector()],
          declarations: [declaration()],
          order: non_neg_integer()
        }
  @type stylesheet :: [rule()]
  @type font_face :: %{
          required(:family) => String.t(),
          required(:sources) => [String.t()],
          required(:weight) => number(),
          required(:style) => :normal | :italic,
          optional(:weight_range) => {number(), number()},
          optional(:width) => number(),
          optional(:width_range) => {number(), number()},
          optional(:oblique) => {number(), number()},
          optional(:unicode_range) => [{non_neg_integer(), non_neg_integer()}]
        }
  @type page_option ::
          {:page_size, PageGeometry.page_size_input()}
          | {:margin, PageGeometry.margin_input()}

  @css_opaque_pattern HtmlValidator.css_opaque_pattern()
  @page_rule_regex Regex.compile!(
                     "#{@css_opaque_pattern}(*SKIP)(*F)|@page\\s*\\{(?<declarations>(?:#{@css_opaque_pattern}|[^{}])*+)\\}",
                     "ui"
                   )
  @page_rule_candidate_regex Regex.compile!(
                               "#{@css_opaque_pattern}(*SKIP)(*F)|@page[^{;]*(?=\\{|;|$)",
                               "ui"
                             )
  @font_face_rule_regex Regex.compile!(
                          "#{@css_opaque_pattern}(*SKIP)(*F)|@font-face\\s*\\{(?<declarations>(?:#{@css_opaque_pattern}|[^{}])*+)\\}",
                          "ui"
                        )
  @css_block_regex Regex.compile!(
                     "#{@css_opaque_pattern}(*SKIP)(*F)|\\{(?:#{@css_opaque_pattern}|[^{}])*+\\}",
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
            :max_input_bytes,
            byte_size(css),
            :css
          )

          RenderCache.fetch_stylesheet(
            {:parsed, css, PrintMediaValidator.cache_context(css, media_context)},
            fn ->
              {projected, namespaces} =
                css |> strip_comments() |> CssSyntaxValidator.stylesheet_projection()

              with {:ok, active_css} <-
                     PrintMediaValidator.select_rules(projected, media_context),
                   {:ok, font_faces} <- parse_font_faces(active_css, css),
                   {:ok, page_options} <- parse_page_rules(active_css, css) do
                parsed_css = active_css |> strip_font_face_rules() |> strip_page_rules()

                {:ok, rules, cascade} = parse_rules(parsed_css, namespaces)

                with {:ok, registrations} <-
                       CssPropertyRegistrationValidator.prepare(
                         Enum.filter(cascade.registrations, &Map.get(&1, :condition_active, true))
                       ) do
                  {:ok,
                   %{
                     rules: rules,
                     font_faces: font_faces,
                     page_options: page_options,
                     cascade: %{
                       layers: Enum.filter(cascade.layers, &Map.get(&1, :condition_active, true)),
                       registrations: registrations,
                       font_definitions:
                         NativeElixirPdfUtilities.Validators.CssFontRuleValidator.prepare(
                           Enum.filter(cascade.font_rules, &Map.get(&1, :condition_active, true))
                         )
                     }
                   }}
                end
              end
            end,
            fn result ->
              case result do
                {:ok, prepared} ->
                  HtmlValidator.reserve_render_resource(
                    :max_entries,
                    length(prepared.rules),
                    :css
                  )

                {:error, _} ->
                  :ok
              end
            end
          )

        {:error, {_reason, _diagnostic}} = error ->
          error
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
          :max_input_bytes,
          byte_size(css),
          :css
        )

        HtmlValidator.reserve_render_resource(:max_work, byte_size(css), :css)
        projection = source_projection(css) |> Map.put(:descriptor, descriptor)

        with {:ok, projected, media} <- source_media_projection(projection, media_context),
             {:ok, fonts} <- source_font_records(projected, projection, media),
             {:ok, pages} <- source_page_records(projected, projection, media),
             rules_css = mask_source_blocks(projected, @font_face_rule_regex),
             rules_css = mask_source_blocks(rules_css, @page_rule_regex),
             {:ok, cascade} <- CssStylesheetValidator.prepare(rules_css, projection.namespaces),
             {:ok, rules} <- source_rules(cascade.rules, rules_css, projection, media),
             {:ok, registrations} <-
               CssPropertyRegistrationValidator.prepare(
                 Enum.filter(
                   cascade.registrations,
                   &(source_active?(&1.offset, media) and Map.get(&1, :condition_active, true))
                 )
               ) do
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
             cascade: %{
               layers:
                 Enum.filter(
                   cascade.layers,
                   &(source_active?(&1.offset, media) and Map.get(&1, :condition_active, true))
                 ),
               registrations: registrations,
               font_definitions:
                 NativeElixirPdfUtilities.Validators.CssFontRuleValidator.prepare(
                   Enum.filter(
                     cascade.font_rules,
                     &(source_active?(&1.offset, media) and Map.get(&1, :condition_active, true))
                   )
                 )
             },
             authored_rules: rules,
             authored_fonts: fonts,
             authored_pages: pages,
             authored_media: media,
             media_provider: PrintMediaValidator,
             media_policy: :unresolved_provider_context
           }}
        end
      end
    end)
  end

  defp source_projection(css) do
    {text, gaps} = HtmlValidator.css_comment_projection(css)
    {text, namespaces} = CssSyntaxValidator.stylesheet_projection(text)
    %{original: css, text: text, gaps: gaps, namespaces: namespaces}
  end

  defp source_record(projection, offset, length) do
    HtmlValidator.reserve_render_resource(
      :max_work,
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
           {:ok, cascade} <- CssStylesheetValidator.prepare(rules_css, projection.namespaces) do
        {records, _cursor} =
          Enum.map_reduce(cascade.rules, 0, fn rule, _cursor ->
            offset = rule.offset
            length = byte_size(rule.source)
            base = Map.get(rule, :body_offset, offset + byte_size(rule.selectors) + 1)

            records =
              if source_active?(base, media) and Map.get(rule, :condition_active, true) do
                Enum.map(declaration_spans(rule.declarations), fn span ->
                  %{source: source_record(projection, base + span.offset, span.length)}
                end)
              else
                []
              end

            {records, offset + length}
          end)

        {:ok, List.flatten(records)}
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
        case parse_declarations_input(css) do
          {:ok, declarations} ->
            {:ok, source_declarations(css, declarations, 0, projection)}

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
          |> Map.put(
            :provider,
            if(Map.get(record, :kind) == :supports,
              do: NativeElixirPdfUtilities.Validators.CssSupportsValidator,
              else: PrintMediaValidator
            )
          )
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

  defp source_rules(sources, _rules_css, projection, media) do
    rules =
      sources
      |> Enum.with_index(fn source, order ->
        {:ok, rule} = parse_rule(source, order, projection.namespaces)
        base = source.body_offset
        records = source_declarations(source.declarations, rule.declarations, base, projection)

        rule
        |> Map.put(:source_declarations, records)
        |> Map.put(
          :source,
          source_record(projection, source.selector_offset, source.selector_length)
        )
        |> Map.put(
          :active,
          source_active?(base, media) and Map.get(source, :condition_active, true)
        )
      end)

    {:ok, rules}
  end

  defp source_font_records(projected, projection, media) do
    @font_face_rule_regex
    |> Regex.scan(projected, return: :index)
    |> Enum.reduce_while({:ok, []}, fn [{offset, length}, {base, bytes}], {:ok, fonts} ->
      block = binary_part(projected, base, bytes)

      {:ok, declarations} = parse_declarations_input(block)
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

      {:ok, declarations} = parse_declarations_input(block)
      records = source_declarations(block, declarations, base, projection)

      result =
        Enum.reduce_while(records, {:ok, [], []}, fn record, {:ok, options, checked} ->
          HtmlValidator.reserve_render_resource(:max_work, 1, :css)

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
            :max_input_bytes,
            byte_size(css),
            :css
          )

          case css |> strip_comments() |> PrintMediaValidator.select_rules(nil) do
            {:ok, active_css} ->
              case parse_font_faces(active_css, active_css) do
                {:ok, font_faces} -> {:ok, font_faces}
                {:error, {reason, _detail}} -> {:error, reason}
              end
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
            :max_input_bytes,
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
  def parse_declarations_detailed(css), do: parse_declarations_input(css)

  defp parse_declarations_input(css) do
    HtmlValidator.with_render_budget(fn ->
      case HtmlValidator.validate_css_source(css, :declarations) do
        {:ok, css} ->
          HtmlValidator.reserve_render_resource(
            :max_input_bytes,
            byte_size(css),
            :css
          )

          {:ok, Enum.map(CssSyntaxValidator.declarations(css), & &1.declaration)}

        {:error, {_reason, _diagnostic}} = error ->
          error
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
    {:ok, declarations} = parse_declarations_detailed(block)

    case CssFontFaceValidator.validate(declarations) do
      {:ok, face} ->
        {:ok, face}

      {:error, %{property: property, index: index}} ->
        {:error, font_face_descriptor_error(block, property, index)}
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

    Diagnostics.diagnostic(:css, :invalid_css, "line #{line}: #{message}",
      line: line,
      column: column,
      source: source
    )
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
    |> CssSyntaxValidator.declarations()
    |> Enum.reduce_while({:ok, []}, fn record, {:ok, acc} ->
      put_page_option(acc, elem(record.declaration, 0), elem(record.declaration, 1), record.text)
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

  defp parse_rules(css, namespaces) do
    {:ok, cascade} = CssStylesheetValidator.prepare(css, namespaces)
    {:ok, rules} = sources_to_rules(cascade.rules, namespaces)
    {:ok, rules, cascade}
  end

  defp sources_to_rules(sources, namespaces) do
    rules =
      sources
      |> Enum.filter(&Map.get(&1, :condition_active, true))
      |> Enum.with_index(fn source, order ->
        {:ok, rule} = parse_rule(source, order, namespaces)
        rule
      end)

    {:ok, rules}
  end

  defp parse_rule(
         %{declarations: declaration_source, parsed_selectors: selectors} = source,
         order,
         _namespaces
       ) do
    HtmlValidator.reserve_render_resource(
      :max_input_bytes,
      byte_size(declaration_source),
      :css
    )

    declarations = Enum.map(CssSyntaxValidator.declarations(declaration_source), & &1.declaration)

    metadata =
      source |> Map.take([:layer, :scopes]) |> Map.reject(fn {_key, value} -> value == [] end)

    {:ok, Map.merge(%{selectors: selectors, declarations: declarations, order: order}, metadata)}
  end

  @doc false
  @spec declaration_sources(String.t()) :: [String.t()]
  def declaration_sources(source),
    do: Enum.map(CssSyntaxValidator.declarations(source), & &1.text)

  defp declaration_spans(source), do: CssSyntaxValidator.declarations(source)

  defp declaration_error_detail(css, declaration) do
    source = HtmlValidator.trim_css_whitespace(declaration)
    {line, column} = source_location(css, source)

    Diagnostics.diagnostic(
      :css,
      :invalid_css,
      ~s(line #{line}: declaration "#{source}" is invalid or unsupported),
      line: line,
      column: column,
      source: source
    )
  end

  defp page_rule_error_detail(css, page_rule) do
    source = String.trim(page_rule)
    {line, column} = source_location(css, source)

    Diagnostics.diagnostic(
      :css,
      :invalid_css,
      ~s(line #{line}: page rule "#{source}" is invalid or unsupported),
      line: line,
      column: column,
      source: source
    )
  end

  defp source_location(css, snippet) do
    case :binary.match(css, snippet) do
      {offset, _length} ->
        lines = css |> binary_part(0, offset) |> String.split("\n", trim: false)
        {length(lines), String.length(List.last(lines) || "") + 1}

      :nomatch ->
        projection = source_projection(css)
        {offset, length} = :binary.match(projection.text, snippet)
        source = source_record(projection, offset, length)
        {source.line, source.column}
    end
  end
end
