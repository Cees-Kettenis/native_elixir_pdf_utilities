defmodule NativeElixirPdfUtilities.Validators.TextShapingValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator
  alias NativeElixirPdfUtilities.Validators.FontValidator

  @ignorable Regex.compile!(
               "\\A(?:" <>
                 NativeElixirPdfUtilities.HtmlToPdf.UnicodeCharacterProperty.expression(
                   "Default_Ignorable_Code_Point",
                   false
                 ) <> ")\\z",
               "u"
             )

  @doc "Identifies default-ignorable scalars shared by coverage, shaping and painting."
  @spec ignorable?(non_neg_integer()) :: boolean()
  def ignorable?(codepoint), do: Regex.match?(@ignorable, <<codepoint::utf8>>)

  @doc "Selects general shaping for Unicode runs and active advanced font controls."
  @spec required?(map(), binary()) :: boolean()
  def required?(font, text) do
    case font do
      %{type: :embedded, source: source}
      when source in [:configured, :bundled, :system] and
             not is_map_key(font, :feature_plan) and not is_map_key(font, :_vertical_text) and
             not is_map_key(font, :emoji_presentation) and
             (not is_map_key(font, :shaping_language) or is_binary(font.shaping_language)) ->
        unicode_text?(text) or (text != "" and ascii_layout_required?(font, text, %{}))

      %{type: :embedded} ->
        plan = :maps.get(:feature_plan, font, %{})
        features = :maps.get(:features, plan, %{})

        not is_binary(:maps.get(:shaping_language, font, "und")) or
          Map.has_key?(font, :_vertical_text) or
          ((:maps.get(:shaping_language, font, "und") not in ["", "und"] or
              :maps.get(:language, plan, :normal) != :normal) and
             :maps.get(:source, font, nil) not in [:configured, :bundled, :system]) or
          :maps.get(:emoji_presentation, font, nil) in [:text, :emoji] or
          :maps.get(:emoji, plan, :normal) in [:text, :emoji, :unicode] or
          Enum.any?(features, fn {tag, value} -> value != 0 and tag not in ["kern", "liga"] end) or
          (Map.get(features, "kern", 0) != 0 and :maps.get(:kerning, font, %{}) == %{}) or
          unicode_text?(text) or
          (text != "" and :maps.get(:source, font, nil) in [:configured, :bundled, :system] and
             ascii_layout_required?(font, text, features))

      _ ->
        false
    end
  end

  defp ascii_layout_required?(font, text, features) do
    result =
      FontValidator.with_budget(fn ->
        {previous, triggers} =
          FontValidator.memo(
            {:ascii_layout_triggers, font.id, :maps.get(:normalized_coordinates, font, nil),
             :maps.get(:shaping_language, font, "und"),
             Map.get(:maps.get(:feature_plan, font, %{}), :language, :normal), features},
            fn ->
              triggers =
                case NativeElixirPdfUtilities.HtmlToPdf.FontCache.ascii_layout(font) do
                  {:ok, %{dynamic?: false, compatible?: true} = info} ->
                    defaults = NativeElixirPdfUtilities.HtmlToPdf.TextShaper.default_features()

                    triggers =
                      Map.new(info.flags, fn {script, flags} ->
                        FontValidator.reserve(:max_work, map_size(flags))

                        selected =
                          Enum.reduce(flags, MapSet.new(), fn {tag, glyphs}, selected ->
                            active? =
                              case tag do
                                {:required, _tag} -> true
                                tag -> Map.get(features, tag, Map.get(defaults, tag, 0)) != 0
                              end

                            if active?, do: MapSet.union(selected, glyphs), else: selected
                          end)

                        {script, selected}
                      end)

                    FontValidator.reserve(
                      :max_entries,
                      1 +
                        Enum.sum(Enum.map(triggers, fn {_, glyphs} -> MapSet.size(glyphs) end))
                    )

                    Map.put(
                      triggers,
                      :empty?,
                      Enum.all?(triggers, fn {_, glyphs} -> MapSet.size(glyphs) == 0 end)
                    )

                  {:ok, _info} ->
                    :general

                  {:error, _} = error ->
                    throw({:font_resource_limit, error})
                end

              {font, triggers}
            end
          )

        # A cached feature projection is valid only for the same physical
        # model. Callers can edit a face while retaining its previous ID.
        cond do
          triggers == :general or
              (previous != font and
                 Map.take(previous, [:data, :cmap, :kerning, :latin_ligatures]) !=
                   Map.take(font, [:data, :cmap, :kerning, :latin_ligatures])) ->
            true

          triggers.empty? ->
            false

          true ->
            script = if Regex.match?(~r/[A-Za-z]/, text), do: "Latn", else: "Zyyy"
            selected = Map.fetch!(triggers, script)
            MapSet.size(selected) != 0 and ascii_trigger?(text, font.cmap, selected)
        end
      end)

    case result do
      {:error, _} = error -> throw({:font_resource_limit, error})
      required? -> required?
    end
  end

  defp ascii_trigger?(text, cmap, selected) do
    case text do
      <<byte, rest::binary>> ->
        MapSet.member?(selected, Map.get(cmap, byte)) or ascii_trigger?(rest, cmap, selected)

      "" ->
        false
    end
  end

  @doc "Identifies admitted inline boundaries whose glyph selection or positioning needs a shared shaping context."
  @spec shared_context?(String.t(), String.t(), map()) ::
          {:ok, boolean()} | {:error, {atom(), map()}}
  def shared_context?(before, after_text, style) do
    font = Map.fetch!(style, :font_face)
    spacing = :maps.get(:letter_spacing, style, 0.0)
    size = :maps.get(:font_size, style, 12.0)

    with {:ok, before} <- HtmlLineBreakValidator.display_text(before, font, false, style),
         {:ok, after_text} <- HtmlLineBreakValidator.display_text(after_text, font, false, style) do
      combined = before <> after_text

      cond do
        before == "" or after_text == "" or size == 0 ->
          {:ok, false}

        required?(font, combined) ->
          with {:ok, together} <- Font.css_glyph_positions(combined, font, size, spacing),
               {:ok, left} <- Font.css_glyph_positions(before, font, size, spacing),
               {:ok, right} <- Font.css_glyph_positions(after_text, font, size, spacing) do
            projection = fn glyphs ->
              Enum.map(
                glyphs,
                &Map.take(&1, [:glyph, :x_advance, :y_advance, :x_offset, :y_offset])
              )
            end

            {:ok, projection.(together.glyphs) != projection.(left.glyphs ++ right.glyphs)}
          end

        true ->
          ligatures? =
            Font.shape_ligatures(combined, font, spacing) !=
              Font.shape_ligatures(before, font, spacing) <>
                Font.shape_ligatures(after_text, font, spacing)

          cmap = :maps.get(:cmap, font, %{})
          first = after_text |> String.to_charlist() |> hd()
          last = before |> String.to_charlist() |> List.last()
          pair = {Map.get(cmap, last), Map.get(cmap, first)}
          {:ok, ligatures? or Map.get(:maps.get(:kerning, font, %{}), pair, 0) != 0}
      end
    end
  end

  defp unicode_text?(text) do
    case text do
      <<bytes::64, remaining::binary>> when :erlang.band(bytes, 0x8080808080808080) == 0 ->
        unicode_text?(remaining)

      <<byte, remaining::binary>> when byte < 128 ->
        unicode_text?(remaining)

      "" ->
        false

      _ ->
        true
    end
  end

  alias NativeElixirPdfUtilities.Validators.{
    CssFontControlsValidator,
    OpenTypeLayoutValidator,
    TextItemizationValidator
  }

  @doc "Admits shaping options, source clusters and the selected font's layout data."
  @spec prepare(term(), term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(text, font, options) do
    FontValidator.with_budget(fn ->
      case {font, options} do
        {%{type: :embedded, cmap: cmap, widths: widths, units_per_em: units, data: data}, options}
        when is_map(cmap) and is_list(widths) and is_integer(units) and units > 0 and
               is_binary(data) and is_list(options) ->
          if Keyword.keyword?(options) and
               Enum.all?(
                 Keyword.keys(options),
                 &(&1 in [:language, :script, :direction, :features, :ppem, :characters])
               ) do
            plan = :maps.get(:feature_plan, font, %{})
            language = Keyword.get(options, :language, :maps.get(:shaping_language, font, "und"))
            script = Keyword.get(options, :script)
            direction = Keyword.get(options, :direction, :auto)
            features = Keyword.get(options, :features, :maps.get(:features, plan, %{}))
            ppem = Keyword.get(options, :ppem)

            admitted_features =
              CssFontControlsValidator.prepare_defaults(%{feature_settings: features})

            cond do
              script != nil and
                  not (is_binary(script) and byte_size(script) == 4 and String.valid?(script)) ->
                failure("script must be a four-byte ISO 15924 or OpenType tag")

              direction not in [:auto, :ltr, :rtl, :ttb] ->
                failure("direction must be :auto, :ltr, :rtl or :ttb")

              ppem != nil and not (is_number(ppem) and ppem > 0) ->
                failure("ppem must be a positive pixel size when specified")

              not is_map(features) ->
                failure(
                  "features must map four-byte ASCII tags to nonnegative integer selections"
                )

              admitted_features == :error ->
                failure(
                  "features must map four-byte ASCII tags to nonnegative integer selections"
                )

              true ->
                {:ok, defaults} = admitted_features

                features =
                  Map.new(:maps.get(:feature_defaults, defaults, %{}), fn {tag, value} ->
                    {tag, trunc(value)}
                  end)

                itemization =
                  case Keyword.fetch(options, :characters) do
                    {:ok, characters} ->
                      TextItemizationValidator.prepare_projection(text, characters, language)

                    :error ->
                      TextItemizationValidator.prepare(text, language)
                  end

                with :ok <- FontValidator.validate_embedded_face(font),
                     {:ok, itemization} <- itemization,
                     {:ok, layout} <- OpenTypeLayoutValidator.for_font(font) do
                  FontValidator.reserve(:max_work, length(itemization.clusters))

                  Enum.each(itemization.clusters, fn cluster ->
                    Enum.each(cluster.scalars, fn scalar ->
                      glyph = Map.get(cmap, scalar.codepoint, 0)

                      if not (is_integer(glyph) and glyph >= 0 and glyph < layout.glyph_count),
                        do:
                          throw(
                            {:invalid_shaping,
                             Diagnostics.error(
                               :font,
                               :invalid_font,
                               "nominal glyph is outside the selected font's maxp count",
                               operation: :shape_text,
                               module: __MODULE__
                             )}
                          )
                    end)
                  end)

                  {:ok,
                   %{
                     itemization: itemization,
                     layout: layout,
                     script: script,
                     direction: direction,
                     features: features,
                     ppem: ppem,
                     language_override: :maps.get(:language, plan, :normal)
                   }}
                else
                  :error ->
                    Diagnostics.error(
                      :font,
                      :invalid_font,
                      "text shaping requires valid selected font metrics",
                      operation: :shape_text,
                      module: __MODULE__
                    )

                  error ->
                    error
                end
            end
          else
            failure("shaping options must be a keyword list of supported options")
          end

        _ ->
          failure("text shaping requires a selected embedded font and keyword options")
      end
    end)
  catch
    {:invalid_shaping, error} -> error
  end

  @doc "Charges nested lookup depth against the caller's configured allowance."
  @spec reserve_depth(non_neg_integer()) :: :ok
  def reserve_depth(depth) do
    FontValidator.check(:max_depth, depth)
  end

  @doc "Charges a glyph allocation before constructing substitution output."
  @spec reserve_glyphs(non_neg_integer()) :: :ok
  def reserve_glyphs(count), do: FontValidator.reserve(:max_entries, count)

  @doc "Bounds attachment depth and rejects cycles before following a glyph's parent chain."
  @spec validate_attachment(non_neg_integer(), %{optional(non_neg_integer()) => true}) :: :ok
  def validate_attachment(parent, path) do
    FontValidator.check(:max_depth, map_size(path))

    if Map.has_key?(path, parent),
      do:
        throw(
          {:invalid_shaping,
           Diagnostics.error(
             :font,
             :invalid_font,
             "font positioning creates a cyclic attachment chain",
             operation: :shape_text,
             module: __MODULE__,
             source: "GPOS attachment"
           )}
        )

    :ok
  end

  @doc "Rejects uncovered visible shaping output while allowing default-ignorable controls."
  @spec validate_visible_glyphs([map()]) :: :ok | {:error, {atom(), map()}}
  def validate_visible_glyphs(glyphs) do
    if Enum.any?(glyphs, &(&1.glyph == 0 and not &1.ignorable?)),
      do:
        Diagnostics.error(
          :font,
          :unsupported_glyph,
          "selected font produced an uncovered shaping cluster",
          operation: :shape_text,
          module: __MODULE__
        ),
      else: :ok
  end

  @doc "Charges generated glyph positions before the writer serializes them."
  @spec validate_paint_positions([map()]) :: :ok
  def validate_paint_positions(boxes) do
    Enum.each(boxes, fn box ->
      Enum.each([box.x, box.y, box.width], fn value ->
        NativeElixirPdfUtilities.Validators.HtmlValidator.check_render_resource(
          :max_numeric_magnitude,
          abs(value),
          :writer
        )
      end)
    end)
  end

  defp failure(message),
    do:
      Diagnostics.error(:font, :invalid_text, message, operation: :shape_text, module: __MODULE__)
end
