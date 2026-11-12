defmodule NativeElixirPdfUtilities.Validators.FontRenderingValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    CssFontControlsValidator,
    EmojiPresentationValidator,
    FontValidator
  }

  @doc "Prepares selected variation coordinates, feature precedence and permitted synthetic weight/style for one physical face."
  @spec prepare(map(), map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(font, style) do
    with {:ok, prepared} <- prepare_face(Map.get(font, :language_source, font), style),
         {:ok, parts} <-
           NativeElixirPdfUtilities.Validators.LanguageValidator.prepare_identifier(
             Map.get(style, :_text_language, "und")
           ) do
      language = Enum.join(parts, "-")

      {:ok, prepared} =
        if prepared.type == :embedded and language not in ["", "und"] do
          suffix =
            :crypto.hash(:sha256, language) |> binary_part(0, 6) |> Base.encode16(case: :lower)

          {:ok,
           prepared
           |> Map.put(:language_source, prepared)
           |> Map.put(:shaping_language, language)
           |> Map.update!(:id, &(&1 <> "-lang-" <> suffix))}
        else
          {:ok, prepared}
        end

      mode = Map.get(style, :writing_mode, :horizontal_tb)

      prepared =
        if mode != :horizontal_tb and prepared.type == :embedded,
          do:
            Map.put(prepared, :_vertical_text, %{
              mode: mode,
              orientation: Map.get(style, :text_orientation, :mixed),
              combine: Map.get(style, :text_combine_upright, :none)
            }),
          else: Map.delete(prepared, :_vertical_text)

      {:ok, prepared}
    end
  end

  defp prepare_face(font, style) do
    ordinary? =
      Map.get(style, :font_controls, %{}) == %{} and not Map.has_key?(font, :rendering_source) and
        not Map.has_key?(font, :variation_source) and
        not Map.has_key?(font, :color_tables) and
        not Map.has_key?(font, :metric_descriptors) and not Map.has_key?(font, :feature_defaults) and
        not Map.has_key?(font, :language_default) and
        (Map.get(style, :font_weight, 400) < 600 or Map.get(font, :weight, 400) >= 600) and
        (Map.get(style, :font_style, :normal) == :normal or
           Map.get(font, :style, :normal) == :italic)

    if Map.get(font, :type) == :built_in or ordinary? do
      {:ok, font}
    else
      request =
        {Map.get(style, :font_controls, %{}), Map.get(style, :font_weight, 400),
         Map.get(style, :font_style, :normal), Map.get(style, :font_width, 100),
         Map.get(style, :font_oblique), Map.get(style, :font_size, 12),
         Map.get(style, :_font_definitions), Map.get(style, :_font_family_scope)}

      if Map.get(font, :rendering_request) == request do
        {:ok, font}
      else
        with {:ok, prepared} <- prepare_rendering(Map.get(font, :rendering_source, font), style),
             {:ok, prepared} <-
               NativeElixirPdfUtilities.Validators.FontMetricValidator.prepare(prepared, style),
             {:ok, prepared} <-
               NativeElixirPdfUtilities.Validators.BitmapFontValidator.prepare_metrics(
                 prepared,
                 NativeElixirPdfUtilities.HtmlToPdf.Font.used_size(
                   prepared,
                   Map.get(style, :font_size, 12)
                 ) / 0.75
               ) do
          {:ok,
           if(prepared == font,
             do: font,
             else:
               prepared
               |> Map.put(:rendering_request, request)
               |> Map.put(:rendering_source, Map.get(font, :rendering_source, font))
           )}
        end
      end
    end
  end

  @doc "Prepares fallback candidates with one shared check for ordinary static requests."
  @spec prepare_candidates([map()], map()) :: {:ok, [map()]} | {:error, {atom(), map()}}
  def prepare_candidates(fonts, style) do
    if Map.get(style, :writing_mode, :horizontal_tb) == :horizontal_tb and
         Map.get(style, :_text_language, "und") in ["", "und"] and
         Map.get(style, :font_controls, %{}) == %{} and
         Map.get(style, :font_style, :normal) == :normal and
         Map.get(style, :font_weight, 400) < 600 and
         not Enum.any?(
           fonts,
           &(Map.has_key?(&1, :variation_source) or Map.has_key?(&1, :metric_descriptors) or
               Map.has_key?(&1, :color_tables) or Map.has_key?(&1, :rendering_source) or
               Map.has_key?(&1, :feature_defaults) or Map.has_key?(&1, :language_default) or
               Map.has_key?(&1, :language_source))
         ) do
      {:ok, fonts}
    else
      Enum.reduce_while(fonts, {:ok, []}, fn font, {:ok, fonts} ->
        case prepare(font, style) do
          {:ok, font} -> {:cont, {:ok, [font | fonts]}}
          error -> {:halt, error}
        end
      end)
      |> case do
        {:ok, fonts} -> {:ok, Enum.reverse(fonts)}
        error -> error
      end
    end
  end

  defp prepare_rendering(font, style) do
    FontValidator.with_budget(fn ->
      source_controls = Map.get(style, :font_controls, %{})
      controls = CssFontControlsValidator.effective(source_controls)

      controls =
        if Map.get(source_controls, :"language-override") in [nil, :normal],
          do: Map.put(controls, :"language-override", Map.get(font, :language_default, :normal)),
          else: controls

      with {:ok, optical_font} <-
             NativeElixirPdfUtilities.Validators.FontMetricValidator.prepare(font, style) do
        optical_size =
          NativeElixirPdfUtilities.HtmlToPdf.Font.used_size(
            optical_font,
            Map.get(style, :font_size, 12)
          ) / 0.75

        result =
          if Map.has_key?(font, :variation_source) do
            settings =
              Enum.reduce(font.variation_axes, %{}, fn axis, settings ->
                value =
                  case axis.tag do
                    "wght" ->
                      Map.get(style, :font_weight, 400)

                    "wdth" ->
                      Map.get(style, :font_width, 100)

                    "ital" ->
                      if Map.get(style, :font_style, :normal) == :italic and
                           Map.get(style, :font_oblique) == nil, do: 1, else: 0

                    "slnt" ->
                      if Map.get(style, :font_style, :normal) == :italic and
                           (Map.get(style, :font_oblique) != nil or
                              not Enum.any?(
                                font.variation_axes,
                                &(&1.tag == "ital" and &1.maximum > 0)
                              )),
                         do: -(Map.get(style, :font_oblique) || 14),
                         else: 0

                    "opsz" ->
                      if controls[:"optical-sizing"] == :auto,
                        do: optical_size,
                        else: axis.default

                    _ ->
                      axis.default
                  end

                Map.put(settings, axis.tag, value)
              end)

            settings = Map.merge(settings, Map.get(font, :variation_defaults, %{}))

            settings =
              if controls[:"optical-sizing"] == :auto and
                   Enum.any?(font.variation_axes, &(&1.tag == "opsz")),
                 do: Map.put(settings, "opsz", optical_size),
                 else: settings

            settings = Map.merge(settings, controls[:"variation-settings"])

            selected =
              Map.new(font.variation_axes, fn axis ->
                {axis.tag, min(axis.maximum, max(axis.minimum, Map.fetch!(settings, axis.tag)))}
              end)

            if selected == font.variation_coordinates do
              {:ok, font}
            else
              FontValidator.memo({:selected_instance, font.id, selected}, fn ->
                FontValidator.prepare_font_instance(font, selected, nil)
              end)
            end
          else
            {:ok, font}
          end

        with {:ok, font} <- result do
          definitions =
            style
            |> Map.get(:_font_definitions, %{})
            |> Map.get(Map.get(style, :_font_family_scope), %{})

          named =
            NativeElixirPdfUtilities.Validators.CssFontRuleValidator.features(
              definitions,
              font.family,
              controls[:"variant-alternates"]
            )

          plan_controls =
            if controls[:"variant-caps"] in [["petite-caps"], ["all-petite-caps"]] do
              available =
                case FontValidator.memo({:feature_capabilities, font.data}, fn ->
                       capabilities(font.data)
                     end) do
                  {:ok, available} -> available
                  error -> throw({:font_rendering_error, error})
                end

              if MapSet.member?(available, "pcap") do
                controls
              else
                fallback =
                  if controls[:"variant-caps"] == ["petite-caps"],
                    do: ["small-caps"],
                    else: ["all-small-caps"]

                Map.put(controls, :"variant-caps", fallback)
              end
            else
              controls
            end

          plan =
            CssFontControlsValidator.feature_plan(
              plan_controls,
              Map.merge(Map.get(font, :feature_defaults, %{}), named)
            )

          palette =
            NativeElixirPdfUtilities.Validators.CssFontRuleValidator.palette(
              definitions,
              font.family,
              plan.palette
            )

          plan = Map.put(plan, :palette, palette)

          desired_weight = Map.get(style, :font_weight, 400)

          actual_weight =
            Map.get(
              Map.get(font, :variation_coordinates, %{}),
              "wght",
              Map.get(font, :weight, 400)
            )

          synthetic_weight =
            controls[:"synthesis-weight"] != :none and desired_weight >= 600 and
              actual_weight < 600

          style_axis? =
            Enum.any?(Map.get(font, :variation_axes, []), fn axis ->
              (axis.tag == "ital" and axis.maximum > 0 and Map.get(style, :font_oblique) == nil) or
                (axis.tag == "slnt" and (axis.minimum != 0 or axis.maximum != 0))
            end)

          synthetic_style =
            (controls[:"synthesis-style"] == :auto or
               (controls[:"synthesis-style"] == :oblique_only and
                  Map.get(style, :font_oblique) != nil)) and
              Map.get(style, :font_style, :normal) == :italic and
              Map.get(font, :style, :normal) == :normal and
              not style_axis?

          angle = if synthetic_style, do: Map.get(style, :font_oblique) || 14, else: 0
          # A 90-degree CSS oblique angle has an infinite tangent. Clamp only
          # the synthetic drawing slant to the conventional finite 89 degrees.
          angle = min(89, max(-89, angle))
          shear = if angle == 0, do: 0, else: :math.tan(angle * :math.pi() / 180)
          synthesis = %{weight: synthetic_weight, shear: shear}

          active? =
            plan.features != %{} or plan.language != :normal or plan.alternates != :normal or
              plan.emoji != :normal or plan.palette != :normal or synthetic_weight or
              synthetic_style

          if active? do
            variants = %{
              caps: controls[:"variant-caps"],
              position: controls[:"variant-position"],
              caps_allowed: controls[:"synthesis-small-caps"] != :none,
              position_allowed: controls[:"synthesis-position"] != :none
            }

            key = {plan, synthesis, variants}

            suffix =
              :crypto.hash(:sha256, :erlang.term_to_binary(key))
              |> binary_part(0, 6)
              |> Base.encode16(case: :lower)

            prepared =
              font
              |> Map.put(:feature_plan, plan)
              |> Map.put(:synthesis, synthesis)
              |> Map.put(:palette, plan.palette)
              |> then(fn font ->
                if variants.caps != :normal or variants.position != :normal or
                     plan.emoji != :normal,
                   do: Map.put(font, :synthetic_variants, variants),
                   else: font
              end)
              |> Map.update!(:id, &(&1 <> "-" <> suffix))

            prepared =
              if Map.get(plan.features, "kern") == 0,
                do: Map.put(prepared, :kerning, %{}),
                else: prepared

            prepared =
              if Map.get(plan.features, "liga") == 0,
                do: Map.put(prepared, :latin_ligatures, %{}),
                else: prepared

            {:ok, prepared}
          else
            {:ok, font}
          end
        end
      end
    end)
  catch
    {:font_rendering_error, error} -> error
  end

  @doc "Selects a covered cluster, preferring complete emoji shaping and its requested presentation."
  @spec select_face([map()], String.t()) :: map() | nil
  def select_face(candidates, text) do
    sequence? = EmojiPresentationValidator.sequence?(text)

    {selected, _rank} =
      Enum.reduce_while(candidates, {nil, nil}, fn font, {selected, best} ->
        if supports_text?(font, text) do
          preference = EmojiPresentationValidator.preference(text, font)
          color? = Map.get(font, :color_tables, []) != []

          presentation =
            if preference == :normal or preference == :emoji == color?, do: 0, else: 1

          complete =
            if sequence? do
              {:ok, shaped} = selected_shape(text, font)
              if length(Enum.reject(shaped.glyphs, & &1.ignorable?)) == 1, do: 0, else: 1
            else
              0
            end

          rank = {complete, presentation}
          result = if best == nil or rank < best, do: {font, rank}, else: {selected, best}
          if rank == {0, 0}, do: {:halt, result}, else: {:cont, result}
        else
          {:cont, {selected, best}}
        end
      end)

    selected
  end

  @doc "Checks selected synthetic glyph coverage through the same face preparation used for painting."
  @spec supports_text?(map(), String.t()) :: boolean()
  def supports_text?(font, text) do
    selected =
      if Map.has_key?(font, :synthetic_variants) do
        case prepare_grapheme(font, text) do
          {:ok, font} -> font
          error -> throw({:font_resource_limit, error})
        end
      else
        font
      end

    if NativeElixirPdfUtilities.Validators.TextShapingValidator.required?(selected, text) do
      case selected_shape(text, selected) do
        {:ok, shaped} -> Enum.all?(shaped.glyphs, &(&1.glyph != 0 or &1.ignorable?))
        error -> throw({:font_resource_limit, error})
      end
    else
      NativeElixirPdfUtilities.HtmlToPdf.Font.supports_text?(selected, text)
    end
  end

  @doc "Selects a synthetic small-cap or script glyph face while preserving its original Unicode and line strut."
  @spec prepare_grapheme(map(), String.t()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_grapheme(font, text) do
    result =
      case Map.get(font, :synthetic_variants) do
        nil ->
          {:ok, font}

        variants ->
          FontValidator.with_budget(fn ->
            with {:ok, available} <-
                   FontValidator.memo({:feature_capabilities, font.data}, fn ->
                     capabilities(font.data)
                   end) do
              caps = variants.caps
              petite? = caps in [["petite-caps"], ["all-petite-caps"]]

              cap_tag =
                cond do
                  petite? and MapSet.member?(available, "pcap") -> "pcap"
                  caps == ["unicase"] -> "unic"
                  true -> "smcp"
                end

              uppercase? = String.upcase(text) == text

              cap? =
                String.upcase(text) != String.downcase(text) and variants.caps_allowed and
                  caps in [
                    ["small-caps"],
                    ["all-small-caps"],
                    ["petite-caps"],
                    ["all-petite-caps"],
                    ["unicase"]
                  ] and
                  if(caps == ["unicase"],
                    do: uppercase?,
                    else: not uppercase? or caps in [["all-small-caps"], ["all-petite-caps"]]
                  ) and
                  not MapSet.member?(available, cap_tag)

              position = variants.position

              position_tag =
                case position do
                  ["sub"] -> "subs"
                  ["super"] -> "sups"
                  _ -> nil
                end

              script? =
                variants.position_allowed and position_tag != nil and
                  not MapSet.member?(available, position_tag)

              if cap? or script? do
                FontValidator.memo(
                  {:synthetic_glyph_face, font.id, cap?, petite?, script?, position},
                  fn ->
                    cmap =
                      if cap? do
                        FontValidator.reserve(:max_work, map_size(font.cmap))

                        Map.new(font.cmap, fn {codepoint, glyph} ->
                          upper = String.upcase(<<codepoint::utf8>>)

                          selected =
                            case String.to_charlist(upper) do
                              [upper] -> Map.get(font.cmap, upper, 0)
                              _ -> glyph
                            end

                          {codepoint, selected}
                        end)
                      else
                        font.cmap
                      end

                    {script_scale, shift} =
                      if script? do
                        {:ok, metrics} = FontValidator.prepare_text_metrics(font)
                        key = if position == ["sub"], do: :subscript, else: :superscript
                        sign = if key == :subscript, do: -1, else: 1

                        case metrics[key] do
                          nil ->
                            {0.65, sign * 0.35}

                          metrics ->
                            {metrics.y_size / font.units_per_em,
                             sign * metrics.y_offset / font.units_per_em}
                        end
                      else
                        {1, 0}
                      end

                    scale = script_scale * if(cap?, do: 0.7, else: 1)
                    key = {scale, shift, cap?}

                    suffix =
                      :crypto.hash(:sha256, :erlang.term_to_binary(key))
                      |> binary_part(0, 6)
                      |> Base.encode16(case: :lower)

                    {:ok,
                     font
                     |> Map.put(:cmap, cmap)
                     |> then(fn font ->
                       if Map.has_key?(font, :unicode_cmap),
                         do:
                           Map.put(
                             font,
                             :unicode_cmap,
                             Map.take(cmap, Map.keys(font.unicode_cmap))
                           ),
                         else: font
                     end)
                     |> Map.put(:glyph_scale, scale)
                     |> Map.put(:baseline_shift, shift)
                     |> Map.update!(:id, &(&1 <> "-glyph-" <> suffix))}
                  end
                )
              else
                {:ok, font}
              end
            end
          end)
      end

    with {:ok, prepared} <- result do
      {:ok, EmojiPresentationValidator.prepare(prepared, text)}
    end
  end

  @doc "Validates the optional synthetic face parameters consumed by PDF text painting."
  @spec validate_paint(map()) :: :ok | {:error, {atom(), map()}}
  def validate_paint(font) do
    with :ok <- NativeElixirPdfUtilities.Validators.FontMetricValidator.validate_paint(font),
         :ok <- NativeElixirPdfUtilities.Validators.BitmapFontValidator.validate_metrics(font) do
      case Map.get(font, :synthesis) do
        nil ->
          :ok

        %{weight: weight, shear: shear} when is_boolean(weight) and is_number(shear) ->
          # The renderer's fixed synthetic design maps CSS's singular +/-90
          # degree endpoint to +/-89 degrees. This is not a resource ceiling.
          if abs(shear) <= :math.tan(89 * :math.pi() / 180),
            do: :ok,
            else:
              failure(
                "synthetic font shear must be within the finite 89-degree drawing range",
                Map.get(font, :family, "font"),
                :write_pdf
              )

        _ ->
          failure(
            "font synthesis requires a boolean weight flag and a finite numeric shear",
            Map.get(font, :family, "font"),
            :write_pdf
          )
      end
    end
  end

  @doc "Admits active shaping requests and reports unsupported synthetic or presentation matching."
  @spec validate_execution(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_execution(font, text) do
    case font do
      %{feature_plan: _plan, data: data} ->
        FontValidator.with_budget(fn ->
          with {:ok, _available} <-
                 FontValidator.memo({:feature_capabilities, data}, fn -> capabilities(data) end) do
            pending = []

            variants = Map.get(font, :synthetic_variants)

            pending =
              if variants != nil and variants.caps_allowed and variants.caps != :normal and
                   Enum.any?(
                     String.codepoints(text),
                     &(length(String.codepoints(String.upcase(&1))) > 1)
                   ), do: ["multi-scalar synthetic capitalization" | pending], else: pending

            if pending == [] or String.trim(text) == "" do
              validate_shaping(font, text)
            else
              Diagnostics.error(
                :font,
                :unsupported_font,
                "selected font features #{Enum.join(Enum.sort(pending), ", ")} require script shaping; feature selection is retained for the shaping stage",
                module: __MODULE__,
                operation: :resolve_fonts,
                source: font.family
              )
            end
          end
        end)

      _ ->
        :ok
    end
  end

  defp selected_shape(text, font) do
    if Map.has_key?(font, :_vertical_text),
      do: NativeElixirPdfUtilities.HtmlToPdf.VerticalText.shape(text, font, []),
      else: NativeElixirPdfUtilities.HtmlToPdf.TextShaper.shape(text, font)
  end

  defp validate_shaping(font, text) do
    if NativeElixirPdfUtilities.Validators.TextShapingValidator.required?(font, text) and
         String.trim(text) != "" do
      with {:ok, shaped} <- NativeElixirPdfUtilities.HtmlToPdf.TextShaper.shape(text, font) do
        NativeElixirPdfUtilities.Validators.TextShapingValidator.validate_visible_glyphs(
          shaped.glyphs
        )
      end
    else
      :ok
    end
  end

  defp capabilities(data) do
    with {:ok, _, directory} <- FontValidator.prepare_table_directory(data) do
      Enum.reduce_while(["GSUB", "GPOS"], {:ok, MapSet.new()}, fn tag, {:ok, tags} ->
        case Map.get(directory, tag) do
          nil ->
            {:cont, {:ok, tags}}

          {offset, size} ->
            table = binary_part(data, offset, size)

            case table do
              <<major::16, minor::16, _script::16, features::16, _lookup::16, _::binary>>
              when major == 1 and minor in [0, 1] and features + 2 <= byte_size(table) ->
                <<count::16>> = binary_part(table, features, 2)
                FontValidator.reserve(:max_work, count)

                if features + 2 + count * 6 <= byte_size(table) do
                  records = binary_part(table, features + 2, count * 6)

                  tags =
                    for <<name::binary-size(4), _offset::16 <- records>>, reduce: tags do
                      tags -> MapSet.put(tags, name)
                    end

                  {:cont, {:ok, tags}}
                else
                  {:halt, failure("OpenType #{tag} feature records are truncated", tag)}
                end

              _ ->
                {:halt, failure("OpenType #{tag} feature header is invalid", tag)}
            end
        end
      end)
    end
  end

  defp failure(message, source, operation \\ :prepare_font_features),
    do:
      Diagnostics.error(:font, :invalid_font, message,
        module: __MODULE__,
        operation: operation,
        source: source
      )
end
