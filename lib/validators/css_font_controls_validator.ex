defmodule NativeElixirPdfUtilities.Validators.CssFontControlsValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, CssSyntaxValidator}

  @groups %{
    "font-variant-ligatures" => [
      ~w(common-ligatures no-common-ligatures),
      ~w(discretionary-ligatures no-discretionary-ligatures),
      ~w(historical-ligatures no-historical-ligatures),
      ~w(contextual no-contextual)
    ],
    "font-variant-numeric" => [
      ~w(lining-nums oldstyle-nums),
      ~w(proportional-nums tabular-nums),
      ~w(diagonal-fractions stacked-fractions),
      ~w(ordinal),
      ~w(slashed-zero)
    ],
    "font-variant-east-asian" => [
      ~w(jis78 jis83 jis90 jis04 simplified traditional),
      ~w(full-width proportional-width),
      ~w(ruby)
    ],
    "font-variant-caps" => [
      ~w(small-caps all-small-caps petite-caps all-petite-caps unicase titling-caps)
    ],
    "font-variant-position" => [~w(sub super)],
    "font-variant-emoji" => [~w(text emoji unicode)]
  }
  @variants ~w(font-variant-ligatures font-variant-caps font-variant-numeric font-variant-east-asian font-variant-position font-variant-alternates font-variant-emoji)
  @synthesis ~w(font-synthesis-weight font-synthesis-style font-synthesis-small-caps font-synthesis-position)
  @properties @variants ++
                @synthesis ++
                ~w(font-feature-settings font-variation-settings font-language-override font-kerning font-optical-sizing font-size-adjust font-palette)
  @fields Map.new(@properties, fn property ->
            {property, String.to_atom(String.replace(property, "font-", ""))}
          end)
  @defaults Map.new(@fields, fn {property, key} ->
              value =
                cond do
                  property in ["font-feature-settings", "font-variation-settings"] ->
                    %{}

                  property in @synthesis or property in ["font-kerning", "font-optical-sizing"] ->
                    :auto

                  property == "font-size-adjust" ->
                    :none

                  true ->
                    :normal
                end

              {key, value}
            end)
  @feature_tokens %{
    "common-ligatures" => %{"liga" => 1, "clig" => 1},
    "no-common-ligatures" => %{"liga" => 0, "clig" => 0},
    "discretionary-ligatures" => %{"dlig" => 1},
    "no-discretionary-ligatures" => %{"dlig" => 0},
    "historical-ligatures" => %{"hlig" => 1},
    "no-historical-ligatures" => %{"hlig" => 0},
    "contextual" => %{"calt" => 1},
    "no-contextual" => %{"calt" => 0},
    "small-caps" => %{"smcp" => 1},
    "all-small-caps" => %{"smcp" => 1, "c2sc" => 1},
    "petite-caps" => %{"pcap" => 1},
    "all-petite-caps" => %{"pcap" => 1, "c2pc" => 1},
    "unicase" => %{"unic" => 1},
    "titling-caps" => %{"titl" => 1},
    "sub" => %{"subs" => 1},
    "super" => %{"sups" => 1},
    "lining-nums" => %{"lnum" => 1},
    "oldstyle-nums" => %{"onum" => 1},
    "proportional-nums" => %{"pnum" => 1},
    "tabular-nums" => %{"tnum" => 1},
    "diagonal-fractions" => %{"frac" => 1},
    "stacked-fractions" => %{"afrc" => 1},
    "ordinal" => %{"ordn" => 1},
    "slashed-zero" => %{"zero" => 1},
    "jis78" => %{"jp78" => 1},
    "jis83" => %{"jp83" => 1},
    "jis90" => %{"jp90" => 1},
    "jis04" => %{"jp04" => 1},
    "simplified" => %{"smpl" => 1},
    "traditional" => %{"trad" => 1},
    "full-width" => %{"fwid" => 1},
    "proportional-width" => %{"pwid" => 1},
    "ruby" => %{"ruby" => 1},
    "historical-forms" => %{"hist" => 1}
  }
  @keywords %{
    "auto" => :auto,
    "none" => :none,
    "normal" => :normal,
    "light" => :light,
    "dark" => :dark,
    "oblique-only" => :oblique_only
  }
  @string Regex.compile!("\\A" <> HtmlValidator.css_string_pattern() <> "\\z", "u")

  @doc "Returns inherited font control longhands with canonical nested cascade fields."
  @spec properties() :: [String.t()]
  def properties, do: @properties

  @doc "Returns the font-variant shorthand's longhands."
  @spec variant_properties() :: [String.t()]
  def variant_properties, do: @variants

  @doc "Returns the font-synthesis shorthand's longhands."
  @spec synthesis_properties() :: [String.t()]
  def synthesis_properties, do: @synthesis

  @doc "Returns the font shorthand's reset-only control longhands."
  @spec font_resets() :: [String.t()]
  def font_resets, do: @properties -- (@synthesis ++ ["font-palette"])

  @doc "Maps each font control to its nested computed-style field."
  @spec cascade_fields() :: map()
  def cascade_fields,
    do: Map.new(@fields, fn {property, key} -> {property, [[:font_controls, key]]} end)

  @doc "Applies the canonical initial values to inherited computed controls."
  @spec effective(map()) :: map()
  def effective(controls),
    do: Map.merge(@defaults, Map.reject(controls, fn {_key, value} -> value == nil end))

  @doc "Validates CSS font controls and shorthands into one computed control map."
  @spec prepare(String.t(), term()) :: {:ok, map()} | :error
  def prepare(property, value) do
    if is_binary(value) do
      tokens = HtmlValidator.split_css_tokens(value)
      lower = Enum.map(tokens, &String.downcase(&1, :ascii))

      cond do
        property == "font-variant" ->
          if lower in [["normal"], ["none"]] do
            result = Map.new(@variants, &{Map.fetch!(@fields, &1), :normal})

            {:ok,
             if(lower == ["none"],
               do: Map.put(result, Map.fetch!(@fields, "font-variant-ligatures"), :none),
               else: result
             )}
          else
            parse_variant_shorthand(tokens)
          end

        property == "font-synthesis" ->
          allowed = ~w(weight style small-caps position)

          if lower == ["none"] or
               (lower !=
                  [] and length(Enum.uniq(lower)) == length(lower) and
                  Enum.all?(lower, &(&1 in allowed))) do
            {:ok,
             Map.new(@synthesis, fn name ->
               token = String.replace_prefix(name, "font-synthesis-", "")
               {Map.fetch!(@fields, name), if(token in lower, do: :auto, else: :none)}
             end)}
          else
            :error
          end

        Map.has_key?(@groups, property) ->
          groups = @groups[property]
          normal? = lower == ["normal"]
          none? = property == "font-variant-ligatures" and lower == ["none"]

          if normal? or none? or grouped_tokens?(lower, groups) do
            result =
              cond do
                normal? -> :normal
                none? -> :none
                true -> lower
              end

            {:ok, %{Map.fetch!(@fields, property) => result}}
          else
            :error
          end

        property == "font-variant-alternates" ->
          alternates(value)

        property in ["font-feature-settings", "font-variation-settings"] ->
          settings(value, property == "font-feature-settings")

        property == "font-language-override" ->
          case if(lower == ["normal"], do: lower, else: tokens) do
            ["normal"] ->
              {:ok, %{:"language-override" => :normal}}

            [token] ->
              if Regex.match?(@string, token) do
                decoded = HtmlValidator.decode_css_string(token)

                if byte_size(decoded) in 1..4 and
                     Enum.all?(:binary.bin_to_list(decoded), &(&1 in 32..126)),
                   do: {:ok, %{:"language-override" => String.pad_trailing(decoded, 4)}},
                   else: :error
              else
                :error
              end

            _ ->
              :error
          end

        property == "font-size-adjust" ->
          size_adjust(lower)

        property == "font-palette" ->
          case if(lower in [["normal"], ["light"], ["dark"]], do: lower, else: tokens) do
            [value] when value in ["normal", "light", "dark"] ->
              {:ok, %{palette: Map.fetch!(@keywords, value)}}

            [value] ->
              case HtmlValidator.parse_css_identifier(value) do
                {:ok, "--" <> name, ""} when name != "" -> {:ok, %{palette: "--" <> name}}
                _ -> :error
              end

            _ ->
              :error
          end

        property in @synthesis ->
          allowed =
            if property == "font-synthesis-style",
              do: ~w(auto none oblique-only),
              else: ~w(auto none)

          keyword(property, lower, allowed)

        property == "font-kerning" ->
          keyword(property, lower, ~w(auto normal none))

        property == "font-optical-sizing" ->
          keyword(property, lower, ~w(auto none))

        true ->
          :error
      end
    else
      :error
    end
  end

  defp keyword(property, tokens, allowed) do
    case tokens do
      [value] ->
        if value in allowed,
          do: {:ok, %{Map.fetch!(@fields, property) => Map.fetch!(@keywords, value)}},
          else: :error

      _ ->
        :error
    end
  end

  defp grouped_tokens?(tokens, groups) do
    tokens !=
      [] and length(Enum.uniq(tokens)) == length(tokens) and
      Enum.all?(tokens, &(&1 in List.flatten(groups))) and
      Enum.all?(groups, fn group -> Enum.count(tokens, &(&1 in group)) <= 1 end)
  end

  defp parse_variant_shorthand(tokens) do
    initial = Map.new(@variants, &{Map.fetch!(@fields, &1), :normal})

    assignments =
      Enum.reduce_while(tokens, {:ok, %{}}, fn token, {:ok, groups} ->
        keyword = String.downcase(token, :ascii)

        property =
          Enum.find_value(@groups, fn {property, categories} ->
            if keyword in List.flatten(categories), do: property
          end)

        property =
          if property == nil and keyword != "normal" and match?({:ok, _}, alternates(token)),
            do: "font-variant-alternates",
            else: property

        if property do
          token = if property == "font-variant-alternates", do: token, else: keyword
          {:cont, {:ok, Map.update(groups, property, [token], &(&1 ++ [token]))}}
        else
          {:halt, :error}
        end
      end)

    case assignments do
      {:ok, groups} when map_size(groups) > 0 ->
        Enum.reduce_while(groups, {:ok, initial}, fn {property, values}, {:ok, result} ->
          case prepare(property, Enum.join(values, " ")) do
            {:ok, parsed} -> {:cont, {:ok, Map.merge(result, parsed)}}
            _ -> {:halt, :error}
          end
        end)

      _ ->
        :error
    end
  end

  @doc "Normalizes font-face OpenType defaults through the same tag and language rules as CSS properties."
  @spec prepare_defaults(map()) :: {:ok, map()} | :error
  def prepare_defaults(config) do
    NativeElixirPdfUtilities.Validators.FontValidator.with_budget(fn ->
      features = Map.get(config, :feature_settings, Map.get(config, "feature_settings", %{}))

      language =
        Map.get(config, :language_override, Map.get(config, "language_override", :normal))

      features =
        case features do
          map when is_map(map) ->
            NativeElixirPdfUtilities.Validators.FontValidator.reserve(
              :max_work,
              map_size(map)
            )

            Enum.each(map, fn {_tag, value} ->
              if is_number(value),
                do:
                  NativeElixirPdfUtilities.Validators.FontValidator.check(
                    :max_numeric_magnitude,
                    abs(value)
                  )
            end)

            if(Enum.all?(map, fn {tag, value} -> valid_setting?(tag, value, true) end),
              do: {:ok, map},
              else: :error
            )

          text when is_binary(text) ->
            case settings(text, true) do
              {:ok, %{:"feature-settings" => features}} -> {:ok, features}
              _ -> :error
            end

          _ ->
            :error
        end

      language =
        case language do
          :normal ->
            {:ok, :normal}

          text when is_binary(text) ->
            case prepare(
                   "font-language-override",
                   if(byte_size(text) in 1..4, do: "\"" <> text <> "\"", else: text)
                 ) do
              {:ok, %{:"language-override" => value}} -> {:ok, value}
              _ -> :error
            end

          _ ->
            :error
        end

      with {:ok, features} <- features, {:ok, language} <- language do
        defaults = %{}

        defaults =
          if features == %{}, do: defaults, else: Map.put(defaults, :feature_defaults, features)

        {:ok,
         if(language == :normal,
           do: defaults,
           else: Map.put(defaults, :language_default, language)
         )}
      end
    end)
  end

  defp valid_setting?(tag, value, features?) do
    is_binary(tag) and byte_size(tag) == 4 and
      Enum.all?(:binary.bin_to_list(tag), &(&1 in 32..126)) and is_number(value) and
      (not features? or (value == trunc(value) and value >= 0))
  end

  defp settings(value, features?) do
    key = if features?, do: :"feature-settings", else: :"variation-settings"

    if String.downcase(String.trim(value), :ascii) == "normal" do
      {:ok, %{key => %{}}}
    else
      segments = HtmlValidator.split_css_comma(value, true)

      Enum.reduce_while(segments, {:ok, %{}}, fn segment, {:ok, values} ->
        case HtmlValidator.split_css_tokens(segment) do
          [tag | rest] when length(rest) <= 1 ->
            if Regex.match?(@string, tag) do
              tag = HtmlValidator.decode_css_string(tag)

              parsed =
                case Enum.map(rest, &String.downcase(&1, :ascii)) do
                  [] ->
                    if features?, do: {:ok, 1}, else: :error

                  ["on"] when features? ->
                    {:ok, 1}

                  ["off"] when features? ->
                    {:ok, 0}

                  [number] ->
                    case HtmlValidator.parse_css_number(number) do
                      {n, ""} ->
                        if not features? or (n >= 0 and Regex.match?(~r/\A[+-]?[0-9]+\z/, number)),
                          do: {:ok, n},
                          else: :error

                      _ ->
                        :error
                    end
                end

              if match?({:ok, _}, parsed) and
                   valid_setting?(
                     tag,
                     if(match?({:ok, _}, parsed), do: elem(parsed, 1), else: nil),
                     features?
                   ) do
                {:ok, n} = parsed
                n = if n == trunc(n), do: trunc(n), else: n
                {:cont, {:ok, Map.put(values, tag, n)}}
              else
                {:halt, :error}
              end
            else
              {:halt, :error}
            end

          _ ->
            {:halt, :error}
        end
      end)
      |> case do
        {:ok, values} when map_size(values) > 0 -> {:ok, %{key => values}}
        _ -> :error
      end
    end
  end

  defp alternates(value) do
    values =
      CssSyntaxValidator.components(value)
      |> Enum.reject(&(&1.kind == :comment or String.trim(&1.text) == ""))

    if length(values) == 1 and String.downcase(hd(values).text, :ascii) == "normal" do
      {:ok, %{:"variant-alternates" => :normal}}
    else
      parsed =
        Enum.map(values, fn component ->
          if String.downcase(component.text, :ascii) == "historical-forms" do
            {:ok, {"historical-forms", []}}
          else
            case component do
              %{kind: :block, opener: opener, children: inner, closed: true} ->
                name =
                  case HtmlValidator.parse_css_identifier(opener) do
                    {:ok, name, "("} -> String.downcase(name, :ascii)
                    _ -> nil
                  end

                identifiers =
                  inner
                  |> Enum.map(& &1.text)
                  |> Enum.join()
                  |> HtmlValidator.split_css_comma(true)

                identifiers =
                  Enum.map(identifiers, &HtmlValidator.parse_css_identifier(String.trim(&1)))

                if name in ~w(stylistic styleset character-variant swash ornaments annotation) and
                     identifiers !=
                       [] and Enum.all?(identifiers, &match?({:ok, _, ""}, &1)) and
                     (name in ["styleset", "character-variant"] or length(identifiers) == 1),
                   do: {:ok, {name, Enum.map(identifiers, &elem(&1, 1))}},
                   else: :error

              _ ->
                :error
            end
          end
        end)

      if parsed != [] and Enum.all?(parsed, &match?({:ok, _}, &1)) do
        parsed = Enum.map(parsed, &elem(&1, 1))

        if length(Enum.uniq_by(parsed, &elem(&1, 0))) == length(parsed),
          do: {:ok, %{:"variant-alternates" => parsed}},
          else: :error
      else
        :error
      end
    end
  end

  defp size_adjust(tokens) do
    {metric, values} =
      case tokens do
        [metric | rest] when metric in ~w(ex-height cap-height ch-width ic-width ic-height) ->
          {metric, rest}

        values ->
          {"ex-height", values}
      end

    case values do
      ["none"] ->
        {:ok, %{:"size-adjust" => :none}}

      ["from-font"] ->
        {:ok, %{:"size-adjust" => {metric, :from_font}}}

      [value] ->
        case HtmlValidator.parse_css_number(value) do
          {n, ""} when n >= 0 -> {:ok, %{:"size-adjust" => {metric, n}}}
          _ -> :error
        end

      _ ->
        :error
    end
  end

  @doc "Resolves CSS feature precedence once, leaving script shaping execution to its owning stage."
  @spec feature_plan(map(), map()) :: map()
  def feature_plan(controls, face_defaults \\ %{}) do
    controls = effective(controls)

    variants =
      Enum.reduce(@variants, %{}, fn property, features ->
        values = Map.get(controls, Map.fetch!(@fields, property), :normal)

        values =
          cond do
            values == :none ->
              ~w(no-common-ligatures no-discretionary-ligatures no-historical-ligatures no-contextual)

            values == :normal ->
              []

            property == "font-variant-alternates" ->
              Enum.map(values, &elem(&1, 0))

            true ->
              values
          end

        Enum.reduce(values, features, fn token, features ->
          Map.merge(features, Map.get(@feature_tokens, token, %{}))
        end)
      end)

    kerning =
      case Map.get(controls, :kerning, :auto) do
        :none -> %{"kern" => 0}
        :normal -> %{"kern" => 1}
        _ -> %{}
      end

    features =
      face_defaults
      |> Map.merge(variants)
      |> Map.merge(kerning)
      |> Map.merge(Map.get(controls, :"feature-settings", %{}))

    %{
      features: features,
      language: Map.get(controls, :"language-override", :normal),
      alternates: Map.get(controls, :"variant-alternates", :normal),
      emoji:
        case Map.get(controls, :"variant-emoji", :normal) do
          ["text"] -> :text
          ["emoji"] -> :emoji
          ["unicode"] -> :unicode
          :normal -> :normal
        end,
      palette: Map.get(controls, :palette, :normal)
    }
  end
end
