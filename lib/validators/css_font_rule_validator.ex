defmodule NativeElixirPdfUtilities.Validators.CssFontRuleValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.{CssSyntaxValidator, HtmlValidator}
  @groups ~w(stylistic styleset character-variant swash ornaments annotation)
  @string Regex.compile!("\\A" <> HtmlValidator.css_string_pattern() <> "\\z", "s")

  @doc "Prepares active font-specific feature names and named palettes with normal CSS rule recovery."
  @spec prepare([map()]) :: map()
  def prepare(records) do
    Enum.reduce(records, %{features: %{}, palettes: %{}}, fn record, result ->
      HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)

      case record.kind do
        "font-feature-values" ->
          with {:ok, families} <- family_names(record.name) do
            groups =
              CssSyntaxValidator.rules(record.declarations)
              |> Enum.reduce(%{}, fn group, groups ->
                name = String.trim(group.selectors)

                if group.block and String.starts_with?(name, "@") and
                     String.downcase(String.slice(name, 1..-1//1), :ascii) in @groups do
                  type = String.downcase(String.slice(name, 1..-1//1), :ascii)

                  definitions =
                    Enum.reduce(
                      CssSyntaxValidator.declarations(group.declarations),
                      %{},
                      fn declaration, definitions ->
                        with {:ok, name, rest} <-
                               HtmlValidator.parse_css_identifier(declaration.text),
                             <<":", value::binary>> <- String.trim_leading(rest),
                             values when values != [] <- HtmlValidator.split_css_tokens(value),
                             true <- Enum.all?(values, &match?({:ok, _}, nonnegative_integer(&1))),
                             indices =
                               Enum.map(values, fn value ->
                                 {:ok, index} = nonnegative_integer(value)
                                 index
                               end),
                             true <- valid_indices?(type, indices) do
                          Map.put(definitions, name, indices)
                        else
                          _ -> definitions
                        end
                      end
                    )

                  Map.update(groups, type, definitions, &Map.merge(&1, definitions))
                else
                  groups
                end
              end)

            features =
              Enum.reduce(families, result.features, fn family, features ->
                Map.update(features, String.downcase(family), groups, fn previous ->
                  Map.merge(previous, groups, fn _, old, new -> Map.merge(old, new) end)
                end)
              end)

            %{result | features: features}
          else
            _ -> result
          end

        "font-palette-values" ->
          name = String.trim(record.name)

          with {:ok, "--" <> suffix, ""} <- HtmlValidator.parse_css_identifier(name),
               true <- suffix != "" do
            properties =
              CssSyntaxValidator.declarations(record.declarations)
              |> Enum.reduce(%{}, fn record, properties ->
                {name, value} = record.declaration
                Map.put(properties, name, value)
              end)

            with {:ok, families} <- family_names(Map.get(properties, "font-family", "")),
                 {:ok, base} <- palette_base(Map.get(properties, "base-palette", "0")),
                 {:ok, overrides} <- palette_colors(Map.get(properties, "override-colors", "")) do
              rule = %{
                families: Enum.map(families, &String.downcase/1),
                base: base,
                overrides: overrides
              }

              families = Map.new(rule.families, &{&1, Map.take(rule, [:base, :overrides])})

              %{
                result
                | palettes: Map.update(result.palettes, name, families, &Map.merge(&1, families))
              }
            else
              _ -> result
            end
          else
            _ -> result
          end
      end
    end)
  end

  @doc "Combines font definitions in their stylesheet tree scope without leaking shadow rules."
  @spec combine([map()]) :: map()
  def combine(entries) do
    Enum.reduce(entries, %{}, fn entry, scopes ->
      if Map.get(entry.prepared, :stylesheet_applicable, true) do
        definitions =
          get_in(entry, [:prepared, :cascade, :font_definitions]) ||
            %{features: %{}, palettes: %{}}

        scope = Map.get(entry, :shadow_scope_id)

        if definitions == %{features: %{}, palettes: %{}} do
          scopes
        else
          Map.update(scopes, scope, definitions, fn previous ->
            %{
              features:
                Map.merge(previous.features, definitions.features, fn _, old, new ->
                  Map.merge(old, new, fn _, a, b -> Map.merge(a, b) end)
                end),
              palettes:
                Map.merge(previous.palettes, definitions.palettes, fn _, old, new ->
                  Map.merge(old, new)
                end)
            }
          end)
        end
      else
        scopes
      end
    end)
  end

  @doc "Resolves font-specific alternate names before the explicit feature-settings property wins."
  @spec features(map(), String.t(), term()) :: map()
  def features(definitions, family, alternates) do
    groups = definitions |> Map.get(:features, %{}) |> Map.get(String.downcase(family), %{})

    if is_list(alternates) do
      Enum.reduce(alternates, %{}, fn {kind, names}, features ->
        Enum.reduce(names, features, fn name, features ->
          case get_in(groups, [kind, name]) do
            nil ->
              features

            indices ->
              case kind do
                "styleset" ->
                  Enum.reduce(
                    Enum.filter(indices, &(&1 in 1..99)),
                    features,
                    &Map.put(&2, "ss" <> String.pad_leading(to_string(&1), 2, "0"), 1)
                  )

                "character-variant" ->
                  [index | value] = indices

                  if index in 1..99 do
                    Map.put(
                      features,
                      "cv" <> String.pad_leading(to_string(index), 2, "0"),
                      if(value == [], do: 1, else: hd(value))
                    )
                  else
                    features
                  end

                "stylistic" ->
                  Map.put(features, "salt", hd(indices))

                "swash" ->
                  features |> Map.put("swsh", hd(indices)) |> Map.put("cswh", hd(indices))

                "ornaments" ->
                  Map.put(features, "ornm", hd(indices))

                "annotation" ->
                  Map.put(features, "nalt", hd(indices))
              end
          end
        end)
      end)
    else
      %{}
    end
  end

  @doc "Selects a named palette only for the font families declared by its rule."
  @spec palette(map(), String.t(), term()) :: term()
  def palette(definitions, family, selection) do
    case get_in(definitions, [:palettes, selection, String.downcase(family)]) do
      %{base: _, overrides: _} = rule ->
        rule

      _ ->
        if(is_binary(selection), do: :normal, else: selection)
    end
  end

  defp family_names(source) do
    source
    |> HtmlValidator.split_css_comma(true)
    |> Enum.reduce_while({:ok, []}, fn family, {:ok, families} ->
      family = String.trim(family)

      result =
        if Regex.match?(@string, family) do
          {:ok, HtmlValidator.decode_css_string(family)}
        else
          tokens = HtmlValidator.split_css_tokens(family)

          if tokens != [] and
               Enum.all?(tokens, &match?({:ok, _, ""}, HtmlValidator.parse_css_identifier(&1))),
             do:
               {:ok,
                Enum.map_join(tokens, " ", fn token ->
                  {:ok, name, ""} = HtmlValidator.parse_css_identifier(token)
                  name
                end)},
             else: :error
        end

      case result do
        {:ok, name} when name != "" -> {:cont, {:ok, [name | families]}}
        _ -> {:halt, :error}
      end
    end)
    |> case do
      {:ok, [_ | _] = families} -> {:ok, Enum.reverse(families)}
      _ -> :error
    end
  end

  defp valid_indices?(kind, indices) do
    case kind do
      "styleset" ->
        true

      "character-variant" ->
        length(indices) in [1, 2]

      _ ->
        length(indices) == 1
    end
  end

  defp nonnegative_integer(value) do
    if Regex.match?(~r/\A[+]?[0-9]+\z/, value) do
      case HtmlValidator.parse_css_number(value) do
        {number, ""} when number >= 0 -> {:ok, trunc(number)}
        _ -> :error
      end
    else
      :error
    end
  end

  defp palette_base(value) do
    case String.downcase(String.trim(value), :ascii) do
      "light" ->
        {:ok, :light}

      "dark" ->
        {:ok, :dark}

      integer ->
        nonnegative_integer(integer)
    end
  end

  defp palette_colors(value) do
    if value == "" do
      {:ok, %{}}
    else
      value
      |> HtmlValidator.split_css_comma(true)
      |> Enum.reduce_while({:ok, %{}}, fn stop, {:ok, colors} ->
        with [index | color] <- HtmlValidator.split_css_tokens(stop),
             {:ok, index} <- nonnegative_integer(index),
             {:ok, rgba} <- HtmlValidator.parse_css_color(Enum.join(color, " "), nil),
             true <- is_tuple(rgba) and tuple_size(rgba) in [3, 4] do
          rgba = if tuple_size(rgba) == 3, do: Tuple.insert_at(rgba, 3, 1), else: rgba
          {:cont, {:ok, Map.put(colors, index, rgba)}}
        else
          _ -> {:halt, :error}
        end
      end)
    end
  end
end
