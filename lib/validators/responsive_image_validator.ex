defmodule NativeElixirPdfUtilities.Validators.ResponsiveImageValidator do
  @moduledoc """
  Canonical static image selection from the pinned HTML Standard of 2026-10-05,
  sections 4.8.4.3.5 through 4.8.4.3.12. Invalid candidate descriptors are discarded
  by the standard's parser. Duplicate normalized densities retain the first URL.
  The standard leaves candidate choice implementation-defined. This renderer
  chooses the smallest density at least the print density, or the largest available.
  Selection never reads resources, retries failed candidates, or changes authorization.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, PrintMediaValidator}

  @type candidate :: %{url: String.t(), width: number() | nil, density: number() | nil}
  @type selection :: %{
          url: String.t(),
          density: number() | :infinity,
          dimensions: map(),
          source: String.t()
        }

  @doc "Checks the canonical picture content model, including script-supporting elements."
  @spec valid_picture_children?([map()]) :: boolean()
  def valid_picture_children?(children) do
    Enum.reduce_while(children, :sources, fn child, state ->
      case child do
        %{type: :text, text: text} ->
          if HtmlValidator.trim_css_whitespace(text) == "",
            do: {:cont, state},
            else: {:halt, :invalid}

        %{tag: tag} when tag in ~w(script template) ->
          {:cont, state}

        %{tag: "source"} when state == :sources ->
          {:cont, state}

        %{tag: "img"} when state == :sources ->
          {:cont, :image}

        _ ->
          {:halt, :invalid}
      end
    end) == :image
  end

  @doc "Parses srcset with the HTML URL/descriptor tokenizer, preserving data URL commas."
  @spec parse_srcset(String.t()) :: [candidate()]
  def parse_srcset(value) do
    HtmlValidator.check_render_resource(:max_html_source_bytes, byte_size(value), :html)
    parse_candidates(value, [], 0)
  end

  @doc "Resolves sizes in a validated print environment. Invalid entries and eager auto are skipped."
  @spec source_size(String.t(), PrintMediaValidator.context()) :: float()
  def source_size(sizes, context) do
    sizes
    |> NativeElixirPdfUtilities.HtmlToPdf.CssParser.strip_comments()
    |> HtmlValidator.split_css_comma()
    |> Enum.reduce_while(context.width, fn entry, default ->
      # The last component is a length or math function, not a percentage.
      case Regex.run(~r/\A(?:(.*?)\s+)?([a-zA-Z_-]+\(.*\)|[^\s()]+)\z/s, entry) do
        [_, media, length] ->
          case PrintMediaValidator.source_length(length, context) do
            {:ok, value} ->
              if PrintMediaValidator.matches?(media, context),
                do: {:halt, value},
                else: {:cont, default}

            _ ->
              {:cont, default}
          end

        _ ->
          {:cont, default}
      end
    end)
  end

  @doc "Selects the img resource using its direct picture parent, without loading unselected URLs."
  @spec select(map(), [map()], keyword()) :: {:ok, selection()} | {:error, {atom(), map()}}
  def select(node, ancestors, opts) do
    HtmlValidator.with_render_budget(fn ->
      if not Map.has_key?(node.attributes, "srcset") and
           not match?([%{tag: "picture"} | _], ancestors) do
        with :ok <- PrintMediaValidator.validate_options(opts) do
          prepare_selection(
            HtmlValidator.trim_css_whitespace(Map.get(node.attributes, "src", "")),
            1.0,
            node.attributes,
            "img"
          )
        end
      else
        with {:ok, context} <- PrintMediaValidator.context(opts) do
          select_responsive(node, ancestors, context)
        end
      end
    end)
  end

  defp select_responsive(node, ancestors, context) do
    sources =
      case ancestors do
        [%{tag: "picture", children: children} | _] ->
          children
          |> Enum.take_while(&(&1 != node))
          |> Enum.filter(&match?(%{tag: "source"}, &1))

        _ ->
          []
      end

    selected =
      Enum.find_value(sources, fn source ->
        attributes = source.attributes
        candidates = parse_srcset(Map.get(attributes, "srcset", ""))

        supported? =
          case Map.get(attributes, "type") do
            nil ->
              true

            type ->
              type
              |> String.split(";", parts: 2)
              |> hd()
              |> String.trim()
              |> String.downcase(:ascii)
              |> then(&(&1 in ~w(image/png image/jpeg image/svg+xml)))
          end

        if candidates != [] and supported? and
             PrintMediaValidator.matches?(Map.get(attributes, "media", ""), context) do
          {candidates, attributes, "source"}
        end
      end)

    {candidates, attributes, source} =
      selected ||
        {parse_srcset(Map.get(node.attributes, "srcset", "")), node.attributes, "img"}

    default_source = HtmlValidator.trim_css_whitespace(Map.get(node.attributes, "src", ""))

    candidates =
      if source == "img" and default_source != "" and
           not Enum.any?(candidates, &(&1.width != nil or &1.density in [nil, 1.0])) do
        candidates ++ [%{url: default_source, width: nil, density: 1.0}]
      else
        candidates
      end

    size = source_size(Map.get(attributes, "sizes", ""), context)

    candidates =
      candidates
      |> Enum.map(fn candidate ->
        density =
          cond do
            is_number(candidate.density) ->
              candidate.density

            is_nil(candidate.width) ->
              1.0

            size == 0 ->
              :infinity

            true ->
              try do
                candidate.width / size
              rescue
                ArithmeticError -> :infinity
              end
          end

        %{candidate | density: density}
      end)
      |> Enum.uniq_by(& &1.density)
      |> Enum.sort_by(& &1.density)

    chosen = Enum.find(candidates, &(&1.density >= context.density)) || List.last(candidates)

    dimensions =
      if source == "source" and
           Enum.any?(~w(width height), &Map.has_key?(attributes, &1)),
         do: attributes,
         else: node.attributes

    if is_nil(chosen) do
      prepare_selection("", 1.0, dimensions, source)
    else
      prepare_selection(chosen.url, chosen.density, dimensions, source)
    end
  end

  defp prepare_selection(url, density, dimensions, source) do
    cond do
      url == "" ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "img requires src or at least one usable srcset candidate",
          module: __MODULE__,
          source: "img"
        )

      density == 0 ->
        Diagnostics.error(
          :style,
          :resource_limit_exceeded,
          "selected 0x image has infinite natural dimensions; use a positive density",
          module: __MODULE__,
          source: url
        )

      true ->
        with :ok <- HtmlValidator.validate_static_url_attributes(%{"src" => url}, "img"),
             {:ok, hints} <- HtmlValidator.image_dimension_hints(dimensions) do
          {:ok, %{url: url, density: density, dimensions: hints, source: source}}
        end
    end
  end

  @doc "Validates density-corrected natural dimensions against the shared numeric limit."
  @spec correct_natural_dimensions(map(), selection()) :: {:ok, map()} | {:error, {atom(), map()}}
  def correct_natural_dimensions(image, selected) do
    result =
      try do
        scale = if selected.density == :infinity, do: 0.0, else: 1.0 / selected.density

        with {:ok, width} <- HtmlValidator.normalize_geometry_number(image.width * scale),
             {:ok, height} <- HtmlValidator.normalize_geometry_number(image.height * scale) do
          {:ok, %{image | width: width, height: height}}
        end
      rescue
        ArithmeticError -> :error
      end

    case result do
      {:ok, _} ->
        result

      _ ->
        Diagnostics.error(
          :style,
          :resource_limit_exceeded,
          "selected image natural dimensions exceed max_css_numeric_magnitude after density correction",
          module: __MODULE__,
          source: selected.url
        )
    end
  end

  defp parse_candidates(input, candidates, count) do
    input = Regex.replace(~r/\A[ \t\n\f\r,]+/, input, "")

    case Regex.run(~r/\A([^ \t\n\f\r]+)(.*)\z/s, input) do
      [_, url, rest] ->
        HtmlValidator.check_render_resource(:max_responsive_image_candidates, count + 1, :html)

        {url, descriptors, rest} =
          if String.ends_with?(url, ",") do
            {String.trim_trailing(url, ","), [], rest}
          else
            {descriptors, rest} = tokenize_descriptors(rest, [], [], false)
            {url, descriptors, rest}
          end

        candidate = parse_descriptors(url, descriptors)
        candidates = if is_nil(candidate), do: candidates, else: [candidate | candidates]
        parse_candidates(rest, candidates, count + 1)

      _ ->
        Enum.reverse(candidates)
    end
  end

  defp tokenize_descriptors(input, descriptors, current, parens?) do
    case input do
      <<char, rest::binary>> ->
        cond do
          char == ?, and not parens? ->
            descriptors =
              if current == [],
                do: descriptors,
                else: [current |> Enum.reverse() |> IO.iodata_to_binary() | descriptors]

            {Enum.reverse(descriptors), rest}

          char in [32, 9, 10, 12, 13] and not parens? ->
            descriptors =
              if current == [],
                do: descriptors,
                else: [current |> Enum.reverse() |> IO.iodata_to_binary() | descriptors]

            tokenize_descriptors(rest, descriptors, [], false)

          true ->
            tokenize_descriptors(
              rest,
              descriptors,
              [char | current],
              if(char == ?), do: false, else: parens? or char == ?()
            )
        end

      "" ->
        descriptors =
          if current == [],
            do: descriptors,
            else: [current |> Enum.reverse() |> IO.iodata_to_binary() | descriptors]

        {Enum.reverse(descriptors), ""}
    end
  end

  defp parse_descriptors(url, descriptors) do
    parsed =
      Enum.reduce_while(descriptors, %{width: nil, density: nil, height: nil}, fn descriptor,
                                                                                  acc ->
        HtmlValidator.check_render_resource(
          :max_html_numeric_token_bytes,
          byte_size(descriptor),
          :html
        )

        case HtmlValidator.parse_css_number(descriptor) do
          {number, unit} ->
            cond do
              unit in ~w(w h) and number > 0 and Regex.match?(~r/\A[0-9]+[wh]\z/, descriptor) and
                  is_nil(acc.density) ->
                key = if unit == "w", do: :width, else: :height
                if is_nil(acc[key]), do: {:cont, Map.put(acc, key, number)}, else: {:halt, nil}

              unit == "x" and number >= 0 and is_nil(acc.width) and is_nil(acc.height) and
                is_nil(acc.density) and
                  Regex.match?(
                    ~r/\A-?(?:[0-9]+(?:\.[0-9]+)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?x\z/,
                    descriptor
                  ) ->
                {:cont, %{acc | density: number}}

              true ->
                {:halt, nil}
            end

          _ ->
            {:halt, nil}
        end
      end)

    if is_map(parsed) and (is_nil(parsed.height) or not is_nil(parsed.width)),
      do: %{url: url, width: parsed.width, density: parsed.density}
  end
end
