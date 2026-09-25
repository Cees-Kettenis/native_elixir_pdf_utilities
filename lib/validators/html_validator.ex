defmodule NativeElixirPdfUtilities.Validators.HtmlValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Limits

  @render_option_keys [
    :forms,
    :page_size,
    :margin,
    :base_url,
    :assets,
    :asset_resolver,
    :stylesheets,
    :default_font,
    :fonts,
    :system_font_discovery,
    :metadata,
    :outlines,
    :page_furniture,
    :unsupported_glyphs
  ]
  @variant_keys [:default, :first, :odd, :even]
  @type image_budget :: :atomics.atomics_ref()

  @css_variable_regex ~r/"(?:\\.|[^"\\])*"(*SKIP)(*F)|'(?:\\.|[^'\\])*'(*SKIP)(*F)|var\(\s*(--[a-zA-Z_][a-zA-Z0-9_-]*)\s*\)/u

  @doc false
  @spec jpeg_metadata(binary()) :: {:ok, map()} | :error
  def jpeg_metadata(data) do
    NativeElixirPdfUtilities.Validators.JpegValidator.metadata(data)
  end

  @doc false
  @spec new_css_budget() :: :atomics.atomics_ref()
  def new_css_budget, do: :atomics.new(2, signed: false)

  @doc false
  @spec compute_custom_properties(map(), :atomics.atomics_ref()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def compute_custom_properties(properties, budget \\ new_css_budget()) do
    Enum.reduce_while(properties, {:ok, %{}}, fn {name, _value}, {:ok, memo} ->
      case resolve_custom_property(name, properties, %{}, memo, budget) do
        {:ok, _value, _height, memo} -> {:cont, {:ok, memo}}
        {:error, _detail} = error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, memo} ->
        {:ok,
         Map.new(properties, fn {name, _value} -> {name, elem(Map.fetch!(memo, name), 0)} end)}

      error ->
        error
    end
  end

  @doc false
  @spec resolve_css_variables(String.t(), map(), map(), :atomics.atomics_ref()) ::
          {:ok, String.t()} | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def resolve_css_variables(value, properties, resolving, budget \\ new_css_budget()) do
    case expand_css_value(value, properties, resolving, %{}, budget) do
      {:ok, nil, _height, _memo} -> :error
      {:ok, resolved, _height, _memo} -> {:ok, resolved}
      error -> error
    end
  end

  defp resolve_custom_property(name, properties, resolving, memo, budget) do
    depth = map_size(resolving)

    with :ok <- css_limit(:max_css_variable_work, :atomics.add_get(budget, 2, 1)),
         :ok <- css_limit(:max_css_variable_depth, depth + 1) do
      case {Map.has_key?(resolving, name), Map.fetch(memo, name), Map.get(properties, name)} do
        {true, _cached, _value} ->
          {:ok, nil, 0, memo}

        {false, {:ok, {value, height}}, _value} ->
          with :ok <- css_limit(:max_css_variable_depth, depth + height) do
            {:ok, value, height, memo}
          end

        {false, :error, nil} ->
          {:ok, nil, 0, Map.put(memo, name, {nil, 0})}

        {false, :error, value} ->
          with {:ok, resolved, height, memo} <-
                 expand_css_value(value, properties, Map.put(resolving, name, true), memo, budget) do
            height = height + 1
            {:ok, resolved, height, Map.put(memo, name, {resolved, height})}
          end
      end
    end
  end

  defp expand_css_value(value, properties, resolving, memo, budget) do
    with :ok <- css_limit(:max_css_variable_bytes, byte_size(value)) do
      matches = Regex.scan(@css_variable_regex, value, return: :index)

      Enum.reduce_while(matches, {:ok, [], 0, 0, 0, memo}, fn
        [{offset, length}, {name_offset, name_length}],
        {:ok, chunks, cursor, size, height, memo} ->
          name = binary_part(value, name_offset, name_length)

          case resolve_custom_property(name, properties, resolving, memo, budget) do
            {:ok, nil, _height, memo} ->
              {:halt, {:ok, nil, 0, memo}}

            {:ok, replacement, dependency_height, memo} ->
              size = size + offset - cursor + byte_size(replacement)

              case css_limit(:max_css_variable_bytes, size) do
                :ok ->
                  prefix = binary_part(value, cursor, offset - cursor)

                  {:cont,
                   {:ok, [replacement, prefix | chunks], offset + length, size,
                    max(height, dependency_height), memo}}

                error ->
                  {:halt, error}
              end

            error ->
              {:halt, error}
          end
      end)
      |> case do
        {:ok, chunks, cursor, size, height, memo} ->
          size = size + byte_size(value) - cursor

          with :ok <- css_limit(:max_css_variable_bytes, size),
               :ok <- css_limit(:max_css_variable_total_bytes, :atomics.add_get(budget, 1, size)) do
            suffix = binary_part(value, cursor, byte_size(value) - cursor)
            {:ok, IO.iodata_to_binary(Enum.reverse([suffix | chunks])), height, memo}
          end

        result ->
          result
      end
    end
  end

  defp css_limit(key, amount) do
    limit = Limits.get(key)

    if amount <= limit do
      :ok
    else
      Diagnostics.error(
        :limits,
        :resource_limit_exceeded,
        "CSS custom-property expansion exceeds #{key}=#{limit}; reduce variable values, references, or dependency depth"
      )
    end
  end

  @doc false
  @spec new_image_budget() :: image_budget()
  def new_image_budget do
    :atomics.new(3, signed: false)
  end

  @doc false
  @spec image_source_read_limit(image_budget() | nil) ::
          {:ok, pos_integer()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def image_source_read_limit(budget) do
    {count, bytes} =
      if is_nil(budget), do: {0, 0}, else: {:atomics.get(budget, 1), :atomics.get(budget, 2)}

    cond do
      count >= Limits.get(:max_image_count) ->
        Diagnostics.error(:limits, :resource_limit_exceeded, "image count exceeds the limit")

      bytes >= Limits.get(:max_aggregate_image_source_bytes) ->
        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "aggregate image source bytes exceed the limit"
        )

      true ->
        {:ok,
         min(
           Limits.get(:max_image_source_bytes),
           Limits.get(:max_aggregate_image_source_bytes) - bytes
         )}
    end
  rescue
    ArgumentError -> Diagnostics.error(:style, :invalid_document, "image budget is invalid")
  end

  @doc false
  @spec reserve_image_source(image_budget(), non_neg_integer()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def reserve_image_source(budget, source_bytes) do
    max_image_source_bytes = Limits.get(:max_image_source_bytes)

    cond do
      not is_integer(source_bytes) or source_bytes < 0 ->
        Diagnostics.error(:style, :invalid_document, "image source size is invalid")

      source_bytes > max_image_source_bytes ->
        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "image source exceeds the #{max_image_source_bytes}-byte limit"
        )

      true ->
        with :ok <-
               reserve_image_budget(
                 budget,
                 1,
                 1,
                 Limits.get(:max_image_count),
                 "image count exceeds the limit"
               ),
             :ok <-
               reserve_image_budget(
                 budget,
                 2,
                 source_bytes,
                 Limits.get(:max_aggregate_image_source_bytes),
                 "aggregate image source bytes exceed the limit"
               ) do
          :ok
        end
    end
  end

  @doc false
  @spec reserve_decoded_image(image_budget(), pos_integer(), pos_integer(), pos_integer()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def reserve_decoded_image(budget, width, height, channels) do
    max_decoded_image_bytes = Limits.get(:max_decoded_image_bytes)

    case is_integer(width) and width > 0 and is_integer(height) and height > 0 and
           is_integer(channels) and channels > 0 do
      true ->
        decoded_bytes = width * height * channels

        cond do
          decoded_bytes > max_decoded_image_bytes ->
            Diagnostics.error(
              :limits,
              :resource_limit_exceeded,
              "decoded image exceeds the #{max_decoded_image_bytes}-byte limit"
            )

          true ->
            reserve_image_budget(
              budget,
              3,
              decoded_bytes,
              Limits.get(:max_aggregate_decoded_image_bytes),
              "aggregate decoded image bytes exceed the limit"
            )
        end

      false ->
        Diagnostics.error(:style, :invalid_document, "decoded image dimensions are invalid")
    end
  end

  @doc false
  @spec validate_page_size(term()) :: :ok | {:error, :invalid_page_size}
  def validate_page_size(page_size) do
    case page_size do
      {width, height} when is_number(width) and is_number(height) and width > 0 and height > 0 ->
        :ok

      _ ->
        {:error, :invalid_page_size}
    end
  end

  @doc false
  @spec validate_margins(term()) :: :ok | {:error, :invalid_margin}
  def validate_margins(margins) do
    case margins do
      %{top: top, right: right, bottom: bottom, left: left}
      when map_size(margins) == 4 and is_number(top) and top >= 0 and is_number(right) and
             right >= 0 and is_number(bottom) and bottom >= 0 and is_number(left) and left >= 0 ->
        :ok

      _ ->
        {:error, :invalid_margin}
    end
  end

  @doc false
  @spec validate_printable_area(term(), term()) :: :ok | {:error, :invalid_margin}
  def validate_printable_area(page_size, margins) do
    case {page_size, margins} do
      {{page_width, page_height}, %{top: top, right: right, bottom: bottom, left: left}}
      when is_number(page_width) and is_number(page_height) and is_number(top) and
             is_number(right) and is_number(bottom) and is_number(left) ->
        case page_width > 0 and page_height > 0 and
               Enum.all?([top, right, bottom, left], &(&1 >= 0)) and
               left + right < page_width and top + bottom < page_height do
          true -> :ok
          false -> {:error, :invalid_margin}
        end

      _ ->
        {:error, :invalid_margin}
    end
  end

  @doc false
  @spec validate_furniture_fit(:header | :footer, number(), number()) ::
          :ok | {:error, {:invalid_layout, Diagnostics.diagnostic()}}
  def validate_furniture_fit(position, height, available_margin) do
    case {position, height, available_margin} do
      {position, height, available_margin}
      when position in [:header, :footer] and is_number(height) and height >= 0 and
             is_number(available_margin) and available_margin >= 0 ->
        case height <= available_margin + 0.0001 do
          true ->
            :ok

          false ->
            Diagnostics.error(
              :layout,
              :invalid_layout,
              "#{position} page furniture height #{format_number(height)}pt exceeds the #{format_number(available_margin)}pt page margin"
            )
        end

      _ ->
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "page furniture fit requires a header or footer and nonnegative numeric dimensions"
        )
    end
  end

  @doc false
  @spec validate_render_request(term(), term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_render_request(html, opts, font_options_result) do
    with {:ok, _html} <- validate_html_source(html),
         :ok <- validate_render_options(opts, font_options_result) do
      :ok
    end
  end

  @doc false
  @spec with_render_budget((-> result)) :: result | {:error, {atom(), Diagnostics.diagnostic()}}
        when result: term()
  def with_render_budget(fun) do
    key = {__MODULE__, :render_budget}

    case Process.get(key) do
      nil ->
        Process.put(key, %{})

        try do
          fun.()
        catch
          {:render_resource_limit, error} -> error
        after
          Process.delete(key)
        end

      _budget ->
        fun.()
    end
  end

  @doc false
  @spec reserve_render_resource(Limits.key(), non_neg_integer(), atom()) :: :ok
  def reserve_render_resource(limit, amount, stage) do
    key = {__MODULE__, :render_budget}
    budget = Process.get(key, %{})
    used = Map.get(budget, limit, 0) + amount
    check_render_resource(limit, used, stage)
    if Process.get(key) != nil, do: Process.put(key, Map.put(budget, limit, used))
    :ok
  end

  @doc false
  @spec check_render_resource(Limits.key(), number(), atom()) :: :ok
  def check_render_resource(limit, amount, stage) do
    case amount <= Limits.get(limit) do
      true ->
        :ok

      false ->
        throw(
          {:render_resource_limit,
           Diagnostics.error(
             stage,
             :resource_limit_exceeded,
             "rendering exceeds #{limit} (configured limit #{Limits.get(limit)})",
             module: __MODULE__
           )}
        )
    end
  end

  @doc false
  @spec within_html_element((-> result)) :: result when result: term()
  def within_html_element(fun) do
    key = {__MODULE__, :render_budget}
    budget = Process.get(key)
    depth = Map.get(budget, :max_html_depth, 0)
    check_render_resource(:max_html_depth, depth + 1, :html)
    Process.put(key, Map.put(budget, :max_html_depth, depth + 1))

    try do
      fun.()
    after
      Process.put(key, Map.put(Process.get(key), :max_html_depth, depth))
    end
  end

  @doc false
  @spec reserve_layout_box(atom()) :: atom()
  def reserve_layout_box(type) do
    reserve_render_resource(:max_layout_boxes, 1, :layout)
    type
  end

  @doc false
  @spec validate_html_source(term()) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_html_source(html) do
    case html do
      html when is_binary(html) ->
        case {byte_size(html) <= Limits.get(:max_html_source_bytes), String.valid?(html)} do
          {false, _} ->
            Diagnostics.error(
              :html,
              :resource_limit_exceeded,
              "HTML input exceeds max_html_source_bytes"
            )

          {true, true} ->
            {:ok, html}

          {true, false} ->
            Diagnostics.error(:html, :invalid_encoding, "HTML input must be valid UTF-8")
        end

      _ ->
        Diagnostics.error(:html, :invalid_html, "HTML input must be a string")
    end
  end

  @doc false
  @spec validate_css_source(term(), :stylesheet | :declarations) ::
          {:ok, binary()} | {:error, {:invalid_css, Diagnostics.diagnostic()}}
  def validate_css_source(css, kind \\ :stylesheet) do
    case {css, kind} do
      {css, kind} when is_binary(css) and kind in [:stylesheet, :declarations] ->
        case {byte_size(css) <= Limits.get(:max_css_source_bytes), String.valid?(css)} do
          {false, _} ->
            Diagnostics.error(:css, :invalid_css, "CSS input exceeds max_css_source_bytes")

          {true, true} ->
            {:ok, css}

          {true, false} ->
            Diagnostics.error(:css, :invalid_css, "CSS input must be valid UTF-8")
        end

      {_css, :declarations} ->
        Diagnostics.error(:css, :invalid_css, "CSS declaration input must be a string")

      _ ->
        Diagnostics.error(:css, :invalid_css, "CSS input must be a string")
    end
  end

  @doc false
  @spec validate_svg_raster(term(), term()) ::
          {:ok, [width: pos_integer(), height: pos_integer()]}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_svg_raster(svg, raster_options) do
    validate_svg_raster(svg, raster_options, nil)
  end

  @doc false
  @spec validate_svg_raster(term(), term(), image_budget() | nil) ::
          {:ok, [width: pos_integer(), height: pos_integer()]}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_svg_raster(svg, raster_options, image_budget) do
    NativeElixirPdfUtilities.Validators.SvgValidator.validate(svg, raster_options, image_budget)
  end

  defp reserve_image_budget(budget, index, amount, limit, message) do
    case :atomics.add_get(budget, index, amount) <= limit do
      true ->
        :ok

      false ->
        Diagnostics.error(:limits, :resource_limit_exceeded, message)
    end
  rescue
    ArgumentError ->
      Diagnostics.error(:style, :invalid_document, "image resource budget is invalid")
  end

  @doc false
  @spec validate_paths(term(), term()) ::
          {:ok, %{input_path: binary(), output_path: binary()}}
          | {:error, {:invalid_path, Diagnostics.diagnostic()}}
  def validate_paths(input_path, output_path) do
    case {input_path, output_path} do
      {input_path, output_path} when is_binary(input_path) and is_binary(output_path) ->
        {:ok, %{input_path: input_path, output_path: output_path}}

      _ ->
        Diagnostics.error(:file, :invalid_path, "input and output paths must be strings")
    end
  end

  @doc false
  @spec valid_link_url?(term()) :: boolean()
  def valid_link_url?(href) do
    case href do
      href when is_binary(href) ->
        Regex.match?(~r/^(https?:\/\/[^\s<>]+|mailto:[^\s<>@]+@[^\s<>@]+)$/iu, href)

      _ ->
        false
    end
  end

  @doc false
  @spec valid_table_header_scope?(term()) :: boolean()
  def valid_table_header_scope?(scope) do
    case scope do
      scope when is_binary(scope) -> String.downcase(scope) in ~w(row col rowgroup colgroup)
      _ -> false
    end
  end

  @doc false
  @spec validate_local_resource_path(term(), term()) ::
          {:ok, String.t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_local_resource_path(source, base_url) do
    result =
      with source when is_binary(source) and source != "" <- source,
           false <- String.contains?(source, ["\0", "://"]),
           {:ok, base_path} <- local_resource_base_path(base_url),
           base_path <- Path.expand(base_path),
           path <-
             (case Path.type(source) do
                :absolute -> Path.expand(source)
                _ -> Path.expand(source, base_path)
              end),
           relative <- Path.relative_to(path, base_path),
           true <-
             relative == "." or
               (Path.type(relative) == :relative and relative != ".." and
                  not String.starts_with?(relative, "../")),
           true <- symlink_free_resource_path?(base_path, relative) do
        {:ok, path}
      else
        _ -> :error
      end

    case result do
      {:ok, path} ->
        {:ok, path}

      :error ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "local document resource path is not authorized by base_url"
        )
    end
  end

  @doc false
  @spec validate_asset_options(term(), term()) ::
          :ok | {:error, {:invalid_options, Diagnostics.diagnostic()}}
  def validate_asset_options(assets, asset_resolver) do
    valid_assets? =
      is_map(assets) and
        Enum.all?(assets, fn
          {reference, {:bytes, bytes}} ->
            is_binary(reference) and String.trim(reference) != "" and is_binary(bytes)

          {reference, {:file, path}} ->
            is_binary(reference) and String.trim(reference) != "" and is_binary(path) and
              String.trim(path) != ""

          _ ->
            false
        end)

    valid_resolver? = is_nil(asset_resolver) or is_function(asset_resolver, 1)

    cond do
      not valid_assets? ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "assets must be a map of non-empty references to {:bytes, binary} or {:file, path}"
        )

      not valid_resolver? ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "asset_resolver must be a one-argument function or nil"
        )

      true ->
        :ok
    end
  end

  @doc false
  @spec validate_asset_resolver_result(term(), String.t()) ::
          {:ok, binary()} | :not_found | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_asset_resolver_result(result, reference) do
    case result do
      {:ok, bytes} when is_binary(bytes) ->
        {:ok, bytes}

      :not_found ->
        :not_found

      {:error, _reason} ->
        Diagnostics.error(
          :asset,
          :invalid_document,
          "caller-provided asset resolver could not resolve the asset",
          source: reference
        )

      _ ->
        Diagnostics.error(
          :asset,
          :invalid_document,
          "asset_resolver must return {:ok, binary}, :not_found, or {:error, reason}",
          source: reference
        )
    end
  end

  @doc false
  @spec validate_table_span_attribute(term(), term()) ::
          {:ok, pos_integer()} | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_table_span_attribute(attributes, name) do
    case {attributes, name} do
      {attributes, name}
      when is_map(attributes) and name in ["span", "colspan", "rowspan"] ->
        case Map.get(attributes, name) do
          nil ->
            {:ok, 1}

          value when is_binary(value) ->
            case Integer.parse(String.trim(value)) do
              {integer, ""} ->
                with :ok <- validate_layout_cardinality(:table_span, integer) do
                  {:ok, integer}
                end

              _ ->
                :error
            end

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  @doc false
  @spec valid_boolean_form_attribute?(String.t(), String.t()) :: boolean()
  def valid_boolean_form_attribute?(tag, name) do
    case {tag, name} do
      {"input", name} when name in ["checked", "disabled"] -> true
      {"select", "disabled"} -> true
      {"option", name} when name in ["selected", "disabled"] -> true
      {"textarea", "disabled"} -> true
      {"button", "disabled"} -> true
      _ -> false
    end
  end

  @doc false
  @spec valid_form_attribute?(String.t(), String.t(), String.t()) :: boolean()
  def valid_form_attribute?(tag, name, value) do
    case {tag, name, value} do
      {"input", "type", value} -> String.downcase(value) in ["text", "checkbox", "radio"]
      {"input", name, _value} when name in ["value", "name", "checked", "disabled"] -> true
      {"select", name, _value} when name in ["name", "disabled"] -> true
      {"option", name, _value} when name in ["value", "selected", "disabled"] -> true
      {"textarea", name, _value} when name in ["value", "name", "disabled"] -> true
      {"button", "type", value} -> String.downcase(value) in ["button", "submit", "reset"]
      {"button", name, _value} when name in ["value", "name", "disabled"] -> true
      _ -> false
    end
  end

  @doc false
  @spec valid_form_element?(String.t(), map(), [term()]) :: boolean()
  def valid_form_element?(tag, attributes, children) do
    case tag do
      "input" ->
        children == [] and
          String.downcase(Map.get(attributes, "type", "text")) in ["text", "checkbox", "radio"] and
          (not Map.has_key?(attributes, "checked") or
             String.downcase(Map.get(attributes, "type", "text")) in ["checkbox", "radio"])

      "select" ->
        children != [] and Enum.all?(children, &match?(%{tag: "option"}, &1)) and
          Enum.count(children, &Map.has_key?(&1.attributes, "selected")) <= 1

      "option" ->
        children != [] and Enum.all?(children, &match?(%{type: :text}, &1))

      "textarea" ->
        Enum.all?(children, &match?(%{type: :text}, &1))

      "button" ->
        valid_button_children?(children)

      _ ->
        false
    end
  end

  defp valid_button_children?(children) do
    Enum.all?(children, fn child ->
      case child do
        %{type: :text} ->
          true

        %{type: :element, tag: tag, children: nested}
        when tag in ["strong", "b", "em", "i", "span"] and is_list(nested) ->
          valid_button_children?(nested)

        _ ->
          false
      end
    end)
  end

  @doc false
  @spec validate_layout_cardinality(:grid_placement | :grid_tracks | :table_span, term()) ::
          :ok | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_layout_cardinality(kind, value) do
    max_layout_cardinality = Limits.get(:max_layout_cardinality)

    case {kind, value} do
      {kind, value}
      when kind in [:grid_placement, :grid_tracks, :table_span] and is_integer(value) and
             value >= 1 and value <= max_layout_cardinality ->
        :ok

      {kind, value}
      when kind in [:grid_placement, :grid_tracks, :table_span] and is_integer(value) and
             value >= 1 ->
        label =
          case kind do
            :grid_placement -> "grid placement"
            :grid_tracks -> "grid track count"
            :table_span -> "table span"
          end

        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "#{label} exceeds the #{max_layout_cardinality}-item limit"
        )

      _ ->
        :error
    end
  end

  @doc false
  @spec validate_background_image_tile_count(term()) ::
          :ok | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_background_image_tile_count(tile_count) do
    max_background_image_tiles = Limits.get(:max_background_image_tiles)

    case tile_count do
      tile_count
      when is_integer(tile_count) and tile_count >= 1 and
             tile_count <= max_background_image_tiles ->
        :ok

      tile_count when is_integer(tile_count) and tile_count >= 1 ->
        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "background image tile count exceeds the #{max_background_image_tiles}-tile limit"
        )

      _ ->
        :error
    end
  end

  @doc false
  @spec validate_background_image_tile_dimensions(
          term(),
          term(),
          term(),
          term(),
          term()
        ) :: :ok | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_background_image_tile_dimensions(
        tile_width,
        tile_height,
        area_width,
        area_height,
        repeat
      ) do
    max_background_image_tiles = Limits.get(:max_background_image_tiles)

    case {tile_width, tile_height, area_width, area_height, repeat} do
      {tile_width, tile_height, area_width, area_height, repeat}
      when is_number(tile_width) and is_number(tile_height) and is_number(area_width) and
             area_width >= 0 and is_number(area_height) and area_height >= 0 and
             repeat in [:repeat, :repeat_x, :repeat_y, :no_repeat] ->
        horizontal_over_limit? =
          repeat in [:repeat, :repeat_x] and tile_width > 0 and
            tile_width < area_width / max_background_image_tiles

        vertical_over_limit? =
          repeat in [:repeat, :repeat_y] and tile_height > 0 and
            tile_height < area_height / max_background_image_tiles

        case horizontal_over_limit? or vertical_over_limit? do
          true -> validate_background_image_tile_count(max_background_image_tiles + 1)
          false -> :ok
        end

      _ ->
        :error
    end
  end

  @doc false
  @spec validate_style_input(term(), term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_style_input(dom, opts, font_options_result) do
    with_render_budget(fn ->
      with :ok <- validate_dom(dom),
           :ok <- validate_style_options(opts, font_options_result) do
        :ok
      else
        :invalid_document ->
          Diagnostics.error(
            :style,
            :invalid_document,
            "document tree must be a parsed HTML document"
          )

        {:error, {_reason, _diagnostic}} = error ->
          error
      end
    end)
  end

  @doc false
  @spec validate_layout_input(term(), term(), term(), term()) ::
          :ok
          | {:error, :invalid_layout | :invalid_margin | :invalid_page_size}
          | {:error, {:resource_limit_exceeded, Diagnostics.diagnostic()}}
  def validate_layout_input(styled_tree, opts, page_size_result, margins_result) do
    with_render_budget(fn ->
      case {styled_tree, opts} do
        {%{type: :document, children: children}, opts}
        when is_list(children) and is_list(opts) ->
          case Keyword.keyword?(opts) and
                 is_boolean(Map.get(styled_tree, :_css_pixel_viewport, false)) do
            true ->
              validate_render_tree(children)

              with :ok <- validate_layout_nodes(children),
                   {:ok, page_size} <- page_size_result,
                   :ok <- validate_page_size(page_size),
                   {:ok, margins} <- margins_result,
                   :ok <- validate_margins(margins),
                   :ok <- validate_printable_area(page_size, margins) do
                :ok
              else
                :invalid_layout -> {:error, :invalid_layout}
                {:error, reason} -> {:error, reason}
              end

            false ->
              {:error, :invalid_layout}
          end

        _ ->
          {:error, :invalid_layout}
      end
    end)
  end

  @doc false
  @spec validate_pagination_input(term(), term(), term()) ::
          :ok | {:error, {:invalid_layout | :resource_limit_exceeded, Diagnostics.diagnostic()}}
  def validate_pagination_input(layout_tree, opts, margins_result) do
    with_render_budget(fn ->
      case {layout_tree, opts} do
        {%{type: :layout, page_size: {width, height} = page_size, boxes: boxes}, opts}
        when is_number(width) and width > 0 and is_number(height) and height > 0 and
               is_list(boxes) and is_list(opts) ->
          check_render_resource(:max_layout_boxes, length(boxes), :pagination)

          with true <- Keyword.keyword?(opts),
               {:ok, margins} <- margins_result,
               :ok <- validate_page_size(page_size),
               :ok <- validate_margins(margins),
               :ok <- validate_printable_area(page_size, margins) do
            :ok
          else
            _ -> invalid_pagination()
          end

        _ ->
          invalid_pagination()
      end
    end)
  end

  @doc false
  @spec validate_furniture_input(term(), term(), term(), term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_furniture_input(pages, layout_tree, opts, margins_result, furniture_result) do
    case {pages, layout_tree, opts} do
      {pages, %{page_size: {width, height} = page_size}, opts}
      when is_list(pages) and is_number(width) and width > 0 and is_number(height) and
             height > 0 and is_list(opts) ->
        case Keyword.keyword?(opts) do
          true ->
            with {:ok, margins} <- margins_result,
                 :ok <- validate_page_size(page_size),
                 :ok <- validate_margins(margins),
                 :ok <- validate_printable_area(page_size, margins),
                 true <- valid_pages?(pages, page_size),
                 :ok <- validate_furniture_result(furniture_result) do
              :ok
            else
              false ->
                invalid_furniture_layout()

              {:error, {_reason, _diagnostic}} = error ->
                error

              {:error, reason} when reason in [:invalid_margin, :invalid_page_size] ->
                invalid_furniture_layout()
            end

          false ->
            invalid_furniture_options("page furniture options require a keyword list")
        end

      {_pages, _layout_tree, opts} when is_list(opts) ->
        case Keyword.keyword?(opts) do
          true -> invalid_furniture_layout()
          false -> invalid_furniture_options("page furniture options require a keyword list")
        end

      _ ->
        invalid_furniture_layout()
    end
  end

  @doc false
  @spec validate_font_fallback_input(term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_font_fallback_input(styled_tree, unsupported_glyphs) do
    with :ok <- validate_unsupported_glyphs(unsupported_glyphs) do
      case styled_tree do
        %{type: :document, children: children} when is_list(children) ->
          validate_font_input_nodes(children)

        _ ->
          invalid_styled_document("font fallback requires a styled document tree")
      end
    end
  end

  @doc false
  @spec validate_font_coverage(term(), term(), String.t()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_font_coverage(styled_tree, unsupported_glyphs, replacement_character) do
    case styled_tree do
      %{type: :document, children: children}
      when is_list(children) and unsupported_glyphs in [:replace, :error] and
             is_binary(replacement_character) ->
        validate_font_coverage_nodes(children, unsupported_glyphs, replacement_character)

      _ ->
        invalid_styled_document("font fallback requires prepared font candidates")
    end
  end

  @doc false
  @spec aspect_ratio_dimension(number(), number(), :width | :height, atom()) :: number()
  def aspect_ratio_dimension(size, ratio, axis, stage) do
    try do
      dimension = if axis == :height, do: size / ratio, else: size * ratio
      check_render_resource(:max_css_numeric_magnitude, abs(dimension), stage)
      dimension
    rescue
      ArithmeticError ->
        throw(
          {:render_resource_limit,
           Diagnostics.error(
             stage,
             :resource_limit_exceeded,
             "aspect-ratio produces an unrepresentable #{axis}; reduce the dimensions or use a less extreme ratio",
             source: "aspect-ratio",
             module: __MODULE__
           )}
        )
    end
  end

  @doc false
  @spec parse_aspect_ratio(String.t()) :: {:ok, float()} | :error
  def parse_aspect_ratio(value) do
    tokens = value |> String.trim() |> String.split("/", trim: true) |> Enum.map(&String.trim/1)

    case tokens do
      [ratio] ->
        case parse_css_number(ratio) do
          {number, ""} when number > 0 -> {:ok, number}
          _ -> :error
        end

      [width, height] ->
        with {width, ""} when width > 0 <- parse_css_number(width),
             {height, ""} when height > 0 <- parse_css_number(height) do
          ratio = aspect_ratio_dimension(width, height, :height, :css)
          if ratio > 0, do: {:ok, ratio}, else: :error
        else
          _ -> :error
        end

      _ ->
        :error
    end
  end

  @doc false
  @spec parse_css_number(term(), number()) :: {float(), binary()} | :error
  def parse_css_number(value, scale \\ 1.0) do
    limit = Limits.get(:max_css_numeric_magnitude)

    case is_binary(value) and is_number(scale) do
      true ->
        case Regex.run(~r/\A([+-]?\d+)(\.\d+)?([eE][+-]?\d+)?(.*)\z/s, value) do
          [_, integer, fraction, exponent, rest] ->
            fraction = if fraction == "", do: ".0", else: fraction

            try do
              number = :erlang.binary_to_float(integer <> fraction <> exponent)

              with true <- abs(number) <= limit and abs(scale) <= limit,
                   scaled = number * scale,
                   true <- abs(scaled) <= limit do
                {scaled, rest}
              else
                false -> :error
              end
            rescue
              ArgumentError -> :error
            end

          _ ->
            :error
        end

      false ->
        :error
    end
  end

  @doc false
  @spec validate_font_configs(term()) :: :ok | :error
  def validate_font_configs(fonts) do
    valid? =
      is_list(fonts) and
        Enum.all?(fonts, fn font ->
          case font do
            %{family: family, path: paths, weight: weight, style: style}
            when map_size(font) == 4 and is_binary(family) and is_list(paths) and
                   is_number(weight) and weight >= 100 and weight <= 900 and
                   style in [:normal, :italic] ->
              String.trim(family) != "" and paths != [] and
                Enum.all?(paths, &(is_binary(&1) and String.trim(&1) != ""))

            %{family: family, data: candidates, weight: weight, style: style}
            when map_size(font) == 4 and is_binary(family) and is_list(candidates) and
                   is_number(weight) and weight >= 100 and weight <= 900 and
                   style in [:normal, :italic] ->
              String.trim(family) != "" and candidates != [] and
                Enum.all?(candidates, &(is_binary(&1) and byte_size(&1) > 0))

            _ ->
              false
          end
        end)

    case valid? do
      true -> :ok
      false -> :error
    end
  end

  @doc false
  @spec validate_font_embedding(String.t(), term(), term()) ::
          :ok | {:error, {:invalid_document, Diagnostics.diagnostic()}}
  def validate_font_embedding(family, embedding_flags, variable_font?) do
    restricted? =
      is_integer(embedding_flags) and
        (Bitwise.band(embedding_flags, 0x0002) != 0 or
           Bitwise.band(embedding_flags, 0x0200) != 0)

    case {variable_font?, restricted?} do
      {true, _restricted?} ->
        Diagnostics.error(
          :font,
          :invalid_document,
          "font #{inspect(family)} uses OpenType variations; provide a static font instance for PDF embedding",
          source: family
        )

      {_variable_font?, true} ->
        Diagnostics.error(
          :font,
          :invalid_document,
          "font #{inspect(family)} does not permit outline embedding in a PDF",
          source: family
        )

      {_variable_font?, false} ->
        :ok
    end
  end

  defp local_resource_base_path(base_url) do
    case base_url do
      base_url when is_binary(base_url) ->
        case URI.parse(base_url) do
          %URI{scheme: nil, host: nil, path: path, query: nil, fragment: nil}
          when is_binary(path) and path != "" ->
            {:ok, path}

          %URI{scheme: "file", host: host, path: path, query: nil, fragment: nil}
          when host in [nil, "", "localhost"] and is_binary(path) and path != "" ->
            {:ok, path}

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp symlink_free_resource_path?(base_path, relative) do
    relative
    |> Path.split()
    |> Enum.reduce_while(base_path, fn component, current_path ->
      path = Path.join(current_path, component)

      case File.lstat(path) do
        {:ok, %File.Stat{type: :symlink}} -> {:halt, false}
        {:ok, _stat} -> {:cont, path}
        {:error, _reason} -> {:halt, false}
      end
    end)
    |> is_binary()
  end

  defp validate_render_options(opts, font_options_result) do
    case Keyword.keyword?(opts) do
      true ->
        unknown =
          opts
          |> Keyword.keys()
          |> Enum.reject(&(&1 in @render_option_keys))
          |> Enum.uniq()
          |> Enum.sort()

        case unknown do
          [] ->
            if Keyword.get(opts, :forms, :interactive) in [:interactive, :static] do
              validate_style_options(opts, font_options_result)
            else
              Diagnostics.error(
                :options,
                :invalid_options,
                "forms must be :interactive or :static"
              )
            end

          unknown ->
            Diagnostics.error(
              :options,
              :invalid_options,
              "render options contain unsupported keys: #{inspect(unknown)}"
            )
        end

      false ->
        Diagnostics.error(:options, :invalid_options, "render options must be a keyword list")
    end
  end

  defp validate_style_options(opts, font_options_result) do
    case Keyword.keyword?(opts) do
      true ->
        with :ok <- validate_stylesheets(Keyword.get(opts, :stylesheets, [])),
             :ok <-
               validate_asset_options(
                 Keyword.get(opts, :assets, %{}),
                 Keyword.get(opts, :asset_resolver)
               ),
             :ok <- validate_base_url(Keyword.get(opts, :base_url)),
             :ok <- validate_default_font(Keyword.get(opts, :default_font, "DejaVu Sans")),
             :ok <-
               validate_system_font_discovery(Keyword.get(opts, :system_font_discovery, true)),
             :ok <- validate_unsupported_glyphs(Keyword.get(opts, :unsupported_glyphs, :replace)),
             :ok <- validate_outline_option(Keyword.get(opts, :outlines)),
             :ok <- validate_font_options_result(font_options_result) do
          :ok
        end

      false ->
        Diagnostics.error(:style, :invalid_document, "style options must be a keyword list")
    end
  end

  defp validate_outline_option(outlines) do
    case is_nil(outlines) or outlines == false or outlines == :headings or is_list(outlines) do
      true ->
        :ok

      false ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "outlines must be :headings, a list, false, or nil"
        )
    end
  end

  defp validate_stylesheets(stylesheets) do
    valid? =
      is_list(stylesheets) and
        Enum.all?(stylesheets, fn stylesheet ->
          case stylesheet do
            {:css, css} when is_binary(css) -> true
            {:file, path} when is_binary(path) -> true
            _ -> false
          end
        end)

    case valid? do
      true ->
        :ok

      false ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "stylesheets option must be a list of {:css, css} or {:file, path} tuples"
        )
    end
  end

  defp validate_base_url(base_url) do
    case is_nil(base_url) or is_binary(base_url) do
      true -> :ok
      false -> invalid_style_configuration()
    end
  end

  defp validate_default_font(default_font) do
    valid? =
      case default_font do
        font when is_binary(font) -> String.trim(font) != ""
        fonts when is_list(fonts) -> fonts != [] and Enum.all?(fonts, &is_binary/1)
        _ -> false
      end

    case valid? do
      true -> :ok
      false -> invalid_style_configuration()
    end
  end

  defp validate_unsupported_glyphs(unsupported_glyphs) do
    case unsupported_glyphs do
      unsupported_glyphs when unsupported_glyphs in [:replace, :error] ->
        :ok

      _ ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "unsupported_glyphs must be :replace or :error"
        )
    end
  end

  defp validate_system_font_discovery(system_font_discovery) do
    case is_boolean(system_font_discovery) do
      true ->
        :ok

      false ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "system_font_discovery must be true or false"
        )
    end
  end

  defp validate_font_options_result(font_options_result) do
    with {:ok, normalized_opts} <- font_options_result,
         fonts <- Keyword.fetch!(normalized_opts, :fonts),
         :ok <- validate_font_configs(fonts) do
      :ok
    else
      _ -> invalid_style_configuration()
    end
  end

  defp invalid_style_configuration do
    Diagnostics.error(
      :style,
      :invalid_document,
      "document style validation failed; check fonts, images, attributes, and supported CSS values"
    )
  end

  # Walk before recursive semantic validation, without creating a flattened tree.
  defp validate_render_tree(nodes, depth \\ 0, count \\ 0) do
    check_render_resource(:max_html_depth, depth, :html)

    Enum.reduce(nodes, count, fn node, count ->
      check_render_resource(:max_html_nodes, count + 1, :html)

      case node do
        %{children: children} when is_list(children) ->
          validate_render_tree(children, depth + 1, count + 1)

        %{text: text} when is_binary(text) ->
          check_render_resource(:max_html_source_bytes, byte_size(text), :html)
          count + 1

        _ ->
          count + 1
      end
    end)
  end

  defp validate_dom(node) do
    case node do
      %{type: :document, children: children} when is_list(children) ->
        validate_render_tree(children)
        validate_dom_nodes(children)

      _ ->
        :invalid_document
    end
  end

  defp validate_dom_nodes(nodes) do
    case Enum.all?(nodes, fn node ->
           case node do
             %{type: :text, text: text} ->
               is_binary(text)

             %{type: :element, tag: tag, attributes: attributes, children: children} ->
               is_binary(tag) and is_map(attributes) and is_list(children) and
                 validate_dom_nodes(children) == :ok

             _ ->
               false
           end
         end) do
      true -> :ok
      false -> :invalid_document
    end
  end

  defp validate_layout_nodes(nodes) do
    case Enum.all?(nodes, fn node ->
           case node do
             %{type: :text, text: text, style: style} ->
               is_binary(text) and String.valid?(text) and valid_layout_style?(style)

             %{type: :element, style: %{display: display} = style}
             when display in [:image, :none] ->
               valid_layout_style?(style)

             %{type: :element, style: style, children: children} ->
               valid_layout_style?(style) and is_list(children) and
                 validate_layout_nodes(children) == :ok

             _ ->
               false
           end
         end) do
      true -> :ok
      false -> :invalid_layout
    end
  end

  defp valid_layout_style?(style) do
    # Collapsed edge metadata is created by Layout after input validation.
    # Incoming styled trees must not supply internal border calculations.
    is_map(style) and not Map.has_key?(style, :_collapsed_borders)
  end

  defp invalid_pagination do
    Diagnostics.error(
      :pagination,
      :invalid_layout,
      "pagination requires a positive page size and margins that leave a positive printable area"
    )
  end

  defp valid_pages?(pages, page_size) do
    Enum.all?(pages, fn page ->
      case page do
        %{size: ^page_size, boxes: boxes} when is_list(boxes) -> true
        _ -> false
      end
    end)
  end

  defp validate_furniture_result(furniture_result) do
    case furniture_result do
      {:ok, normalized} ->
        validate_furniture_option(normalized)

      {:error, :invalid_furniture_container} ->
        invalid_furniture_options("page_furniture must be a keyword list or map")

      {:error, {:unknown_furniture_keys, unknown}} ->
        invalid_furniture_options("page_furniture contains unsupported keys: #{inspect(unknown)}")

      {:error, {:unknown_variant_keys, position, unknown}} ->
        invalid_furniture_options(
          "#{position} page furniture contains unsupported keys: #{inspect(unknown)}"
        )

      {:error, {:invalid_variants, position}} ->
        invalid_furniture_options(
          "#{position} page furniture must be HTML or default/first/odd/even HTML variants"
        )
    end
  end

  @doc false
  @spec validate_furniture_option(term()) ::
          :ok | {:error, {:invalid_options, Diagnostics.diagnostic()}}
  def validate_furniture_option(furniture) do
    valid? =
      case furniture do
        nil ->
          true

        %{header: header, footer: footer} when map_size(furniture) == 2 ->
          Enum.all?([header, footer], fn variants ->
            is_map(variants) and
              Enum.all?(variants, fn {variant, template} ->
                variant in @variant_keys and (is_binary(template) or template in [nil, false])
              end)
          end)

        _ ->
          false
      end

    case valid? do
      true -> :ok
      false -> invalid_furniture_options("page_furniture normalization produced invalid data")
    end
  end

  defp invalid_furniture_options(message) do
    Diagnostics.error(:options, :invalid_options, message)
  end

  defp invalid_furniture_layout do
    Diagnostics.error(
      :layout,
      :invalid_layout,
      "page furniture requires valid pages and matching page geometry"
    )
  end

  defp validate_font_input_nodes(nodes) do
    Enum.reduce_while(nodes, :ok, fn node, :ok ->
      case node do
        %{
          type: :text,
          text: text,
          style: %{
            _font_registry: registry,
            font_face: selected,
            font_families: families,
            font_weight: weight,
            font_style: font_style
          }
        }
        when is_binary(text) and is_map(registry) and is_map(selected) and is_list(families) and
               is_number(weight) and font_style in [:normal, :italic] ->
          case String.valid?(text) do
            true ->
              {:cont, :ok}

            false ->
              {:halt,
               Diagnostics.error(:font, :invalid_encoding, "styled text must be valid UTF-8")}
          end

        %{type: :element, children: children} when is_list(children) ->
          case validate_font_input_nodes(children) do
            :ok -> {:cont, :ok}
            {:error, {_reason, _diagnostic}} = error -> {:halt, error}
          end

        %{type: :text} ->
          {:halt, invalid_styled_document("styled text is missing its resolved font registry")}

        _ ->
          {:halt, invalid_styled_document("font fallback encountered an invalid styled node")}
      end
    end)
  end

  defp validate_font_coverage_nodes(nodes, unsupported_glyphs, replacement_character) do
    Enum.reduce_while(nodes, :ok, fn node, :ok ->
      case node do
        %{type: :text, _font_candidates: candidates, _font_graphemes: graphemes}
        when is_list(candidates) and candidates != [] and is_list(graphemes) ->
          case validate_supported_graphemes(
                 graphemes,
                 candidates,
                 unsupported_glyphs,
                 replacement_character
               ) do
            :ok -> {:cont, :ok}
            {:error, {_reason, _diagnostic}} = error -> {:halt, error}
          end

        %{type: :element, children: children} when is_list(children) ->
          case validate_font_coverage_nodes(
                 children,
                 unsupported_glyphs,
                 replacement_character
               ) do
            :ok -> {:cont, :ok}
            {:error, {_reason, _diagnostic}} = error -> {:halt, error}
          end

        _ ->
          {:halt, invalid_styled_document("font fallback requires prepared font candidates")}
      end
    end)
  end

  defp validate_supported_graphemes(
         graphemes,
         candidates,
         unsupported_glyphs,
         replacement_character
       ) do
    Enum.reduce_while(graphemes, :ok, fn grapheme, :ok ->
      case grapheme do
        %{text: text, layout_whitespace?: layout_whitespace?}
        when is_binary(text) and is_boolean(layout_whitespace?) ->
          supported? = layout_whitespace? or Enum.any?(candidates, &Font.supports_text?(&1, text))

          replacement_supported? =
            unsupported_glyphs == :replace and
              Enum.any?(candidates, &Font.supports_text?(&1, replacement_character))

          case supported? or replacement_supported? do
            true -> {:cont, :ok}
            false -> {:halt, unsupported_glyph(text)}
          end

        _ ->
          {:halt, invalid_styled_document("font fallback requires prepared graphemes")}
      end
    end)
  end

  defp invalid_styled_document(message) do
    Diagnostics.error(:font, :invalid_document, message)
  end

  defp unsupported_glyph(grapheme) do
    codepoints =
      grapheme
      |> String.to_charlist()
      |> Enum.map_join(
        " ",
        &("U+" <> (&1 |> Integer.to_string(16) |> String.upcase() |> String.pad_leading(4, "0")))
      )

    Diagnostics.error(
      :font,
      :unsupported_glyph,
      "no requested, configured, or bundled font contains every glyph in #{inspect(grapheme)} (#{codepoints})",
      source: grapheme
    )
  end

  defp format_number(number) do
    number
    |> Kernel.*(100)
    |> round()
    |> Kernel./(100)
    |> to_string()
  end
end
