defmodule NativeElixirPdfUtilities.Validators.CssContainmentValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{CssMathValidator, HtmlValidator}

  @containment %{
    "size" => :size,
    "inline-size" => :inline_size,
    "layout" => :layout,
    "paint" => :paint,
    "style" => :style
  }

  @doc "Parses independent containment axes and the two containment presets."
  @spec declarations([String.t()]) :: {:ok, [atom()]} | :error
  def declarations(terms) do
    case terms do
      ["none"] ->
        {:ok, []}

      ["strict"] ->
        {:ok, [:layout, :paint, :size, :style]}

      ["content"] ->
        {:ok, [:layout, :paint, :style]}

      _ ->
        effects = terms |> Enum.map(&Map.get(@containment, &1)) |> Enum.sort()
        if effects != [] and valid_effects?(effects), do: {:ok, effects}, else: :error
    end
  end

  @doc "Validates the canonical computed containment list."
  @spec valid_effects?(term()) :: boolean()
  def valid_effects?(effects) do
    is_list(effects) and
      Enum.all?(effects, &(&1 in [:size, :inline_size, :layout, :paint, :style])) and
      effects == Enum.sort(Enum.uniq(effects)) and
      not (:size in effects and :inline_size in effects)
  end

  @doc "Parses a physical intrinsic placeholder or expands the two-axis shorthand."
  @spec intrinsic_size(map(), String.t(), String.t()) :: {:ok, map()} | :error | {:error, term()}
  def intrinsic_size(style, property, source) do
    tokens = source |> HtmlValidator.split_css_tokens() |> Enum.map(&String.downcase(&1, :ascii))

    with {:ok, first, rest} <- intrinsic_value(tokens, style) do
      case {property, rest} do
        {"contain-intrinsic-width", []} ->
          {:ok, %{contain_intrinsic_width: first}}

        {"contain-intrinsic-height", []} ->
          {:ok, %{contain_intrinsic_height: first}}

        {"contain-intrinsic-size", []} ->
          {:ok, %{contain_intrinsic_width: first, contain_intrinsic_height: first}}

        {"contain-intrinsic-size", remaining} ->
          with {:ok, second, []} <- intrinsic_value(remaining, style) do
            {:ok, %{contain_intrinsic_width: first, contain_intrinsic_height: second}}
          else
            {:ok, _, _} -> :error
            error -> error
          end

        _ ->
          :error
      end
    end
  end

  defp intrinsic_value(tokens, style) do
    # A printed document has no remembered size from a previous visibility state.
    case tokens do
      ["auto", length | remaining] when length != "auto" ->
        intrinsic_value([length | remaining], style)

      ["0" | remaining] ->
        {:ok, 0.0, remaining}

      ["none" | remaining] ->
        {:ok, 0.0, remaining}

      [length | remaining] ->
        with {:ok, %{percentage?: false}} <- CssMathValidator.length_dependencies(length),
             {:ok, prepared} <- CssMathValidator.compute_length(length, style),
             {:ok, size} when size >= 0 <- CssMathValidator.resolve_length(prepared, 0.0) do
          {:ok, size, remaining}
        else
          {:error, _} = error -> error
          _ -> :error
        end

      [] ->
        :error
    end
  end

  @doc "Computes containment on boxes to which it applies, including visibility and query effects."
  @spec effects(map()) :: [atom()]
  def effects(style) do
    case style do
      style
      when not is_map_key(style, :contain) and not is_map_key(style, :content_visibility) and
             not is_map_key(style, :container_type) ->
        []

      _ ->
        if Map.get(style, :display) in [
             :none,
             :contents,
             :inline,
             :table_section,
             :table_row,
             :table_cell,
             :table_column,
             :table_column_group
           ] do
          []
        else
          authored = Map.get(style, :contain, [])

          query =
            case Map.get(style, :container_type) do
              :size -> [:size, :style]
              :inline_size -> [:inline_size, :style]
              _ -> []
            end

          visibility =
            case Map.get(style, :content_visibility) do
              :hidden -> [:size, :layout, :paint, :style]
              :auto -> [:layout, :paint, :style]
              _ -> []
            end

          authored ++ query ++ visibility
        end
    end
  end

  @doc "Reports whether descendants establish an independent formatting and paint context."
  @spec independent?(map()) :: boolean()
  def independent?(style) do
    case style do
      style when not is_map_key(style, :contain) and not is_map_key(style, :content_visibility) ->
        false

      _ ->
        Enum.any?(effects(style), &(&1 in [:layout, :paint]))
    end
  end

  @doc "Returns the static intrinsic replacement for an axis subject to size containment."
  @spec intrinsic(map(), :width | :height) :: number() | nil
  def intrinsic(style, axis) do
    case style do
      style
      when not is_map_key(style, :contain) and not is_map_key(style, :container_type) and
             not is_map_key(style, :_html_content_skipped) ->
        nil

      _ ->
        effects = effects(style)
        vertical = Map.get(style, :writing_mode, :horizontal_tb) != :horizontal_tb

        if :size in effects or
             (:inline_size in effects and axis == if(vertical, do: :height, else: :width)) do
          case axis do
            :width -> Map.get(style, :contain_intrinsic_width, 0.0)
            :height -> intrinsic_height(style)
          end
        end
    end
  end

  defp intrinsic_height(style) do
    case style do
      %{contain_intrinsic_height: height} ->
        height

      %{content_visibility: :hidden, form_kind: kind, appearance: appearance}
      when kind in [:text, :textarea, :select, :listbox] and appearance != :none ->
        # The UA's initial host metrics remain visible when its editor skips paint.
        nil

      _ ->
        0.0
    end
  end
end
