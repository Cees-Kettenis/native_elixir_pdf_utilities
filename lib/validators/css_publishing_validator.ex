defmodule NativeElixirPdfUtilities.Validators.CssPublishingValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    CssCounterStyleValidator,
    CssGeneratedContentValidator,
    HtmlValidator
  }

  @properties ~w(page running string-set footnote-display footnote-policy float-reference float-defer float-offset bookmark-level bookmark-label bookmark-state box-decoration-break margin-break)
  @doc "Returns the supported static publishing properties."
  @spec properties() :: [String.t()]
  def properties, do: @properties

  @doc "Prepares publishing declarations without performing layout or resource loading."
  @spec prepare(map(), String.t(), String.t()) :: {:ok, map()} | {:error, atom()}
  def prepare(style, property, source) do
    value = String.trim(source)

    fields =
      case {property, value} do
        {"page", "auto"} ->
          {:ok, %{page_name: ""}}

        {"page", name} ->
          if CssCounterStyleValidator.identifier?(name),
            do: {:ok, %{page_name: name}},
            else: :error

        {"position", value} ->
          case Regex.run(~r/^running\(\s*([\w-]+)\s*\)$/u, value) do
            [_, name] ->
              {:ok, %{position: :static, running_name: name, running_copy: false}}

            _ ->
              :error
          end

        {"running", "none"} ->
          {:ok, %{running_name: nil, running_copy: false}}

        {"running", name} ->
          if CssCounterStyleValidator.identifier?(name),
            do: {:ok, %{running_name: name, running_copy: true}},
            else: :error

        {"float", "footnote"} ->
          {:ok, %{float: :none, publishing_float: :footnote}}

        {"float", value}
        when value in ~w(top bottom page-top page-bottom column-top column-bottom block-start block-end) ->
          {:ok, %{float: :none, publishing_float: value}}

        {"string-set", "none"} ->
          {:ok, %{string_set: []}}

        {"string-set", value} ->
          entries =
            Enum.map(HtmlValidator.split_css_comma(value, true), fn text ->
              case String.split(text, ~r/\s+/u, parts: 2) do
                [name, content] ->
                  with true <- CssCounterStyleValidator.identifier?(name),
                       {:ok, parts} when is_list(parts) <-
                         CssGeneratedContentValidator.prepare(content),
                       true <- CssGeneratedContentValidator.valid_context?(parts, :string_set),
                       do: {name, parts}

                _ ->
                  :error
              end
            end)

          if entries != [] and Enum.all?(entries, &(is_tuple(&1) and tuple_size(&1) == 2)),
            do: {:ok, %{string_set: entries}},
            else: :error

        {"footnote-display", value} when value in ~w(block inline compact) ->
          {:ok, %{footnote_display: value}}

        {"footnote-policy", value} when value in ~w(auto line block) ->
          {:ok, %{footnote_policy: value}}

        {"float-reference", value} when value in ~w(inline column page region) ->
          {:ok, %{float_reference: value}}

        {"float-defer", "none"} ->
          {:ok, %{float_defer: 0}}

        {"float-defer", "last"} ->
          {:ok, %{float_defer: :last}}

        {"float-defer", value} ->
          case NativeElixirPdfUtilities.Validators.CssMathValidator.prepare_scalar(
                 value,
                 :integer
               ) do
            {:ok, n} ->
              HtmlValidator.check_render_resource(:max_pages, abs(n), :style)
              {:ok, %{float_defer: n}}

            _ ->
              :error
          end

        {"float-offset", value} ->
          case HtmlValidator.parse_css_number(value) do
            {number, "%"} ->
              {:ok, %{float_offset: {:percent, number / 100}}}

            _ ->
              case HtmlValidator.parse_css_length(
                     value,
                     :margin,
                     Map.get(style, :font_size, 12.0)
                   ) do
                {:ok, n} when is_number(n) -> {:ok, %{float_offset: n}}
                _ -> :error
              end
          end

        {"bookmark-level", "none"} ->
          {:ok, %{bookmark_level: nil}}

        {"bookmark-level", value} ->
          case NativeElixirPdfUtilities.Validators.CssMathValidator.prepare_scalar(
                 value,
                 :integer
               ) do
            {:ok, n} when n > 0 ->
              HtmlValidator.check_render_resource(:max_depth, n, :style)
              {:ok, %{bookmark_level: n}}

            _ ->
              :error
          end

        {"bookmark-label", value} ->
          with {:ok, parts} <- CssGeneratedContentValidator.prepare(value),
               true <- CssGeneratedContentValidator.valid_context?(parts, :bookmark) do
            {:ok, %{bookmark_label: if(parts == :none, do: [{:content, "text"}], else: parts)}}
          else
            _ -> :error
          end

        {"bookmark-state", value} when value in ~w(open closed) ->
          {:ok, %{bookmark_state: value}}

        {"box-decoration-break", value} when value in ~w(slice clone) ->
          {:ok, %{box_decoration_break: value}}

        {"margin-break", value} when value in ~w(auto keep discard) ->
          {:ok, %{margin_break: value}}

        _ ->
          :error
      end

    case fields do
      {:ok, fields} -> {:ok, Map.merge(style, fields)}
      :error -> {:error, :invalid_document}
    end
  end

  @doc "Checks publishing fields on caller-supplied computed styles before layout."
  @spec valid_style?(map()) :: boolean()
  def valid_style?(style) do
    case style do
      style
      when not is_map_key(style, :footnote_display) and
             not is_map_key(style, :footnote_policy) and
             not is_map_key(style, :float_reference) and
             not is_map_key(style, :bookmark_state) and
             not is_map_key(style, :box_decoration_break) and
             not is_map_key(style, :margin_break) and
             not is_map_key(style, :page_name) and
             not is_map_key(style, :running_name) and
             not is_map_key(style, :running_copy) and
             not is_map_key(style, :bookmark_level) and
             not is_map_key(style, :float_defer) and
             not is_map_key(style, :float_offset) and
             not is_map_key(style, :publishing_float) and
             not is_map_key(style, :string_set) and
             not is_map_key(style, :bookmark_label) ->
        true

      _ ->
        enums = %{
          footnote_display: ~w(block inline compact),
          footnote_policy: ~w(auto line block),
          float_reference: ~w(inline column page region),
          bookmark_state: ~w(open closed),
          box_decoration_break: ~w(slice clone),
          margin_break: ~w(auto keep discard)
        }

        Enum.all?(enums, fn {key, allowed} ->
          not Map.has_key?(style, key) or Map.get(style, key) in allowed
        end) and
          Enum.all?([:page_name, :running_name], fn key ->
            case Map.fetch(style, key) do
              :error ->
                true

              {:ok, nil} ->
                key == :running_name

              {:ok, value} ->
                is_binary(value) and (value == "" or CssCounterStyleValidator.identifier?(value))
            end
          end) and
          (not Map.has_key?(style, :running_copy) or is_boolean(style.running_copy)) and
          (not Map.has_key?(style, :bookmark_level) or style.bookmark_level == nil or
             (is_integer(style.bookmark_level) and style.bookmark_level > 0)) and
          (not Map.has_key?(style, :float_defer) or style.float_defer == :last or
             is_integer(style.float_defer)) and
          (not Map.has_key?(style, :float_offset) or is_number(style.float_offset) or
             match?({:percent, n} when is_number(n), style.float_offset)) and
          (not Map.has_key?(style, :publishing_float) or
             style.publishing_float in [
               :footnote
               | ~w(top bottom page-top page-bottom column-top column-bottom block-start block-end)
             ]) and
          (not Map.has_key?(style, :string_set) or
             case style.string_set do
               entries when is_list(entries) and length(entries) >= 0 ->
                 Enum.all?(entries, fn
                   {name, parts} ->
                     CssCounterStyleValidator.identifier?(name) and
                       CssGeneratedContentValidator.valid_parts?(parts) and
                       CssGeneratedContentValidator.valid_context?(parts, :string_set)

                   _ ->
                     false
                 end)

               _ ->
                 false
             end) and
          (not Map.has_key?(style, :bookmark_label) or
             (CssGeneratedContentValidator.valid_parts?(style.bookmark_label) and
                CssGeneratedContentValidator.valid_context?(style.bookmark_label, :bookmark)))
    end
  end

  @doc "Resolves a footnote area's authored maximum against the actual page capacity."
  @spec footnote_capacity(map(), number(), number(), number()) ::
          {:ok, number()} | {:error, term()}
  def footnote_capacity(style, available, full, line) do
    maximum =
      case Map.get(style, :max_height) do
        n when is_number(n) -> n
        {:percent, ratio} -> full * ratio
        _ -> full
      end

    capacity = min(available, maximum)

    if capacity >= line,
      do: {:ok, capacity},
      else:
        failure(
          "the footnote area cannot contain one line; increase its max-height or the printable area",
          maximum
        )
  end

  @doc "Reports a document-dependent publishing failure using the shared contract."
  @spec failure(String.t(), term()) :: {:error, {atom(), Diagnostics.diagnostic()}}
  def failure(message, source) do
    Diagnostics.error(:pagination, :invalid_layout, message,
      operation: :paginate,
      module: __MODULE__,
      source: inspect(source)
    )
  end

  @doc "Checks that another convergence pass can make progress within shared work and depth budgets."
  @spec convergence(non_neg_integer(), term(), map()) :: :ok | {:error, term()}
  def convergence(iteration, signature, seen) do
    HtmlValidator.check_render_resource(:max_depth, iteration, :pagination)
    HtmlValidator.reserve_render_resource(:max_work, 1, :pagination)

    if Map.has_key?(seen, signature),
      do:
        failure(
          "publishing layout repeats an earlier state; simplify cyclic page references, footnotes or floats",
          signature
        ),
      else: :ok
  end
end
