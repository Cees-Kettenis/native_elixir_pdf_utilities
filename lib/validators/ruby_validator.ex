defmodule NativeElixirPdfUtilities.Validators.RubyValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Rejects caller-supplied ruby measurements, which are created only inside inline layout."
  @spec validate_layout_metadata(term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_layout_metadata(node) do
    case node do
      %{style: %{_ruby_box: _}} ->
        failure("ruby measurements are internal inline state; pass ordinary styled ruby children")

      _ ->
        :ok
    end
  end

  @doc "Prepares horizontal ruby base groups and annotation rows from a styled ruby element."
  @spec prepare(term()) :: {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(node) do
    case node do
      %{type: :element, tag: "ruby", style: style, children: children}
      when is_map(style) and is_list(children) ->
        if Enum.all?(children, fn child ->
             match?(
               %{type: :text, text: text, style: child_style}
               when is_binary(text) and is_map(child_style),
               child
             ) or
               match?(
                 %{type: :element, style: child_style, children: nested}
                 when is_map(child_style) and is_list(nested),
                 child
               )
           end) do
          {groups, pending} =
            Enum.reduce(children, {[], []}, fn child, {groups, pending} ->
              HtmlValidator.reserve_render_resource(
                :max_html_tree_work,
                length(groups) + length(pending) + 1,
                :layout
              )

              case child do
                %{style: %{display: :none}} ->
                  {groups, pending}

                %{tag: "rt"} ->
                  cond do
                    pending != [] ->
                      {groups ++ [%{base: pending, annotations: [child]}], []}

                    true ->
                      {groups ++ [%{base: [], annotations: [child]}], []}
                  end

                _ ->
                  {groups, pending ++ [child]}
              end
            end)

          groups =
            if pending == [], do: groups, else: groups ++ [%{base: pending, annotations: []}]

          {groups, _} =
            Enum.map_reduce(groups, "", fn group, previous ->
              text = base_text(group.base)

              break_before =
                previous == "" or Regex.match?(~r/[ \t\n\r\f]$/u, previous) or
                  Regex.match?(~r/^[ \t\n\r\f]|^[\p{Han}\p{Hiragana}\p{Katakana}]/u, text) or
                  Map.get(style, :line_break) == :anywhere

              {Map.put(group, :break_before, break_before), text}
            end)

          {:ok, groups}
        else
          failure("ruby requires styled text or element children")
        end

      _ ->
        failure("ruby measurement requires a styled ruby element with children")
    end
  catch
    {:render_resource_limit, error} -> error
  end

  defp base_text(children) do
    Enum.map_join(children, fn child ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :layout)

      case child do
        %{type: :text, text: text} -> text
        %{children: children} -> base_text(children)
      end
    end)
  end

  defp failure(message) do
    Diagnostics.error(:layout, :invalid_layout, message,
      operation: :measure_ruby,
      module: __MODULE__,
      source: "ruby"
    )
  end
end
