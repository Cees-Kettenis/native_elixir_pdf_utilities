defmodule NativeElixirPdfUtilities.Validators.HtmlSelectorStateValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    HtmlUrlValidator,
    HtmlUrlIdnaValidator
  }

  @doc "Matches authored initial selector states without simulating user interaction."
  @spec matches?(atom(), map(), [map()], map()) :: boolean()
  def matches?(kind, node, _ancestors, context) do
    html = Map.get(node, :namespace, :html) == :html
    attributes = Map.get(node, :attributes, %{})
    state = Map.get(node, :_initial_form_state, %{})

    case kind do
      kind when kind in [:any_link, :link, :local_link] ->
        link =
          (html and node.tag in ["a", "area"] and Map.has_key?(attributes, "href")) or
            (Map.get(node, :namespace) == :svg and node.tag == "a" and
               (Map.has_key?(attributes, "href") or Map.has_key?(attributes, "xlink:href")))

        if link and kind == :local_link do
          href = Map.get(attributes, "href", Map.get(attributes, "xlink:href", ""))

          url =
            Map.get(context, :url_context, %{
              fallback_base: nil,
              resolution_base: nil,
              authored_base?: false
            })

          match?({:ok, {:fragment, _}}, HtmlUrlValidator.prepare_anchor(href, url))
        else
          link
        end

      :unchecked ->
        html and
          ((node.tag == "input" and
              HtmlValidator.input_type(Map.get(attributes, "type", "text")) in [
                "checkbox",
                "radio"
              ] and not Map.get(state, :indeterminate, false)) or node.tag == "option") and
          not HtmlValidator.control_state_matches?(:checked, node)

      :defined ->
        not (html and
               (HtmlValidator.custom_html_tag?(node.tag) or
                  HtmlValidator.custom_html_tag?(Map.get(attributes, "is"))))

      :target ->
        case Map.get(context, :initial_target) do
          {_document, _tree, id} -> Map.get(node, :_selector_id) == id
          nil -> false
        end

      :open ->
        html and node.tag in ["details", "dialog"] and Map.has_key?(attributes, "open")

      :placeholder_shown ->
        html and Map.get(state, :placeholder_shown, false)

      :default ->
        html and
          case node.tag do
            "option" ->
              Map.has_key?(attributes, "selected")

            "input" ->
              (HtmlValidator.input_type(Map.get(attributes, "type", "text")) in [
                 "checkbox",
                 "radio"
               ] and Map.has_key?(attributes, "checked")) or
                Map.get(state, :default_button, false)

            "button" ->
              Map.get(state, :default_button, false)

            _ ->
              false
          end

      kind when kind in [:in_range, :out_of_range] ->
        numeric = Map.get(state, :number) || Map.get(state, :temporal) || %{}

        range =
          html and node.tag == "input" and not Map.get(state, :disabled, false) and
            (Map.has_key?(state, :range) or Map.get(numeric, :min) != nil or
               Map.get(numeric, :max) != nil)

        outside =
          Map.get(numeric, :range_underflow, false) or Map.get(numeric, :range_overflow, false)

        range and if(kind == :out_of_range, do: outside, else: not outside)

      # Initial documents have no history, user edits, pointer, focus or target activation.
      _ ->
        false
    end
  end

  @doc "Resolves HTML directionality, including auto text and inherited direction."
  @spec direction(map(), [map()]) :: String.t()
  def direction(node, ancestors) do
    attributes = Map.get(node, :attributes, %{})
    value = attributes |> Map.get("dir", "") |> String.downcase(:ascii)
    auto = value == "auto" or (value not in ["ltr", "rtl"] and Map.get(node, :tag) == "bdi")

    cond do
      value in ["ltr", "rtl"] ->
        value

      auto ->
        text =
          if node.tag in ["input", "textarea"],
            do: get_in(node, [:_initial_form_state, :value]) || Map.get(attributes, "value", ""),
            else: directional_text(Map.get(node, :children, []))

        HtmlValidator.reserve_render_resource(:max_css_work, byte_size(text), :css)

        Enum.find_value(String.to_charlist(text), &HtmlUrlIdnaValidator.strong_direction/1) ||
          inherited_direction(ancestors)

      node.tag == "input" and
          HtmlValidator.input_type(Map.get(attributes, "type", "text")) == "tel" ->
        "ltr"

      true ->
        inherited_direction(ancestors)
    end
  end

  @doc "Keeps right-to-left painting gated while allowing initial direction selector matching."
  @spec validate_paint_direction(map(), [map()]) :: :ok | {:error, {atom(), map()}}
  def validate_paint_direction(node, ancestors) do
    if Map.has_key?(node.attributes, "dir") and direction(node, ancestors) == "rtl" do
      NativeElixirPdfUtilities.Diagnostics.error(
        :style,
        :not_implemented,
        "right-to-left direction is available for selector matching; bidirectional layout and painting are not implemented yet",
        module: __MODULE__,
        source: "dir=" <> node.attributes["dir"]
      )
    else
      :ok
    end
  end

  defp inherited_direction(ancestors) do
    case ancestors do
      [parent | rest] -> direction(parent, rest)
      [] -> "ltr"
    end
  end

  defp directional_text(children) do
    Enum.map_join(children, fn child ->
      HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)

      case child do
        %{type: :text, text: text} ->
          text

        %{type: :element, tag: tag, attributes: attributes, children: children} ->
          if tag in ["bdi", "script", "style", "textarea"] or
               String.downcase(Map.get(attributes, "dir", ""), :ascii) in ["ltr", "rtl", "auto"],
             do: "",
             else: directional_text(children)

        _ ->
          ""
      end
    end)
  end
end
