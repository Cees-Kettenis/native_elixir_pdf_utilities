defmodule NativeElixirPdfUtilities.Validators.HtmlVisibilityValidator do
  @moduledoc "Initial HTML hidden-until-found content and box applicability."
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Returns applicable initial hidden content defaults while preserving source attributes."
  @spec defaults(map()) :: map()
  def defaults(node) do
    if Map.get(node, :namespace, :html) == :html and
         is_nil(Map.get(Map.get(node, :attribute_namespaces, %{}), "hidden")) do
      case HtmlValidator.html_hidden_state(Map.get(node, :attributes, %{})) do
        :until_found ->
          if Map.get(node, :tag) == "embed", do: %{}, else: %{content_visibility: :hidden}

        :hidden ->
          %{display: :none}

        :not_hidden ->
          %{}
      end
    else
      %{}
    end
  end

  @doc "Records final content-skipping eligibility after the actual CSS cascade."
  @spec prepare_style(map()) :: map()
  def prepare_style(style) do
    if Map.get(style, :content_visibility, :visible) == :hidden and
         if(Map.get(style, :display) == :image,
           do: Map.get(style, :image_display, :inline),
           else: Map.get(style, :display, :inline)
         ) not in [:inline, :contents],
       do: Map.put(style, :_html_content_skipped, true),
       else: Map.delete(style, :_html_content_skipped)
  end

  @doc "Validates canonical skipped-content transport before layout execution."
  @spec validate_layout_metadata(term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_layout_metadata(node) do
    # The enclosing layout validator owns missing or malformed style structure.
    style =
      case node do
        %{style: style} when is_map(style) -> style
        _ -> %{}
      end

    if Map.get(style, :content_visibility) in [nil, :visible] and
         not Map.has_key?(style, :_html_content_skipped) do
      :ok
    else
      expected = prepare_style(Map.delete(style, :_html_content_skipped))

      if Map.get(style, :content_visibility) in [nil, :visible, :hidden] and
           Map.fetch(style, :_html_content_skipped) == Map.fetch(expected, :_html_content_skipped) and
           (not Map.has_key?(expected, :_html_content_skipped) or
              (Map.get(node, :children, []) == [] and not Map.has_key?(style, :_form_control) and
                 not Enum.any?(
                   [
                     :_temporal_state,
                     :_temporal_display,
                     :_file_display,
                     :_image_input_state,
                     :image
                   ],
                   &Map.has_key?(style, &1)
                 ))) do
        :ok
      else
        Diagnostics.error(
          :layout,
          :invalid_document,
          "content visibility requires canonical final box applicability and empty skipped painted content",
          module: __MODULE__,
          operation: :layout,
          source: Map.get(node, :tag, "text")
        )
      end
    end
  end
end
