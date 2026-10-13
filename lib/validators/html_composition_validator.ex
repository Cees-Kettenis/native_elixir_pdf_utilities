defmodule NativeElixirPdfUtilities.Validators.HtmlCompositionValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{HtmlTreeValidator, HtmlValidator}

  # The eligible hosts are fixed by HTML, not a tunable resource limit.
  @hosts ~w(article aside blockquote body div footer h1 h2 h3 h4 h5 h6 header main nav p section span)

  @doc false
  @spec validate_attributes(map()) :: :ok | {:error, {atom(), map()}}
  def validate_attributes(attributes) do
    if Map.has_key?(attributes, "for") do
      Diagnostics.error(
        :html,
        :unsupported_html,
        "template replacement and declarative shadow composition cannot be combined",
        module: __MODULE__,
        operation: :compose,
        source: "template"
      )
    else
      :ok
    end
  end

  @doc false
  @spec shadow_configuration(map(), map()) :: map() | nil
  def shadow_configuration(host, template) do
    attributes = Map.get(template, :attributes, %{})
    mode = String.downcase(Map.get(attributes, "shadowrootmode", ""), :ascii)
    host_tag = Map.get(host, :tag, "")

    if Map.get(host, :namespace, :html) == :html and
         (host_tag in @hosts or HtmlValidator.custom_html_tag?(host_tag)) and
         Map.get(template, :namespace, :html) == :html and
         Map.get(template, :tag) == "template" and mode in ["open", "closed"] do
      %{
        mode: mode,
        slot_assignment:
          if(
            String.downcase(Map.get(attributes, "shadowrootslotassignment", ""), :ascii) ==
              "manual",
            do: :manual,
            else: :named
          ),
        delegates_focus: Map.has_key?(attributes, "shadowrootdelegatesfocus"),
        clonable: Map.has_key?(attributes, "shadowrootclonable"),
        serializable: Map.has_key?(attributes, "shadowrootserializable")
      }
    end
  end

  @doc false
  @spec composition_required?([map()]) :: boolean()
  def composition_required?(children) do
    Enum.any?(children, fn node ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

      case node do
        %{type: :element, tag: "template"} ->
          false

        %{type: :element, children: children} ->
          Enum.any?(children, &(not is_nil(shadow_configuration(node, &1)))) or
            composition_required?(children)

        _ ->
          false
      end
    end)
  end

  @doc false
  @spec validate_input(term()) :: :ok | {:error, {atom(), map()}}
  def validate_input(dom) do
    case dom do
      %{type: type, children: children}
      when type in [:document, :document_fragment] and is_list(children) ->
        with :ok <- HtmlTreeValidator.validate_transport(dom),
             do: HtmlValidator.validate_composition_document(dom)

      _ ->
        Diagnostics.error(
          :html,
          :invalid_document,
          "shadow composition requires a document or document fragment tree",
          module: __MODULE__,
          operation: :compose
        )
    end
  end

  @doc false
  @spec validate_active_children([map()]) :: :ok | {:error, {atom(), map()}}
  def validate_active_children(children), do: HtmlValidator.validate_composed_fragment(children)
end
