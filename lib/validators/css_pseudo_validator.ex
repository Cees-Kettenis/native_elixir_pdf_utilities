defmodule NativeElixirPdfUtilities.Validators.CssPseudoValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.{CssSyntaxValidator, HtmlValidator}

  @kinds %{
    "before" => :before,
    "after" => :after,
    "picker-icon" => :picker_icon,
    "first-line" => :first_line,
    "first-letter" => :first_letter,
    "marker" => :marker,
    "placeholder" => :placeholder,
    "-webkit-input-placeholder" => :placeholder,
    "details-content" => :details_content,
    "backdrop" => :backdrop,
    "checkmark" => :checkmark,
    "file-selector-button" => :file_selector_button,
    "color-swatch" => :color_swatch,
    "column" => :column,
    "field-component" => :field_component,
    "field-content" => :field_content,
    "field-separator" => :field_separator,
    "field-text" => :field_text,
    "fill" => :fill,
    "slider-fill" => :slider_fill,
    "slider-thumb" => :slider_thumb,
    "slider-track" => :slider_track,
    "step-control" => :step_control,
    "step-down" => :step_down,
    "step-up" => :step_up,
    "thumb" => :thumb,
    "track" => :track
  }

  @doc "Validates a static pseudo-element name and its optional function argument."
  @spec parse(String.t(), String.t()) :: {:ok, atom() | tuple()} | :error
  def parse(name, tail) do
    case {name, tail} do
      {"first-letter", <<"::", child::binary>>} ->
        case HtmlValidator.parse_css_identifier(child) do
          {:ok, name, ""} ->
            case String.downcase(name, :ascii) do
              "prefix" -> {:ok, {:first_letter, :prefix}}
              "suffix" -> {:ok, {:first_letter, :suffix}}
              _ -> :error
            end

          _ ->
            :error
        end

      {"picker", _} ->
        with {:ok, body, ""} <- CssSyntaxValidator.function(":picker" <> tail, "picker"),
             {:ok, "select", ""} <-
               HtmlValidator.parse_css_identifier(HtmlValidator.trim_css_whitespace(body)) do
          {:ok, :picker}
        else
          _ -> :error
        end

      {"nth-fragment", _} ->
        with {:ok, body, ""} <-
               CssSyntaxValidator.function(":nth-fragment" <> tail, "nth-fragment"),
             {:ok, {:nth_child, index}, ""} <-
               HtmlValidator.parse_css_nth_pseudo_class(":nth-child(" <> body <> ")") do
          {:ok, {:nth_fragment, index}}
        else
          _ -> :error
        end

      {name, ""} ->
        case Map.fetch(@kinds, name) do
          {:ok, kind} -> {:ok, kind}
          :error -> :error
        end

      _ ->
        :error
    end
  end

  @doc "Determines the initial originating element while later layout and paint owners remain gated."
  @spec owner?(atom() | tuple(), map(), map()) :: boolean()
  def owner?(pseudo, node, style) do
    html = Map.get(node, :namespace, :html) == :html
    tag = Map.get(node, :tag)
    input = html and tag == "input"
    type = if input, do: HtmlValidator.input_type(Map.get(node.attributes, "type", "text"))

    case pseudo do
      kind when kind in [:before, :after, :first_line, :first_letter, :column] ->
        true

      {kind, _} when kind in [:nth_fragment, :first_letter] ->
        true

      :marker ->
        tag == "li" or Map.get(style, :display) == :list_item

      :placeholder ->
        html and
          (tag == "textarea" or (input and type in ~w(text search tel url email password number))) and
          get_in(node, [:_initial_form_state, :placeholder_shown]) == true

      :details_content ->
        html and tag == "details"

      :backdrop ->
        false

      kind when kind in [:picker, :picker_icon] ->
        html and tag == "select"

      :checkmark ->
        html and (tag == "option" or (input and type in ["checkbox", "radio"]))

      :file_selector_button ->
        input and type == "file"

      :color_swatch ->
        input and type == "color"

      kind when kind in [:slider_fill, :slider_thumb, :slider_track, :fill, :thumb, :track] ->
        (input and
           (type == "range" or (type == "checkbox" and Map.has_key?(node.attributes, "switch")))) or
          (html and tag in ["progress", "meter"])

      kind when kind in [:step_control, :step_down, :step_up] ->
        input and type == "number" and Map.get(style, :appearance) != :textfield

      kind when kind in [:field_component, :field_separator] ->
        input and type in ~w(date month week time datetime-local)

      kind when kind in [:field_text, :field_content] ->
        (input and type in ~w(text search tel url email password number)) or
          (html and tag == "textarea") or
          (kind == :field_content and input and type in ~w(date month week time datetime-local))
    end
  end

  @doc "Counts pseudo-element specificity, including first-letter sub-elements."
  @spec specificity(nil | atom() | tuple()) :: non_neg_integer()
  def specificity(pseudo) do
    case pseudo do
      nil -> 0
      {:first_letter, _} -> 2
      _ -> 1
    end
  end

  @doc "Identifies pseudo-elements whose layout and painting already have an implementation owner."
  @spec paint_ready?(atom() | tuple()) :: boolean()
  def paint_ready?(pseudo), do: pseudo in [:before, :after, :picker_icon, :first_letter]

  @doc "Keeps selected pseudo-elements gated until their layout and painting implementation is ready."
  @spec validate_paint([map()], map()) :: :ok | {:error, {atom(), map()}}
  def validate_paint(owners, node) do
    case Enum.find(owners, &(not &1.paint_ready)) do
      nil ->
        :ok

      owner ->
        name =
          case owner.pseudo do
            {:nth_fragment, _} -> "nth-fragment"
            {:first_letter, child} -> "first-letter::" <> Atom.to_string(child)
            pseudo -> pseudo |> Atom.to_string() |> String.replace("_", "-")
          end

        source = Map.get(node, :source, %{})

        NativeElixirPdfUtilities.Diagnostics.error(
          :css,
          :not_implemented,
          "::#{name} selects an originating element, but its layout and painting are not implemented yet",
          module: __MODULE__,
          source: "::" <> name,
          line: Map.get(source, :line),
          column: Map.get(source, :column)
        )
    end
  end
end
