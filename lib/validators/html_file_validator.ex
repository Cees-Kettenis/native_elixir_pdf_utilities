defmodule NativeElixirPdfUtilities.Validators.HtmlFileValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font

  alias NativeElixirPdfUtilities.Validators.{
    HtmlFormValidator,
    HtmlValidator,
    LanguageValidator,
    WriterValidator
  }

  @doc false
  @spec prepare_render_style(map(), map(), [map()], keyword()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare_render_style(node, style, ancestors, opts) do
    if node.tag == "input" and Map.get(node, :namespace, :html) == :html and
         HtmlValidator.input_type(
           Map.get(HtmlFormValidator.initial_control_attributes(node), "type", "text")
         ) == "file" and
         style.display != :none do
      cond do
        Map.get(style, :_html_content_skipped, false) ->
          {:ok, style}

        Keyword.get(opts, :forms, :interactive) == :interactive and
          not node._initial_form_state.disabled and not Map.has_key?(node, :_html_inert) ->
          Diagnostics.error(
            :forms,
            :unsupported_form,
            "PDF file-selection actions are not supported; use forms: :static for the empty initial chooser or disable the control",
            module: __MODULE__,
            operation: :compute,
            source: "input/file"
          )

        LanguageValidator.resolved_language(node, ancestors) not in [
          [],
          [""],
          ["en"],
          ["en", "us"]
        ] ->
          Diagnostics.error(
            :style,
            :unsupported_html,
            "Native file labels currently use English US chooser strings; use lang=en-US for this initial display",
            module: __MODULE__,
            operation: :compute,
            source: "input/file"
          )

        true ->
          with {:ok, _, button_face} <-
                 Font.resolve(["Arial", "sans-serif"], 400, :normal, style._font_registry),
               {:ok, display} <- prepare_display(style, node._initial_form_state, button_face),
               {:ok, width} <-
                 HtmlFormValidator.validate_initial_control_dimension(
                   display.intrinsic_width,
                   :width
                 ),
               {:ok, height} <-
                 HtmlFormValidator.validate_initial_control_dimension(
                   display.intrinsic_height,
                   :height
                 ) do
            control = %{
              id: node._selector_id,
              order: node._document_order,
              source_id: node.node_id,
              scope: {node.document_id, node.tree_scope_id},
              tag: "input",
              attributes: HtmlFormValidator.initial_control_attributes(node),
              initial_state: node._initial_form_state
            }

            HtmlValidator.reserve_render_resource(
              :max_rendered_text_bytes,
              byte_size(display.button_text) + byte_size(display.filename),
              :style
            )

            style = style |> Map.put(:_file_display, display) |> Map.put(:_file_control, control)
            style = if style.width in [nil, :auto], do: Map.put(style, :width, width), else: style

            style =
              if style.height in [nil, :auto], do: Map.put(style, :height, height), else: style

            {:ok, style}
          end
      end
    else
      {:ok, style}
    end
  end

  @doc false
  @spec prepare_display(map(), map(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_display(style, state, button_face) do
    with %{value: "", files: [], disabled: disabled, multiple: multiple} <- state,
         true <- is_boolean(disabled) and is_boolean(multiple),
         %{
           type: :embedded,
           id: id,
           ascent: ascent,
           descent: descent,
           line_gap: gap,
           units_per_em: units
         } <-
           Map.get(style, :font_face),
         true <-
           is_binary(id) and is_number(ascent) and is_number(descent) and is_number(gap) and
             is_integer(units) and
             units > 0,
         %{
           type: :embedded,
           id: button_id,
           ascent: button_ascent,
           descent: button_descent,
           line_gap: button_gap,
           units_per_em: button_units
         } <- button_face,
         true <-
           is_binary(button_id) and is_number(button_ascent) and is_number(button_descent) and
             is_number(button_gap) and
             is_integer(button_units) and button_units > 0,
         %{font_size: font_size, line_height: _, letter_spacing: spacing, color: color} <- style,
         {:ok, _} <- HtmlValidator.normalize_geometry_number(spacing),
         {:ok, size} <- HtmlValidator.normalize_geometry_number(font_size),
         true <- size >= 0 do
      button_text = if multiple, do: "Choose Files", else: "Choose File"
      filename = "No file chosen"

      proofs =
        Enum.map([{button_text, button_face}, {filename, style.font_face}], fn {text, face} ->
          %{
            type: :text,
            x: 0,
            y: 0,
            text: text,
            font: Font.pdf_name(face),
            font_face: face,
            font_size: size,
            color: color
          }
        end)

      with {:ok, _} <- WriterValidator.prepare([%{size: {100, 100}, boxes: proofs}], []) do
        button_style = %{style | font_face: button_face}
        button_height = HtmlValidator.normal_line_height(button_style) + 4.5
        label_height = HtmlValidator.normal_line_height(style)
        label_baseline = HtmlValidator.text_baseline_depth(%{style | line_height: label_height})

        button_baseline =
          HtmlValidator.text_baseline_depth(%{button_style | line_height: button_height - 4.5}) +
            2.25

        baseline = max(label_baseline, button_baseline)
        height = baseline + max(label_height - label_baseline, button_height - button_baseline)
        shaped_size = Float.floor(size / 0.75 * 64) / 64 * 0.75

        button_width =
          Float.ceil(Font.text_width(button_text, button_face, shaped_size) / 0.75 * 64) / 64 *
            0.75 +
            12.0

        label_width =
          Font.text_width(filename, style.font_face, shaped_size, style.letter_spacing)

        width =
          Float.ceil(
            max(
              34 * Font.text_width("0", style.font_face, shaped_size, style.letter_spacing),
              button_width + 3.0 + label_width
            ) / 0.75
          ) * 0.75

        {:ok,
         %{
           button_text: button_text,
           filename: filename,
           button_face: button_face,
           button_width: button_width,
           button_height: button_height,
           button_baseline: button_baseline,
           label_height: label_height,
           label_baseline: label_baseline,
           intrinsic_width: width,
           intrinsic_height: height,
           baseline: baseline,
           disabled: disabled
         }}
      end
    else
      {:error, _} = error ->
        error

      _ ->
        Diagnostics.error(
          :style,
          :unsupported_html,
          "Empty file display requires actual embedded metrics for the filename face and independently selected Arial chooser face; register both fonts through the font API",
          module: __MODULE__,
          operation: :compute,
          source: "input/file"
        )
    end
  end

  @doc false
  @spec filename_parts(map(), number()) :: [%{text: String.t(), width: number()}]
  def filename_parts(style, available_width) do
    text = style._file_display.filename
    face = style.font_face
    size = Float.floor(style.font_size / 0.75 * 64) / 64 * 0.75
    width = fn value -> Font.text_width(value, face, size, style.letter_spacing) end
    full_width = width.(text)
    ellipsis = if Map.get(face.cmap, 0x2026, 0) != 0, do: "…", else: "..."
    ellipsis_width = width.(ellipsis)

    # Blink LineTruncator uses middle ellipsis only for the file status text.
    # Its fixed empty English label bounds this search to fourteen scalars.
    if full_width > available_width and available_width > ellipsis_width do
      half = (available_width - ellipsis_width) / 2

      prefix =
        Enum.reduce_while(1..String.length(text), "", fn count, previous ->
          candidate = String.slice(text, 0, count)

          if width.(candidate) <= half or count == 1,
            do: {:cont, candidate},
            else: {:halt, previous}
        end)

      right_width = half + half - width.(prefix)

      suffix =
        Enum.reduce_while(1..(String.length(text) - String.length(prefix)), "", fn count,
                                                                                   previous ->
          candidate = String.slice(text, -count, count)
          if width.(candidate) <= right_width, do: {:cont, candidate}, else: {:halt, previous}
        end)

      [prefix, ellipsis, suffix]
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(&%{text: &1, width: width.(&1)})
    else
      [%{text: text, width: full_width}]
    end
  end

  @doc false
  @spec validate_initial_layout(map()) :: :ok | {:error, {atom(), map()}}
  def validate_initial_layout(node) do
    case node do
      %{style: %{display: :none}} ->
        :ok

      %{
        style:
          %{
            _file_display: display,
            _file_control: %{tag: "input", attributes: attrs, initial_state: state}
          } = style
      }
      when is_map(attrs) and is_map(state) and is_map(display) ->
        valid? =
          HtmlValidator.input_type(Map.get(attrs, "type", "text")) == "file" and
            Enum.all?(attrs, fn {key, value} -> is_binary(key) and is_binary(value) end) and
            Map.get(state, :multiple) == Map.has_key?(attrs, "multiple") and
            case prepare_display(style, state, Map.get(display, :button_face)) do
              {:ok, expected} -> expected == display
              _ -> false
            end

        if valid?, do: :ok, else: invalid_layout()

      %{style: style} when is_map(style) ->
        if Map.get(style, :form_kind) == :file or Map.has_key?(style, :_file_display) or
             Map.has_key?(style, :_file_control),
           do: invalid_layout(),
           else: :ok

      _ ->
        :ok
    end
  end

  defp invalid_layout do
    Diagnostics.error(
      :layout,
      :invalid_layout,
      "File layout requires canonical empty files and selected-font chooser geometry; compute styles before layout",
      module: __MODULE__,
      operation: :layout,
      source: "input/file"
    )
  end
end
