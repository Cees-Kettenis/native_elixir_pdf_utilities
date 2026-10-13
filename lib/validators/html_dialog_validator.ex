defmodule NativeElixirPdfUtilities.Validators.HtmlDialogValidator do
  @moduledoc "Initial authored nonmodal dialog state and its HTML user-agent defaults."

  alias NativeElixirPdfUtilities.Validators.CssValueValidator

  @doc "Returns nonmodal dialog defaults without inferring script-created modal state."
  @spec defaults(map(), number()) :: map()
  def defaults(attributes, font_size) do
    %{
      display: if(Map.has_key?(attributes, "open"), do: :block, else: :none),
      position: :absolute,
      offsets: CssValueValidator.edges(:auto, 0.0, :auto, 0.0),
      width: :fit_content,
      padding: CssValueValidator.edges(font_size),
      margin: CssValueValidator.edges(0.0, :auto, 0.0, :auto),
      border_widths: CssValueValidator.edges(2.25),
      border_styles: CssValueValidator.edges(:solid),
      border_colors: CssValueValidator.edges({0.0, 0.0, 0.0}),
      background_color: {1.0, 1.0, 1.0},
      color: {0.0, 0.0, 0.0},
      _dialog: true,
      _dialog_default_padding: true
    }
  end
end
