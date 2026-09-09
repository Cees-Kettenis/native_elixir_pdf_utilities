defmodule NativeElixirPdfUtilities.Stamp do
  @moduledoc """
  Adds text, watermarks, page numbers, and PDF-page overlays to existing PDFs.

  Stamp page selections are one-based. Coordinates use the displayed page after
  CropBox, rotation, and UserUnit are applied. The origin is at the top-left,
  X increases rightward, and Y increases downward. Named positions and explicit
  `{x, y}` positions use PDF points in that coordinate system.

  Successful operations append an incremental revision. Existing document
  bytes, metadata, outlines, forms, and unselected page objects remain intact.
  PDF overlays import page artwork only; annotations, outlines, metadata, and
  interactive forms from the overlay PDF are not copied.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Pdf.StampWriter
  alias NativeElixirPdfUtilities.Validators.StampValidator

  @type page_selector :: pos_integer() | Range.t()
  @type position ::
          :top_left
          | :top_center
          | :top_right
          | :center_left
          | :center
          | :center_right
          | :bottom_left
          | :bottom_center
          | :bottom_right
          | {number(), number()}
  @type text_option ::
          {:pages, :all | [page_selector()]}
          | {:position, position()}
          | {:margin, non_neg_integer() | float()}
          | {:font, String.t()}
          | {:fonts, [map() | keyword() | {String.t(), String.t()}]}
          | {:font_weight, 100..900}
          | {:font_style, :normal | :italic}
          | {:size, :auto | number()}
          | {:color, {number(), number(), number()}}
          | {:opacity, number()}
          | {:rotation, number()}
          | {:system_font_discovery, boolean()}
  @type overlay_option ::
          {:pages, :all | [page_selector()]}
          | {:overlay_pages, :match | {:repeat, pos_integer()}}
          | {:fit, :exact | :contain | :cover | :stretch}
          | {:opacity, number()}
  @type error_reason ::
          :encrypted_pdf
          | :invalid_options
          | :invalid_page_selection
          | :invalid_pdf_input
          | :invalid_stamp
          | :page_out_of_bounds
          | :resource_limit_exceeded
          | :unsupported_glyph
          | :unsupported_pdf_feature

  @doc """
  Adds one text stamp to all or selected pages.

  The default position is `:center`. The bundled DejaVu Sans face is used unless
  another family and matching `:fonts` configuration are supplied.
  """
  @spec text(binary(), String.t(), [text_option()]) ::
          {:ok, binary()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def text(pdf, text, options \\ []) do
    generated_text_stamp(pdf, text, options, :text, :stamp_text)
  end

  @doc """
  Adds a centered, diagonal, translucent text watermark.

  Watermarks default to an automatically fitted size, 15 percent opacity, and
  45 degrees clockwise rotation. Any text-stamp option can override a default.
  """
  @spec watermark(binary(), String.t(), [text_option()]) ::
          {:ok, binary()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def watermark(pdf, text, options \\ []) do
    generated_text_stamp(pdf, text, options, :watermark, :watermark)
  end

  @doc """
  Adds page artwork from another PDF over all or selected target pages.

  `overlay_pages: {:repeat, page}` repeats one overlay page and is the default
  with page one. `overlay_pages: :match` maps overlay pages to selected target
  pages in order and requires equal counts. `fit: :exact` is the default;
  `:contain`, `:cover`, and `:stretch` opt into scaling.
  """
  @spec overlay(binary(), binary(), [overlay_option()]) ::
          {:ok, binary()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def overlay(pdf, overlay_pdf, options \\ []) do
    with {:ok, target_context} <- Reader.read_validated(pdf),
         {:ok, overlay_context} <- Reader.read_validated(overlay_pdf),
         {:ok, plan} <- StampValidator.prepare_overlay(target_context, overlay_context, options),
         {:ok, stamped} <- StampWriter.write(plan) do
      {:ok, stamped}
    else
      {:error, error} -> owned_error(error, :overlay_pdf)
    end
  end

  @doc """
  Adds formatted page numbers to all or selected pages.

  The format supports `{{page}}` and `{{pages}}`. `numbering: :document` uses
  physical document page numbers and is the default. `numbering: :selection`
  numbers only selected pages. Text options configure position, font, size,
  color, opacity, and rotation.
  """
  @spec page_numbers(binary(), [
          text_option() | {:format, String.t()} | {:numbering, :document | :selection}
        ]) ::
          {:ok, binary()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def page_numbers(pdf, options \\ []) do
    with {:ok, target_context} <- Reader.read_validated(pdf),
         {:ok, text_plan} <- StampValidator.prepare_page_numbers(target_context, options),
         {:ok, stamped} <- render_generated_stamp(target_context, text_plan) do
      {:ok, stamped}
    else
      {:error, error} -> owned_error(error, :page_numbers)
    end
  end

  defp generated_text_stamp(pdf, text, options, kind, operation) do
    with {:ok, target_context} <- Reader.read_validated(pdf),
         {:ok, text_plan} <- StampValidator.prepare_text(target_context, text, options, kind),
         {:ok, stamped} <- render_generated_stamp(target_context, text_plan) do
      {:ok, stamped}
    else
      {:error, error} -> owned_error(error, operation)
    end
  end

  defp render_generated_stamp(target_context, text_plan) do
    pages = Enum.map(text_plan.appearances, & &1.page)

    with {:ok, appearance_pdf} <- PdfWriter.render(pages),
         {:ok, appearance_context} <- Reader.read_validated(appearance_pdf),
         {:ok, plan} <-
           StampValidator.prepare_generated_overlay(target_context, appearance_context, text_plan) do
      StampWriter.write(plan)
    end
  end

  defp owned_error({reason, diagnostic}, operation) do
    {:error,
     {reason,
      diagnostic
      |> Map.put(:operation, operation)
      |> Map.put(:module, __MODULE__)}}
  end
end
