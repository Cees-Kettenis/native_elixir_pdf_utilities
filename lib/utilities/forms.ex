defmodule NativeElixirPdfUtilities.Forms do
  @moduledoc """
  Inspects, fills and flattens supported AcroForm fields in existing PDFs.

  Values use fully qualified field names. Filling generates self-contained
  appearances using bundled DejaVu Sans, black text, and automatic sizing.
  Flattening removes active interactivity, not historical data or revisions.
  XFA and changes to signed documents are unsupported.
  """
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.{FormWriter, Reader}
  alias NativeElixirPdfUtilities.Validators.FormValidator

  @doc "Returns field names, types, values, choices, read-only flags and widget page rectangles."
  @spec fields(binary()) :: {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def fields(pdf) do
    result =
      with {:ok, context} <- Reader.read_validated(pdf),
           {:ok, form} <- FormValidator.inspect_document(context) do
        {:ok,
         Enum.map(form.fields, fn field ->
           field
           |> Map.take([:name, :type, :value, :read_only, :choices])
           |> Map.put(:widgets, Enum.map(field.widgets, &Map.take(&1, [:page, :rect])))
           |> Map.put(
             :export_values,
             Enum.flat_map(field.widgets, & &1.states)
             |> Enum.reject(&(&1 == "Off"))
             |> Enum.uniq()
           )
         end)}
      end

    own_error(result, :fields)
  end

  @doc "Fills named fields; `flatten: true` removes interactivity for the supplied fields after filling."
  @spec fill(binary(), map(), keyword()) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def fill(pdf, values, opts \\ []) do
    result =
      with {:ok, opts} <- FormValidator.options(opts, :fill),
           {:ok, values} <- FormValidator.prepare_values(values),
           {:ok, context} <- Reader.read_validated(pdf),
           {:ok, form} <- FormValidator.inspect_document(context),
           {:ok, fields} <- FormValidator.prepare_write(context, form, values, opts, :fill) do
        case fields do
          [] ->
            {:ok, pdf}

          _ ->
            with {:ok, updated} <- FormWriter.fill(context, form, fields) do
              if Map.get(opts, :flatten, false),
                do: flatten(updated, fields: Map.keys(values)),
                else: {:ok, updated}
            end
        end
      end

    own_error(result, :fill)
  end

  @doc "Flattens all supported fields, or the names supplied in `fields: [...]`, using their current appearances."
  @spec flatten(binary(), keyword()) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def flatten(pdf, opts \\ []) do
    result =
      with {:ok, opts} <- FormValidator.options(opts, :flatten),
           {:ok, context} <- Reader.read_validated(pdf),
           {:ok, form} <- FormValidator.inspect_document(context),
           {:ok, fields} <- FormValidator.prepare_write(context, form, %{}, opts, :flatten),
           {:ok, placements} <- FormValidator.prepare_flatten(context, fields) do
        case fields do
          [] -> {:ok, pdf}
          _ -> FormWriter.flatten(context, form, fields, placements)
        end
      end

    own_error(result, :flatten)
  end

  defp own_error(result, operation) do
    case result do
      {:error, {reason, diagnostic}} ->
        {:error, {reason, Map.merge(diagnostic, %{module: __MODULE__, operation: operation})}}

      success ->
        success
    end
  end
end
