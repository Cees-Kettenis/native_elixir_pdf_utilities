defmodule NativeElixirPdfUtilities.TestSupport.FontFixture do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.Sfnt
  alias NativeElixirPdfUtilities.Validators.FontValidator

  @doc "Returns an admitted fixture's exact source tables for format-specific mutation tests."
  @spec tables(binary()) :: {non_neg_integer(), map()}
  def tables(data) do
    {:ok, scaler, directory} = FontValidator.prepare_table_directory(data)

    {scaler,
     Map.new(directory, fn {tag, {offset, size}} -> {tag, binary_part(data, offset, size)} end)}
  end

  @doc "Rebuilds an admitted fixture after an explicit set of test-owned table changes."
  @spec rebuild(binary(), map()) :: binary()
  def rebuild(source, changes) do
    {scaler, tables} = tables(source)

    tables =
      Enum.reduce(changes, tables, fn {tag, value}, tables ->
        if value == nil, do: Map.delete(tables, tag), else: Map.put(tables, tag, value)
      end)

    Sfnt.rebuild(scaler, Enum.sort(tables))
  end

  @doc "Replaces one fixed-width field while preserving all other fixture bytes."
  @spec replace(binary(), non_neg_integer(), binary()) :: binary()
  def replace(data, offset, replacement) do
    <<before::binary-size(^offset), _old::binary-size(byte_size(^replacement)), rest::binary>> =
      data

    before <> replacement <> rest
  end
end
