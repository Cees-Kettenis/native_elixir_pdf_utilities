defmodule NativeElixirPdfUtilities.Validators.ModificationValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  @doc false
  @spec validate(map()) :: :ok | {:error, {atom(), map()}}
  def validate(context) do
    signed =
      Map.has_key?(context.catalog, "Perms") or
        Enum.any?(context.document.objects, fn {_ref, object} ->
          case object.value do
            dictionary when is_map(dictionary) ->
              dictionary["Type"] == {:name, "Sig"} or
                (dictionary["FT"] == {:name, "Sig"} and not is_nil(dictionary["V"]))

            _ ->
              false
          end
        end)

    if signed,
      do:
        Diagnostics.error(
          :validation,
          :unsupported_form,
          "modifying signed PDFs or documents with signature permissions is unsupported"
        ),
      else: :ok
  end
end
