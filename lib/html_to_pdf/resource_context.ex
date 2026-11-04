defmodule NativeElixirPdfUtilities.HtmlToPdf.ResourceContext do
  @moduledoc """
  Internal resource scope for local embedded documents.

  The authorization root is fixed for the scope. Resolution bases and document
  identities belong to individual documents. Children share the outer render,
  font and image budgets and never acquire a resolver. These contexts must be
  used inside `run/3`; they are not persistent permissions.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.ResourceValidator

  @enforce_keys [:scope, :authorization_root, :resolution_base, :image_budget]
  defstruct @enforce_keys ++
              [document_id: nil, tree_scope_id: nil, ancestors: [], depth: 0]

  @type t :: %__MODULE__{
          scope: reference(),
          authorization_root: String.t() | nil,
          resolution_base: String.t() | nil,
          image_budget: HtmlValidator.image_budget(),
          document_id: reference(),
          tree_scope_id: reference(),
          ancestors: [term()],
          depth: non_neg_integer()
        }

  @doc "Runs local child work in the existing render scopes, sharing the supplied image budget."
  @spec run(keyword(), HtmlValidator.image_budget(), (t() -> result)) ::
          result | {:error, {atom(), Diagnostics.diagnostic()}}
        when result: term()
  def run(opts, image_budget, fun) do
    HtmlValidator.with_render_budget(fn ->
      FontValidator.with_budget(fn ->
        RenderCache.with_stylesheets(fn ->
          RenderCache.run(fn scope ->
            with {:ok, context} <- ResourceValidator.prepare_context(opts, image_budget, scope) do
              Process.put({__MODULE__, scope}, %{
                authorization_root: context.authorization_root,
                image_budget: image_budget,
                documents: %{
                  context.document_id => {context.tree_scope_id, context.ancestors, context.depth}
                }
              })

              try do
                with_document(context, fn -> fun.(context) end)
              after
                Process.delete({__MODULE__, scope})
              end
            end
          end)
        end)
      end)
    end)
  end

  @doc false
  @spec current() :: t() | nil
  def current do
    Process.get({__MODULE__, :document})
  end

  @doc "Runs an issued document in its own resource scope while preserving cumulative caches."
  @spec with_document(t(), (-> result)) :: result | {:error, {atom(), Diagnostics.diagnostic()}}
        when result: term()
  def with_document(context, fun) do
    with :ok <- ResourceValidator.validate_context(context) do
      key = {__MODULE__, :document}
      previous = Process.get(key)
      Process.put(key, context)

      try do
        fun.()
      after
        if previous == nil, do: Process.delete(key), else: Process.put(key, previous)
      end
    end
  end

  @doc """
  Changes resolution without changing permission roots, issued identities or budgets.

  Local strings and file URIs retain the directory API. Use
  `{:document_url, absolute_url}` for a validated document URL, whose final path
  segment remains a document name rather than becoming a resolution directory.
  """
  @spec rebase(t(), term()) :: {:ok, t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def rebase(context, base) do
    ResourceValidator.rebase(context, base)
  end
end
