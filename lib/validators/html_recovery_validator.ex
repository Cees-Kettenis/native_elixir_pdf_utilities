defmodule NativeElixirPdfUtilities.Validators.HtmlRecoveryValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @key {__MODULE__, :enabled}

  @doc "Validates the opt-in HTML recovery option."
  @spec prepare_options(term()) :: {:ok, boolean()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_options(opts) do
    cond do
      not Keyword.keyword?(opts) ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "HTML parsing options must be a keyword list"
        )

      Enum.any?(Keyword.keys(opts), &(&1 != :html_recovery)) or length(opts) > 1 ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "HTML parsing accepts only one html_recovery option"
        )

      true ->
        case Keyword.get(opts, :html_recovery, enabled?()) do
          enabled when is_boolean(enabled) ->
            {:ok, enabled}

          _ ->
            Diagnostics.error(:options, :invalid_options, "html_recovery must be a boolean")
        end
    end
  end

  @doc "Scopes validated recovery through nested document, fragment and source parsing."
  @spec with_options(term(), (-> result)) :: result | {:error, {atom(), Diagnostics.diagnostic()}}
        when result: term()
  def with_options(opts, fun) do
    with {:ok, enabled} <- prepare_options(opts) do
      previous = Process.get(@key)
      Process.put(@key, enabled)

      try do
        fun.()
      after
        if is_nil(previous), do: Process.delete(@key), else: Process.put(@key, previous)
      end
    end
  end

  @doc "Scopes recovery from render options while leaving their other validation to HtmlValidator."
  @spec with_render_options(term(), (-> result)) ::
          result | {:error, {atom(), Diagnostics.diagnostic()}}
        when result: term()
  def with_render_options(opts, fun) do
    if Keyword.keyword?(opts),
      do: with_options(Keyword.take(opts, [:html_recovery]), fun),
      else: fun.()
  end

  @doc "Returns whether the current parsing scope opted into HTML recovery."
  @spec enabled?() :: boolean()
  def enabled?, do: Process.get(@key, false)

  @doc "Prepares HTML trailing slashes without changing SVG or MathML self-closing syntax."
  @spec prepare_self_closing(:html | :svg | :mathml, String.t(), boolean()) ::
          {:ok, boolean()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_self_closing(namespace, tag, authored?) do
    cond do
      namespace != :html ->
        {:ok, authored?}

      HtmlValidator.html_void_tag?(tag) ->
        {:ok, true}

      authored? and not enabled?() ->
        Diagnostics.error(
          :html,
          :unsupported_html,
          "<#{tag}/> cannot self-close in HTML; use <#{tag}></#{tag}> or set html_recovery: true to treat it as an opening tag",
          module: __MODULE__
        )

      true ->
        {:ok, false}
    end
  end

  @doc "Reports an unclosed element with its authored opening location."
  @spec missing_end_tag(String.t(), map(), term()) :: {:error, {atom(), Diagnostics.diagnostic()}}
  def missing_end_tag(tag, source, context) do
    message =
      if is_tuple(context),
        do:
          "missing closing </#{tag}> in foreign content; close it explicitly or use valid SVG/MathML self-closing syntax",
        else:
          "missing closing </#{tag}>; add the closing tag or set html_recovery: true for supported end-tag recovery"

    Diagnostics.error(
      :html,
      :unsupported_html,
      message,
      module: __MODULE__,
      source: source.text,
      line: source.line,
      column: source.column
    )
  end
end
