defmodule NativeElixirPdfUtilities.Validators.CssLinkParameterValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgScene

  alias NativeElixirPdfUtilities.Validators.{
    CssColorValidator,
    CssSubstitutionValidator,
    CssSyntaxValidator,
    HtmlValidator,
    SvgSceneValidator,
    SvgValidator
  }

  @doc "Parses the published comma-form link parameter list."
  @spec prepare(String.t()) :: {:ok, map()} | :error
  def prepare(value) do
    if String.downcase(String.trim(value), :ascii) == "none" do
      {:ok, %{}}
    else
      Enum.reduce_while(CssSyntaxValidator.split_trimmed(value, ","), {:ok, %{}}, fn token,
                                                                                     {:ok, acc} ->
        case parameter(token) do
          {:ok, name, value} -> {:cont, {:ok, Map.put(acc, name, value)}}
          :error -> {:halt, :error}
        end
      end)
    end
  end

  defp parameter(source) do
    with {:ok, body, ""} <- CssSyntaxValidator.function(":" <> source, "param"),
         [name, value] <- CssSyntaxValidator.split_trimmed(body, ","),
         {:ok, "--" <> suffix = name, ""} when suffix != "" <-
           HtmlValidator.parse_css_identifier(name),
         true <- safe_value?(value) do
      {:ok, name, value}
    else
      _ -> :error
    end
  end

  # A print document can pass literal paint values. Resources continue to be
  # supplied by the SVG document's own authored and authorized dependencies.
  defp safe_value?(value) do
    value == "" or value in ["none", "currentcolor"] or
      match?({:ok, _}, CssColorValidator.prepare(value, nil)) or
      case HtmlValidator.parse_css_number(value) do
        {_, unit} -> unit in ~w(% px pt em rem deg) or unit == ""
        _ -> false
      end
  end

  @doc "Separates SVG fragment parameters from the resource authority before fetching bytes."
  @spec reference(String.t()) :: {String.t(), map()}
  def reference(source) do
    case String.split(source, "#", parts: 2) do
      [path, fragment] ->
        {others, parameters} =
          fragment
          |> String.split("&")
          |> Enum.reduce({[], %{}}, fn source, {others, parameters} ->
            case parameter(URI.decode(source)) do
              {:ok, name, value} -> {others, Map.put(parameters, name, value)}
              :error -> {[source | others], parameters}
            end
          end)

        reference =
          if others == [], do: path, else: path <> "#" <> Enum.join(Enum.reverse(others), "&")

        {reference, parameters}

      _ ->
        {source, %{}}
    end
  end

  @doc "Prepares the quoted URL and its optional published param() URL modifiers."
  @spec modified_url(String.t()) :: {:ok, String.t()} | :error | {:error, term()}
  def modified_url(source) do
    with {:ok, body, ""} <- CssSyntaxValidator.function(":" <> source, "url"),
         [quoted, _ | _] = parts <- HtmlValidator.split_css_tokens(body),
         [%{kind: :string}] <- CssSyntaxValidator.components(quoted),
         {:ok, reference} <-
           HtmlValidator.parse_background_image_reference("url(" <> quoted <> ")"),
         {:ok, _} <- prepare(Enum.join(tl(parts), ",")) do
      separator = if String.contains?(reference, "#"), do: "&", else: "#"

      fragments =
        Enum.map_join(tl(parts), "&", &URI.encode(&1, fn c -> URI.char_unreserved?(c) end))

      {:ok, reference <> separator <> fragments}
    else
      {:error, {_, _}} = error -> error
      _ -> :error
    end
  end

  @doc "Resolves authored SVG environment uses before the ordinary converter validates the result."
  @spec svg(String.t(), map()) :: {:ok, String.t()} | {:error, term()}
  def svg(source, parameters) do
    if :binary.match(source, "env(") == :nomatch do
      {:ok, source}
    else
      with {:ok, %{document: document}} <- SvgValidator.validate_extraction(source),
           {:ok, root} <- SvgSceneValidator.extraction_root(document) do
        {:ok, SvgScene.authored_source(resolve_node(root, parameters))}
      end
    end
  end

  defp resolve_node(node, parameters) do
    HtmlValidator.reserve_render_resource(:max_work, 1, :style)

    attributes =
      Map.new(node.attributes, fn {name, value} ->
        value =
          if name == "style" or
               name in ~w(fill stroke opacity fill-opacity stroke-opacity stop-color flood-color lighting-color stroke-width) do
            resolve(value, parameters)
          else
            value
          end

        {name, value}
      end)

    children =
      Enum.map(node.children, fn child ->
        case child do
          %{type: :element} ->
            resolve_node(child, parameters)

          %{type: :text, text: text} when node.tag == "style" ->
            %{child | text: resolve(text, parameters)}

          _ ->
            child
        end
      end)

    %{node | attributes: attributes, children: children}
  end

  defp resolve(source, parameters) do
    if :binary.match(source, "env(") == :nomatch do
      source
    else
      case CssSubstitutionValidator.resolve(source, %{_css_environment_parameters: parameters}) do
        {:ok, value} -> value
        :error -> source
        {:error, _} = error -> throw({:render_resource_limit, error})
      end
    end
  end
end
