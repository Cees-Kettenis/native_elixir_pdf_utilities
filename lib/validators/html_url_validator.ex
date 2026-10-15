defmodule NativeElixirPdfUtilities.Validators.HtmlUrlValidator do
  @moduledoc """
  Prepares static document URLs independently of resource authorization.

  The first HTML `base[href]` outside template and foreign trees supplies the
  resolution base. `:base_url` supplies the fallback URL; local caller values
  retain their existing directory meaning. Consumers must authorize resolved
  resources against the original caller root, never against this context.

  Anchor destinations are `{:uri, url}` or `{:fragment, identifier}`. The latter
  requires final document target positions and must not be emitted as a URI.
  This context contains no filesystem permissions and grants no access.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @type context :: %{
          fallback_base: String.t() | nil,
          resolution_base: String.t() | nil,
          authored_base?: boolean()
        }
  @type destination :: {:uri, String.t()} | {:fragment, String.t()}

  @doc "Tests an absolute initial URL input without a base or resource access."
  @spec valid_initial_input_url?(term()) ::
          {:ok, boolean()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def valid_initial_input_url?(value) do
    with {:ok, request} <-
           NativeElixirPdfUtilities.Validators.HtmlFormValueValidator.prepare(:url, value, []),
         {:ok, state} <-
           NativeElixirPdfUtilities.Validators.HtmlFormValueValidator.result(
             NativeElixirPdfUtilities.HtmlToPdf.FormValueBackend.run(request)
           ) do
      {:ok, state == :valid_url}
    end
  end

  @doc "Admits an absolute initial input URL without granting resource access."
  @spec absolute_input_url?(binary()) :: boolean()
  def absolute_input_url?(source) do
    source = normalize_reference(source)

    case Regex.run(~r/\A([A-Za-z][A-Za-z0-9+.-]*):(.*)\z/s, source) do
      [_, scheme, rest] ->
        scheme = String.downcase(scheme, :ascii)

        if scheme in ~w(http https ftp ws wss) do
          # Special URL schemes treat backslashes as slashes before host parsing.
          rest = rest |> String.replace("\\", "/") |> String.trim_leading("/")

          case URI.new(
                 scheme <> "://" <> URI.encode(rest, &(&1 == ?% or URI.char_unescaped?(&1)))
               ) do
            {:ok, %URI{host: host, port: port}} when is_binary(host) and host != "" ->
              decoded = URI.decode(host)
              valid_host? = not Regex.match?(~r/[\x00-\x20<>\\#\/?@^|%]/u, decoded)

              address? =
                if String.contains?(host, ":"),
                  do: match?({:ok, _}, :inet.parse_ipv6_address(String.to_charlist(host))),
                  else: valid_numeric_host?(decoded)

              valid_host? and address? and (is_nil(port) or port in 0..65_535)

            _ ->
              false
          end
        else
          # Non-special URLs may have opaque paths, including mailto: and data:.
          if scheme == "file", do: not Regex.match?(~r/\A\/\/[^\/]*[:@]/, rest), else: true
        end

      _ ->
        false
    end
  rescue
    ArgumentError -> false
  end

  # URL Standard host parsing treats a numeric final label as an IPv4 address,
  # including shortened, hexadecimal and octal spellings. It cannot fall back
  # to a domain name when that address is out of range.
  # https://url.spec.whatwg.org/#concept-ipv4-parser
  defp valid_numeric_host?(host) do
    parts = String.split(host, ".")
    parts = if List.last(parts) == "", do: Enum.drop(parts, -1), else: parts
    last = List.last(parts)

    if Regex.match?(~r/\A[0-9]+\z/, last) or
         Regex.match?(~r/\A0[xX][0-9a-fA-F]*\z/, last) do
      numbers = Enum.map(parts, &ipv4_number/1)

      length(numbers) <= 4 and Enum.all?(numbers, &is_integer/1) and
        Enum.all?(Enum.drop(numbers, -1), &(&1 <= 255)) and
        List.last(numbers) < Integer.pow(256, 5 - length(numbers))
    else
      true
    end
  end

  defp ipv4_number(source) do
    {radix, digits} =
      case source do
        <<"0", x, rest::binary>> when x in [?x, ?X] -> {16, rest}
        <<"0", rest::binary>> when rest != "" -> {8, rest}
        _ -> {10, source}
      end

    digits = String.trim_leading(digits, "0")

    cond do
      source == "" ->
        nil

      digits == "" ->
        0

      # An IPv4 component has at most 32 bits. Longer significant spellings
      # cannot fit even in the least compact accepted radix, octal.
      byte_size(digits) > 11 or not Regex.match?(~r/\A[0-9a-fA-F]+\z/, digits) ->
        nil

      true ->
        case Integer.parse(digits, radix) do
          {number, ""} when number >= 0 -> number
          _ -> nil
        end
    end
  end

  @doc "Classifies the HTML about:blank URL family without granting resource access."
  @spec blank_document_url(term()) :: {:ok, String.t()} | :not_blank
  def blank_document_url(reference) do
    # HTML matches-about:blank permits query/fragment but requires the exact
    # opaque path "blank", a null host and empty credentials.
    if is_binary(reference) and String.valid?(reference) do
      case resolve_reference(reference, nil) do
        {:ok, url} ->
          case URI.parse(url) do
            %URI{scheme: "about", path: "blank", host: nil, userinfo: nil} -> {:ok, url}
            _ -> :not_blank
          end

        {:error, _} ->
          :not_blank
      end
    else
      :not_blank
    end
  end

  @doc false
  @spec prepare_style_output_options(keyword()) ::
          {:ok, keyword()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_style_output_options(opts) do
    case Keyword.fetch(opts, :__document_url_context__) do
      :error ->
        {:ok, opts}

      {:ok, %{fallback_base: fallback, resolution_base: base, authored_base?: authored} = urls}
      when map_size(urls) == 3 and (is_binary(fallback) or is_nil(fallback)) and
             (is_binary(base) or is_nil(base)) and is_boolean(authored) ->
        case NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.current() do
          %{depth: depth, resolution_base: ^base} = context when depth > 0 ->
            with :ok <-
                   NativeElixirPdfUtilities.Validators.ResourceValidator.validate_context(context),
                 {:ok, ^fallback} <-
                   if(is_nil(fallback), do: {:ok, nil}, else: resolve_reference(fallback, nil)) do
              {:ok, Keyword.delete(opts, :__document_url_context__)}
            else
              _ -> url_error("__document_url_context__")
            end

          _ ->
            {:ok, opts}
        end

      _ ->
        {:ok, opts}
    end
  end

  @doc "Prepares the document's resolution base without changing caller permissions."
  @spec document_context(map(), keyword()) ::
          {:ok, context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def document_context(dom, opts) do
    with {:ok, fallback} <- fallback_base(Keyword.get(opts, :base_url)) do
      document_context_from_url(dom, fallback)
    end
  end

  @doc """
  Prepares an authored base against an already validated document URL.

  Unlike `document_context/2`, this preserves a child document's file URL as a
  document URL. The caller supplies a validated parsed DOM and absolute fallback
  URL, and retains ownership of authorization, document/tree identities and budgets.
  """
  @spec document_context_from_url(map(), String.t() | nil) ::
          {:ok, context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def document_context_from_url(dom, fallback) do
    href = first_base_href(dom.children)

    case href do
      nil ->
        {:ok, %{fallback_base: fallback, resolution_base: fallback, authored_base?: false}}

      href ->
        with :ok <- HtmlValidator.validate_static_url_attributes(%{"href" => href}, "base") do
          # HTML freezes the fallback when the first base cannot be parsed or
          # uses data:. A later base never replaces that first element.
          base =
            case resolve_reference(href, fallback) do
              {:ok, url} ->
                if URI.parse(url).scheme == "data", do: fallback, else: url

              {:error, _} ->
                fallback
            end

          {:ok, %{fallback_base: fallback, resolution_base: base, authored_base?: true}}
        end
    end
  end

  @doc "Validates a base element's attributes and empty content."
  @spec validate_base_element(map(), list()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_base_element(attributes, children) do
    if children == [] and
         (Map.has_key?(attributes, "href") or Map.has_key?(attributes, "target")) do
      :ok
    else
      Diagnostics.error(:html, :invalid_document, "base requires href or target and no children",
        module: __MODULE__,
        source: "base"
      )
    end
  end

  @doc "Resolves a static anchor into a URI or a same-document fragment destination."
  @spec prepare_anchor(term(), context()) ::
          {:ok, destination()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_anchor(href, context) do
    case href do
      href when is_binary(href) ->
        with :ok <- HtmlValidator.validate_static_url_attributes(%{"href" => href}, "a") do
          normalized = normalize_reference(href)

          cond do
            normalized == "" and is_nil(context.resolution_base) ->
              {:ok, {:fragment, ""}}

            String.starts_with?(normalized, "#") and
                same_document_url?(context.resolution_base, context.fallback_base) ->
              with {:ok, _uri} <- validated_reference(normalized) do
                {:ok,
                 {:fragment,
                  normalized
                  |> binary_part(1, byte_size(normalized) - 1)
                  |> URI.decode()
                  |> String.replace_invalid()}}
              end

            true ->
              with {:ok, url} <- resolve_reference(normalized, context.resolution_base) do
                case URI.parse(url) do
                  %URI{scheme: "mailto"} ->
                    if Regex.match?(~r/^mailto:[^\s<>@]+@[^\s<>@]+$/iu, url),
                      do: {:ok, {:uri, url}},
                      else: url_error(href)

                  %URI{scheme: scheme, fragment: fragment}
                  when scheme in ["http", "https", "file"] ->
                    if same_document_url?(url, context.fallback_base),
                      do:
                        {:ok,
                         {:fragment,
                          fragment
                          |> then(&(&1 || ""))
                          |> URI.decode()
                          |> String.replace_invalid()}},
                      else: {:ok, {:uri, url}}

                  _ ->
                    url_error(href)
                end
              end
          end
        end

      _ ->
        url_error("href")
    end
  rescue
    ArgumentError -> url_error(href)
  end

  @doc "Resolves a URL reference against a prepared base, without authorizing or reading it."
  @spec resolve_reference(String.t(), String.t() | nil) ::
          {:ok, String.t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def resolve_reference(reference, base) do
    reference = normalize_reference(reference)

    with {:ok, uri} <- validated_reference(reference) do
      base_uri = if is_binary(base), do: URI.parse(base), else: nil

      resolved =
        cond do
          is_binary(uri.scheme) ->
            uri

          is_struct(base_uri, URI) and is_binary(base_uri.scheme) and
              (is_binary(base_uri.host) or base_uri.scheme == "file" or
                 (is_binary(base_uri.path) and String.starts_with?(base_uri.path, "/")) or
                 (is_binary(uri.fragment) and is_nil(uri.path) and is_nil(uri.query))) ->
            URI.merge(base_uri, uri)

          true ->
            nil
        end

      case resolved do
        %URI{scheme: scheme} = uri ->
          uri = %{
            uri
            | scheme: String.downcase(scheme, :ascii),
              host: if(is_binary(uri.host), do: String.downcase(uri.host), else: uri.host)
          }

          valid? =
            case uri.scheme do
              scheme when scheme in ["http", "https"] ->
                # Fixed TCP/UDP port range, not a configurable resource limit.
                is_binary(uri.host) and uri.host != "" and
                  not Regex.match?(~r/[\x00-\x20<>\\#\/?@\[\]\^|%]/u, URI.decode(uri.host)) and
                  (is_nil(uri.port) or uri.port in 0..65_535)

              "file" ->
                uri.host in [nil, "", "localhost"] and is_binary(uri.path) and
                  String.starts_with?(uri.path, "/")

              _ ->
                true
            end

          if valid? do
            {:ok, uri |> URI.to_string() |> URI.encode(&(&1 == ?% or URI.char_unescaped?(&1)))}
          else
            url_error(reference)
          end

        _ ->
          url_error(reference)
      end
    end
  rescue
    ArgumentError -> url_error(reference)
  end

  @doc "Prepares a resource reference for the existing authorized asset loader."
  @spec resource_reference(String.t(), context()) ::
          {:ok, String.t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def resource_reference(reference, context) do
    if context.authored_base? do
      with {:ok, url} <- resolve_reference(reference, context.resolution_base) do
        case URI.parse(url) do
          %URI{scheme: "file", path: path} -> {:ok, URI.decode(path)}
          _ -> {:ok, url}
        end
      end
    else
      {:ok, reference}
    end
  end

  # URI.parse tolerates malformed authorities, including nonnumeric ports.
  # Validate the encoded URI before merging so these cannot change destination.
  defp validated_reference(reference) do
    encoded = URI.encode(reference, &(&1 == ?% or URI.char_unescaped?(&1)))

    # HTML URL fragments may contain additional hash characters. Encode those
    # within the fragment before using the stricter RFC URI parser.
    encoded =
      case String.split(encoded, "#", parts: 2) do
        [prefix, fragment] -> prefix <> "#" <> String.replace(fragment, "#", "%23")
        [url] -> url
      end

    invalid? =
      String.contains?(reference, "\\") or Regex.match?(~r/%(?![0-9a-fA-F]{2})/, reference)

    case {invalid?, URI.new(encoded)} do
      {false, {:ok, uri}} -> {:ok, uri}
      _ -> url_error(reference)
    end
  end

  defp fallback_base(base) do
    case base do
      nil ->
        {:ok, nil}

      base when is_binary(base) ->
        case URI.parse(base) do
          %URI{scheme: nil} ->
            path = Path.expand(base) |> String.trim_trailing("/")

            resolve_reference(
              "file://" <> URI.encode(path, &(&1 == ?/ or URI.char_unreserved?(&1))) <> "/",
              nil
            )

          %URI{scheme: "file", host: host, path: path}
          when host in [nil, "", "localhost"] and is_binary(path) ->
            path = path |> URI.decode() |> Path.expand() |> String.trim_trailing("/")

            resolve_reference(
              "file://" <> URI.encode(path, &(&1 == ?/ or URI.char_unreserved?(&1))) <> "/",
              nil
            )

          _ ->
            resolve_reference(base, nil)
        end

      _ ->
        url_error("base_url")
    end
  end

  defp first_base_href(nodes) do
    Enum.find_value(nodes, fn node ->
      case node do
        %{tag: "template"} -> nil
        %{namespace: namespace} when namespace != :html -> nil
        %{tag: "base", attributes: %{"href" => href}} -> href
        %{tag: tag, children: children} when tag in ["html", "head"] -> first_base_href(children)
        _ -> nil
      end
    end)
  end

  @doc false
  @spec raw_fragment(String.t()) :: String.t()
  def raw_fragment(href) do
    case String.split(normalize_reference(href), "#", parts: 2) do
      [_prefix, fragment] -> fragment
      _ -> ""
    end
  end

  # URL identity excludes the fragment. The fallback identifies the original
  # document even when an authored base points elsewhere.
  defp same_document_url?(url, fallback) do
    case {url, fallback} do
      {nil, nil} ->
        true

      {url, fallback} when is_binary(url) and is_binary(fallback) ->
        Map.drop(URI.parse(url), [:fragment, :authority]) ==
          Map.drop(URI.parse(fallback), [:fragment, :authority])

      _ ->
        false
    end
  end

  defp normalize_reference(reference) do
    reference
    |> String.replace(~r/\A[\x00-\x20]+|[\x00-\x20]+\z/u, "")
    |> String.replace(["\t", "\n", "\r"], "")
  end

  defp url_error(source) do
    Diagnostics.error(
      :style,
      :invalid_document,
      "URL cannot be resolved in the static document context; supply a valid URL and an absolute base for relative references",
      module: __MODULE__,
      source: if(String.valid?(source), do: source, else: inspect(source))
    )
  end
end
