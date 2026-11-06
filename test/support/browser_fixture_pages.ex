Code.require_file("browser_fixture_sheets.ex", __DIR__)
Code.require_file("browser_fixture_page_validator.ex", __DIR__)
Code.require_file("browser_parity_telemetry.ex", __DIR__)

defmodule NativeElixirPdfUtilities.TestSupport.BrowserFixturePages do
  @moduledoc false
  alias NativeElixirPdfUtilities.TestSupport.BrowserFixturePageValidator, as: Validator

  @doc "Builds a document with independently scoped original scenarios beginning on separate pages."
  @spec build!(String.t(), map()) :: String.t()
  def build!(directory, group) do
    sources = Validator.prepare!(directory, group)

    {styles, bodies} =
      sources
      |> Enum.with_index()
      |> Enum.map(fn {source, index} ->
        boundary = Validator.marker_uri(source.marker)

        marker_image =
          "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR4nGNgAAIAAAUAAXpeqz8AAAAASUVORK5CYII="

        page_rule =
          if group["page_mode"] == "named",
            do: "@page #{source.marker}{#{source.metadata["page_css"]}}",
            else: ""

        page_name = if group["page_mode"] == "named", do: "page:#{source.marker};", else: ""

        page_scope =
          if group["page_mode"] == "named", do: ":where(#page_#{index})", else: "#page_#{index}"

        body_scope =
          if group["page_mode"] == "named", do: ":where(#{source.scope})", else: source.scope

        style =
          page_rule <>
            "#{page_scope}{#{page_name}position:relative;break-before:#{if index == 0, do: "auto", else: "page"}}" <>
            "#{body_scope}{display:block;position:relative;box-sizing:border-box}" <> source.css

        body =
          "<section id=\"page_#{index}\"#{source.root_attributes}><a href=\"#{boundary}\" style=\"display:block;width:0.75pt;height:0;line-height:0;font-size:0\"><img src=\"data:image/png;base64,#{marker_image}\" style=\"display:block;width:0.75pt;height:0.75pt;border:0\"></a>" <>
            "<div id=\"body_#{index}\"#{source.body_attributes}>#{source.body}</div></section>"

        {style, body}
      end)
      |> Enum.unzip()

    html = """
    <!doctype html><html><head><meta charset="utf-8"><style>
    @page{#{group["page_css"]}}
    html,body{margin:0;padding:0;font-size:12pt}
    #{Enum.join(styles, "\n")}
    </style></head><body>#{Enum.join(bodies, "\n")}</body></html>
    """

    Validator.assert_compilation!(group, html)
    html
  end

  @doc "Starts a suite-local lazy reference scope; focused tests capture only groups they use."
  @spec start() :: {:ok, pid()}
  def start do
    Agent.start_link(
      fn ->
        path =
          NativeElixirPdfUtilities.TestSupport.Paths.fixture(
            "html_to_pdf/browser_parity/pages.json"
          )

        groups = if File.regular?(path), do: :json.decode(File.read!(path)), else: []

        members =
          for group <- groups,
              source <- group["sources"],
              do:
                {Path.join(
                   NativeElixirPdfUtilities.TestSupport.Paths.fixture(
                     group["directory"] || "html_to_pdf/browser_parity"
                   ),
                   source["name"]
                 ), group}

        Validator.assert_owners!(members)
        %{groups: Map.new(members), references: %{}}
      end,
      name: __MODULE__
    )
  end

  @doc "Restores the original reviewed browser options, rebasing bundled font paths to this checkout."
  @spec entries(map()) :: [{String.t(), keyword()}]
  def entries(group) do
    Enum.map(group["sources"], &{&1["name"], decode_options(&1["options"])})
  end

  @doc "Discards the suite-local scope after all comparisons finish."
  @spec stop() :: :ok
  def stop do
    if pid = Process.whereis(__MODULE__), do: Agent.stop(pid)
    :ok
  end

  @doc "Returns a reviewed page reference without changing the original scenario's assertions or thresholds."
  @spec reference(String.t(), keyword(), (String.t(), map(), list(), keyword() -> map())) ::
          map() | nil
  def reference(fixture_path, opts, prepare) do
    alias NativeElixirPdfUtilities.TestSupport.BrowserParityTelemetry
    scope = Process.whereis(__MODULE__)

    group =
      if scope && System.get_env("BROWSER_PARITY_PAGES") != "off",
        do: Agent.get(scope, &Map.get(&1.groups, Path.expand(fixture_path)))

    group =
      if group do
        source = Enum.find(group["sources"], &(&1["name"] == Path.basename(fixture_path)))
        if Validator.same_browser_options?(opts, decode_options(source["options"])), do: group
      end

    if group do
      context = BrowserParityTelemetry.context()

      environment =
        System.get_env()
        |> Map.take([
          "FONTCONFIG_FILE",
          "FONTCONFIG_PATH",
          "LANG",
          "LC_ALL",
          "BROWSER_PARITY_BROWSER",
          "CHROMIUM_BIN",
          "PDFTOPPM_BIN"
        ])

      fontconfig = environment["FONTCONFIG_FILE"]

      fingerprint =
        if fontconfig && File.regular?(fontconfig),
          do: :crypto.hash(:sha256, File.read!(fontconfig))

      key =
        {group["name"], environment, fingerprint,
         Keyword.take(opts, [:chromium_bin, :pdftoppm_bin])}

      result =
        Agent.get_and_update(
          scope,
          fn state ->
            case Map.fetch(state.references, key) do
              {:ok, result} ->
                {result, state}

              :error ->
                previous = BrowserParityTelemetry.context()
                BrowserParityTelemetry.attach(context)

                result =
                  try do
                    entries = entries(group)

                    directory = Path.dirname(Path.expand(fixture_path))

                    options =
                      hd(entries)
                      |> elem(1)
                      |> Keyword.merge(Keyword.take(opts, [:chromium_bin, :pdftoppm_bin]))

                    input =
                      Path.join(
                        directory,
                        ".chromium-input-#{Path.rootname(group["name"])}-#{System.pid()}-#{System.unique_integer([:positive])}.html"
                      )

                    File.write!(input, build!(directory, group))

                    try do
                      {:ok, prepare.(input, group, entries, options)}
                    after
                      File.rm!(input)
                    end
                  catch
                    kind, reason -> {:error, kind, reason, __STACKTRACE__}
                  after
                    BrowserParityTelemetry.attach(previous)
                  end

                {result, put_in(state, [:references, key], result)}
            end
          end,
          :infinity
        )

      case result do
        {:ok, references} -> Map.fetch!(references, Path.basename(fixture_path))
        {:error, kind, reason, stack} -> :erlang.raise(kind, reason, stack)
      end
    end
  end

  defp decode_options(options) do
    Enum.map(options, fn {key, value} ->
      key = String.to_existing_atom(key)

      value =
        case value do
          :null -> nil
          %{"atom" => atom} -> String.to_existing_atom(atom)
          %{"tuple" => values} -> List.to_tuple(values)
          _ -> value
        end

      value =
        case key do
          :render_opts ->
            decode_options(value)

          :fonts ->
            Enum.map(value, fn font -> Map.new(decode_options(font)) end)

          key when key in [:path, :base_url] ->
            if value == "/workspace" or String.starts_with?(value, "/workspace/"),
              do:
                NativeElixirPdfUtilities.TestSupport.Paths.project(
                  value
                  |> String.replace_prefix("/workspace", "")
                  |> String.trim_leading("/")
                ),
              else: value

          key when key in [:page_size, :chromium_page_size] ->
            if is_list(value), do: List.to_tuple(value), else: value

          key when key in [:forms, :style, :svg_reference] ->
            if is_binary(value), do: String.to_existing_atom(value), else: value

          _ ->
            value
        end

      {key, value}
    end)
  end
end
