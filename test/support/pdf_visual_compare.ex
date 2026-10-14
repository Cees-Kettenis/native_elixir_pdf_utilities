defmodule NativeElixirPdfUtilities.TestSupport.PdfVisualCompare do
  @moduledoc false

  import ExUnit.Assertions

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.CssParser

  @type comparison_stats :: %{
          required(:fixture) => String.t(),
          required(:page_count) => non_neg_integer(),
          required(:max_changed_ratio) => float(),
          required(:max_average_delta) => float(),
          required(:max_channel_delta) => non_neg_integer(),
          required(:pages) => [map()],
          required(:dpi) => pos_integer(),
          required(:pixel_channel_tolerance) => non_neg_integer(),
          required(:chromium_version) => String.t(),
          required(:scripting) => :disabled,
          required(:rasterizer_version) => String.t(),
          required(:font_sources) => [map()],
          required(:embedded_fonts) => %{chromium: String.t(), native: String.t()},
          required(:artifact_dir) => String.t()
        }

  @type visual_change_stats :: %{
          required(:page) => pos_integer(),
          required(:width) => pos_integer(),
          required(:height) => pos_integer(),
          required(:changed_pixels) => pos_integer(),
          required(:changed_ratio) => float(),
          required(:bounds) =>
            {non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()}
        }

  @doc false
  @spec gauge_reference(String.t()) :: String.t()
  def gauge_reference(html) do
    # Chromium 152 overflows max-min for these finite meter endpoints and paints
    # zero. HTML requires the relative midpoint. Compare equivalent finite range
    # geometry while the native validator/layout tests retain the extreme inputs.
    String.replace(html, "min=-1e308 max=1e308 value=0", "min=0 max=1 value=.5")
  end

  @doc false
  @spec render_initial_numbers(String.t(), keyword()) :: {:ok, binary()} | {:error, term()}
  def render_initial_numbers(html, opts) do
    # Supply the physical caller font which Chromium already selects for the
    # untouched Arial UA style. Reference fonts and markup remain unchanged.
    directory = liberation_font_directory("initial Number controls")

    font = %{
      family: "Arial",
      path: Path.join(directory, "LiberationSans-Regular.ttf"),
      weight: 400
    }

    source_sha256 =
      font.path
      |> File.read!()
      |> then(&:crypto.hash(:sha256, &1))
      |> Base.encode16(case: :lower)

    assert source_sha256 in [
             "bade59d822652f76e6941aa87b40a87c13d1cc70db98ededb5011127efafd1d3",
             "baccc64becc3eb7d104b7c84d99f5314a0a1f896e2b3ea6c2f22fc08d2003bee"
           ],
           "Initial Number Arial source does not match the verified Chromium font: #{font.path} SHA256 #{source_sha256}"

    HtmlToPdf.render(html, Keyword.update!(opts, :fonts, &(&1 ++ [font])))
  end

  @doc "Renders unchanged embedded HTML using the exact caller-provided font bytes in each child scope."
  @spec render_embedded_font_bytes(String.t(), keyword(), String.t()) ::
          {:ok, binary()} | {:error, term()}
  def render_embedded_font_bytes(html, opts, fixture_path) do
    fonts =
      Enum.map(Keyword.fetch!(default_render_opts(fixture_path), :fonts), fn font ->
        font |> Map.put(:data, File.read!(Map.fetch!(font, :path))) |> Map.delete(:path)
      end)

    HtmlToPdf.render(html, Keyword.put(opts, :fonts, fonts))
  end

  @doc "Runs a browser comparison with the exact bundled font directory and the existing host font-rendering policy, restoring the caller environment."
  @spec with_embedded_font_configuration(String.t(), (-> term())) :: term()
  def with_embedded_font_configuration(fixture_path, callback) do
    fonts = Keyword.fetch!(default_render_opts(fixture_path), :fonts)
    directory = Path.expand("tmp/embedded-document-fontconfig")
    File.mkdir_p!(directory)
    configuration = Path.join(directory, "fonts.conf")
    font_directories = fonts |> Enum.map(&Path.dirname(&1.path)) |> Enum.uniq()
    declarations = Enum.map_join(font_directories, &"<dir>#{&1}</dir>")

    File.write!(configuration, """
    <?xml version="1.0"?>
    <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
    <fontconfig><include>/etc/fonts/fonts.conf</include>#{declarations}<cachedir>#{directory}/cache</cachedir></fontconfig>
    """)

    previous = System.get_env("FONTCONFIG_FILE")
    System.put_env("FONTCONFIG_FILE", configuration)

    try do
      callback.()
    after
      if previous,
        do: System.put_env("FONTCONFIG_FILE", previous),
        else: System.delete_env("FONTCONFIG_FILE")
    end
  end

  @doc "Checks that converted SVG artwork is stored as image data rather than custom glyph programs."
  @spec assert_svg_image_artwork!(binary()) :: :ok
  def assert_svg_image_artwork!(pdf) do
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    images =
      for {_ref, %{value: %{"Subtype" => {:name, "Image"}}} = object} <- document.objects,
          do: object

    assert images != [], "Expected converted SVG artwork in a PDF Image XObject"

    for image <- images do
      assert is_integer(image.value["Width"]) and image.value["Width"] > 0
      assert is_integer(image.value["Height"]) and image.value["Height"] > 0
      assert image.value["BitsPerComponent"] == 8
      assert is_binary(image.stream) and byte_size(image.stream) > 0
    end

    refute pdf =~ "/Subtype /Type3"
    refute pdf =~ "/NativeOriginalGID"
    refute pdf =~ "/NativeSourceFont"
    :ok
  end

  @doc "Checks that SVG image artwork has no selectable or source-only text."
  @spec assert_svg_image_without_text!(binary()) :: :ok
  def assert_svg_image_without_text!(pdf) do
    assert {:error,
            {:no_extractable_text,
             %{stage: :text_extraction, reason: :no_extractable_text, message: message}}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert is_binary(message) and message != ""
    refute pdf =~ "/ActualText"
    assert_svg_image_artwork!(pdf)
  end

  @doc "Preserves the original Chromium reference while checking the native foreignObject diagnostic, without claiming visual parity."
  @spec assert_browser_unsupported!(String.t(), keyword()) :: map()
  def assert_browser_unsupported!(fixture_path, opts \\ []) do
    context = prepare_browser_fixture!(fixture_path, opts)

    assert {:error,
            {:unsupported_html,
             %{
               stage: :layout,
               reason: :unsupported_html,
               operation: :convert,
               module: NativeElixirPdfUtilities.Validators.SvgConverterValidator,
               source: "foreignObject",
               message: message
             } = diagnostic}} = context.native_result

    assert message =~ "foreignObject" and message =~ "converter"
    assert message =~ "cannot be omitted"
    refute File.exists?(context.native_pdf)

    pages =
      rasterize_pdf!(
        context.pdftoppm_bin,
        context.chromium_pdf,
        Path.join(context.artifact_dir, "chromium"),
        context.dpi
      )

    assert pages != []
    File.cp!(context.fixture_path, Path.join(context.artifact_dir, "original.html"))

    File.write!(
      Path.join(context.artifact_dir, "native-render-options.exs"),
      inspect(context.render_opts, pretty: true, limit: :infinity)
    )

    stats = %{
      status: :unsupported_feature,
      visual_comparison: :not_performed,
      fixture: context.fixture_path,
      diagnostic: diagnostic,
      chromium_page_count: length(pages),
      chromium_pages: pages,
      chromium_version: command_output!(context.chromium_bin, ["--version"]),
      scripting: :disabled,
      dpi: context.dpi,
      rasterizer_version:
        command_output!(context.pdftoppm_bin, ["-v"]) |> String.split("\n") |> hd(),
      font_sources: source_fonts(context.render_opts),
      embedded_fonts: %{chromium: command_output!(pdffonts_bin!(), [context.chromium_pdf])},
      artifact_dir: Path.expand(context.artifact_dir)
    }

    File.write!(
      Path.join(context.artifact_dir, "unsupported-feature.exs"),
      inspect(stats, pretty: true, limit: :infinity)
    )

    stats
  end

  @doc false
  @spec assert_browser_match!(String.t(), keyword()) :: comparison_stats()
  def assert_browser_match!(fixture_path, opts \\ []) do
    %{
      fixture_path: fixture_path,
      artifact_dir: artifact_dir,
      chromium_bin: chromium_bin,
      pdftoppm_bin: pdftoppm_bin,
      dpi: dpi,
      render_opts: render_opts,
      chromium_pdf: chromium_pdf,
      native_pdf: native_pdf,
      native_result: native_result
    } = prepare_browser_fixture!(fixture_path, opts)

    assert {:ok, native_pdf_binary} = native_result
    File.write!(native_pdf, native_pdf_binary)

    chromium_pages =
      rasterize_pdf!(pdftoppm_bin, chromium_pdf, Path.join(artifact_dir, "chromium"), dpi)

    native_pages =
      rasterize_pdf!(pdftoppm_bin, native_pdf, Path.join(artifact_dir, "native"), dpi)

    assert length(native_pages) == length(chromium_pages),
           """
           Browser parity page count mismatch for #{fixture_path}
           chromium pages: #{length(chromium_pages)}
           native pages: #{length(native_pages)}
           artifacts: #{Path.expand(artifact_dir)}
           """

    page_stats =
      chromium_pages
      |> Enum.zip(native_pages)
      |> Enum.with_index(1)
      |> Enum.map(fn {{chromium_page, native_page}, page_number} ->
        compare_page!(chromium_page, native_page, page_number, artifact_dir)
      end)

    stats = %{
      fixture: fixture_path,
      page_count: length(page_stats),
      pages: page_stats,
      max_changed_ratio: max_stat(page_stats, :changed_ratio),
      max_average_delta: max_stat(page_stats, :average_delta),
      max_channel_delta: max_stat(page_stats, :max_channel_delta),
      dpi: dpi,
      pixel_channel_tolerance: 12,
      chromium_version: command_output!(chromium_bin, ["--version"]),
      scripting: :disabled,
      rasterizer_version: command_output!(pdftoppm_bin, ["-v"]) |> String.split("\n") |> hd(),
      font_sources: source_fonts(render_opts),
      embedded_fonts: %{
        chromium: command_output!(pdffonts_bin!(), [chromium_pdf]),
        native: command_output!(pdffonts_bin!(), [native_pdf])
      },
      artifact_dir: Path.expand(artifact_dir)
    }

    File.write!(
      Path.join(artifact_dir, "stats.exs"),
      inspect(stats, pretty: true, limit: :infinity)
    )

    max_changed_ratio = Keyword.get(opts, :max_changed_ratio, 0.12)
    max_average_delta = Keyword.get(opts, :max_average_delta, 0.03)

    assert stats.max_changed_ratio < max_changed_ratio,
           """
           Browser parity changed-pixel threshold exceeded for #{fixture_path}
           changed ratio: #{Float.round(stats.max_changed_ratio, 5)}
           allowed: #{max_changed_ratio}
           average delta: #{Float.round(stats.max_average_delta, 5)}
           max channel delta: #{stats.max_channel_delta}
           artifacts: #{stats.artifact_dir}
           """

    assert stats.max_average_delta <= max_average_delta,
           """
           Browser parity average-delta threshold exceeded for #{fixture_path}
           average delta: #{Float.round(stats.max_average_delta, 5)}
           allowed: #{max_average_delta}
           changed ratio: #{Float.round(stats.max_changed_ratio, 5)}
           max channel delta: #{stats.max_channel_delta}
           artifacts: #{stats.artifact_dir}
           """

    stats
  end

  @doc false
  @spec assert_pdf_visual_change!(binary(), binary(), keyword()) :: [visual_change_stats()]
  def assert_pdf_visual_change!(before_pdf, after_pdf, opts \\ []) do
    stats = pdf_visual_stats!(before_pdf, after_pdf, opts)

    for page <- stats do
      assert page.changed_pixels > 0,
             "Visual comparison found no changed pixels on page #{page.page}"
    end

    stats
  end

  @doc false
  @spec pdf_visual_stats!(binary(), binary(), keyword()) :: [visual_change_stats()]
  def pdf_visual_stats!(before_pdf, after_pdf, opts \\ []) do
    artifact_dir = Keyword.get(opts, :artifact_dir, Path.join("tmp", "stamp_visual"))
    pdftoppm_bin = Keyword.get(opts, :pdftoppm_bin, pdftoppm_bin!())
    dpi = Keyword.get(opts, :dpi, 72)

    File.rm_rf!(artifact_dir)
    File.mkdir_p!(artifact_dir)

    before_path = Path.join(artifact_dir, "before.pdf")
    after_path = Path.join(artifact_dir, "after.pdf")
    File.write!(before_path, before_pdf)
    File.write!(after_path, after_pdf)

    before_pages =
      rasterize_pdf!(pdftoppm_bin, before_path, Path.join(artifact_dir, "before"), dpi)

    after_pages =
      rasterize_pdf!(pdftoppm_bin, after_path, Path.join(artifact_dir, "after"), dpi)

    assert length(before_pages) == length(after_pages),
           "Visual comparison page count changed; artifacts: #{Path.expand(artifact_dir)}"

    before_pages
    |> Enum.zip(after_pages)
    |> Enum.with_index(1)
    |> Enum.map(fn {{before_page, after_page}, page_number} ->
      visual_change_stats!(before_page, after_page, page_number, artifact_dir)
    end)
  end

  defp prepare_browser_fixture!(fixture_path, opts) do
    fixture_path = Path.expand(fixture_path)
    fixture_name = fixture_path |> Path.basename() |> Path.rootname()

    artifact_dir =
      Keyword.get(opts, :artifact_dir, Path.join(["tmp", "browser_parity", fixture_name]))

    chromium_bin = Keyword.get(opts, :chromium_bin, chromium_bin!())
    pdftoppm_bin = Keyword.get(opts, :pdftoppm_bin, pdftoppm_bin!())
    dpi = Keyword.get(opts, :dpi, 72)

    render_opts =
      fixture_path
      |> default_render_opts()
      |> Keyword.merge(Keyword.get(opts, :render_opts, []))

    File.rm_rf!(artifact_dir)
    File.mkdir_p!(artifact_dir)

    chromium_pdf = Path.join(artifact_dir, "chromium.pdf")
    native_pdf = Path.join(artifact_dir, "native.pdf")

    {chromium_fixture_path, temporary_input} =
      chromium_fixture_path!(fixture_path, artifact_dir, opts, render_opts)

    try do
      render_chromium_pdf!(chromium_bin, chromium_fixture_path, chromium_pdf)
    after
      if temporary_input, do: File.rm!(temporary_input)
    end

    html = File.read!(fixture_path)
    native_renderer = Keyword.get(opts, :native_renderer, &HtmlToPdf.render/2)

    %{
      fixture_path: fixture_path,
      artifact_dir: artifact_dir,
      chromium_bin: chromium_bin,
      pdftoppm_bin: pdftoppm_bin,
      dpi: dpi,
      render_opts: render_opts,
      chromium_pdf: chromium_pdf,
      native_pdf: native_pdf,
      native_result: native_renderer.(html, render_opts)
    }
  end

  defp source_fonts(render_opts) do
    render_opts
    |> Keyword.get(:fonts, [])
    |> Enum.map(fn font ->
      font
      |> Map.take([:family, :path, :weight, :style])
      |> Map.put(
        :sha256,
        font.path
        |> File.read!()
        |> then(&:crypto.hash(:sha256, &1))
        |> Base.encode16(case: :lower)
      )
    end)
  end

  # Both ordinary explicit-family fixtures and the native-only Number caller
  # input use the same installed Liberation source directory.
  defp liberation_font_directory(fixture_path) do
    directory =
      ["/usr/share/fonts/liberation", "/usr/share/fonts/truetype/liberation"]
      |> Enum.find(&File.regular?(Path.join(&1, "LiberationSans-Regular.ttf")))

    assert directory,
           "Install Liberation Sans to compare #{fixture_path} with identical font files"

    directory
  end

  defp default_render_opts(fixture_path) do
    page_opts =
      fixture_path
      |> fixture_page_options()
      |> Keyword.put(:base_url, Path.dirname(fixture_path))

    font_dir = Path.expand("../../priv/fonts/dejavu", __DIR__)

    fonts = [
      %{family: "DejaVu Sans", path: Path.join(font_dir, "DejaVuSans.ttf"), weight: 400},
      %{family: "DejaVu Sans", path: Path.join(font_dir, "DejaVuSans-Bold.ttf"), weight: 700},
      %{
        family: "DejaVu Sans",
        path: Path.join(font_dir, "DejaVuSans-Oblique.ttf"),
        weight: 400,
        style: :italic
      },
      %{
        family: "DejaVu Sans",
        path: Path.join(font_dir, "DejaVuSans-BoldOblique.ttf"),
        weight: 700,
        style: :italic
      }
    ]

    fonts =
      case String.contains?(File.read!(fixture_path), "Liberation Sans") do
        true ->
          liberation_dir = liberation_font_directory(fixture_path)

          fonts ++
            [
              %{
                family: "Liberation Sans",
                path: Path.join(liberation_dir, "LiberationSans-Regular.ttf"),
                weight: 400
              },
              %{
                family: "Liberation Sans",
                path: Path.join(liberation_dir, "LiberationSans-Bold.ttf"),
                weight: 700
              }
            ]

        false ->
          fonts
      end

    fonts =
      if String.contains?(File.read!(fixture_path), "DejaVu Sans Mono") do
        fonts ++
          Enum.map(
            [
              {"", 400, :normal},
              {"-Bold", 700, :normal},
              {"-Oblique", 400, :italic},
              {"-BoldOblique", 700, :italic}
            ],
            fn {suffix, weight, style} ->
              %{
                family: "DejaVu Sans Mono",
                path: Path.join(font_dir, "DejaVuSansMono#{suffix}.ttf"),
                weight: weight,
                style: style
              }
            end
          )
      else
        fonts
      end

    fonts =
      if String.contains?(File.read!(fixture_path), "STIX Two Math") do
        fonts ++
          [
            %{
              family: "STIX Two Math",
              path: Path.expand("../../priv/fonts/stix/STIXTwoMath-Regular.ttf", __DIR__),
              weight: 400,
              style: :normal
            }
          ]
      else
        fonts
      end

    fonts =
      if String.contains?(File.read!(fixture_path), "Math Glyph Test Reference") do
        fonts ++
          [
            %{
              family: "Math Glyph Test Reference",
              weight: 400,
              style: :normal,
              path:
                Path.expand(
                  "../fixtures/html_to_pdf/browser_parity/assets/math-glyph-reference.ttf",
                  __DIR__
                )
            }
          ]
      else
        fonts
      end

    fonts =
      if String.contains?(File.read!(fixture_path), "Math Row Test Reference") do
        fonts ++
          [
            %{
              family: "Math Row Test Reference",
              weight: 400,
              style: :normal,
              path:
                Path.expand(
                  "../fixtures/html_to_pdf/browser_parity/assets/math-row-reference.ttf",
                  __DIR__
                )
            }
          ]
      else
        fonts
      end

    fonts =
      if String.contains?(File.read!(fixture_path), "Math Stretch Test Reference") do
        fonts ++
          [
            %{
              family: "Math Stretch Test Reference",
              weight: 400,
              style: :normal,
              path:
                Path.expand(
                  "../fixtures/html_to_pdf/browser_parity/assets/math-stretch-reference.ttf",
                  __DIR__
                )
            }
          ]
      else
        fonts
      end

    fonts =
      if String.contains?(File.read!(fixture_path), "Math Operator Core Reference") do
        fonts ++
          [
            %{
              family: "Math Operator Core Reference",
              weight: 400,
              style: :normal,
              path:
                Path.expand(
                  "../fixtures/html_to_pdf/browser_parity/assets/math-operator-core-reference.ttf",
                  __DIR__
                )
            }
          ]
      else
        fonts
      end

    Enum.each(fonts, fn font ->
      assert File.regular?(font.path), "Missing bundled browser parity font: #{font.path}"
    end)

    Keyword.put(page_opts, :fonts, fonts)
  end

  defp fixture_page_options(fixture_path) do
    case raw_fixture_page_options(fixture_path) do
      {:ok, []} -> [page_size: {612, 792}, margin: 0]
      {:ok, page_options} -> page_options
      {:error, :invalid_css} -> [page_size: {612, 792}, margin: 0]
    end
  end

  defp raw_fixture_page_options(fixture_path) do
    fixture_path
    |> File.read!()
    |> CssParser.page_options()
  end

  defp chromium_bin! do
    System.get_env("CHROMIUM_BIN") ||
      System.find_executable("chromium") ||
      System.find_executable("chromium-browser") ||
      System.find_executable("google-chrome") ||
      System.find_executable("google-chrome-stable") ||
      flunk("Set CHROMIUM_BIN to a Chromium-compatible browser binary for browser parity tests")
  end

  defp pdftoppm_bin! do
    System.get_env("PDFTOPPM_BIN") ||
      System.find_executable("pdftoppm") ||
      flunk("Set PDFTOPPM_BIN to pdftoppm for browser parity tests")
  end

  defp pdffonts_bin! do
    System.find_executable("pdffonts") ||
      flunk("Install pdffonts from Poppler to record the fonts used by browser parity tests")
  end

  defp command_output!(command, args) do
    case System.cmd(command, args, stderr_to_stdout: true) do
      {output, 0} -> String.trim(output)
      {output, status} -> flunk("#{command} failed with status #{status}: #{output}")
    end
  end

  defp render_chromium_pdf!(chromium_bin, fixture_path, output_path) do
    profile_dir = Path.join(Path.dirname(output_path), "chromium-profile")
    File.mkdir_p!(Path.join(profile_dir, "Default"))

    File.write!(
      Path.join([profile_dir, "Default", "Preferences"]),
      :json.encode(%{profile: %{default_content_setting_values: %{javascript: 2}}})
    )

    args = [
      "--headless",
      "--no-sandbox",
      "--disable-gpu",
      "--disable-dev-shm-usage",
      "--user-data-dir=#{Path.expand(profile_dir)}",
      "--allow-file-access-from-files",
      "--run-all-compositor-stages-before-draw",
      "--virtual-time-budget=1000",
      "--no-pdf-header-footer",
      "--print-to-pdf=#{output_path}",
      file_url(fixture_path)
    ]

    case System.cmd(chromium_bin, args, stderr_to_stdout: true) do
      {output, 0} ->
        assert File.exists?(output_path),
               "Chromium completed but did not write #{output_path}: #{output}"

        File.rm_rf!(profile_dir)

      {output, status} ->
        flunk("""
        Chromium PDF rendering failed with status #{status}
        command: #{chromium_bin} #{Enum.join(args, " ")}
        output:
        #{output}
        """)
    end
  end

  @doc "Builds equivalent CSS artwork for the fixed horizontal-rule fixture's pinned HTML presentation hints."
  @spec horizontal_rule_reference(String.t()) :: String.t()
  def horizontal_rule_reference(html) do
    # Chromium 151 sets hr color as background/border color and implements size
    # as content height, including invalid/nonpositive sizes. Pinned HTML instead
    # specifies color and, for color/noshade, half-size border widths. Keep every
    # native case unchanged; supply independent, explicit CSS to the browser.
    css = """
    <style>
    hr {color:#808080;background-color:transparent;border-color:#808080;}
    hr[color="RebeccaPurple"] {color:#663399;border-color:#663399;}
    hr[color="chucknorris"] {color:#c00000;border-color:#c00000;}
    hr[color="#abc"] {color:#aabbcc;border-color:#aabbcc;}
    hr[color="#abcdef12"] {color:#abde12;border-color:#abde12;}
    hr[color="red"] {color:#ff0000;border-color:#ff0000;}
    hr[color="navy"] {color:#000080;border-color:#000080;}
    hr[color="goldenrod"] {color:#daa520;border-color:#daa520;}
    hr[size="0"],hr[size="-1"] {height:auto;border-bottom-width:1px;}
    hr[size="0"][noshade] {border-width:0;}
    hr[size="1"][noshade] {height:auto;border-width:0.5px;}
    hr[size="3"][noshade],hr[size="3"][color] {height:auto;border-width:1.5px;}
    hr[size="8"][color] {height:auto;border-width:4px;}
    </style>
    """

    String.replace(html, "</head>", css <> "</head>", global: false)
  end

  defp chromium_fixture_path!(fixture_path, artifact_dir, opts, render_opts) do
    page_size =
      Keyword.get(opts, :chromium_page_size) ||
        chromium_default_page_size(fixture_path, render_opts)

    page_furniture = Keyword.get(opts, :chromium_page_furniture)
    fonts = Keyword.get(render_opts, :fonts, [])

    chromium_transform = Keyword.get(opts, :chromium_transform)

    case {page_size, page_furniture, fonts, chromium_transform} do
      {nil, nil, [], nil} ->
        {fixture_path, nil}

      _configured ->
        chromium_fixture_path = Path.join(artifact_dir, "chromium-input.html")

        html = File.read!(fixture_path)
        html = if chromium_transform, do: chromium_transform.(html), else: html

        html
        |> maybe_inject_chromium_fonts(fonts)
        |> maybe_inject_chromium_page_css(page_size)
        |> maybe_inject_chromium_page_furniture(
          page_furniture,
          Keyword.get(opts, :chromium_page_margin, 0)
        )
        |> maybe_inject_chromium_page_margin_css(Keyword.get(opts, :chromium_page_margin_css))
        |> then(&File.write!(chromium_fixture_path, &1))

        # Font injection must preserve the original document's resource directory.
        # Loading the artifact copy changes relative authored image/child URLs.
        # Retain that exact evidence copy while loading a temporary sibling;
        # source markup/resource references receive no reference transformation.
        temporary_input =
          Path.join(
            Path.dirname(fixture_path),
            ".chromium-input-#{System.pid()}-#{System.unique_integer([:positive])}.html"
          )

        File.cp!(chromium_fixture_path, temporary_input)
        {temporary_input, temporary_input}
    end
  end

  defp maybe_inject_chromium_fonts(html, fonts) do
    declarations =
      Enum.map_join(fonts, "\n", fn font ->
        family = Map.fetch!(font, :family)
        path = Map.fetch!(font, :path)
        weight = Map.get(font, :weight, 400)
        style = Map.get(font, :style, :normal)

        "@font-face { font-family: #{inspect(family)}; src: url(#{inspect(file_url(path))}) format(\"truetype\"); font-weight: #{weight}; font-style: #{style}; }"
      end)

    case declarations do
      "" ->
        html

      declarations ->
        style = "<style>\n#{declarations}\n</style>"

        case Regex.run(~r/<head\b[^>]*>/iu, html) do
          [head_tag] -> String.replace(html, head_tag, head_tag <> "\n" <> style, global: false)
          _ -> style <> "\n" <> html
        end
    end
  end

  defp maybe_inject_chromium_page_css(html, page_size) do
    case page_size do
      nil -> html
      page_size -> inject_chromium_page_css(html, page_size)
    end
  end

  defp maybe_inject_chromium_page_furniture(html, page_furniture, margin) do
    case {page_furniture, margin} do
      {nil, _margin} ->
        html

      {page_furniture, margin} when is_list(page_furniture) and is_number(margin) ->
        header = Keyword.get(page_furniture, :header, "")
        footer = Keyword.get(page_furniture, :footer, "")

        furniture = """
        <style>
          .browser-parity-running-header,
          .browser-parity-running-footer {
            position: fixed;
            left: 0;
            right: 0;
          }
          .browser-parity-running-header { top: -#{margin}pt; }
          .browser-parity-running-footer { bottom: -#{margin}pt; }
        </style>
        <div class="browser-parity-running-header">#{header}</div>
        <div class="browser-parity-running-footer">#{footer}</div>
        """

        case Regex.run(~r/<body\b[^>]*>/iu, html) do
          [body_tag] ->
            String.replace(html, body_tag, body_tag <> "\n" <> furniture, global: false)

          _ ->
            furniture <> "\n" <> html
        end
    end
  end

  defp maybe_inject_chromium_page_margin_css(html, css) do
    case css do
      nil ->
        html

      css when is_binary(css) ->
        style = "<style>\n#{css}\n</style>"

        case Regex.run(~r/<head\b[^>]*>/iu, html) do
          [head_tag] -> String.replace(html, head_tag, head_tag <> "\n" <> style, global: false)
          _ -> style <> "\n" <> html
        end
    end
  end

  defp chromium_default_page_size(fixture_path, render_opts) do
    case raw_fixture_page_options(fixture_path) do
      {:ok, []} -> Keyword.get(render_opts, :page_size)
      _page_options -> nil
    end
  end

  defp inject_chromium_page_css(html, page_size) do
    page_css = """
    <style>
      @page { size: #{chromium_page_size_css(page_size)}; margin: 0; }
      html, body { margin: 0; }
    </style>
    """

    case Regex.run(~r/<head\b[^>]*>/iu, html) do
      [head_tag] -> String.replace(html, head_tag, head_tag <> "\n" <> page_css, global: false)
      _ -> html <> "\n" <> page_css
    end
  end

  defp chromium_page_size_css(page_size) do
    case page_size do
      :a4 ->
        "A4"

      :letter ->
        "letter"

      {width, height}
      when is_number(width) and is_number(height) and width <= 20 and height <= 20 ->
        "#{width}in #{height}in"

      {width, height} when is_number(width) and is_number(height) ->
        "#{width}pt #{height}pt"
    end
  end

  defp file_url(path) do
    path
    |> Path.expand()
    |> URI.encode()
    |> then(&"file://#{&1}")
  end

  defp rasterize_pdf!(pdftoppm_bin, pdf_path, output_prefix, dpi) do
    case System.cmd(pdftoppm_bin, ["-r", to_string(dpi), pdf_path, output_prefix],
           stderr_to_stdout: true
         ) do
      {_output, 0} ->
        pages =
          output_prefix
          |> Kernel.<>("-*.ppm")
          |> Path.wildcard()
          |> Enum.sort_by(&page_number/1)

        assert pages != [],
               "pdftoppm completed but produced no PPM pages for #{pdf_path}"

        pages

      {output, status} ->
        flunk("""
        PDF rasterization failed with status #{status}
        command: #{pdftoppm_bin} -r #{dpi} #{pdf_path} #{output_prefix}
        output:
        #{output}
        """)
    end
  end

  defp page_number(path) do
    case Regex.run(~r/-(\d+)\.ppm\z/, path) do
      [_match, number] -> String.to_integer(number)
      _ -> 0
    end
  end

  defp compare_page!(chromium_page, native_page, page_number, artifact_dir) do
    chromium = read_ppm!(chromium_page)
    native = read_ppm!(native_page)

    width_delta = abs(native.width - chromium.width)
    height_delta = abs(native.height - chromium.height)

    assert width_delta <= 1 and height_delta <= 1,
           """
           Browser parity page dimensions differ on page #{page_number}
           chromium: #{chromium.width}x#{chromium.height}
           native: #{native.width}x#{native.height}
           artifacts: #{Path.expand(artifact_dir)}
           """

    comparison_width = min(chromium.width, native.width)
    comparison_height = min(chromium.height, native.height)

    stats =
      diff_stats(
        crop_pixels(chromium, comparison_width, comparison_height),
        crop_pixels(native, comparison_width, comparison_height)
      )

    Map.put(stats, :page, page_number)
  end

  defp visual_change_stats!(before_page, after_page, page_number, artifact_dir) do
    before = read_ppm!(before_page)
    after_image = read_ppm!(after_page)

    assert {before.width, before.height} == {after_image.width, after_image.height},
           "Visual comparison page dimensions changed on page #{page_number}; artifacts: #{Path.expand(artifact_dir)}"

    initial = %{
      changed_pixels: 0,
      min_x: before.width,
      min_y: before.height,
      max_x: 0,
      max_y: 0
    }

    changes =
      Enum.reduce(0..(before.width * before.height - 1), initial, fn pixel, acc ->
        offset = pixel * 3

        changed? =
          Enum.any?(0..2, fn channel ->
            abs(
              :binary.at(before.pixels, offset + channel) -
                :binary.at(after_image.pixels, offset + channel)
            ) > 12
          end)

        case changed? do
          true ->
            x = rem(pixel, before.width)
            y = div(pixel, before.width)

            %{
              changed_pixels: acc.changed_pixels + 1,
              min_x: min(acc.min_x, x),
              min_y: min(acc.min_y, y),
              max_x: max(acc.max_x, x),
              max_y: max(acc.max_y, y)
            }

          false ->
            acc
        end
      end)

    %{
      page: page_number,
      width: before.width,
      height: before.height,
      changed_pixels: changes.changed_pixels,
      changed_ratio: changes.changed_pixels / (before.width * before.height),
      bounds: {changes.min_x, changes.min_y, changes.max_x, changes.max_y}
    }
  end

  defp crop_pixels(image, width, height) do
    row_size = image.width * 3
    cropped_row_size = width * 3

    0..(height - 1)
    |> Enum.map(fn row ->
      binary_part(image.pixels, row * row_size, cropped_row_size)
    end)
    |> IO.iodata_to_binary()
  end

  defp read_ppm!(path) do
    binary = File.read!(path)

    with {"P6", offset} <- next_ppm_token(binary, 0),
         {width, offset} <- next_integer_token(binary, offset),
         {height, offset} <- next_integer_token(binary, offset),
         {255, offset} <- next_integer_token(binary, offset),
         offset <- skip_single_whitespace(binary, offset),
         pixels <- binary_part(binary, offset, byte_size(binary) - offset) do
      %{width: width, height: height, pixels: pixels}
    else
      _ -> flunk("Unsupported or invalid PPM output: #{path}")
    end
  end

  defp next_integer_token(binary, offset) do
    case next_ppm_token(binary, offset) do
      {token, next_offset} -> {String.to_integer(token), next_offset}
      nil -> nil
    end
  end

  defp next_ppm_token(binary, offset) do
    offset = skip_ppm_ignored(binary, offset)

    case offset < byte_size(binary) do
      true ->
        end_offset = scan_token_end(binary, offset)
        {binary_part(binary, offset, end_offset - offset), end_offset}

      false ->
        nil
    end
  end

  defp skip_ppm_ignored(binary, offset) do
    cond do
      offset >= byte_size(binary) ->
        offset

      :binary.at(binary, offset) in ~c[ \t\r\n] ->
        skip_ppm_ignored(binary, offset + 1)

      :binary.at(binary, offset) == ?# ->
        skip_ppm_ignored(binary, skip_comment(binary, offset + 1))

      true ->
        offset
    end
  end

  defp skip_comment(binary, offset) do
    cond do
      offset >= byte_size(binary) -> offset
      :binary.at(binary, offset) == ?\n -> offset + 1
      true -> skip_comment(binary, offset + 1)
    end
  end

  defp scan_token_end(binary, offset) do
    cond do
      offset >= byte_size(binary) -> offset
      :binary.at(binary, offset) in ~c[ \t\r\n] -> offset
      true -> scan_token_end(binary, offset + 1)
    end
  end

  defp skip_single_whitespace(binary, offset) do
    case offset < byte_size(binary) and :binary.at(binary, offset) in ~c[ \t\r\n] do
      true -> offset + 1
      false -> offset
    end
  end

  defp diff_stats(chromium_pixels, native_pixels) do
    chromium_pixels
    |> :binary.bin_to_list()
    |> Enum.zip(:binary.bin_to_list(native_pixels))
    |> Enum.chunk_every(3)
    |> Enum.reduce(
      %{changed_pixels: 0, total_pixels: 0, total_delta: 0, max_channel_delta: 0},
      fn pixel_channels, acc ->
        channel_deltas =
          Enum.map(pixel_channels, fn {chromium_channel, native_channel} ->
            abs(chromium_channel - native_channel)
          end)

        changed? = Enum.any?(channel_deltas, &(&1 > 12))

        %{
          changed_pixels: acc.changed_pixels + if(changed?, do: 1, else: 0),
          total_pixels: acc.total_pixels + 1,
          total_delta: acc.total_delta + Enum.sum(channel_deltas),
          max_channel_delta: max(acc.max_channel_delta, Enum.max(channel_deltas))
        }
      end
    )
    |> normalize_diff_stats()
  end

  defp normalize_diff_stats(stats) do
    %{
      changed_pixels: stats.changed_pixels,
      total_pixels: stats.total_pixels,
      changed_ratio: stats.changed_pixels / stats.total_pixels,
      average_delta: stats.total_delta / (stats.total_pixels * 3 * 255),
      max_channel_delta: stats.max_channel_delta
    }
  end

  defp max_stat(stats, key) do
    case stats do
      [] ->
        0.0

      stats ->
        stats
        |> Enum.map(&Map.fetch!(&1, key))
        |> Enum.max()
    end
  end
end
