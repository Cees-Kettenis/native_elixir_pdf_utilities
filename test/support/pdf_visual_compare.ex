defmodule NativeElixirPdfUtilities.TestSupport.PdfVisualCompare do
  @moduledoc false

  import ExUnit.Assertions

  alias NativeElixirPdfUtilities.HtmlToPdf

  @type comparison_stats :: %{
          required(:fixture) => String.t(),
          required(:page_count) => non_neg_integer(),
          required(:max_changed_ratio) => float(),
          required(:max_average_delta) => float(),
          required(:max_channel_delta) => non_neg_integer(),
          required(:artifact_dir) => String.t()
        }

  @doc false
  @spec assert_browser_match!(String.t(), keyword()) :: comparison_stats()
  def assert_browser_match!(fixture_path, opts \\ []) do
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
    chromium_fixture_path = chromium_fixture_path!(fixture_path, artifact_dir, opts)

    render_chromium_pdf!(chromium_bin, chromium_fixture_path, chromium_pdf)

    html = File.read!(fixture_path)
    assert {:ok, native_pdf_binary} = HtmlToPdf.render(html, render_opts)
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
      max_changed_ratio: max_stat(page_stats, :changed_ratio),
      max_average_delta: max_stat(page_stats, :average_delta),
      max_channel_delta: max_stat(page_stats, :max_channel_delta),
      artifact_dir: Path.expand(artifact_dir)
    }

    max_changed_ratio = Keyword.get(opts, :max_changed_ratio, 0.12)
    max_average_delta = Keyword.get(opts, :max_average_delta, 0.03)

    assert stats.max_changed_ratio <= max_changed_ratio,
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

  defp default_render_opts(fixture_path) do
    page_opts = [page_size: {612, 792}, margin: 0, base_url: Path.dirname(fixture_path)]

    case ttf_font_path() do
      nil -> page_opts
      path -> Keyword.put(page_opts, :fonts, [%{family: "DejaVu Sans", path: path}])
    end
  end

  defp ttf_font_path do
    [
      "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
      "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
      "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf"
    ]
    |> Enum.find(&File.exists?/1)
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

  defp render_chromium_pdf!(chromium_bin, fixture_path, output_path) do
    args = [
      "--headless",
      "--no-sandbox",
      "--disable-gpu",
      "--disable-dev-shm-usage",
      "--allow-file-access-from-files",
      "--run-all-compositor-stages-before-draw",
      "--virtual-time-budget=1000",
      "--no-pdf-header-footer",
      "--print-to-pdf=#{output_path}",
      file_url(fixture_path)
    ]

    case System.cmd(chromium_bin, args, stderr_to_stdout: true) do
      {_output, 0} ->
        assert File.exists?(output_path),
               "Chromium completed but did not write #{output_path}"

      {output, status} ->
        flunk("""
        Chromium PDF rendering failed with status #{status}
        command: #{chromium_bin} #{Enum.join(args, " ")}
        output:
        #{output}
        """)
    end
  end

  defp chromium_fixture_path!(fixture_path, artifact_dir, opts) do
    case Keyword.get(opts, :chromium_page_size) do
      nil ->
        fixture_path

      page_size ->
        chromium_fixture_path = Path.join(artifact_dir, "chromium-input.html")

        fixture_path
        |> File.read!()
        |> inject_chromium_page_css(page_size)
        |> then(&File.write!(chromium_fixture_path, &1))

        chromium_fixture_path
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
      _ -> page_css <> "\n" <> html
    end
  end

  defp chromium_page_size_css(page_size) do
    case page_size do
      :a4 -> "A4"
      :letter -> "letter"
      {width, height} when is_number(width) and is_number(height) -> "#{width}in #{height}in"
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

    diff_stats(
      crop_pixels(chromium, comparison_width, comparison_height),
      crop_pixels(native, comparison_width, comparison_height)
    )
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
      changed_ratio: stats.changed_pixels / stats.total_pixels,
      average_delta: stats.total_delta / (stats.total_pixels * 3 * 255),
      max_channel_delta: stats.max_channel_delta
    }
  end

  defp max_stat([], _key), do: 0.0

  defp max_stat(stats, key) do
    stats
    |> Enum.map(&Map.fetch!(&1, key))
    |> Enum.max()
  end
end
