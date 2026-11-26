defmodule NativeElixirPdfUtilities.PackageContentsTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.Paths

  test "Hex package includes runtime files and licenses and excludes development files" do
    directory =
      Paths.project("tmp/package-contents-#{System.unique_integer([:positive, :monotonic])}")

    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)
    archive_path = Path.join(directory, "package.tar")

    {output, status} =
      System.cmd("mix", ["hex.build", "--output", archive_path],
        cd: Paths.project("."),
        env: [
          {"MIX_ENV", Atom.to_string(Mix.env())},
          {"ERL_FLAGS", "+S 2:2 +SDcpu 2:2 +SDio 1"}
        ],
        stderr_to_stdout: true
      )

    assert status == 0, output
    assert {:ok, archive} = :erl_tar.extract(String.to_charlist(archive_path), [:memory])
    assert {~c"contents.tar.gz", contents} = List.keyfind(archive, ~c"contents.tar.gz", 0)
    assert {:ok, files} = :erl_tar.extract({:binary, contents}, [:compressed, :memory])
    files = Map.new(files, fn {path, bytes} -> {List.to_string(path), bytes} end)

    for path <- ~w(
          mix.exs
          lib/html_to_pdf.ex
          LICENSE
          priv/html_to_pdf/temporal/calendar_icon.svg
          priv/html_to_pdf/temporal/time_icon.svg
          priv/html_to_pdf/temporal/LICENSE.chromium
          priv/html_to_pdf/temporal/README.md
          priv/fonts/brotli/dictionary.bin
          priv/fonts/dejavu/DejaVuSans.ttf
          priv/fonts/dejavu/DejaVuSans-Bold.ttf
          priv/fonts/dejavu/DejaVuSans-Oblique.ttf
          priv/fonts/dejavu/DejaVuSans-BoldOblique.ttf
          priv/fonts/dejavu/DejaVuSansMono.ttf
          priv/fonts/dejavu/DejaVuSansMono-Bold.ttf
          priv/fonts/dejavu/DejaVuSansMono-Oblique.ttf
          priv/fonts/dejavu/DejaVuSansMono-BoldOblique.ttf
          priv/fonts/dejavu/LICENSE.txt
          priv/fonts/dejavu/LICENSE-2.37.txt
          priv/fonts/stix/STIXTwoMath-Regular.ttf
          priv/fonts/stix/OFL.txt
          priv/fonts/stix/token-ink-metrics.bin
          priv/licenses/brotli-MIT.txt
          priv/licenses/cff-character-mappings-MIT.txt
          priv/licenses/icu-78-quotation-LICENSE.txt
          priv/licenses/unicode-character-properties-UNICODE-3.0.txt
        ) do
      assert Map.fetch(files, path) == {:ok, File.read!(Paths.project(path))},
             "Hex archive omitted or changed #{path}"
    end

    excluded_directories =
      ~w(.git/ .github/ .quality/ _build/ ci/ cover/ deps/ dev/ doc/ docs/development/ scripts/ test/ tmp/)

    assert [] == Enum.filter(Map.keys(files), &String.starts_with?(&1, excluded_directories))
    refute Map.has_key?(files, "AGENTS.md")
  end
end
