defmodule NativeElixirPdfUtilities.PackageContentsTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.Paths

  test "Hex packages retain temporal picker sources, licenses and provenance" do
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

    for name <- ~w(calendar_icon.svg time_icon.svg LICENSE.chromium README.md) do
      path = "priv/html_to_pdf/temporal/" <> name

      assert Map.fetch(files, path) == {:ok, File.read!(Paths.project(path))},
             "Hex archive omitted or changed #{path}"
    end
  end
end
