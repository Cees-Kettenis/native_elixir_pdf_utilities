alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.ProjectStructureTest do
  use ExUnit.Case, async: true

  test "source paths follow their declared module names" do
    mismatches =
      Paths.project("lib/**/*.ex")
      |> Path.wildcard()
      |> Enum.flat_map(fn path ->
        [_, name] =
          Regex.run(~r/^defmodule (NativeElixirPdfUtilities\.[\w.]+) do/m, File.read!(path))

        relative =
          name |> String.replace_prefix("NativeElixirPdfUtilities.", "") |> Macro.underscore()

        expected = Paths.project("lib/" <> relative <> ".ex")
        if path == expected, do: [], else: [{path, expected}]
      end)

    assert mismatches == []
  end

  test "tests and fixtures use feature names instead of development sequence labels" do
    paths =
      Path.wildcard(Paths.project("test/**/*_test.exs")) ++
        Path.wildcard(Paths.fixture("**/*.html"))

    unclear =
      Enum.filter(paths, fn path ->
        Regex.match?(
          ~r/(?:^|_)(?:wave\d+|batch\d*|followup|closure|remaining|residual|restoration|evidence|regression)(?:_|\.)/i,
          Path.basename(path)
        )
      end)

    assert unclear == []
  end

  test "test suites have an explicit owner and dedicated browser suites use a consistent suffix" do
    allowed = ~w(api core pdf validators project html_to_pdf)
    paths = Path.wildcard(Paths.project("test/**/*_test.exs"))

    for path <- paths do
      relative = Path.relative_to(path, Paths.project("test"))
      [owner | rest] = Path.split(relative)
      assert owner in allowed and rest != [], "Test has no feature owner: #{relative}"

      if Regex.match?(~r/^\s*@moduletag\s+:browser_parity\s*$/m, File.read!(path)) do
        assert String.ends_with?(path, "_browser_parity_test.exs")
        assert String.starts_with?(relative, "html_to_pdf/browser_parity/")
      end
    end
  end
end
