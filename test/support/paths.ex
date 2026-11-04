defmodule NativeElixirPdfUtilities.TestSupport.Paths do
  @moduledoc "Repository paths shared by tests, independent of a suite's directory."
  @root Path.expand("../..", __DIR__)

  @doc "Returns an absolute path inside the repository."
  @spec project(String.t()) :: String.t()
  def project(relative), do: Path.expand(relative, @root)

  @doc "Returns an absolute path inside the maintained fixture corpus."
  @spec fixture(String.t()) :: String.t()
  def fixture(relative), do: Path.expand(relative, Path.join(@root, "test/fixtures"))

  @doc "Returns an absolute path to a shared test helper."
  @spec support(String.t()) :: String.t()
  def support(relative), do: Path.expand(relative, __DIR__)
end
