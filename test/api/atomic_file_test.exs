defmodule NativeElixirPdfUtilities.AtomicFileTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{AtomicFile, HtmlToPdf, Limits, Operation, Text}

  @moduletag :tmp_dir

  test "rendered output reopens, and render or input errors retain its bytes", %{tmp_dir: dir} do
    input = Path.join(dir, "input.html")
    output = Path.join(dir, "result.pdf")

    for word <- ["First", "Second"] do
      File.write!(input, "<p>#{word}</p>")
      assert :ok = HtmlToPdf.render_file(input, output)
      assert {:ok, ^word} = Text.extract_file(output, layout: false)
    end

    bytes = File.read!(output)
    File.write!(input, "<video></video>")
    assert {:error, {:unsupported_html, _}} = HtmlToPdf.render_file(input, output)
    assert {:error, {:enoent, _}} = HtmlToPdf.render_file(input <> ".missing", output)
    assert File.read!(output) == bytes
    assert Enum.sort(File.ls!(dir)) == ["input.html", "result.pdf"]
  end

  @tag skip: match?({:win32, _}, :os.type())
  test "output links follow the target and retain its ordinary permissions", %{tmp_dir: dir} do
    input = Path.join(dir, "input.html")
    output = Path.join(dir, "result.pdf")
    other = Path.join(dir, "hard-link")
    link = Path.join(dir, "relative-link")
    File.write!(input, "<p>Replacement</p>")
    File.write!(output, "original")
    File.chmod!(output, 0o640)
    File.ln!(output, other)
    File.ln_s!("result.pdf", link)
    assert :ok = HtmlToPdf.render_file(input, link)
    assert {:ok, "Replacement"} = Text.extract_file(output, layout: false)
    assert File.read_link!(link) == "result.pdf"
    assert File.read!(other) == "original"
    relative = Path.relative_to_cwd(link)
    assert :ok = HtmlToPdf.render_file(input, relative)
    assert Bitwise.band(File.stat!(output).mode, 0o777) == 0o640

    fifo = Path.join(dir, "fifo")
    assert {_, 0} = System.cmd("mkfifo", [fifo])
    loop_path = Path.join(dir, "loop")
    File.ln_s!("loop", loop_path)

    for {path, code} <- [
          {"", :enoent},
          {output <> "/", :enotdir},
          {fifo, :eacces},
          {loop_path, :eloop},
          {dir, :eisdir},
          {Path.join(output, "child"), :enotdir},
          {Path.join(dir, "missing/x"), :enoent}
        ] do
      assert {:error, {^code, %{operation: :write, source: ^path}}} =
               HtmlToPdf.render_file(input, path)
    end
  end

  @tag skip: match?({:win32, _}, :os.type()) or System.cmd("id", ["-u"]) == {"0\n", 0}
  test "a restricted output directory returns a write diagnostic and preserves the PDF", %{
    tmp_dir: dir
  } do
    restricted = Path.join(dir, "restricted")
    File.mkdir!(restricted)
    input = Path.join(restricted, "source.html")
    output = Path.join(restricted, "existing.pdf")
    File.write!(input, "<p>New</p>")
    assert {:ok, original} = HtmlToPdf.render("<p>Original</p>")
    File.write!(output, original)
    File.chmod!(restricted, 0o555)

    try do
      assert File.stat!(output).access == :read_write
      assert :ok = HtmlToPdf.render_file(input, Path.join(dir, "allowed.pdf"))

      for denied <- [output, Path.join(restricted, "new.pdf")] do
        assert {:error, {:eacces, diagnostic}} = HtmlToPdf.render_file(input, denied)
        assert %{module: HtmlToPdf, stage: :file, operation: :write, source: ^denied} = diagnostic
      end

      assert File.read!(output) == original
      assert Enum.sort(File.ls!(restricted)) == ["existing.pdf", "source.html"]
    after
      File.chmod!(restricted, 0o755)
    end
  end

  test "failed writes and failed installation clean up and retain the old output", %{tmp_dir: dir} do
    path = Path.join(dir, "result")
    File.write!(path, "original")

    assert {:error, :eexist} =
             AtomicFile.replace(path, fn prepare ->
               {:ok, staged} = prepare.()
               File.write!(staged, "partial", [:exclusive])
               assert File.read!(path) == "original"
               File.write(staged, "rest", [:exclusive])
             end)

    # The guard finishes discarding asynchronously on an operation error.
    assert_clean(dir, ["result"])

    missing = Path.join(dir, "new")

    assert {:error, :enoent} =
             AtomicFile.replace(missing, fn prepare ->
               {:ok, staged} = prepare.()
               File.write!(staged, "completed")
               File.rm!(staged)
               :ok
             end)

    assert {:error, :enoent} =
             AtomicFile.replace(path, fn prepare ->
               {:ok, _staged} = prepare.()
               :ok
             end)

    assert File.read!(path) == "original"
    assert_clean(dir, ["result"])
  end

  test "a bounded worker timeout after writing discards its staged file", %{tmp_dir: dir} do
    path = Path.join(dir, "result")
    File.write!(path, "original")
    owner = self()
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_operation_timeout_ms: 400})

    assert {:error, {:resource_limit_exceeded, %{operation: :render_file}}} =
             AtomicFile.replace(path, fn prepare ->
               Operation.run(HtmlToPdf, :render_file, fn ->
                 {:ok, staged} = prepare.()
                 File.write!(staged, "unfinished")
                 send(owner, :wrote_file)

                 receive do
                   :finish -> :ok
                 end
               end)
             end)

    assert_received :wrote_file
    assert File.read!(path) == "original"
    assert_clean(dir, ["result"])
  end

  test "caller exit discards staged bytes even if its after block cannot run", %{tmp_dir: dir} do
    parent = self()
    path = Path.join(dir, "result")
    File.write!(path, "original")

    {caller, monitor} =
      spawn_monitor(fn ->
        AtomicFile.replace(path, fn prepare ->
          {:ok, staged} = prepare.()
          File.write!(staged, "unpublished")
          send(parent, :staged)

          receive do
            :finish -> :ok
          end
        end)
      end)

    assert_receive :staged, 5_000
    Process.exit(caller, :kill)
    assert_receive {:DOWN, ^monitor, :process, ^caller, :killed}
    assert_clean(dir, ["result"])
    assert File.read!(path) == "original"
  end

  test "overlapping writers publish complete files in commit order", %{tmp_dir: dir} do
    parent = self()
    path = Path.join(dir, "result")
    File.write!(path, "original")

    tasks =
      for contents <- ["first", "second"] do
        task =
          Task.async(fn ->
            AtomicFile.replace(path, fn prepare ->
              {:ok, staged} = prepare.()
              File.write!(staged, contents)
              send(parent, {:staged, contents})

              receive do
                :commit -> :ok
              end
            end)
          end)

        assert_receive {:staged, ^contents}, 5_000
        {task, contents}
      end

    assert File.read!(path) == "original"

    for {task, contents} <- Enum.reverse(tasks) do
      send(task.pid, :commit)
      assert :ok = Task.await(task)
      assert File.read!(path) == contents
    end

    assert_clean(dir, ["result"])
  end

  defp assert_clean(dir, expected) do
    # Permit cleanup to run on its own scheduler after a cancellation.
    assert Enum.any?(1..200, fn _ ->
             if Enum.sort(File.ls!(dir)) == expected do
               true
             else
               Process.sleep(10)
               false
             end
           end)
  end
end
