defmodule NativeElixirPdfUtilities.FileReaderTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{FileReader, HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{AssetLoader, Style}
  alias NativeElixirPdfUtilities.Validators.{FileValidator, HtmlValidator}

  setup do
    root = Path.join(System.tmp_dir!(), "bounded-#{System.unique_integer([:positive])}")
    File.mkdir_p!(root)
    limits = Limits.effective()

    on_exit(fn ->
      File.rm_rf!(root)
      Limits.install(limits)
    end)

    {:ok, root: root}
  end

  test "portable reads enforce byte limits and validate regular files", %{root: root} do
    path = Path.join(root, "file")
    File.write!(path, "1234")
    assert {:ok, "1234"} = FileReader.read(path, 4)

    assert {:error, {:resource_limit_exceeded, %{stage: :file, source: ^path}}} =
             FileReader.read(path, 3)

    File.write!(path, "")
    assert {:ok, ""} = FileReader.read(path, 1)
    assert {:error, :enoent} = FileReader.read(Path.join(root, "missing"), 1)
    assert {:error, {:invalid_options, _}} = FileReader.read(nil, 1)

    assert {:error, {:invalid_document, _}} =
             FileValidator.validate_info(%File.Stat{type: :directory}, 10, path)

    assert {:error, {:invalid_options, %{stage: :file}}} = FileReader.read(path, 0)

    assert {:error, {:invalid_document, %{stage: :file, source: ^root}}} =
             FileValidator.validate_info(%File.Stat{type: :directory, size: 0}, 1, root)

    assert {:error, {:invalid_document, %{stage: :file, source: ^root}}} =
             FileReader.read(root, 1)
  end

  @tag skip: :os.type() != {:unix, :linux}
  test "regular files with a zero reported size still obey the actual byte ceiling" do
    # Linux exposes this process's newline-terminated status as a regular
    # file. Its reported size is zero even though the descriptor yields data.
    path = "/proc/self/comm"
    assert File.stat!(path).size == 0
    expected = File.read!(path)
    assert byte_size(expected) > 1
    assert {:ok, ^expected} = FileReader.read(path, byte_size(expected))

    assert {:error, {:resource_limit_exceeded, %{source: ^path}}} =
             FileReader.read(path, byte_size(expected) - 1)
  end

  test "an empty authorized file still fits an exhausted operation", %{root: root} do
    path = Path.join(root, "empty.css")
    File.write!(path, "")
    Limits.install(%{Limits.effective() | max_input_bytes: 1})

    HtmlValidator.with_render_budget(fn ->
      HtmlValidator.reserve_render_resource(:max_input_bytes, 1, :html)
      assert {:ok, ""} = FileReader.read(path, :remaining_input)
      File.write!(path, "x")

      assert {:error, {:resource_limit_exceeded, %{stage: :file, source: ^path}}} =
               FileReader.read(path, :remaining_input)
    end)
  end

  test "remaining-byte admission rejects a new oversized source before opening it", %{root: root} do
    exact = Path.join(root, "exact.txt")
    excess = Path.join(root, "excess.txt")
    File.write!(exact, "yes")
    File.write!(excess, "more")
    Limits.install(%{Limits.effective() | max_input_bytes: 5})

    task =
      Task.async(fn ->
        receive do
          :read -> :ok
        end

        HtmlValidator.with_render_budget(fn ->
          HtmlValidator.reserve_render_resource(:max_input_bytes, 2, :html)
          assert {:ok, "yes"} = FileReader.read(exact, :remaining_input)
          FileReader.read(excess, :remaining_input)
        end)
      end)

    :erlang.trace_pattern({:file, :open, 2}, true, [:local])

    try do
      :erlang.trace(task.pid, true, [:call])
      send(task.pid, :read)
      assert {:error, {:resource_limit_exceeded, %{source: ^excess}}} = Task.await(task)
      assert_receive {:trace, _, :call, {:file, :open, [^exact, _]}}
      refute_receive {:trace, _, :call, {:file, :open, [^excess, _]}}
    after
      :erlang.trace_pattern({:file, :open, 2}, false, [:local])
    end
  end

  @tag skip: match?({:win32, _}, :os.type())
  test "FIFO inputs and symlinks return diagnostics without waiting for a writer", %{root: root} do
    fifo = Path.join(root, "input.fifo")
    link = Path.join(root, "input-link")
    assert {_, 0} = System.cmd("mkfifo", [fifo])
    File.ln_s!(fifo, link)

    for path <- [fifo, link] do
      reader = Task.async(fn -> FileReader.read(path, 1024) end)

      try do
        assert {:ok, {:error, {:invalid_document, diagnostic}}} = Task.yield(reader, 1_000)
        assert diagnostic.stage == :file
        assert diagnostic.reason == :invalid_document
        assert diagnostic.source == path
        assert diagnostic.message == "approved input must be a regular file"
      after
        # Release a blocked open even when testing the old implementation.
        # Killing its task alone cannot interrupt the runtime's file syscall.
        {:ok, writer} = :file.open(fifo, [:raw, :read, :write])
        Task.shutdown(reader, 1_000)
        :file.close(writer)
      end
    end
  end

  @tag skip: match?({:win32, _}, :os.type())
  test "public file and asset APIs reject FIFOs before parsing", %{root: root} do
    fifo = Path.join(root, "input.fifo")
    assert {_, 0} = System.cmd("mkfifo", [fifo])

    reader =
      Task.async(fn ->
        [
          HtmlToPdf.render_file(fifo, Path.join(root, "out.pdf")),
          Text.extract_file(fifo),
          Text.extract_file_spans(fifo),
          Style.load_stylesheets(%{type: :document, children: []}, stylesheets: [{:file, fifo}]),
          AssetLoader.resolve("input.fifo", :image, base_url: root),
          AssetLoader.resolve("input.fifo", :font, base_url: root)
        ]
      end)

    try do
      assert {:ok, results} = Task.yield(reader, 1_000)

      for {result, operation} <-
            Enum.zip(results, [:render_file, :extract_file, :extract_file_spans]) do
        assert {:error, {:invalid_document, diagnostic}} = result
        assert diagnostic.reason == :invalid_document
        assert diagnostic.stage == :file
        assert diagnostic.source == fifo
        assert diagnostic.operation == operation
        assert diagnostic.module == if(operation == :render_file, do: HtmlToPdf, else: Text)
        assert diagnostic.message =~ "regular file"
      end

      assert {:error, {:invalid_document, %{source: ^fifo}}} = Enum.at(results, 3)

      for result <- Enum.drop(results, 4) do
        assert {:error, {:invalid_document, %{stage: :file, source: ^fifo}}} = result
      end

      assert {:ok, "approved"} =
               AssetLoader.resolve("input.fifo", :image,
                 base_url: root,
                 asset_resolver: fn _ -> {:ok, "approved"} end
               )

      refute File.exists?(Path.join(root, "out.pdf"))
    after
      {:ok, writer} = :file.open(fifo, [:raw, :read, :write])
      Task.shutdown(reader, 1_000)
      :file.close(writer)
    end
  end

  test "base_url image and font reads require no external executable", %{root: root} do
    path = System.get_env("PATH")
    System.put_env("PATH", root)

    try do
      File.write!(Path.join(root, "asset"), "approved")

      for kind <- [:image, :font] do
        assert {:ok, "approved"} = AssetLoader.resolve("asset", kind, base_url: root)
      end

      assert {:ok, "resolved"} =
               AssetLoader.resolve("missing", :image,
                 base_url: root,
                 asset_resolver: fn _ -> {:ok, "resolved"} end
               )
    after
      if path, do: System.put_env("PATH", path), else: System.delete_env("PATH")
    end
  end

  test "unreadable local assets use the resolver or return a diagnostic", %{root: root} do
    assert {:error, {:invalid_document, %{stage: :file, source: ^root}}} =
             AssetLoader.resolve(".", :image, base_url: root)

    assert {:ok, "approved"} =
             AssetLoader.resolve(".", :image,
               base_url: root,
               asset_resolver: fn _ -> {:ok, "approved"} end
             )
  end

  test "HTML CSS and PDF entry points reject oversized files before parsing", %{root: root} do
    path = Path.join(root, "large")
    File.write!(path, "12345")

    Limits.install(%{
      Limits.effective()
      | max_input_bytes: 4
    })

    assert {:error, {:resource_limit_exceeded, %{operation: :render_file, source: ^path}}} =
             HtmlToPdf.render_file(path, Path.join(root, "out.pdf"))

    assert {:error, {:resource_limit_exceeded, %{source: ^path}}} =
             Style.load_stylesheets(%{type: :document, children: []},
               stylesheets: [{:file, path}]
             )

    assert {:error, {:resource_limit_exceeded, %{operation: :extract_file, source: ^path}}} =
             Text.extract_file(path)

    assert {:error, {:resource_limit_exceeded, %{operation: :extract_file_spans, source: ^path}}} =
             Text.extract_file_spans(path)
  end

  test "configured stylesheets consume aggregate input as each file is admitted", %{root: root} do
    alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache
    dom = %{type: :document, children: []}
    first = Path.join(root, "first.css")
    second = Path.join(root, "second.css")
    File.write!(first, "/*a*/")
    File.write!(second, "/*b*/")
    opts = [stylesheets: [{:file, first}, {:file, second}]]
    limits = Limits.effective()

    for load <- [&Style.load_stylesheets/2, &Style.prepare_stylesheets/2] do
      Limits.install(%{limits | max_input_bytes: 9})

      assert {:error, {:resource_limit_exceeded, %{stage: :file, source: ^second}}} =
               load.(dom, opts)

      repeated = [stylesheets: [{:file, first}, {:file, first}]]

      RenderCache.with_stylesheets(fn ->
        Limits.install(%{limits | max_input_bytes: 10})
        assert {:ok, [%{css: "/*a*/"}, %{css: "/*a*/"}]} = load.(dom, repeated)
        Limits.install(%{limits | max_input_bytes: 9})
        assert {:error, {:resource_limit_exceeded, %{stage: :css}}} = load.(dom, repeated)
      end)

      Limits.install(%{limits | max_input_bytes: 10})
      assert {:ok, [%{css: "/*a*/"}, %{css: "/*b*/"}]} = load.(dom, opts)
    end

    Limits.install(limits)
    File.write!(second, <<255>>)
    assert {:error, {:invalid_css, %{stage: :css}}} = Style.prepare_stylesheets(dom, opts)
  end

  test "local image reads use remaining aggregate capacity and stop at count limits", %{
    root: root
  } do
    File.write!(Path.join(root, "file"), "1234")

    Limits.install(%{
      Limits.effective()
      | max_input_bytes: 5,
        max_entries: 2
    })

    budget = HtmlValidator.new_image_budget()
    assert {:ok, "1234"} = AssetLoader.resolve("file", :image, [base_url: root], budget)

    assert {:error, {:resource_limit_exceeded, _}} =
             AssetLoader.resolve("file", :image, [base_url: root], budget)

    assert :ok = HtmlValidator.reserve_image_source(budget, 1)
    assert {:error, {:resource_limit_exceeded, _}} = HtmlValidator.image_source_read_limit(budget)
    Limits.install(%{Limits.effective() | max_entries: 3})
    assert {:error, {:resource_limit_exceeded, _}} = HtmlValidator.image_source_read_limit(budget)
  end
end
