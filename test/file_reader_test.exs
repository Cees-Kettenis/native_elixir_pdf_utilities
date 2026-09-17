defmodule NativeElixirPdfUtilities.FileReaderTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{FileReader, HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{AssetLoader, Style}
  alias NativeElixirPdfUtilities.Validators.{FileValidator, HtmlValidator}

  setup do
    root = Path.join(System.tmp_dir!(), "confined-#{System.unique_integer([:positive])}")
    File.mkdir_p!(root)
    limits = Limits.effective()

    on_exit(fn ->
      File.rm_rf!(root)
      Limits.install(limits)
    end)

    {:ok, root: root}
  end

  test "confined reads bound regular files and reject special files without blocking", %{
    root: root
  } do
    File.write!(Path.join(root, "file"), "1234")
    assert {:ok, "1234"} = FileReader.read_confined(root, "file", 4)

    assert {:error, {:resource_limit_exceeded, %{stage: :file}}} =
             FileReader.read_confined(root, "file", 3)

    File.write!(Path.join(root, "empty"), "")
    assert {:ok, ""} = FileReader.read_confined(root, "empty", 1)
    assert {:error, {:invalid_document, _}} = FileReader.read_confined(root, "missing", 10)
    assert {:error, {:invalid_document, _}} = FileReader.read_confined(root, ".", 10)
    assert {_, 0} = System.cmd("mkfifo", [Path.join(root, "pipe")])
    assert {:error, {:invalid_document, _}} = FileReader.read_confined(root, "pipe", 10)
  end

  test "replacing prepared files or directories with symlinks never authorizes outside bytes", %{
    root: root
  } do
    File.mkdir_p!(Path.join(root, "inside"))
    File.write!(Path.join(root, "outside"), "secret")
    File.write!(Path.join(root, "inside/file"), "safe")

    assert {:ok, context} =
             HtmlValidator.prepare_local_resource_path("file", Path.join(root, "inside"))

    File.rm!(context.path)
    File.ln_s!(Path.join(root, "outside"), context.path)

    assert {:error, {:invalid_document, _}} =
             FileReader.read_confined(context.root, context.relative, 100)

    File.rename!(Path.join(root, "inside"), Path.join(root, "old"))
    File.ln_s!(root, Path.join(root, "inside"))

    assert {:error, {:invalid_document, _}} =
             FileReader.read_confined(context.root, "outside", 100)

    assert {:ok, "approved"} =
             AssetLoader.resolve("file", :image,
               base_url: context.root,
               asset_resolver: fn _ -> {:ok, "approved"} end
             )
  end

  test "open descriptors pin approved inodes and growth reads only one sentinel byte", %{
    root: root
  } do
    path = Path.join(root, "file")
    File.write!(path, "safe")
    File.write!(Path.join(root, "outside"), "secret")
    port = start_reader(root, "file")
    assert_receive {^port, {:data, <<1, _mode::32, 4::64>>}}, 2000
    File.rename!(path, path <> ".old")
    File.ln_s!(Path.join(root, "outside"), path)
    Port.command(port, <<4::64>>)
    assert_receive {^port, {:data, <<0, "safe">>}}, 2000
    assert_receive {^port, {:exit_status, 0}}, 2000

    File.rm!(path)
    File.write!(path, "safe")
    port = start_reader(root, "file")
    assert_receive {^port, {:data, <<1, _mode::32, 4::64>>}}, 2000
    File.write!(path, String.duplicate("x", 100_000))
    Port.command(port, <<4::64>>)
    assert_receive {^port, {:data, <<0, bytes::binary>>}}, 2000
    assert bytes == "xxxxx"

    assert {:error, {:resource_limit_exceeded, _}} =
             FileValidator.validate_size(byte_size(bytes), 4, path)

    assert_receive {^port, {:exit_status, 0}}, 2000
  end

  test "the helper exits when its owner dies before approving a read", %{root: root} do
    File.write!(Path.join(root, "file"), "safe")
    parent = self()

    owner =
      spawn(fn ->
        port = start_reader(root, "file")

        receive do
          {^port, {:data, <<1, _::binary>>}} ->
            {:os_pid, pid} = Port.info(port, :os_pid)
            send(parent, {:helper, pid})

            receive do
              :finish -> :ok
            end
        end
      end)

    assert_receive {:helper, pid}, 2000
    Process.exit(owner, :kill)

    assert Enum.reduce_while(1..100, false, fn _, _ ->
             if File.exists?("/proc/#{pid}") do
               Process.sleep(10)
               {:cont, false}
             else
               {:halt, true}
             end
           end)
  end

  test "bounded timeout and missing or broken backend return actionable diagnostics", %{
    root: root
  } do
    File.write!(Path.join(root, "file"), "safe")
    Limits.install(%{Limits.effective() | max_asset_file_read_timeout_ms: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FileReader.read_confined(root, "file", 10)

    assert diagnostic.message =~ "max_asset_file_read_timeout_ms"
    Limits.install(Limits.defaults())

    assert {:error, {:invalid_document, diagnostic}} =
             FileValidator.validate_confined_runtime({:unix, :darwin}, "/usr/bin/python3")

    assert diagnostic.message =~ "Python 3"
    path = System.get_env("PATH")
    on_exit(fn -> System.put_env("PATH", path) end)
    System.put_env("PATH", root)
    assert {:error, {:invalid_document, _}} = FileReader.read_confined(root, "file", 10)

    html =
      "<style>@font-face { font-family: Test; src: url(\"file.ttf\"); } p { font-family: Test; }</style><p>Test</p>"

    assert {:error, {:invalid_document, %{message: message}}} =
             HtmlToPdf.render(html, base_url: root)

    assert message =~ "Python 3"
    File.write!(Path.join(root, "python3"), "#!/bin/sh\nexit 0\n")
    File.chmod!(Path.join(root, "python3"), 0o755)
    assert {:error, {:invalid_document, _}} = FileReader.read_confined(root, "file", 10)
    File.write!(Path.join(root, "python3"), "#!/bin/sh\nexit 124\n")
    assert {:error, {:resource_limit_exceeded, _}} = FileReader.read_confined(root, "file", 10)
    File.write!(Path.join(root, "python3"), "#!/missing-interpreter\n")
    assert {:error, {:invalid_document, _}} = FileReader.read_confined(root, "file", 10)
  end

  test "a helper disappearing between metadata and approval returns a diagnostic", %{root: root} do
    python = Path.join(root, "python3")
    File.write!(python, "#!/usr/bin/python3\nimport sys\nsys.stdin.buffer.read()\n")
    File.chmod!(python, 0o755)
    path = System.get_env("PATH")
    on_exit(fn -> System.put_env("PATH", path) end)
    System.put_env("PATH", root)
    parent = self()
    owner = spawn(fn -> send(parent, {:result, FileReader.read_confined(root, "file", 10)}) end)

    port =
      Enum.reduce_while(1..100, nil, fn _, _ ->
        case Enum.find(Port.list(), &(Port.info(&1, :connected) == {:connected, owner})) do
          nil ->
            Process.sleep(10)
            {:cont, nil}

          port ->
            {:halt, port}
        end
      end)

    assert is_port(port)
    :erlang.suspend_process(owner)
    send(owner, {port, {:data, <<1, 0o100644::32, 4::64>>}})
    Port.close(port)
    :erlang.resume_process(owner)
    assert_receive {:result, {:error, {:invalid_document, %{source: source}}}}, 2000
    assert source == Path.join(root, "file")
  end

  test "confined arguments reject traversal, invalid roots and OS pathname violations", %{
    root: root
  } do
    for {base, relative, maximum} <- [
          {nil, "f", 1},
          {root, nil, 1},
          {"relative", "f", 1},
          {root, "../f", 1},
          {root, "/f", 1},
          {root, "f\0", 1},
          {root, String.duplicate("a", 4096), 1},
          {root, "f", 0},
          {root, "f", 0x10000000000000000},
          {root, "f", 0xFFFFFFFE}
        ] do
      assert {:error, {_, %{stage: :file}}} = FileReader.read_confined(base, relative, maximum)
    end
  end

  test "HTML CSS and PDF entry points reject oversized files before parsing", %{root: root} do
    path = Path.join(root, "large")
    File.write!(path, "12345")

    Limits.install(%{
      Limits.effective()
      | max_html_source_bytes: 4,
        max_css_source_bytes: 4,
        max_pdf_input_bytes: 4
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

  test "local image reads use remaining aggregate capacity and stop at count limits", %{
    root: root
  } do
    File.write!(Path.join(root, "file"), "1234")

    Limits.install(%{
      Limits.effective()
      | max_image_source_bytes: 4,
        max_aggregate_image_source_bytes: 5,
        max_image_count: 2
    })

    budget = HtmlValidator.new_image_budget()
    assert {:ok, "1234"} = AssetLoader.resolve("file", :image, [base_url: root], budget)

    assert {:error, {:resource_limit_exceeded, _}} =
             AssetLoader.resolve("file", :image, [base_url: root], budget)

    assert :ok = HtmlValidator.reserve_image_source(budget, 1)
    assert {:error, {:resource_limit_exceeded, _}} = HtmlValidator.image_source_read_limit(budget)
    Limits.install(%{Limits.effective() | max_image_count: 3})
    assert {:error, {:resource_limit_exceeded, _}} = HtmlValidator.image_source_read_limit(budget)
  end

  defp start_reader(root, relative) do
    Port.open({:spawn_executable, System.find_executable("python3")}, [
      :binary,
      :exit_status,
      :use_stdio,
      {:packet, 4},
      {:args,
       ["-I", "-S", "-c", File.read!("lib/confined_file_reader.py"), root, relative, "5000"]}
    ])
  end
end
