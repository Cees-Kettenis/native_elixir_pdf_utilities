defmodule NativeElixirPdfUtilities.TestSupport.ChromiumPool do
  @moduledoc false
  use GenServer
  import ExUnit.Assertions

  @doc "Prints a fixture in a fresh browser context, reusing a bounded pool of Chrome processes."
  @spec print_pdf!(String.t(), String.t(), String.t(), map() | nil) :: :ok
  def print_pdf!(binary, fixture, output, reference \\ nil) do
    slot = :erlang.phash2(self(), ExUnit.configuration()[:max_cases])
    name = {:global, {__MODULE__, binary, slot, System.get_env("FONTCONFIG_FILE")}}

    pid =
      case GenServer.start(__MODULE__, {binary, slot}, name: name) do
        {:ok, pid} -> pid
        {:error, {:already_started, pid}} -> pid
      end

    case GenServer.call(pid, {:pdf, fixture, output, reference}, :infinity) do
      :ok -> :ok
      {:error, message} -> flunk(message)
    end
  end

  @doc "Closes the suite's browser processes and removes their temporary profiles."
  @spec stop() :: :ok
  def stop do
    for {__MODULE__, _binary, _slot, _fontconfig} = name <- :global.registered_names() do
      case :global.whereis_name(name) do
        :undefined -> :ok
        pid -> GenServer.stop(pid)
      end
    end

    :ok
  end

  @impl true
  def init({binary, slot}) do
    unique = "#{System.pid()}-#{System.unique_integer([:positive])}"
    profile = Path.join(System.tmp_dir!(), "native-pdf-chrome-#{unique}")
    log = Path.expand("tmp/browser_parity/.chrome/worker-#{slot}-#{unique}.log")
    File.mkdir_p!(Path.join(profile, "Default"))
    File.mkdir_p!(Path.dirname(log))

    File.write!(
      Path.join(profile, "Default/Preferences"),
      :json.encode(%{profile: %{default_content_setting_values: %{javascript: 2}}})
    )

    args = [
      "--headless",
      "--no-sandbox",
      "--disable-gpu",
      "--disable-dev-shm-usage",
      "--allow-file-access-from-files",
      "--run-all-compositor-stages-before-draw",
      "--remote-debugging-pipe",
      "--user-data-dir=#{profile}"
    ]

    # Chrome's pipe uses descriptors 3 and 4. The shell only connects those
    # descriptors to the Erlang port; every executable argument stays quoted.
    port =
      Port.open({:spawn_executable, System.find_executable("sh")}, [
        :binary,
        :exit_status,
        args: [
          "-c",
          "browser_log=$1; shift; exec \"$@\" 3<&0 4>&1 2>\"$browser_log\"",
          "browser-pipe",
          log,
          binary | args
        ]
      ])

    {:ok, %{port: port, profile: profile, log: log, next_id: 1, buffer: "", events: []}}
  end

  @impl true
  def handle_call({:pdf, fixture, output, reference}, _from, state) do
    try do
      {context, state} = command(state, "Target.createBrowserContext")

      {target, state} =
        command(state, "Target.createTarget", %{
          url: "about:blank",
          browserContextId: context["browserContextId"]
        })

      {attached, state} =
        command(state, "Target.attachToTarget", %{targetId: target["targetId"], flatten: true})

      session = attached["sessionId"]
      {_, state} = command(state, "Page.enable", %{}, session)
      {_, state} = command(state, "Page.setLifecycleEventsEnabled", %{enabled: true}, session)
      {_, state} = command(state, "Emulation.setScriptExecutionDisabled", %{value: true}, session)
      {_, state} = command(state, "Emulation.setEmulatedMedia", %{media: "print"}, session)

      state =
        if reference do
          # Print pages have no outer scrolling gutter. Screenshot capture
          # otherwise removes that gutter temporarily, reflowing percentage
          # tables after their crop coordinates have already been measured.
          {_, state} = command(state, "Emulation.setScrollbarsHidden", %{hidden: true}, session)

          {_, state} =
            command(
              state,
              "Emulation.setDeviceMetricsOverride",
              %{
                width: ceil(reference.width),
                height: ceil(reference.height),
                deviceScaleFactor: reference.scale,
                mobile: false
              },
              session
            )

          state
        else
          state
        end

      {navigation, state} =
        command(
          %{state | events: []},
          "Page.navigate",
          %{
            url: "file://" <> URI.encode(Path.expand(fixture))
          },
          session
        )

      if navigation["errorText"], do: raise("Chrome navigation failed: #{inspect(navigation)}")

      state =
        await_event(state, session, "Page.lifecycleEvent", fn event ->
          event["loaderId"] == navigation["loaderId"] and event["name"] == "load"
        end)

      {_, state} =
        command(
          state,
          "Emulation.setVirtualTimePolicy",
          %{
            policy: "pauseIfNetworkFetchesPending",
            budget: 1000
          },
          session
        )

      state = await_event(state, session, "Emulation.virtualTimeBudgetExpired", fn _ -> true end)

      {pdf, state} = print_page(state, session)

      {pdf, state} =
        if reference,
          do: raster_svg_reference(state, session, output, pdf),
          else: {pdf, state}

      File.write!(output, Base.decode64!(pdf["data"]))

      {_, state} =
        command(state, "Target.disposeBrowserContext", %{
          browserContextId: context["browserContextId"]
        })

      {:reply, :ok, %{state | events: []}}
    rescue
      exception ->
        {:stop, :normal,
         {:error,
          "Chromium PDF rendering failed for file://#{Path.expand(fixture)}: #{Exception.message(exception)}; log: #{state.log}"},
         state}
    end
  end

  @impl true
  def handle_info(message, state) do
    case message do
      {port, {:data, data}} when port == state.port ->
        {:noreply, %{state | buffer: state.buffer <> data}}

      {port, {:exit_status, _status}} when port == state.port ->
        {:stop, :normal, state}
    end
  end

  @impl true
  def terminate(_reason, state) do
    if Port.info(state.port) do
      # Closing the pipe lets Chrome flush its profile asynchronously. Ask it
      # to exit and wait before removing files it may still be writing.
      Port.command(state.port, [:json.encode(%{id: state.next_id, method: "Browser.close"}), 0])
      port = state.port

      receive do
        {^port, {:exit_status, _status}} -> :ok
      after
        ExUnit.configuration()[:timeout] -> raise("Chrome did not exit; log: #{state.log}")
      end
    end

    File.rm_rf!(state.profile)
    :ok
  end

  defp raster_svg_reference(state, session, output, pdf) do
    File.write!(Path.rootname(output) <> "-vector.pdf", Base.decode64!(pdf["data"]))

    {targets, state} =
      evaluate(state, session, File.read!(Path.join(__DIR__, "chromium_svg_reference.js")))

    if targets == [], do: raise("Chrome SVG PNG reference found no painted SVG viewports")
    File.write!(Path.join(Path.dirname(output), "svg-viewports.json"), :json.encode(targets))

    {_, state} =
      command(
        state,
        "Emulation.setDefaultBackgroundColorOverride",
        %{color: %{r: 0, g: 0, b: 0, a: 0}},
        session
      )

    # Capture all original SVGs before replacing any of them. An inline SVG
    # stylesheet can affect sibling SVGs and ordinary document text.
    {captures, state} =
      Enum.map_reduce(targets, state, fn target, state ->
        {_, state} =
          evaluate(state, session, "__nativePdfSvgReference.prepare(#{target["index"]})")

        {capture, state} =
          command(
            state,
            "Page.captureScreenshot",
            %{
              format: "png",
              captureBeyondViewport: true,
              clip: Map.take(target, ["x", "y", "width", "height"]) |> Map.put("scale", 1)
            },
            session
          )

        {_, state} =
          evaluate(state, session, "__nativePdfSvgReference.restore(#{target["index"]})")

        File.write!(
          Path.join(Path.dirname(output), "chromium-svg-#{target["index"]}.png"),
          Base.decode64!(capture["data"])
        )

        {{target["index"], capture["data"]}, state}
      end)

    state =
      Enum.reduce(captures, state, fn {index, data}, state ->
        {_, state} =
          evaluate(state, session, "__nativePdfSvgReference.replace(#{index}, '#{data}')", true)

        state
      end)

    {_, state} = command(state, "Emulation.setDefaultBackgroundColorOverride", %{}, session)
    {changed, state} = evaluate(state, session, "__nativePdfSvgReference.changed()")

    if changed != [],
      do:
        raise(
          "Chrome SVG PNG replacement changed surrounding layout or styles: #{inspect(changed)}"
        )

    print_page(state, session)
  end

  defp print_page(state, session) do
    command(
      state,
      "Page.printToPDF",
      %{
        displayHeaderFooter: false,
        printBackground: true,
        preferCSSPageSize: true,
        marginTop: 0,
        marginRight: 0,
        marginBottom: 0,
        marginLeft: 0
      },
      session
    )
  end

  defp evaluate(state, session, expression, await_promise \\ false) do
    {result, state} =
      command(
        state,
        "Runtime.evaluate",
        %{expression: expression, returnByValue: true, awaitPromise: await_promise},
        session
      )

    if result["exceptionDetails"],
      do: raise("Chrome SVG reference evaluation failed: #{inspect(result["exceptionDetails"])}")

    {get_in(result, ["result", "value"]), state}
  end

  defp command(state, method, params \\ %{}, session \\ nil) do
    message = %{id: state.next_id, method: method, params: params}
    message = if session, do: Map.put(message, :sessionId, session), else: message
    Port.command(state.port, [:json.encode(message), 0])
    await_response(%{state | next_id: state.next_id + 1}, message.id)
  end

  defp await_response(state, id) do
    {message, state} = read_message(state)

    if message["id"] == id do
      if message["error"], do: raise("Chrome command failed: #{inspect(message)}")
      {message["result"], state}
    else
      await_response(%{state | events: state.events ++ [message]}, id)
    end
  end

  defp await_event(state, session, method, predicate) do
    {before, remaining} =
      Enum.split_while(state.events, fn event ->
        not (event["sessionId"] == session and event["method"] == method and
               predicate.(event["params"]))
      end)

    case remaining do
      [_event | rest] ->
        %{state | events: before ++ rest}

      [] ->
        {message, state} = read_message(state)
        await_event(%{state | events: state.events ++ [message]}, session, method, predicate)
    end
  end

  defp read_message(state) do
    case :binary.match(state.buffer, <<0>>) do
      {offset, 1} ->
        message = binary_part(state.buffer, 0, offset)
        rest = binary_part(state.buffer, offset + 1, byte_size(state.buffer) - offset - 1)
        {:json.decode(message), %{state | buffer: rest}}

      :nomatch ->
        port = state.port

        receive do
          {^port, {:data, data}} -> read_message(%{state | buffer: state.buffer <> data})
          {^port, {:exit_status, status}} -> raise("Chrome pipe exited with status #{status}")
        after
          ExUnit.configuration()[:timeout] -> raise("Timed out waiting for Chrome pipe")
        end
    end
  end
end
