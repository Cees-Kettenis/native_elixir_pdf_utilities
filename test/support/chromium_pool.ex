defmodule NativeElixirPdfUtilities.TestSupport.ChromiumPool do
  @moduledoc false
  use GenServer
  import ExUnit.Assertions

  @doc "Prints a fixture in a fresh browser context, reusing a bounded pool of Chrome processes."
  @spec print_pdf!(String.t(), String.t(), String.t(), map() | nil) :: :ok
  def print_pdf!(binary, fixture, output, reference \\ nil) do
    slot = :erlang.phash2(self(), ExUnit.configuration()[:max_cases])
    fontconfig = System.get_env("FONTCONFIG_FILE")
    name = {:global, {__MODULE__, binary, slot, nil}}

    pid =
      if fontconfig do
        # A fontconfig override used to launch a fresh CLI process. Keep that
        # process isolation while using the same readiness barrier as the pool.
        {:ok, pid} = GenServer.start(__MODULE__, {binary, slot})
        pid
      else
        case GenServer.start(__MODULE__, {binary, slot}, name: name) do
          {:ok, pid} -> pid
          {:error, {:already_started, pid}} -> pid
        end
      end

    try do
      case GenServer.call(pid, {:pdf, fixture, output, reference}, :infinity) do
        :ok -> :ok
        {:error, message} -> flunk(message)
      end
    after
      if fontconfig && Process.alive?(pid), do: GenServer.stop(pid)
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

    {:ok,
     %{
       port: port,
       profile: profile,
       log: log,
       next_id: 1,
       buffer: "",
       events: [],
       pending: %{},
       loading: MapSet.new(),
       deadline: nil,
       session: nil,
       readiness: []
     }}
  end

  @impl true
  def handle_call({:pdf, fixture, output, reference}, _from, state) do
    state = %{
      state
      | deadline: System.monotonic_time(:millisecond) + ExUnit.configuration()[:timeout],
        pending: %{},
        session: nil,
        loading: MapSet.new(),
        readiness: []
    }

    url =
      if URI.parse(fixture).scheme in ["http", "https", "file"],
        do: fixture,
        else: "file://" <> URI.encode(Path.expand(fixture))

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
      state = %{state | session: session}
      {_, state} = command(state, "Page.enable", %{}, session)
      {_, state} = command(state, "Network.enable", %{}, session)
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
            url: url
          },
          session
        )

      if navigation["errorText"], do: raise("Chrome navigation failed: #{inspect(navigation)}")

      state =
        await_event(state, session, "Page.lifecycleEvent", fn event ->
          event["loaderId"] == navigation["loaderId"] and event["name"] == "load"
        end)

      state = await_documents(state, session)
      File.write!(output <> ".readiness.json", :json.encode(state.readiness))

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

      {:reply, :ok, %{state | events: [], deadline: nil}}
    rescue
      exception ->
        File.write!(output <> ".readiness-error.txt", Exception.message(exception))

        {:stop, :normal,
         {:error,
          "Chromium PDF rendering failed for #{url}: #{Exception.message(exception)}; log: #{state.log}"},
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

  # The parent load event does not establish that every embedded document has
  # reached its final layout. Drain actual loads, settle each frame, then check
  # that no new frame or resource appeared while doing so.
  defp await_documents(state, session) do
    if System.monotonic_time(:millisecond) >= state.deadline,
      do: readiness_timeout!(state)

    if map_size(state.pending) > 0 or MapSet.size(state.loading) > 0 do
      {message, state} = read_message(state)
      await_documents(record_event(state, message), session)
    else
      {tree, state} = command(state, "Page.getFrameTree", %{}, session)

      if map_size(state.pending) > 0 or MapSet.size(state.loading) > 0 do
        await_documents(state, session)
      else
        flatten = fn flatten, node ->
          [
            node["frame"]
            | Enum.flat_map(Map.get(node, "childFrames", []), &flatten.(flatten, &1))
          ]
        end

        frames = flatten.(flatten, tree["frameTree"])
        expression = File.read!(Path.join(__DIR__, "chromium_document_readiness.js"))

        {readiness, state} =
          Enum.map_reduce(frames, state, fn frame, state ->
            {world, state} =
              command(
                state,
                "Page.createIsolatedWorld",
                %{
                  frameId: frame["id"],
                  worldName: "pdf-comparison-readiness"
                },
                session
              )

            {result, state} =
              command(
                state,
                "Runtime.evaluate",
                %{
                  expression: expression,
                  contextId: world["executionContextId"],
                  returnByValue: true,
                  awaitPromise: true
                },
                session
              )

            if result["exceptionDetails"],
              do: raise("Chromium frame readiness failed: #{inspect(result["exceptionDetails"])}")

            {Map.merge(frame, get_in(result, ["result", "value"])), state}
          end)

        state = %{state | readiness: readiness}
        {_, state} = command(state, "Page.getLayoutMetrics", %{}, session)
        {after_tree, state} = command(state, "Page.getFrameTree", %{}, session)

        if tree == after_tree and map_size(state.pending) == 0 and MapSet.size(state.loading) == 0 and
             Enum.all?(readiness, &(&1["ready"] == "complete" and &1["fonts"] == "loaded")) do
          state
        else
          await_documents(state, session)
        end
      end
    end
  end

  defp record_event(state, message) do
    session = message["sessionId"]
    params = message["params"] || %{}

    state =
      case if(session == state.session, do: message["method"], else: nil) do
        "Network.requestWillBeSent" ->
          %{
            state
            | pending:
                Map.put(
                  state.pending,
                  {session, params["requestId"]},
                  get_in(params, ["request", "url"])
                )
          }

        method when method in ["Network.loadingFinished", "Network.loadingFailed"] ->
          %{state | pending: Map.delete(state.pending, {session, params["requestId"]})}

        "Page.frameStartedLoading" ->
          %{state | loading: MapSet.put(state.loading, {session, params["frameId"]})}

        method when method in ["Page.frameStoppedLoading", "Page.frameDetached"] ->
          %{state | loading: MapSet.delete(state.loading, {session, params["frameId"]})}

        _ ->
          state
      end

    %{state | events: [message | state.events]}
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
      await_response(record_event(state, message), id)
    end
  end

  defp await_event(state, session, method, predicate) do
    {before, remaining} =
      Enum.split_while(Enum.reverse(state.events), fn event ->
        not (event["sessionId"] == session and event["method"] == method and
               predicate.(event["params"]))
      end)

    case remaining do
      [_event | rest] ->
        %{state | events: Enum.reverse(before ++ rest)}

      [] ->
        {message, state} = read_message(state)
        await_event(record_event(state, message), session, method, predicate)
    end
  end

  defp readiness_timeout!(state) do
    raise(
      "Chromium readiness/command deadline exceeded; pending=#{inspect(state.pending)} loading=#{inspect(state.loading)} frames=#{inspect(state.readiness)}; log=#{state.log}"
    )
  end

  defp read_message(state) do
    if state.deadline && System.monotonic_time(:millisecond) >= state.deadline,
      do: readiness_timeout!(state)

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
          max(
            0,
            (state.deadline ||
               System.monotonic_time(:millisecond) + ExUnit.configuration()[:timeout]) -
                System.monotonic_time(:millisecond)
          ) ->
            readiness_timeout!(state)
        end
    end
  end
end
