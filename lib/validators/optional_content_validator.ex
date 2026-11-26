defmodule NativeElixirPdfUtilities.Validators.OptionalContentValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Pdf.InfoCodec
  alias NativeElixirPdfUtilities.Pdf.ObjectMapping
  alias NativeElixirPdfUtilities.Validators.AssemblyValidator
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @array_keys ~w(ON OFF Order AS RBGroups Locked)

  # ISO 32000-1, table 99. Apply the deleted-member exception before page
  # resources are inherited and before any importer validates its object graph.
  @doc false
  @spec import_objects(map(), map(), map()) :: map()
  def import_objects(objects, xref, trailer) do
    {updated, detached} =
      Enum.reduce(objects, {objects, MapSet.new()}, fn {ref, object}, {updated, detached} ->
        if Enum.member?(object.tokens, {:name, "OCGs"}) do
          {value, detached} = normalize_memberships(object.value, detached, objects, xref)

          if value === object.value do
            {updated, detached}
          else
            tail =
              if object.stream, do: [:stream, {:stream_data, object.stream}, :endstream], else: []

            object = %{object | value: value, tokens: value_tokens(value, tail)}
            {Map.put(updated, ref, object), detached}
          end
        else
          {updated, detached}
        end
      end)

    # An indirect membership array may also be referenced by an unrelated
    # field. Leave that original object (and its strict validation) intact.
    if MapSet.size(detached) == 0 do
      updated
    else
      unused = retain_referenced(trailer, detached, updated)

      unused =
        Enum.reduce(updated, unused, fn {ref, object}, unused ->
          if MapSet.member?(detached, ref),
            do: unused,
            else: retain_referenced(object.value, unused, updated)
        end)

      Map.drop(updated, MapSet.to_list(unused))
    end
  end

  defp retain_referenced(value, unused, objects) do
    {:cont, unused} =
      ObjectMapping.reduce_references_while(value, unused, fn ref, remaining ->
        if MapSet.member?(remaining, ref) do
          {:cont, retain_referenced(objects[ref].value, MapSet.delete(remaining, ref), objects)}
        else
          {:cont, remaining}
        end
      end)

    unused
  end

  defp normalize_memberships(value, detached, objects, xref) do
    case value do
      dict when is_map(dict) ->
        {dict, detached} =
          Enum.reduce(dict, {dict, detached}, fn {key, child}, {dict, detached} ->
            {child, detached} = normalize_memberships(child, detached, objects, xref)
            {Map.put(dict, key, child), detached}
          end)

        case dict do
          %{"Type" => type, "OCGs" => members} ->
            if match?({{:name, "OCMD"}, _}, member_value(type, objects, xref, [])) do
              normalize_groups(dict, members, detached, objects, xref)
            else
              {dict, detached}
            end

          _ ->
            {dict, detached}
        end

      array when is_list(array) ->
        Enum.map_reduce(array, detached, &normalize_memberships(&1, &2, objects, xref))

      other ->
        {other, detached}
    end
  end

  defp normalize_groups(dict, members, detached, objects, xref) do
    {value, path} = member_value(members, objects, xref, [])

    case value do
      groups when is_list(groups) ->
        {live, detached} =
          Enum.reduce(groups, {[], detached}, fn group, {live, detached} ->
            PdfValidator.charge_reader_budget(:max_work, 1)

            case member_value(group, objects, xref, []) do
              {nil, path} -> {live, Enum.into(path, detached)}
              _ -> {[group | live], detached}
            end
          end)

        live = Enum.reverse(live)

        if live == groups and live != [] do
          {dict, detached}
        else
          dict = if live == [], do: Map.delete(dict, "OCGs"), else: Map.put(dict, "OCGs", live)
          {dict, Enum.into(path, detached)}
        end

      nil ->
        {Map.delete(dict, "OCGs"), Enum.into(path, detached)}

      _ ->
        {dict, detached}
    end
  end

  # Keep unresolved values for ordinary graph validation. Only an older
  # generation of a freed or reused slot establishes that a member was deleted.
  defp member_value(value, objects, xref, path) do
    case value do
      {:ref, {id, generation} = ref} ->
        case objects do
          %{^ref => %{value: next}} ->
            PdfValidator.charge_reader_budget(:max_work, 1)

            if length(path) < Limits.get(:max_depth) and ref not in path,
              do: member_value(next, objects, xref, [ref | path]),
              else: {value, []}

          _ ->
            case xref[id] do
              {kind, _, current}
              when kind in [:free, :uncompressed] and id > 0 and generation >= 0 and
                     generation < current ->
                {nil, path}

              _ ->
                {value, []}
            end
        end

      _ ->
        {value, path}
    end
  end

  # Rebuild only changed dictionaries from parsed values, without re-tokenizing
  # binary strings or streams. The remaining object tokens stay byte-for-byte intact.
  defp value_tokens(value, tail) do
    case value do
      dict when is_map(dict) ->
        [
          :dict_start
          | Enum.reduce(dict, [:dict_end | tail], fn {k, v}, rest ->
              [{:name, k} | value_tokens(v, rest)]
            end)
        ]

      array when is_list(array) ->
        [:lbracket | List.foldr(array, [:rbracket | tail], &value_tokens/2)]

      {:ref, {id, gen}} ->
        [{:int, id}, {:int, gen}, :R | tail]

      {:hex, bytes} ->
        [{:hex_string, bytes} | tail]

      v when is_integer(v) ->
        [{:int, v} | tail]

      v when is_float(v) ->
        [{:real, v} | tail]

      nil ->
        [:null | tail]

      token ->
        [token | tail]
    end
  end

  @doc "Prepares the catalog configuration, retaining indirect references to the original layer objects."
  @spec prepare(map()) :: {:ok, map() | nil} | {:error, {atom(), map()}}
  def prepare(context) do
    case get_in(context, [:catalog, "OCProperties"]) do
      nil ->
        {:ok, nil}

      entry ->
        PdfValidator.with_reader_budget(fn ->
          PdfValidator.with_reference_resolution([], fn opts ->
            read = &layer_value(context.document, &1, opts)

            case read.(entry) do
              nil ->
                {:ok, nil}

              %{"OCGs" => groups} = properties ->
                groups = array(read.(groups))

                Enum.each(groups, fn group ->
                  with {:ref, _} <- group,
                       %{"Type" => type} <- read.(group),
                       {:name, "OCG"} <- read.(type) do
                    :ok
                  else
                    _ -> invalid("OCGs must list indirect optional-content groups")
                  end
                end)

                known = MapSet.new(groups)
                default = configuration(read.(properties["D"]), read, known)

                default =
                  case default["BaseState"] do
                    {:name, "OFF"} ->
                      on = MapSet.new(Map.get(default, "ON", []))

                      default
                      |> Map.delete("ON")
                      |> Map.put("BaseState", {:name, "ON"})
                      |> Map.put("OFF", Enum.reject(groups, &MapSet.member?(on, &1)))

                    {:name, "Unchanged"} ->
                      invalid("default layer state must be determinate")

                    _ ->
                      default
                  end

                alternates = array(read.(properties["Configs"]) || [])
                configs = bounded_configs(alternates, &configuration(read.(&1), read, known))

                {:ok,
                 properties
                 |> Map.put("OCGs", groups)
                 |> Map.put("D", default)
                 |> Map.put("Configs", configs)}

              _ ->
                invalid("OCProperties requires an OCGs array")
            end
          end)
        end)
    end
  catch
    {:layer_error, result} -> result
  end

  @typep destination_lookup :: (tuple() -> {:ok, list() | nil} | {:error, {atom(), map()}})

  @doc false
  @spec bookmark_action(PdfValidator.document(), map(), MapSet.t() | nil, destination_lookup()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def bookmark_action(document, item, groups, resolve_destination) do
    if groups && is_nil(item["Dest"]) && item["A"] do
      PdfValidator.with_reader_budget(fn ->
        kinds =
          bookmark_actions(
            document,
            [item["A"]],
            groups,
            resolve_destination,
            %{},
            %{}
          )

        cond do
          not Map.has_key?(kinds, :switch) ->
            {:ok, %{}}

          Map.has_key?(kinds, :unsupported) ->
            Diagnostics.error(
              :outlines,
              :unsupported_pdf_feature,
              "layer bookmark contains an unsupported action or unresolved destination",
              module: __MODULE__
            )

          true ->
            {:ok, %{layer_action: item["A"]}}
        end
      end)
    else
      {:ok, %{}}
    end
  catch
    {:layer_error, result} -> result
  end

  # Only /Next links actions. Retain shared indirect actions once and leave
  # their references intact for the ordinary assembly remapper.
  defp bookmark_actions(document, pending, groups, resolve_destination, seen, kinds) do
    case pending do
      [] ->
        kinds

      [value | rest] ->
        PdfValidator.charge_reader_budget(:max_work, 1)

        if match?({:ref, _}, value) and Map.has_key?(seen, value) do
          bookmark_actions(document, rest, groups, resolve_destination, seen, kinds)
        else
          seen = if match?({:ref, _}, value), do: Map.put(seen, value, true), else: seen

          {pending, kinds} =
            case layer_value(document, value) do
              actions when is_list(actions) ->
                {actions ++ rest, kinds}

              action when is_map(action) ->
                kind = bookmark_action_kind(document, action, groups, resolve_destination)
                {List.wrap(action["Next"]) ++ rest, Map.put(kinds, kind, true)}

              _ ->
                {rest, Map.put(kinds, :unsupported, true)}
            end

          bookmark_actions(document, pending, groups, resolve_destination, seen, kinds)
        end
    end
  end

  defp bookmark_action_kind(document, action, groups, resolve_destination) do
    case layer_value(document, action["S"]) do
      {:name, "SetOCGState"} ->
        validate_switch(document, action, groups)
        :switch

      {:name, "GoTo"} ->
        case layer_value(document, action["D"]) do
          [{:ref, _} | _] ->
            :portable

          {kind, _} = name when kind in [:name, :string, :hex] ->
            case resolve_destination.(name) do
              {:ok, nil} -> :unsupported
              {:ok, _} -> :portable
              error -> throw({:layer_error, error})
            end

          _ ->
            :unsupported
        end

      {:name, "URI"} ->
        :portable

      _ ->
        :unsupported
    end
  end

  defp validate_switch(document, action, groups) do
    state = array(layer_value(document, action["State"]))

    status =
      Enum.reduce(state, :start, fn operand, status ->
        PdfValidator.charge_reader_budget(:max_work, 1)

        cond do
          MapSet.member?(groups, operand) and status != :start ->
            :has_group

          status != :needs_group and
              layer_value(document, operand) in [{:name, "ON"}, {:name, "OFF"}, {:name, "Toggle"}] ->
            :needs_group

          true ->
            invalid(
              "bookmark State requires an ON, OFF or Toggle operator followed by registered layer references"
            )
        end
      end)

    if status == :needs_group, do: invalid("bookmark State operator requires a layer reference")

    unless layer_value(document, Map.get(action, "PreserveRB", true)) in [true, false, nil],
      do: invalid("bookmark PreserveRB must be a boolean")
  end

  defp layer_value(document, value, opts \\ []) do
    case PdfValidator.resolve(document, value, opts) do
      {:ok, resolved} -> resolved
      error -> throw({:layer_error, error})
    end
  end

  defp configuration(value, read, known) do
    value = if is_nil(value), do: %{}, else: value
    unless is_map(value), do: invalid("expected an optional-content configuration dictionary")

    config =
      Enum.reduce(@array_keys ++ ~w(BaseState Intent ListMode), value, fn key, config ->
        case read.(config[key]) do
          nil ->
            Map.delete(config, key)

          entry when key in ~w(ON OFF Locked) ->
            refs = array(entry)

            unless Enum.all?(refs, &MapSet.member?(known, &1)),
              do: invalid("#{key} references an unregistered group")

            Map.put(config, key, refs)

          entry when key in @array_keys ->
            Map.put(config, key, array(entry))

          entry
          when key == "BaseState" and
                 entry not in [{:name, "ON"}, {:name, "OFF"}, {:name, "Unchanged"}] ->
            invalid("invalid optional-content BaseState")

          mode
          when key == "ListMode" and mode not in [{:name, "AllPages"}, {:name, "VisiblePages"}] ->
            invalid("invalid optional-content ListMode")

          intents when key == "Intent" and is_list(intents) ->
            names = intents |> Enum.map(read) |> Enum.uniq() |> Enum.sort()

            case names do
              [one] -> Map.put(config, key, one)
              _ -> Map.put(config, key, names)
            end

          entry ->
            Map.put(config, key, entry)
        end
      end)

    on = MapSet.new(Map.get(config, "ON", []))

    if Enum.any?(Map.get(config, "OFF", []), &MapSet.member?(on, &1)),
      do: invalid("the same group is listed as both ON and OFF")

    config
  end

  defp array(value) do
    if is_list(value), do: value, else: invalid("expected an optional-content array")
  end

  defp invalid(message) do
    throw(
      {:layer_error,
       Diagnostics.error(:optional_content, :invalid_pdf_input, message, module: __MODULE__)}
    )
  end

  @doc "Combines already remapped catalogs. Layer identity is the object reference, independent of its display name."
  @spec combine([map()]) :: {:ok, map() | nil} | {:error, {atom(), map()}}
  def combine(catalogs) do
    case catalogs do
      [] ->
        {:ok, nil}

      [catalog] ->
        {:ok, catalog}

      catalogs ->
        PdfValidator.with_reader_budget(fn ->
          defaults = Enum.map(catalogs, & &1["D"])
          groups = combined_array(catalogs, "OCGs")
          expansion = length(defaults) + length(groups)
          default = combined_configuration(catalogs, defaults, hd(defaults), false)

          alternates =
            for {catalog, index} <- Enum.with_index(catalogs),
                alternate <- catalog["Configs"],
                do: {catalog, index, alternate}

          configs =
            bounded_configs(alternates, fn {catalog, index, alternate} ->
              PdfValidator.charge_reader_budget(:max_work, expansion)
              inherited = Map.take(catalog["D"], ~w(Order RBGroups))
              selected = Map.merge(inherited, alternate)

              combined_configuration(
                catalogs,
                List.replace_at(defaults, index, selected),
                alternate,
                true
              )
            end)

          {:ok, %{"OCGs" => groups, "D" => default, "Configs" => configs}}
        end)
    end
  catch
    {:layer_error, result} -> result
  end

  # Configurations can share indirect arrays in the source. Bound their
  # cumulative inline representation before retaining each expanded preset.
  defp bounded_configs(entries, prepare) do
    case PdfValidator.validate_container(entries, module: __MODULE__) do
      :ok -> :ok
      error -> throw({:layer_error, error})
    end

    {configs, _bytes} =
      Enum.map_reduce(entries, 0, fn entry, bytes ->
        config = prepare.(entry)

        with {:ok, encoded} <- InfoCodec.serialize_value(config),
             total = bytes + :erlang.iolist_size(encoded),
             :ok <- AssemblyValidator.validate_output_size(total) do
          {config, total}
        else
          error -> throw({:layer_error, error})
        end
      end)

    configs
  end

  defp combined_configuration(catalogs, configs, metadata, alternate?) do
    # ISO 32000-1 table 101. Expand each document's BaseState within its own
    # group set before expressing the result as one document-wide configuration.
    base = if alternate?, do: "Unchanged", else: "ON"

    states =
      Enum.zip_with(catalogs, configs, fn catalog, config ->
        groups = catalog["OCGs"]
        PdfValidator.charge_reader_budget(:max_work, length(groups))
        on = Map.get(config, "ON", [])
        off = Map.get(config, "OFF", [])

        case Map.get(config, "BaseState", {:name, "ON"}) do
          {:name, "ON"} ->
            hidden = MapSet.new(off)
            {Enum.reject(groups, &MapSet.member?(hidden, &1)), off}

          {:name, "OFF"} ->
            visible = MapSet.new(on)
            {on, Enum.reject(groups, &MapSet.member?(visible, &1))}

          {:name, "Unchanged"} ->
            {on, off}
        end
      end)

    config = metadata |> Map.drop(@array_keys) |> Map.put("BaseState", {:name, base})
    config = Map.put(config, "OFF", Enum.flat_map(states, &elem(&1, 1)))

    config =
      if alternate?, do: Map.put(config, "ON", Enum.flat_map(states, &elem(&1, 0))), else: config

    intent = Map.get(metadata, "Intent", {:name, "View"})

    unless Enum.all?(configs, &(Map.get(&1, "Intent", {:name, "View"}) == intent)) do
      throw(
        {:layer_error,
         Diagnostics.error(
           :optional_content,
           :unsupported_pdf_feature,
           "layer configurations require different document-wide Intent settings",
           module: __MODULE__
         )}
      )
    end

    # AllPages is the PDF default for an absent ListMode. Both modes affect
    # only the panel listing, so a mixed panel can display groups from all pages.
    list_mode = Map.get(metadata, "ListMode", {:name, "AllPages"})

    config =
      if Enum.all?(configs, &(Map.get(&1, "ListMode", {:name, "AllPages"}) == list_mode)),
        do: config,
        else: Map.put(config, "ListMode", {:name, "AllPages"})

    Enum.reduce(~w(Order AS RBGroups Locked), config, fn key, config ->
      Map.put(config, key, combined_array(configs, key))
    end)
  end

  defp combined_array(dictionaries, key) do
    arrays = Enum.map(dictionaries, &Map.get(&1, key, []))
    count = Enum.reduce(arrays, 0, &(length(&1) + &2))

    case PdfValidator.validate_container_entries(count, module: __MODULE__) do
      :ok ->
        PdfValidator.charge_reader_budget(:max_work, count)
        Enum.concat(arrays)

      error ->
        throw({:layer_error, error})
    end
  end
end
