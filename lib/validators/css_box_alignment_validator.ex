defmodule NativeElixirPdfUtilities.Validators.CssBoxAlignmentValidator do
  @moduledoc "Shared grammar for flex and grid box alignment and place shorthands."
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.CssLengthValidator

  @doc "Freezes gap units at the declaring element while retaining percentage dependencies."
  @spec compute_gaps(map()) :: {:ok, map()} | :error | {:error, term()}
  def compute_gaps(style) do
    case style do
      style when not is_map_key(style, :row_gap) and not is_map_key(style, :column_gap) ->
        {:ok, style}

      _ ->
        Enum.reduce_while([:row_gap, :column_gap], {:ok, style}, fn key, {:ok, style} ->
          case Map.get(style, key) do
            nil ->
              {:cont, {:ok, style}}

            value ->
              case CssLengthValidator.resolve(value, nil, style) do
                {:ok, value} -> {:cont, {:ok, Map.put(style, key, value)}}
                error -> {:halt, error}
              end
          end
        end)
    end
  end

  @doc "Resolves used gutters against the owning content axes; cyclic intrinsic percentages contribute zero."
  @spec used_gaps(map(), number() | nil, number() | nil) :: map()
  def used_gaps(style, width, height) do
    case style do
      style when not is_map_key(style, :row_gap) and not is_map_key(style, :column_gap) ->
        style

      _ ->
        Enum.reduce([{:column_gap, width}, {:row_gap, height}], style, fn {key, basis}, style ->
          case Map.get(style, key) do
            nil ->
              style

            value when is_number(value) ->
              style

            value ->
              case CssLengthValidator.resolve(value, basis || 0.0, style) do
                {:ok, value} -> Map.put(style, key, value)
                error -> throw({:render_resource_limit, error})
              end
          end
        end)
    end
  end

  @doc "Checks prepared gaps and their normal-value provenance before layout."
  @spec validate_gaps(map(), String.t()) :: :ok | {:error, term()}
  def validate_gaps(style, source) do
    case style do
      style
      when not is_map_key(style, :row_gap) and not is_map_key(style, :column_gap) and
             not is_map_key(style, :_row_gap_normal) and
             not is_map_key(style, :_column_gap_normal) ->
        :ok

      _ ->
        valid? =
          Enum.all?([:row_gap, :column_gap], fn key ->
            not Map.has_key?(style, key) or
              CssLengthValidator.valid_edge?(Map.get(style, key), :padding)
          end) and
            Enum.all?([:_row_gap_normal, :_column_gap_normal], fn key ->
              not Map.has_key?(style, key) or is_boolean(Map.get(style, key))
            end)

        if valid?,
          do: :ok,
          else:
            Diagnostics.error(
              :layout,
              :invalid_layout,
              "gaps require nonnegative prepared lengths or percentages and boolean normal-value provenance",
              operation: :layout,
              module: __MODULE__,
              source: source
            )
    end
  end

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @properties ~w(align-content justify-content align-items justify-items align-self justify-self place-content place-items place-self)
  @positions ~w(start end self-start self-end flex-start flex-end center left right)
  @keywords Map.new(
              @positions ++
                ~w(stretch auto normal baseline first-baseline last-baseline space-between space-around space-evenly),
              &{&1, String.to_atom(String.replace(&1, "-", "_"))}
            )
  @fields Map.new(
            ~w(align-content justify-content align-items justify-items align-self justify-self),
            &{&1, String.to_atom(String.replace(&1, "-", "_"))}
          )
  @sources @keywords
           |> Map.new(fn {source, value} -> {value, source} end)
           |> Map.put(:first_baseline, "first baseline")
           |> Map.put(:last_baseline, "last baseline")

  @doc "Returns the alignment declarations owned by this validator."
  @spec properties() :: [String.t()]
  def properties, do: @properties

  @doc "Prepares one alignment declaration or an align/justify shorthand pair."
  @spec prepare(String.t(), String.t()) :: {:ok, map()} | {:error, :invalid_document}
  def prepare(property, value) do
    tokens = value |> String.trim() |> String.downcase(:ascii) |> HtmlValidator.split_css_tokens()

    case {property, components(tokens)} do
      {"place-" <> suffix, {:ok, [first]}} when suffix in ["content", "items", "self"] ->
        with {:ok, align} <- scalar("align-" <> suffix, first),
             {:ok, justify} <- scalar("justify-" <> suffix, first),
             do:
               {:ok,
                %{field("align-" <> suffix) => align, field("justify-" <> suffix) => justify}}

      {"place-" <> suffix, {:ok, [first, second]}} when suffix in ["content", "items", "self"] ->
        with {:ok, align} <- scalar("align-" <> suffix, first),
             {:ok, justify} <- scalar("justify-" <> suffix, second),
             do:
               {:ok,
                %{field("align-" <> suffix) => align, field("justify-" <> suffix) => justify}}

      {property, {:ok, [value]}} when is_map_key(@fields, property) ->
        with {:ok, alignment} <- scalar(property, value),
             do: {:ok, %{field(property) => alignment}}

      _ ->
        {:error, :invalid_document}
    end
  end

  defp components(tokens) do
    Enum.reduce_while(tokens, {:ok, [], nil}, fn token, {:ok, values, modifier} ->
      case {modifier, token} do
        {nil, token} when token in ["safe", "unsafe", "first", "last"] ->
          {:cont, {:ok, values, token}}

        {nil, token} ->
          {:cont, {:ok, [token | values], nil}}

        {modifier, token} ->
          {:cont, {:ok, [modifier <> " " <> token | values], nil}}
      end
    end)
    |> case do
      {:ok, values, nil} -> {:ok, Enum.reverse(values)}
      _ -> {:error, :invalid_document}
    end
  end

  defp scalar(property, value) do
    content? = property in ["align-content", "justify-content"]
    self? = property in ["align-self", "justify-self"]
    justify? = String.starts_with?(property, "justify-")

    case String.split(value, " ") do
      [modifier, position] when modifier in ["safe", "unsafe"] and position in @positions ->
        with {:ok, position} <- scalar(property, position),
             do: {:ok, {if(modifier == "safe", do: :safe, else: :unsafe), position}}

      [modifier, "baseline"] when modifier in ["first", "last"] ->
        if property == "justify-content",
          do: {:error, :invalid_document},
          else: {:ok, if(modifier == "first", do: :first_baseline, else: :last_baseline)}

      ["normal"] ->
        {:ok, :stretch}

      ["auto"] when self? ->
        {:ok, :auto}

      ["baseline"] when property != "justify-content" ->
        {:ok, :first_baseline}

      [position] when position in ["left", "right"] and justify? ->
        {:ok, Map.fetch!(@keywords, position)}

      [position] when position in ["self-start", "self-end"] and not content? ->
        {:ok, Map.fetch!(@keywords, position)}

      [position]
      when position in ["start", "end", "flex-start", "flex-end", "center", "stretch"] ->
        {:ok, Map.fetch!(@keywords, position)}

      [distribution]
      when content? and distribution in ["space-between", "space-around", "space-evenly"] ->
        {:ok, Map.fetch!(@keywords, distribution)}

      _ ->
        {:error, :invalid_document}
    end
  end

  defp field(property) do
    Map.fetch!(@fields, property)
  end

  @doc "Checks supplied computed alignment values against the same declaration grammar."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    case style do
      style
      when not is_map_key(style, :align_content) and
             not is_map_key(style, :justify_content) and
             not is_map_key(style, :align_items) and
             not is_map_key(style, :justify_items) and
             not is_map_key(style, :align_self) and
             not is_map_key(style, :justify_self) ->
        :ok

      _ ->
        Enum.reduce_while(
          ~w(align-content justify-content align-items justify-items align-self justify-self),
          :ok,
          fn property, :ok ->
            value = Map.get(style, field(property))

            valid? =
              case value do
                nil ->
                  true

                :none when property in ["align-content", "justify-content"] ->
                  true

                atom when is_map_key(@sources, atom) ->
                  match?({:ok, _}, prepare(property, Map.fetch!(@sources, atom)))

                {safety, position} when safety in [:safe, :unsafe] and is_atom(position) ->
                  match?(
                    {:ok, _},
                    prepare(
                      property,
                      Atom.to_string(safety) <>
                        " " <> String.replace(Atom.to_string(position), "_", "-")
                    )
                  )

                _ ->
                  false
              end

            if valid?,
              do: {:cont, :ok},
              else:
                {:halt,
                 Diagnostics.error(
                   :layout,
                   :invalid_layout,
                   "#{property} requires a validated alignment keyword or safe/unsafe position; recompute styles",
                   operation: :layout,
                   module: __MODULE__,
                   source: source
                 )}
          end
        )
    end
  end
end
