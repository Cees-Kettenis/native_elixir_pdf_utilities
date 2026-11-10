defmodule NativeElixirPdfUtilities.Validators.CssLogicalValidator do
  @moduledoc "Shared logical-to-physical property mapping using computed writing mode and direction."
  alias NativeElixirPdfUtilities.Validators.{
    CssCascadeValidator,
    CssMathValidator,
    CssSizingValidator
  }

  @properties Map.new(
                for axis <- ~w(inline block),
                    family <- ~w(margin padding inset border),
                    endpoint <- [nil, "start", "end"],
                    suffix <-
                      if(family == "border", do: [nil, "width", "style", "color"], else: [nil]) do
                  name =
                    Enum.reject([family, axis, endpoint, suffix], &is_nil/1) |> Enum.join("-")

                  {name, {:edges, family, axis, endpoint, suffix}}
                end
              )
              |> Map.merge(
                Map.new(
                  for axis <- ~w(inline block), prefix <- ["", "min-", "max-"] do
                    {prefix <> axis <> "-size", {:size, axis, prefix}}
                  end
                )
              )
              |> Map.put("inset", :inset)

  @doc "Lists the fixed grammar of logical sizing and edge properties."
  @spec properties() :: [String.t()]
  def properties, do: Map.keys(@properties)

  @doc "Identifies properties whose physical assignments depend on the computed axes."
  @spec logical?(String.t()) :: boolean()
  def logical?(property), do: Map.has_key?(@properties, property)

  @doc "Returns physical sides and dimensions for all selected writing-mode and direction combinations."
  @spec axes(map()) :: map()
  def axes(style) do
    rtl? = Map.get(style, :direction, :ltr) == :rtl
    mode = Map.get(style, :writing_mode, :horizontal_tb)

    {inline_start, inline_end, block_start, block_end, inline_size, block_size} =
      case mode do
        :horizontal_tb -> {:left, :right, :top, :bottom, :width, :height}
        :vertical_rl -> {:top, :bottom, :right, :left, :height, :width}
        :vertical_lr -> {:top, :bottom, :left, :right, :height, :width}
        :sideways_rl -> {:top, :bottom, :right, :left, :height, :width}
        :sideways_lr -> {:bottom, :top, :left, :right, :height, :width}
      end

    %{
      inline_start: if(rtl?, do: inline_end, else: inline_start),
      inline_end: if(rtl?, do: inline_start, else: inline_end),
      block_start: block_start,
      block_end: block_end,
      inline_size: inline_size,
      block_size: block_size
    }
  end

  @doc "Parses writing-mode and direction identifiers through the shared escaped identifier grammar."
  @spec parse_axis(String.t(), String.t()) :: {:ok, {atom(), atom()}} | :error
  def parse_axis(property, value) do
    allowed =
      case property do
        "writing-mode" ->
          %{
            "horizontal-tb" => :horizontal_tb,
            "vertical-rl" => :vertical_rl,
            "vertical-lr" => :vertical_lr,
            "sideways-rl" => :sideways_rl,
            "sideways-lr" => :sideways_lr
          }

        "direction" ->
          %{"ltr" => :ltr, "rtl" => :rtl}
      end

    with {:ok, name, ""} <-
           NativeElixirPdfUtilities.Validators.HtmlValidator.parse_css_identifier(value),
         {:ok, axis} <- Map.fetch(allowed, String.downcase(name, :ascii)),
         do: {:ok, {if(property == "writing-mode", do: :writing_mode, else: :direction), axis}},
         else: (_ -> :error)
  end

  @doc "Maps a logical property to physical declarations without interpreting the physical value grammar."
  @spec physical_properties(String.t(), map()) :: [String.t()] | nil
  def physical_properties(property, style) do
    case Map.get(@properties, property) do
      nil ->
        nil

      :inset ->
        ~w(top right bottom left)

      {:size, axis, prefix} ->
        [
          prefix <>
            Atom.to_string(Map.fetch!(axes(style), String.to_existing_atom(axis <> "_size")))
        ]

      {:edges, family, axis, endpoint, suffix} ->
        endpoints = if endpoint, do: [endpoint], else: ["start", "end"]

        Enum.map(endpoints, fn endpoint ->
          side =
            Map.fetch!(axes(style), String.to_existing_atom(axis <> "_" <> endpoint))
            |> Atom.to_string()

          if family == "inset",
            do: side,
            else: Enum.reject([family, side, suffix], &is_nil/1) |> Enum.join("-")
        end)
    end
  end

  @doc "Expands balanced logical pairs and physical inset shorthand through the shared component tokenizer."
  @spec expand(String.t(), String.t(), map()) ::
          {:ok, [{String.t(), String.t()}]} | :error | :unhandled
  def expand(property, value, style) do
    case Map.fetch(@properties, property) do
      :error ->
        :unhandled

      {:ok, descriptor} ->
        properties = physical_properties(property, style)

        values =
          cond do
            CssCascadeValidator.keyword(value) != nil ->
              List.duplicate(value, length(properties))

            match?({:edges, "border", _, _, nil}, descriptor) ->
              List.duplicate(value, length(properties))

            length(properties) == 1 ->
              [value]

            true ->
              case {length(properties), CssMathValidator.component_terms(value, false)} do
                {2, [one]} -> [one, one]
                {2, [one, two]} -> [one, two]
                {4, [one]} -> [one, one, one, one]
                {4, [one, two]} -> [one, two, one, two]
                {4, [one, two, three]} -> [one, two, three, two]
                {4, [one, two, three, four]} -> [one, two, three, four]
                _ -> nil
              end
          end

        if values, do: {:ok, Enum.zip(properties, values)}, else: :error
    end
  end

  @doc "Inherits logical computed values using the parent's axes before assigning the child's physical fields."
  @spec inherit_style(map(), String.t()) :: {:ok, map()}
  def inherit_style(style, property) do
    parent = Map.get(style, :_cascade_inherited, %{})
    initial = CssCascadeValidator.initial(parent)

    from =
      physical_properties(property, parent)
      |> Enum.flat_map(&CssCascadeValidator.targets(&1, parent))

    to =
      physical_properties(property, style)
      |> Enum.flat_map(&CssCascadeValidator.targets(&1, style))

    inherited =
      Enum.zip(from, to)
      |> Enum.reduce(style, fn {from, to}, result ->
        source_path = CssCascadeValidator.primary_field(from, parent)
        target_path = CssCascadeValidator.primary_field(to, style)

        source =
          case source_path do
            [key] ->
              if Map.has_key?(parent, key), do: parent, else: initial

            [key, side] ->
              if Map.has_key?(Map.get(parent, key, %{}), side), do: parent, else: initial
          end

        value = get_in(source, source_path)

        {value, result} =
          if String.starts_with?(from, "margin-") do
            [_, side] = String.split(from, "-")
            axis = if side in ["left", "right"], do: :width, else: :height
            {start?, end?} = CssSizingValidator.margin_flags(parent, axis)
            auto? = if side in ["left", "top"], do: start?, else: end?

            {if(auto?, do: :auto, else: value),
             Map.drop(result, [
               :_margin_before_em,
               :_margin_after_em
             ])}
          else
            {value, result}
          end

        result =
          if String.starts_with?(to, "margin-") do
            [_, side] = String.split(to, "-")
            side = String.to_existing_atom(side)

            {flag, sides} =
              if side in [:left, :right],
                do: {:margin_inline_auto, [:left, :right]},
                else: {:margin_block_auto, [:top, :bottom]}

            flags =
              Map.get(result, flag, Map.new(sides, &{&1, false})) |> Map.put(side, value == :auto)

            if Enum.any?(flags, fn {_, auto?} -> auto? end),
              do: Map.put(result, flag, flags),
              else: Map.delete(result, flag)
          else
            result
          end

        result = put_in(result, Enum.map(target_path, &Access.key(&1, %{})), value)
        if to == "margin-bottom", do: Map.put(result, :margin_after, value), else: result
      end)

    {:ok, inherited}
  end
end
