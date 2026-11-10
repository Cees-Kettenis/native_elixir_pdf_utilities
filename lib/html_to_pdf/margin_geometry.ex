defmodule NativeElixirPdfUtilities.HtmlToPdf.MarginGeometry do
  @moduledoc "Adjoining block margins retain their positive and negative components until separation."

  @type strut :: {number(), number()}

  @doc "Combines adjoining margins without losing the extremes of an earlier collapse set."
  @spec join(number() | strut(), number() | strut()) :: strut()
  def join(first, second) do
    {positive, negative} = if is_tuple(first), do: first, else: {max(first, 0), min(first, 0)}

    {other_positive, other_negative} =
      if is_tuple(second), do: second, else: {max(second, 0), min(second, 0)}

    {max(positive, other_positive), min(negative, other_negative)}
  end

  @doc "Returns the used distance of an adjoining margin set."
  @spec distance(number() | strut()) :: number()
  def distance(strut) do
    case strut do
      {positive, negative} -> positive + negative
      number -> number
    end
  end

  @doc "Returns an edge's retained collapse set, or its original single margin."
  @spec edge(map(), :top | :bottom) :: number() | strut()
  def edge(style, edge) do
    case Map.get(style, :_margin_struts) do
      nil -> Map.get(Map.get(style, :margin, %{}), edge, 0.0)
      struts -> Map.get(struts, edge, Map.get(Map.get(style, :margin, %{}), edge, 0.0))
    end
  end

  @doc "Stores a collapse set and its used edge distance together."
  @spec put_edge(map(), :top | :bottom, number() | strut()) :: map()
  def put_edge(style, edge, strut) do
    style
    |> Map.update(
      :margin,
      Map.put(%{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}, edge, distance(strut)),
      &Map.put(&1, edge, distance(strut))
    )
    |> Map.update(:_margin_struts, %{edge => strut}, &Map.put(&1, edge, strut))
  end
end
