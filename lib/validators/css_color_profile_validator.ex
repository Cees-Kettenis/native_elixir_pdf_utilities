defmodule NativeElixirPdfUtilities.Validators.CssColorProfileValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{CssSyntaxValidator, HtmlValidator}

  @doc "Collects the last author definition of each named relative-colorimetric profile."
  @spec rules(String.t()) :: map()
  def rules(source) do
    if Regex.match?(~r/@color-profile\b/i, source) do
      Enum.reduce(CssSyntaxValidator.rules(source), %{}, fn rule, profiles ->
        with true <- rule.block,
             <<"@", prelude::binary>> <- String.trim(rule.selectors),
             {:ok, keyword, rest} <- HtmlValidator.parse_css_identifier(prelude),
             "color-profile" <- String.downcase(keyword, :ascii),
             {:ok, "--" <> suffix = name, ""} when suffix != "" <-
               HtmlValidator.parse_css_identifier(String.trim(rest)) do
          descriptors =
            for %{declaration: {name, value}} <-
                  CssSyntaxValidator.declarations(rule.declarations),
                into: %{},
                do: {name, value}

          case descriptors do
            %{"src" => source} ->
              if Map.get(descriptors, "rendering-intent", "relative-colorimetric") ==
                   "relative-colorimetric" do
                case HtmlValidator.parse_background_image_reference(source) do
                  {:ok, reference} when is_binary(reference) -> Map.put(profiles, name, reference)
                  _ -> Map.delete(profiles, name)
                end
              else
                Map.delete(profiles, name)
              end

            _ ->
              Map.delete(profiles, name)
          end
        else
          _ -> profiles
        end
      end)
    else
      %{}
    end
  end

  @doc "Validates an RGB matrix ICC profile with an XYZ profile connection space."
  @spec prepare(binary()) :: {:ok, map()} | :error
  def prepare(bytes) do
    HtmlValidator.reserve_render_resource(:max_input_bytes, byte_size(bytes), :asset)

    case bytes do
      <<size::32, _::binary-size(4), version, _::binary-size(3), class::binary-size(4), "RGB ",
        "XYZ ", _::binary-size(12), "acsp", _::binary-size(88), count::32, table::binary>>
      when size == byte_size(bytes) and version in [2, 4] and
             class in ["scnr", "mntr", "prtr", "spac"] and
             count <= div(byte_size(table), 12) ->
        HtmlValidator.check_render_resource(:max_entries, count, :asset)
        HtmlValidator.reserve_render_resource(:max_work, count, :asset)
        table_size = count * 12
        <<records::binary-size(^table_size), _::binary>> = table

        tags =
          for <<name::binary-size(4), offset::32, length::32 <- records>>,
              offset >= 132 + count * 12,
              rem(offset, 4) == 0,
              offset + length <= size,
              into: %{},
              do: {name, binary_part(bytes, offset, length)}

        with {:ok, red, r_curve} <- channel(tags, "r"),
             {:ok, green, g_curve} <- channel(tags, "g"),
             {:ok, blue, b_curve} <- channel(tags, "b") do
          {:ok, %{columns: [red, green, blue], curves: [r_curve, g_curve, b_curve]}}
        end

      _ ->
        :error
    end
  end

  defp channel(tags, name) do
    case {Map.get(tags, name <> "XYZ"), Map.get(tags, name <> "TRC")} do
      {<<"XYZ ", 0::32, x::signed-32, y::signed-32, z::signed-32>>, source} ->
        with {:ok, curve} <- curve(source),
             do: {:ok, [x / 65536, y / 65536, z / 65536], curve}

      _ ->
        :error
    end
  end

  defp curve(source) do
    case source do
      <<"curv", 0::32, 0::32>> ->
        {:ok, {:gamma, 1.0}}

      <<"curv", 0::32, 1::32, gamma::16>> when gamma > 0 ->
        {:ok, {:gamma, gamma / 256}}

      <<"para", 0::32, 0::16, 0::16, gamma::signed-32>> when gamma > 0 ->
        {:ok, {:gamma, gamma / 65536}}

      <<"curv", 0::32, size::32, data::binary>> when size > 1 and byte_size(data) == size * 2 ->
        HtmlValidator.check_render_resource(:max_entries, size, :asset)
        HtmlValidator.reserve_render_resource(:max_work, size, :asset)
        {:ok, {:table, data, size}}

      _ ->
        :error
    end
  end
end
