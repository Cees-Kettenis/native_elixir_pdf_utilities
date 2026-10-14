defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgSourcePolicyTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.{SvgReferenceValidator, SvgStyleValidator}

  test "SVG transforms retain authored converter source instead of a reconstructed CTM" do
    for source <- [
          "none",
          "translate(10,20) scale(2,-3)",
          "rotate(90 10 20)",
          "matrix(1 0.5 2 3 4 5)",
          "skewX(10)"
        ] do
      assert {:ok, ^source} = SvgStyleValidator.parse("transform", source)
    end

    for invalid <- [nil, 12, :none, []] do
      assert :error = SvgStyleValidator.parse("transform", invalid)
    end

    assert SvgStyleValidator.defaults(%{}).svg_transform == "none"
  end

  test "reference extraction collects local source use identifiers without resolving geometry" do
    node = fn tag, attributes, children ->
      %{tag: tag, attributes: attributes, children: children, type: :element}
    end

    source =
      node.("svg", %{}, [
        node.("g", %{}, [node.("use", %{"href" => "#shared"}, [])]),
        node.("use", %{"xlink:href" => "#shared"}, []),
        node.("use", %{"href" => "#another"}, []),
        node.("use", %{"href" => "remote.svg#external"}, []),
        node.("path", %{"href" => "#shape"}, [])
      ])

    assert SvgReferenceValidator.referenced_ids(source) == MapSet.new(["shared", "another"])
  end
end
