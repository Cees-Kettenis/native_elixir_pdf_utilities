defmodule NativeElixirPdfUtilities.Validators.PdfValidator do
  @moduledoc """
  Shared semantic validation for parsed PDF documents.

  The validator consumes the object model produced by the PDF reader. It does
  not tokenize or load PDF bytes again. Successful validation returns a
  prepared context containing the resolved catalog, page-tree root, semantic
  page traversal, inherited page values, and the reader-compatible document.

  Operation validators should consume this context instead of walking the raw
  page tree or resolving document structure independently.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.Reader

  @max_pages 10_000
  @max_page_tree_depth 1_000
  @inheritable_page_keys ["Resources", "MediaBox", "CropBox", "Rotate"]

  @typedoc "An indirect PDF object reference."
  @type ref :: {non_neg_integer(), non_neg_integer()}

  @typedoc "A parsed PDF value."
  @type value ::
          nil
          | boolean()
          | integer()
          | float()
          | {:name, binary()}
          | {:string, binary()}
          | {:hex, binary()}
          | {:ref, ref()}
          | [value()]
          | %{optional(binary()) => value()}

  @typedoc "Diagnostic ownership supplied by the public operation."
  @type diagnostic_option ::
          {:operation, atom()} | {:module, module()} | {:source, String.t() | nil}

  @typedoc "A parsed reader document before or after semantic validation."
  @type document :: %{
          required(:objects) => %{optional(ref()) => map()},
          optional(atom()) => term()
        }

  @typedoc "The nearest inherited value and the page-tree node that declared it."
  @type inherited_value :: %{required(:value) => value(), required(:source_ref) => ref()}

  @typedoc "A semantically identified page prepared during page-tree traversal."
  @type page_context :: %{
          required(:ref) => ref(),
          required(:dictionary) => map(),
          required(:resources) => value(),
          required(:rotate) => value(),
          required(:media_box) => value(),
          required(:crop_box) => value(),
          required(:inherited) => %{optional(binary()) => inherited_value()}
        }

  @typedoc "Reusable validated PDF structure consumed by operation validators."
  @type context :: %{
          required(:document) => document(),
          required(:catalog) => map(),
          required(:catalog_ref) => ref() | nil,
          required(:page_tree_ref) => ref(),
          required(:pages) => [page_context()]
        }

  @typedoc "A structurally validated indirect stream."
  @type stream_context :: %{
          required(:ref) => ref(),
          required(:dictionary) => map(),
          required(:stream) => binary()
        }

  @doc """
  Parses and validates a PDF binary into the reusable shared context.

  This is the utility entry point for future public PDF validation and other
  PDF-consuming features. Existing callers can continue using
  `NativeElixirPdfUtilities.Pdf.Reader.read/1` for its compatibility document.
  """
  @spec validate_pdf(binary()) ::
          {:ok, context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_pdf(pdf) do
    case Reader.read_validated(pdf) do
      {:ok, context} ->
        {:ok, context}

      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          diagnostic
          |> Map.put(:operation, :validate_pdf)
          |> Map.put(:module, __MODULE__)}}
    end
  end

  @doc """
  Validates reusable catalog, page-tree, reference, and inherited-value invariants.

  The returned context contains `:document` in the same shape returned by
  `NativeElixirPdfUtilities.Pdf.Reader.read/1`.
  """
  @spec validate(document(), [diagnostic_option()]) ::
          {:ok, context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate(document, opts \\ []) do
    case document do
      %{objects: objects, trailer: trailer} when is_map(objects) and is_map(trailer) ->
        root = Map.get(trailer, "Root")
        traversal = %{seen: %{}, pages: [], page_count: 0}

        with {:ok, catalog} <- dictionary(document, root, opts),
             true <- named?(Map.get(catalog, "Type"), "Catalog"),
             {:ok, page_tree_ref} <- required_reference(catalog, "Pages", opts),
             {:ok, traversal, _descendant_count} <-
               walk_page_tree(
                 document,
                 page_tree_ref,
                 nil,
                 %{},
                 %{},
                 traversal,
                 0,
                 opts
               ) do
          pages = Enum.reverse(traversal.pages)

          reader_pages =
            Enum.map(pages, fn page ->
              %{
                ref: page.ref,
                resources: page.resources,
                rotate: page.rotate,
                media_box: page.media_box
              }
            end)

          {:ok,
           %{
             document: Map.put(document, :pages, reader_pages),
             catalog: catalog,
             catalog_ref: reference_identity(root),
             page_tree_ref: reference_identity(page_tree_ref),
             pages: pages
           }}
        else
          false -> error(:page_tree, :invalid_pdf_input, "catalog object is malformed", opts)
          {:error, _} = validation_error -> validation_error
        end

      _ ->
        error(:validation, :invalid_pdf_input, "parsed PDF document is malformed", opts)
    end
  end

  @doc """
  Resolves an indirect value with missing-reference and cycle diagnostics.
  """
  @spec resolve(document(), value(), [diagnostic_option()]) ::
          {:ok, value()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def resolve(document, value, opts \\ []) do
    case value do
      {:ref, ref} -> resolve_reference(document, ref, %{}, opts)
      value -> {:ok, value}
    end
  end

  @doc """
  Resolves a value and requires the resulting semantic value to be a dictionary.
  """
  @spec dictionary(document(), value(), [diagnostic_option()]) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def dictionary(document, value, opts \\ []) do
    with {:ok, resolved} <- resolve(document, value, opts),
         true <- is_map(resolved) do
      {:ok, resolved}
    else
      false -> error(:resolution, :invalid_pdf_input, "expected a PDF dictionary", opts)
      {:error, _} = resolution_error -> resolution_error
    end
  end

  @doc """
  Resolves a dictionary and returns one of its values.
  """
  @spec fetch(document(), value(), binary(), [diagnostic_option()]) ::
          {:ok, value()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def fetch(document, dictionary_value, key, opts \\ []) do
    case key do
      key when is_binary(key) ->
        with {:ok, dictionary} <- dictionary(document, dictionary_value, opts) do
          {:ok, Map.get(dictionary, key)}
        end

      _ ->
        error(:resolution, :invalid_pdf_input, "dictionary key must be a binary", opts)
    end
  end

  @doc """
  Resolves and validates an indirect stream and its declared byte length.

  This validates structure shared by PDF-consuming operations but does not
  decode operation-specific filters.
  """
  @spec validate_stream(document(), value(), [diagnostic_option()]) ::
          {:ok, stream_context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_stream(document, value, opts \\ []) do
    case value do
      {:ref, ref} ->
        with {:ok, stream_ref, object} <- resolve_stream_object(document, ref, %{}, opts),
             %{value: dictionary, stream: stream} <- object,
             true <- is_map(dictionary) and is_binary(stream),
             :ok <- validate_stream_length(stream, dictionary, document, stream_ref, opts) do
          {:ok, %{ref: stream_ref, dictionary: dictionary, stream: stream}}
        else
          false -> error(:stream, :invalid_pdf_input, "object is not a stream", opts, object: ref)
          {:error, _} = stream_error -> stream_error
          _ -> error(:stream, :invalid_pdf_input, "object is not a stream", opts, object: ref)
        end

      _ ->
        error(:stream, :invalid_pdf_input, "stream must be an indirect object", opts)
    end
  end

  @doc """
  Resolves a fixed-length array whose elements must all be numbers.

  Operation validators can use this helper for page rectangles, matrices, and
  other shared PDF number-array structures.
  """
  @spec number_array(document(), value(), non_neg_integer(), [diagnostic_option()]) ::
          {:ok, [number()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def number_array(document, value, expected_length, opts \\ []) do
    with {:ok, values} <- resolve(document, value, opts),
         true <- is_list(values) and length(values) == expected_length do
      values
      |> Enum.reduce_while({:ok, []}, fn item, {:ok, numbers} ->
        case resolve(document, item, opts) do
          {:ok, number} when is_number(number) -> {:cont, {:ok, [number | numbers]}}
          {:ok, _value} -> {:halt, :invalid}
          {:error, _} = resolution_error -> {:halt, resolution_error}
        end
      end)
      |> case do
        {:ok, numbers} -> {:ok, Enum.reverse(numbers)}
        :invalid -> error(:validation, :invalid_pdf_input, "expected a PDF number array", opts)
        {:error, _} = resolution_error -> resolution_error
      end
    else
      false -> error(:validation, :invalid_pdf_input, "expected a PDF number array", opts)
      {:error, _} = resolution_error -> resolution_error
    end
  end

  defp walk_page_tree(
         document,
         page_ref,
         expected_parent,
         inherited,
         ancestors,
         traversal,
         depth,
         opts
       ) do
    ref = reference_identity(page_ref)

    cond do
      Map.has_key?(ancestors, ref) ->
        error(:page_tree, :invalid_pdf_input, "page tree contains a cycle", opts, object: ref)

      Map.has_key?(traversal.seen, ref) ->
        error(
          :page_tree,
          :invalid_pdf_input,
          "page tree contains a duplicate reference",
          opts,
          object: ref
        )

      depth >= @max_page_tree_depth ->
        error(:limits, :resource_limit_exceeded, "PDF page tree depth exceeds the limit", opts,
          object: ref
        )

      traversal.page_count >= @max_pages ->
        error(:limits, :resource_limit_exceeded, "PDF page count exceeds the limit", opts)

      true ->
        with {:ok, dictionary} <- dictionary(document, page_ref, opts),
             :ok <- validate_page_tree_parent(dictionary, ref, expected_parent, opts) do
          inherited = inherit_page_values(dictionary, ref, inherited)
          ancestors = Map.put(ancestors, ref, true)
          traversal = %{traversal | seen: Map.put(traversal.seen, ref, true)}

          case Map.get(dictionary, "Type") do
            {:name, "Page"} ->
              page = %{
                ref: ref,
                dictionary: dictionary,
                resources: inherited_value(inherited, "Resources"),
                rotate: inherited_value(inherited, "Rotate"),
                media_box: inherited_value(inherited, "MediaBox"),
                crop_box: inherited_value(inherited, "CropBox"),
                inherited: inherited
              }

              traversal = %{
                traversal
                | pages: [page | traversal.pages],
                  page_count: traversal.page_count + 1
              }

              {:ok, traversal, 1}

            {:name, "Pages"} ->
              with {:ok, declared_count} <- page_tree_count(document, dictionary, ref, opts),
                   {:ok, kids} <- page_tree_kids(document, dictionary, opts),
                   {:ok, traversal, actual_count} <-
                     walk_kids(
                       document,
                       kids,
                       inherited,
                       ancestors,
                       traversal,
                       ref,
                       depth + 1,
                       opts
                     ),
                   :ok <- validate_page_tree_count(declared_count, actual_count, ref, opts) do
                {:ok, traversal, actual_count}
              end

            _ ->
              error(:page_tree, :invalid_pdf_input, "page tree node has an invalid Type", opts,
                object: ref
              )
          end
        end
    end
  end

  defp walk_kids(document, kids, inherited, ancestors, traversal, parent_ref, depth, opts) do
    Enum.reduce_while(
      kids,
      {:ok, traversal, 0},
      fn kid, {:ok, traversal, descendant_count} ->
        case reference_identity(kid) do
          nil ->
            {:halt,
             error(
               :page_tree,
               :invalid_pdf_input,
               "Pages Kids array contains a non-reference",
               opts
             )}

          _ref ->
            case walk_page_tree(
                   document,
                   kid,
                   parent_ref,
                   inherited,
                   ancestors,
                   traversal,
                   depth,
                   opts
                 ) do
              {:ok, traversal, child_count} ->
                {:cont, {:ok, traversal, descendant_count + child_count}}

              {:error, _} = page_error ->
                {:halt, page_error}
            end
        end
      end
    )
  end

  defp page_tree_kids(document, dictionary, opts) do
    with {:ok, kids} <- resolve(document, Map.get(dictionary, "Kids"), opts),
         true <- is_list(kids) do
      {:ok, kids}
    else
      false ->
        error(:page_tree, :invalid_pdf_input, "Pages node is missing a valid Kids array", opts)

      {:error, _} = resolution_error ->
        resolution_error
    end
  end

  defp page_tree_count(document, dictionary, ref, opts) do
    case resolve(document, Map.get(dictionary, "Count"), opts) do
      {:ok, count} when is_integer(count) and count >= 0 ->
        {:ok, count}

      _ ->
        error(:page_tree, :invalid_pdf_input, "Pages node is missing a valid Count", opts,
          object: ref
        )
    end
  end

  defp validate_page_tree_parent(dictionary, ref, expected_parent, opts) do
    case expected_parent do
      nil ->
        case Map.has_key?(dictionary, "Parent") do
          false ->
            :ok

          true ->
            error(
              :page_tree,
              :invalid_pdf_input,
              "page tree root must not declare Parent",
              opts,
              object: ref
            )
        end

      expected_parent ->
        case Map.fetch(dictionary, "Parent") do
          {:ok, {:ref, ^expected_parent}} ->
            :ok

          :error ->
            error(
              :page_tree,
              :invalid_pdf_input,
              "page tree node is missing its required Parent",
              opts,
              object: ref
            )

          {:ok, {:ref, _parent}} ->
            error(
              :page_tree,
              :invalid_pdf_input,
              "page tree node Parent does not match its containing Pages node",
              opts,
              object: ref
            )

          {:ok, _parent} ->
            error(
              :page_tree,
              :invalid_pdf_input,
              "page tree node has a malformed Parent",
              opts,
              object: ref
            )
        end
    end
  end

  defp validate_page_tree_count(declared_count, actual_count, ref, opts) do
    case declared_count == actual_count do
      true ->
        :ok

      false ->
        error(
          :page_tree,
          :invalid_pdf_input,
          "Pages node Count #{declared_count} does not match #{actual_count} descendant pages",
          opts,
          object: ref
        )
    end
  end

  defp inherit_page_values(dictionary, ref, inherited) do
    Enum.reduce(@inheritable_page_keys, inherited, fn key, inherited ->
      case Map.has_key?(dictionary, key) do
        true -> Map.put(inherited, key, %{value: Map.fetch!(dictionary, key), source_ref: ref})
        false -> inherited
      end
    end)
  end

  defp inherited_value(inherited, key) do
    case Map.get(inherited, key) do
      nil -> nil
      %{value: value} -> value
    end
  end

  defp required_reference(dictionary, key, opts) do
    case Map.get(dictionary, key) do
      {:ref, _ref} = reference -> {:ok, reference}
      _ -> error(:page_tree, :invalid_pdf_input, "required #{key} reference is missing", opts)
    end
  end

  defp resolve_reference(document, ref, seen, opts) do
    cond do
      not valid_ref?(ref) ->
        error(:resolution, :invalid_pdf_input, "indirect reference is malformed", opts)

      Map.has_key?(seen, ref) ->
        error(:resolution, :invalid_pdf_input, "indirect reference chain contains a cycle", opts,
          object: ref
        )

      true ->
        case fetch_object_record(document, ref) do
          {:ok, %{value: {:ref, next_ref}}} ->
            resolve_reference(document, next_ref, Map.put(seen, ref, true), opts)

          {:ok, %{value: value}} ->
            {:ok, value}

          {:ok, _object} ->
            error(:resolution, :invalid_pdf_input, "indirect object record is malformed", opts,
              object: ref
            )

          :missing ->
            error(:resolution, :invalid_pdf_input, "indirect object reference is missing", opts,
              object: ref
            )

          :malformed_document ->
            error(
              :resolution,
              :invalid_pdf_input,
              "parsed PDF document object table is malformed",
              opts
            )
        end
    end
  end

  defp resolve_stream_object(document, ref, seen, opts) do
    cond do
      not valid_ref?(ref) ->
        error(:resolution, :invalid_pdf_input, "indirect stream reference is malformed", opts)

      Map.has_key?(seen, ref) ->
        error(:resolution, :invalid_pdf_input, "indirect stream reference contains a cycle", opts,
          object: ref
        )

      true ->
        case fetch_object_record(document, ref) do
          {:ok, %{value: {:ref, next_ref}, stream: nil}} ->
            resolve_stream_object(document, next_ref, Map.put(seen, ref, true), opts)

          {:ok, %{stream: stream} = object} when is_binary(stream) ->
            {:ok, ref, object}

          {:ok, %{stream: _stream}} ->
            error(:stream, :invalid_pdf_input, "object is not a stream", opts, object: ref)

          {:ok, _object} ->
            error(:stream, :invalid_pdf_input, "stream object record is malformed", opts,
              object: ref
            )

          :missing ->
            error(:resolution, :invalid_pdf_input, "stream reference is missing", opts,
              object: ref
            )

          :malformed_document ->
            error(
              :resolution,
              :invalid_pdf_input,
              "parsed PDF document object table is malformed",
              opts
            )
        end
    end
  end

  defp fetch_object_record(document, ref) do
    case document do
      %{objects: objects} when is_map(objects) ->
        case Map.fetch(objects, ref) do
          {:ok, object} -> {:ok, object}
          :error -> :missing
        end

      _ ->
        :malformed_document
    end
  end

  defp validate_stream_length(stream, dictionary, document, ref, opts) do
    case Map.get(dictionary, "Length") do
      length when is_integer(length) and length == byte_size(stream) ->
        :ok

      {:ref, _} = length_ref ->
        with {:ok, length} <- resolve(document, length_ref, opts),
             true <- is_integer(length) and length == byte_size(stream) do
          :ok
        else
          _ ->
            error(:stream, :invalid_pdf_input, "stream length does not match stream bytes", opts,
              object: ref
            )
        end

      _ ->
        error(:stream, :invalid_pdf_input, "stream has an invalid Length", opts, object: ref)
    end
  end

  defp reference_identity(value) do
    case value do
      {:ref, ref} -> ref
      _ -> nil
    end
  end

  defp valid_ref?(ref) do
    case ref do
      {object, generation}
      when is_integer(object) and object >= 0 and is_integer(generation) and generation >= 0 ->
        true

      _ ->
        false
    end
  end

  defp named?(value, expected) do
    case value do
      {:name, name} -> name == expected
      _ -> false
    end
  end

  defp error(stage, reason, message, opts, details \\ []) do
    message =
      Enum.reduce(details, message, fn detail, message ->
        case detail do
          {:object, {object, generation}} -> "#{message}; object #{object} #{generation}"
        end
      end)

    Diagnostics.error(stage, reason, message, opts)
  end
end
