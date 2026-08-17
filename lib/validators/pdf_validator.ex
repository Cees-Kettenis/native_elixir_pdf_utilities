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

  @max_objects 100_000
  @max_object_stream_entries 10_000
  @max_pages 10_000
  @max_page_tree_depth 1_000
  @max_reference_chain_depth 1_000
  @max_reference_resolution_work 25_000
  @max_value_depth 100
  @max_input_bytes 50_000_000
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

  @typedoc "A parsed cross-reference entry."
  @type xref_entry ::
          {:free, non_neg_integer(), non_neg_integer()}
          | {:uncompressed, non_neg_integer(), non_neg_integer()}
          | {:compressed, pos_integer(), non_neg_integer()}

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
          required(:stream) => binary(),
          optional(:filters) => [filter_context()]
        }

  @typedoc "A normalized stream filter and its validated decoding parameters."
  @type filter_context :: %{
          required(:name) => binary(),
          required(:parameters) => map() | nil
        }

  @doc """
  Validates the public PDF reader input boundary.
  """
  @spec validate_input(term(), [diagnostic_option()]) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_input(pdf, opts \\ []) do
    cond do
      not is_binary(pdf) ->
        error(:input, :invalid_pdf_input, "PDF input must be a binary", opts)

      byte_size(pdf) > @max_input_bytes ->
        error(:limits, :resource_limit_exceeded, "PDF input exceeds the byte limit", opts)

      Regex.match?(~r/\A%PDF-(1\.[0-7]|2\.0)(?:\s|\r|\n)/, pdf) ->
        :ok

      true ->
        error(
          :header,
          :invalid_pdf_input,
          "PDF header is missing or has an unsupported version",
          opts
        )
    end
  end

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
  Validates a parsed cross-reference table and its trailer.

  Object zero must be present as the free-list head with generation 65535.
  All remaining entries must fit the trailer's declared object and byte bounds.
  """
  @spec validate_xref(
          %{optional(integer()) => xref_entry()},
          map(),
          binary(),
          [diagnostic_option()]
        ) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_xref(entries, trailer, pdf, opts \\ []) do
    case {entries, trailer, pdf} do
      {entries, trailer, pdf} when is_map(entries) and is_map(trailer) and is_binary(pdf) ->
        size = Map.get(trailer, "Size")

        cond do
          not is_integer(size) or size <= 0 or size > @max_objects + 1 ->
            error(:xref, :invalid_pdf_input, "xref Size is malformed", opts)

          map_size(entries) > @max_objects ->
            error(:limits, :resource_limit_exceeded, "PDF object count exceeds the limit", opts)

          not match?({:ref, _}, Map.get(trailer, "Root")) ->
            error(
              :trailer,
              :invalid_pdf_input,
              "trailer does not contain a catalog reference",
              opts
            )

          not valid_object_zero_entry?(Map.get(entries, 0), size) ->
            error(
              :xref,
              :invalid_pdf_input,
              "xref object 0 must be a free entry with generation 65535",
              opts
            )

          Enum.any?(entries, fn {object, entry} ->
            not is_integer(object) or object < 0 or object >= size or
                not valid_xref_entry?(entry, pdf, size)
          end) ->
            error(:xref, :invalid_pdf_input, "xref entry is outside its declared bounds", opts)

          not is_nil(Map.get(trailer, "Encrypt")) ->
            error(:encryption, :encrypted_pdf, "encrypted PDFs are not supported", opts)

          true ->
            :ok
        end

      _ ->
        error(:xref, :invalid_pdf_input, "parsed xref context is malformed", opts)
    end
  end

  @doc """
  Validates object-stream metadata before its decoded header is scanned.

  Object streams are limited independently from the document-wide object count
  so attacker-controlled entry counts cannot create excessive parsing work.
  """
  @spec validate_object_stream_header(term(), term(), term(), [diagnostic_option()]) ::
          {:ok, %{count: non_neg_integer(), first: non_neg_integer()}}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_object_stream_header(dictionary, decoded_size, ref, opts \\ []) do
    case {dictionary, decoded_size, ref} do
      {dictionary, decoded_size, {object, generation}}
      when is_map(dictionary) and is_integer(decoded_size) and decoded_size >= 0 and
             is_integer(object) and object >= 0 and is_integer(generation) and generation >= 0 ->
        count = Map.get(dictionary, "N")
        first = Map.get(dictionary, "First")

        cond do
          not is_integer(count) or count < 0 ->
            error(:object_stream, :invalid_pdf_input, "object stream header is invalid", opts,
              object: ref
            )

          count > @max_object_stream_entries ->
            error(
              :limits,
              :resource_limit_exceeded,
              "PDF object stream entry count exceeds the limit",
              opts,
              object: ref
            )

          not is_integer(first) or first < 0 or first > decoded_size ->
            error(:object_stream, :invalid_pdf_input, "object stream header is invalid", opts,
              object: ref
            )

          true ->
            {:ok, %{count: count, first: first}}
        end

      _ ->
        error(:object_stream, :invalid_pdf_input, "object stream header is invalid", opts)
    end
  end

  @doc false
  @spec validate_value_depth(term(), [diagnostic_option()]) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_value_depth(depth, opts \\ []) do
    case depth do
      depth when is_integer(depth) and depth >= 0 and depth <= @max_value_depth ->
        :ok

      depth when is_integer(depth) and depth > @max_value_depth ->
        error(
          :limits,
          :resource_limit_exceeded,
          "PDF value nesting depth exceeds the #{@max_value_depth}-level limit",
          opts
        )

      _ ->
        error(:object, :invalid_pdf_input, "PDF value nesting depth is invalid", opts)
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

        traversal = %{
          seen: %{},
          pages: [],
          page_count: 0,
          reference_resolution: %{cache: %{}, work: 0}
        }

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
      {:ref, ref} ->
        case resolve_reference(document, ref, %{cache: %{}, work: 0}, %{}, 0, opts) do
          {:ok, resolved, _terminal_ref, _resolution} -> {:ok, resolved}
          {:error, _} = resolution_error -> resolution_error
        end

      value ->
        {:ok, value}
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
  Resolves a stream and prepares its semantic filter chain for byte decoding.

  The returned filter names are canonical and their parameters have validated
  predictor dimensions and LZW settings. Encoded-byte integrity and resource
  limits remain the reader's responsibility.
  """
  @spec prepare_decoded_stream(document(), value(), [diagnostic_option()]) ::
          {:ok, stream_context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_decoded_stream(document, value, opts \\ []) do
    with {:ok, stream_context} <- validate_stream(document, value, opts),
         {:ok, filter_value} <-
           resolve(document, Map.get(stream_context.dictionary, "Filter"), opts),
         {:ok, parameter_value} <-
           resolve(document, Map.get(stream_context.dictionary, "DecodeParms"), opts),
         {:ok, filters} <- filter_names(filter_value, opts),
         {:ok, parameters} <- filter_parameters(document, parameter_value, length(filters), opts),
         {:ok, filters} <- prepare_filters(Enum.zip(filters, parameters), opts) do
      {:ok, Map.put(stream_context, :filters, filters)}
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

  defp filter_names(value, opts) do
    names =
      case value do
        nil ->
          {:ok, []}

        {:name, filter} ->
          {:ok, [filter]}

        filters when is_list(filters) ->
          case Enum.all?(filters, &match?({:name, _}, &1)) do
            true -> {:ok, Enum.map(filters, fn {:name, filter} -> filter end)}
            false -> :error
          end

        _ ->
          :error
      end

    case names do
      {:ok, names} -> {:ok, names}
      :error -> error(:filter, :invalid_pdf_input, "Filter is malformed", opts)
    end
  end

  defp filter_parameters(document, parameters, count, opts) do
    case parameters do
      nil ->
        {:ok, List.duplicate(nil, count)}

      parameters when is_map(parameters) and count == 1 ->
        {:ok, [parameters]}

      parameters when is_list(parameters) and length(parameters) == count ->
        parameters
        |> Enum.reduce_while({:ok, []}, fn parameter, {:ok, resolved} ->
          case resolve(document, parameter, opts) do
            {:ok, parameter} when is_map(parameter) or is_nil(parameter) ->
              {:cont, {:ok, [parameter | resolved]}}

            _ ->
              {:halt, error(:filter, :invalid_pdf_input, "DecodeParms array is malformed", opts)}
          end
        end)
        |> case do
          {:ok, resolved} -> {:ok, Enum.reverse(resolved)}
          {:error, _} = parameter_error -> parameter_error
        end

      _ ->
        error(:filter, :invalid_pdf_input, "DecodeParms does not match Filter", opts)
    end
  end

  defp prepare_filters(filters, opts) do
    Enum.reduce_while(filters, {:ok, []}, fn {filter, parameters}, {:ok, prepared} ->
      canonical =
        case filter do
          filter when filter in ["FlateDecode", "Fl"] -> "FlateDecode"
          filter when filter in ["ASCIIHexDecode", "AHx"] -> "ASCIIHexDecode"
          filter when filter in ["ASCII85Decode", "A85"] -> "ASCII85Decode"
          filter when filter in ["RunLengthDecode", "RL"] -> "RunLengthDecode"
          filter when filter in ["LZWDecode", "LZW"] -> "LZWDecode"
          _ -> nil
        end

      case canonical do
        nil ->
          {:halt,
           error(
             :filter,
             :unsupported_pdf_feature,
             "unsupported PDF stream filter #{filter}",
             opts
           )}

        canonical ->
          parameters = parameters || %{}
          predictor = Map.get(parameters, "Predictor", 1)
          colors = Map.get(parameters, "Colors", 1)
          bits = Map.get(parameters, "BitsPerComponent", 8)
          columns = Map.get(parameters, "Columns", 1)
          early_change = Map.get(parameters, "EarlyChange", 1)

          cond do
            predictor not in [1, 2, 10, 11, 12, 13, 14, 15] ->
              {:halt,
               error(:filter, :unsupported_pdf_feature, "unsupported stream predictor", opts)}

            not is_integer(colors) or colors <= 0 or bits not in [1, 2, 4, 8, 16] or
              not is_integer(columns) or columns <= 0 ->
              {:halt,
               error(:filter, :invalid_pdf_input, "predictor dimensions are invalid", opts)}

            canonical == "LZWDecode" and early_change not in [0, 1] ->
              {:halt, error(:filter, :invalid_pdf_input, "LZW EarlyChange must be 0 or 1", opts)}

            true ->
              normalized = %{
                "Predictor" => predictor,
                "Colors" => colors,
                "BitsPerComponent" => bits,
                "Columns" => columns,
                "EarlyChange" => early_change
              }

              {:cont, {:ok, [%{name: canonical, parameters: normalized} | prepared]}}
          end
      end
    end)
    |> case do
      {:ok, prepared} -> {:ok, Enum.reverse(prepared)}
      {:error, _} = filter_error -> filter_error
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
    initial_ref = reference_identity(page_ref)

    with {:ok, ref, dictionary} <- referenced_dictionary(document, initial_ref, opts) do
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
          with {:ok, traversal} <-
                 validate_page_tree_parent(
                   document,
                   dictionary,
                   ref,
                   expected_parent,
                   traversal,
                   opts
                 ) do
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

  defp validate_page_tree_parent(
         document,
         dictionary,
         ref,
         expected_parent,
         traversal,
         opts
       ) do
    case expected_parent do
      nil ->
        case Map.get(dictionary, "Parent") do
          nil ->
            {:ok, traversal}

          _parent ->
            error(
              :page_tree,
              :invalid_pdf_input,
              "page tree root must not declare Parent",
              opts,
              object: ref
            )
        end

      expected_parent ->
        case Map.get(dictionary, "Parent") do
          {:ref, parent_ref} ->
            case resolve_reference(
                   document,
                   parent_ref,
                   traversal.reference_resolution,
                   %{},
                   0,
                   opts
                 ) do
              {:ok, _parent, ^expected_parent, resolution} ->
                {:ok, %{traversal | reference_resolution: resolution}}

              {:error, {:resource_limit_exceeded, _diagnostic}} = limit_error ->
                limit_error

              _ ->
                error(
                  :page_tree,
                  :invalid_pdf_input,
                  "page tree node Parent does not match its containing Pages node",
                  opts,
                  object: ref
                )
            end

          nil ->
            error(
              :page_tree,
              :invalid_pdf_input,
              "page tree node is missing its required Parent",
              opts,
              object: ref
            )

          _parent ->
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
      case Map.get(dictionary, key) do
        nil -> inherited
        value -> Map.put(inherited, key, %{value: value, source_ref: ref})
      end
    end)
  end

  defp valid_object_zero_entry?(entry, size) do
    case entry do
      {:free, next, 65_535} when is_integer(next) and next >= 0 and next < size -> true
      _ -> false
    end
  end

  defp valid_xref_entry?(entry, pdf, size) do
    case entry do
      {:free, next, generation}
      when is_integer(next) and is_integer(generation) ->
        next >= 0 and next < size and generation in 0..65_535

      {:uncompressed, offset, generation}
      when is_integer(offset) and is_integer(generation) ->
        offset >= 0 and offset < byte_size(pdf) and generation in 0..65_535

      {:compressed, object_stream, index}
      when is_integer(object_stream) and is_integer(index) ->
        object_stream > 0 and object_stream < size and index >= 0

      _ ->
        false
    end
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

  defp resolve_reference(document, ref, resolution, seen, depth, opts) do
    cond do
      not valid_ref?(ref) ->
        error(:resolution, :invalid_pdf_input, "indirect reference is malformed", opts)

      Map.has_key?(seen, ref) ->
        error(:resolution, :invalid_pdf_input, "indirect reference chain contains a cycle", opts,
          object: ref
        )

      depth >= @max_reference_chain_depth ->
        error(
          :limits,
          :resource_limit_exceeded,
          "indirect reference chain depth exceeds the limit",
          opts,
          object: ref
        )

      resolution.work >= @max_reference_resolution_work ->
        error(
          :limits,
          :resource_limit_exceeded,
          "indirect reference resolution work exceeds the limit",
          opts,
          object: ref
        )

      true ->
        resolution = %{resolution | work: resolution.work + 1}

        case Map.fetch(resolution.cache, ref) do
          {:ok, {value, terminal_ref}} ->
            {:ok, value, terminal_ref, resolution}

          :error ->
            case fetch_object_record(document, ref) do
              {:ok, %{value: {:ref, next_ref}}} ->
                case resolve_reference(
                       document,
                       next_ref,
                       resolution,
                       Map.put(seen, ref, true),
                       depth + 1,
                       opts
                     ) do
                  {:ok, value, terminal_ref, resolution} ->
                    resolution = %{
                      resolution
                      | cache: Map.put(resolution.cache, ref, {value, terminal_ref})
                    }

                    {:ok, value, terminal_ref, resolution}

                  {:error, _} = resolution_error ->
                    resolution_error
                end

              {:ok, %{value: value}} ->
                resolution = %{
                  resolution
                  | cache: Map.put(resolution.cache, ref, {value, ref})
                }

                {:ok, value, ref, resolution}

              {:ok, _object} ->
                error(
                  :resolution,
                  :invalid_pdf_input,
                  "indirect object record is malformed",
                  opts,
                  object: ref
                )

              :missing ->
                error(
                  :resolution,
                  :invalid_pdf_input,
                  "indirect object reference is missing",
                  opts,
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
  end

  defp referenced_dictionary(document, ref, opts) do
    with {:ok, resolved, terminal_ref, _resolution} <-
           resolve_reference(document, ref, %{cache: %{}, work: 0}, %{}, 0, opts),
         true <- is_map(resolved) do
      {:ok, terminal_ref, resolved}
    else
      false -> error(:resolution, :invalid_pdf_input, "expected a PDF dictionary", opts)
      {:error, _} = resolution_error -> resolution_error
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
