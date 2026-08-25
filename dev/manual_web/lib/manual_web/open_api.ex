defmodule ManualWeb.OpenApi do
  @moduledoc """
  OpenAPI description for the local manual-check endpoints.
  """

  @doc "Returns the OpenAPI 3.1 document for the manual app."
  @spec document() :: map()
  def document do
    %{
      "openapi" => "3.1.0",
      "info" => %{
        "title" => "Native Elixir PDF Utilities manual checks",
        "version" => "0.1.0",
        "description" => "Local-only endpoints for manually checking the library."
      },
      "servers" => [%{"url" => "http://127.0.0.1:4001"}],
      "paths" => %{
        "/" => %{
          "get" => response_operation("Open the manual testing interface", "text/html")
        },
        "/brand-banner.svg" => %{
          "get" => response_operation("Read the README brand banner", "image/svg+xml")
        },
        "/favicon.ico" => %{
          "get" => response_operation("Read the application icon", "image/x-icon")
        },
        "/openapi.json" => %{
          "get" => response_operation("Read this OpenAPI document", "application/json")
        },
        "/merge" => %{
          "post" =>
            upload_operation(
              "Merge two or more uploaded PDFs",
              %{
                "pdfs" => %{
                  "type" => "array",
                  "minItems" => 2,
                  "items" => %{"type" => "string", "format" => "binary"}
                },
                "disposition" => disposition_schema()
              },
              ["pdfs"],
              "application/pdf"
            )
        },
        "/html-to-pdf" => %{
          "post" =>
            upload_operation(
              "Render uploaded or pasted HTML as PDF",
              %{
                "html_file" => %{"type" => "string", "format" => "binary"},
                "html" => %{"type" => "string"},
                "page_size" => %{"type" => "string"},
                "orientation" => %{"type" => "string", "enum" => ["portrait", "landscape"]},
                "margin" => %{"type" => "string"},
                "unsupported_glyphs" => %{"type" => "string", "enum" => ["replace", "error"]},
                "disposition" => disposition_schema()
              },
              ["page_size", "orientation"],
              "application/pdf"
            )
        },
        "/text" => %{
          "post" =>
            upload_operation(
              "Extract plain text or positioned spans from a PDF",
              %{
                "pdf" => %{"type" => "string", "format" => "binary"},
                "mode" => %{
                  "type" => "string",
                  "enum" => ["text_layout", "text_source", "spans_source", "spans_visual"]
                }
              },
              ["pdf", "mode"],
              "text/html"
            )
        },
        "/info" => %{
          "post" =>
            upload_operation(
              "Inspect PDF metadata, encryption, page count, and page sizes",
              %{"pdf" => %{"type" => "string", "format" => "binary"}},
              ["pdf"],
              "text/html"
            )
        },
        "/info/update" => %{
          "post" =>
            upload_operation(
              "Apply a metadata patch and return the updated PDF",
              info_update_properties(),
              ["pdf"],
              "application/pdf"
            )
        },
        "/tokenize" => %{
          "post" =>
            upload_operation(
              "Tokenize an uploaded PDF or pasted PDF syntax",
              %{
                "pdf" => %{"type" => "string", "format" => "binary"},
                "source" => %{"type" => "string"}
              },
              [],
              "text/html"
            )
        }
      }
    }
  end

  defp response_operation(summary, content_type) do
    %{
      "summary" => summary,
      "responses" => %{
        "200" => %{
          "description" => "Successful response",
          "content" => %{content_type => %{"schema" => %{"type" => "string"}}}
        }
      }
    }
  end

  defp upload_operation(summary, properties, required, response_type) do
    response_operation(summary, response_type)
    |> Map.put("requestBody", %{
      "required" => true,
      "content" => %{
        "multipart/form-data" => %{
          "schema" => %{
            "type" => "object",
            "properties" => properties,
            "required" => required
          }
        }
      }
    })
  end

  defp disposition_schema do
    %{"type" => "string", "enum" => ["inline", "attachment"], "default" => "inline"}
  end

  defp info_update_properties do
    text_fields =
      for field <- ~w(title author subject keywords producer creation_date modification_date),
          into: %{} do
        {field, %{"type" => "string"}}
      end

    removal_fields =
      for field <- ~w(title author subject keywords producer creation_date modification_date),
          into: %{} do
        {"remove_#{field}", %{"type" => "string", "enum" => ["on"]}}
      end

    text_fields
    |> Map.merge(removal_fields)
    |> Map.put("pdf", %{"type" => "string", "format" => "binary"})
    |> Map.put("disposition", disposition_schema())
  end
end
