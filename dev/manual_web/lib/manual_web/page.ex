defmodule ManualWeb.Page do
  @moduledoc """
  Builds the small, dependency-free HTML interface used by the manual app.
  """

  @doc "Returns the manual testing home page."
  @spec index() :: binary()
  def index do
    document(
      "Native Elixir PDF Utilities manual checks",
      ~S"""
      <header>
        <img class="brand-banner" src="/brand-banner.svg" alt="Native Elixir PDF Utilities">
      </header>

      <main>
        <section>
          <h2>Merge PDFs</h2>
          <p>Choose two or more PDFs. The merged file opens in a new tab for a page-by-page check.</p>
          <form action="/merge" method="post" enctype="multipart/form-data" target="_blank">
            <label>PDF files <input type="file" name="pdfs[]" accept="application/pdf,.pdf" multiple required></label>
            <label>Response
              <select name="disposition">
                <option value="inline">Open in browser</option>
                <option value="attachment">Download</option>
              </select>
            </label>
            <button type="submit">Merge and open</button>
          </form>
        </section>

        <section>
          <h2>HTML to PDF</h2>
          <p>Paste HTML or choose a file. Keep CSS inline and embed images or fonts as data URIs.</p>
          <form action="/html-to-pdf" method="post" enctype="multipart/form-data" target="_blank">
            <label>HTML file <input type="file" name="html_file" accept="text/html,.html,.htm"></label>
            <label>Or paste HTML <textarea name="html" rows="10" spellcheck="false"><!doctype html>
      <html>
      <head><title>Manual render</title></head>
      <body><h1>Hello from Elixir</h1><p>Check this PDF visually.</p></body>
      </html></textarea></label>
            <div class="fields">
              <label>Page size
                <select name="page_size">
                  <option value="a4">A4</option>
                  <option value="a5">A5</option>
                  <option value="a3">A3</option>
                  <option value="b5">B5</option>
                  <option value="b4">B4</option>
                  <option value="jis_b5">JIS B5</option>
                  <option value="jis_b4">JIS B4</option>
                  <option value="letter">Letter</option>
                  <option value="legal">Legal</option>
                  <option value="ledger">Ledger</option>
                </select>
              </label>
              <label>Orientation
                <select name="orientation">
                  <option value="portrait">Portrait</option>
                  <option value="landscape">Landscape</option>
                </select>
              </label>
              <label>Margin <input type="text" name="margin" value="20mm" placeholder="20mm or 36"></label>
              <label>Missing glyphs
                <select name="unsupported_glyphs">
                  <option value="replace">Replace visibly</option>
                  <option value="error">Return an error</option>
                </select>
              </label>
              <label>Response
                <select name="disposition">
                  <option value="inline">Open in browser</option>
                  <option value="attachment">Download</option>
                </select>
              </label>
            </div>
            <button type="submit">Render and open</button>
          </form>
        </section>

        <section>
          <h2>Extract text</h2>
          <p>Text modes return copyable content. Span modes include page coordinates and reading order as JSON.</p>
          <form action="/text" method="post" enctype="multipart/form-data">
            <label>PDF file <input type="file" name="pdf" accept="application/pdf,.pdf" required></label>
            <label>Mode
              <select name="mode">
                <option value="text_layout">Text with visual line grouping</option>
                <option value="text_source">Text in content-stream order</option>
                <option value="spans_source">Positioned spans in source order</option>
                <option value="spans_visual">Positioned spans in visual order</option>
              </select>
            </label>
            <button type="submit">Extract</button>
          </form>
        </section>

        <section>
          <h2>Inspect PDF information</h2>
          <p>Read the PDF Info dictionary, encryption flag, page count, MediaBox, page size, and rotation.</p>
          <form action="/info" method="post" enctype="multipart/form-data">
            <label>PDF file <input type="file" name="pdf" accept="application/pdf,.pdf" required></label>
            <button type="submit">Inspect</button>
          </form>
        </section>

        <section>
          <h2>Update PDF information</h2>
          <p>Empty fields keep their current value. Remove deletes a field. Dates accept ISO 8601 or PDF date syntax.</p>
          <form action="/info/update" method="post" enctype="multipart/form-data" target="_blank">
            <label>PDF file <input type="file" name="pdf" accept="application/pdf,.pdf" required></label>
            <div class="metadata">
              <label>Title <input type="text" name="title"><span><input type="checkbox" name="remove_title"> Remove</span></label>
              <label>Author <input type="text" name="author"><span><input type="checkbox" name="remove_author"> Remove</span></label>
              <label>Subject <input type="text" name="subject"><span><input type="checkbox" name="remove_subject"> Remove</span></label>
              <label>Keywords <input type="text" name="keywords"><span><input type="checkbox" name="remove_keywords"> Remove</span></label>
              <label>Producer <input type="text" name="producer"><span><input type="checkbox" name="remove_producer"> Remove</span></label>
              <label>Creation date <input type="text" name="creation_date" placeholder="2026-08-25T14:30:00"><span><input type="checkbox" name="remove_creation_date"> Remove</span></label>
              <label>Modification date <input type="text" name="modification_date" placeholder="D:20260825143000+08'00'"><span><input type="checkbox" name="remove_modification_date"> Remove</span></label>
            </div>
            <label>Response
              <select name="disposition">
                <option value="inline">Open in browser</option>
                <option value="attachment">Download</option>
              </select>
            </label>
            <button type="submit">Apply metadata</button>
          </form>
        </section>

        <section>
          <h2>Inspect tokenizer output</h2>
          <p>Choose a PDF or paste a small fragment. Each row shows the token and its source byte offsets.</p>
          <form action="/tokenize" method="post" enctype="multipart/form-data">
            <label>PDF file <input type="file" name="pdf" accept="application/pdf,.pdf"></label>
            <label>Or paste PDF syntax <textarea name="source" rows="8" spellcheck="false">&lt;&lt; /Type /Example /Count 2 /Enabled true &gt;&gt;</textarea></label>
            <button type="submit">Show tokens</button>
          </form>
        </section>
      </main>

      <footer>
        <a href="/openapi.json">OpenAPI description</a>
      </footer>
      """
    )
  end

  @doc "Returns a result page containing escaped plain text."
  @spec text_result(String.t(), String.t()) :: binary()
  def text_result(title, text) do
    result(title, ["<pre>", escape(text), "</pre>"])
  end

  @doc "Returns a result page containing pretty-printed JSON."
  @spec json_result(String.t(), term()) :: binary()
  def json_result(title, value) do
    json = Jason.encode_to_iodata!(value, pretty: true)
    result(title, ["<pre>", escape(json), "</pre>"])
  end

  @doc "Returns a result page containing an escaped Elixir term."
  @spec term_result(String.t(), term()) :: binary()
  def term_result(title, value) do
    rendered = inspect(value, pretty: true, limit: :infinity, printable_limit: :infinity)
    result(title, ["<pre>", escape(rendered), "</pre>"])
  end

  @doc "Returns an error page containing the complete structured failure."
  @spec error_result(term()) :: binary()
  def error_result(error) do
    rendered = inspect(error, pretty: true, limit: :infinity, printable_limit: :infinity)

    result(
      "Operation failed",
      [
        "<div class=\"error\"><h2>Operation failed</h2>",
        "<p>Here is the exact error returned by the library or form validator.</p><pre>",
        escape(rendered),
        "</pre></div>"
      ]
    )
  end

  @doc "Wraps trusted page content in the shared HTML document."
  @spec document(String.t(), iodata()) :: binary()
  def document(title, body) do
    IO.iodata_to_binary([
      "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">",
      "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
      "<link rel=\"icon\" href=\"/favicon.ico\" sizes=\"any\">",
      "<title>",
      escape(title),
      "</title>",
      style(),
      "</head><body>",
      body,
      "</body></html>"
    ])
  end

  defp result(title, content) do
    document(title, [
      "<main class=\"result\"><p><a href=\"/\">Back to manual checks</a></p><h1>",
      escape(title),
      "</h1>",
      content,
      "</main>"
    ])
  end

  defp escape(value) do
    value
    |> IO.iodata_to_binary()
    |> Plug.HTML.html_escape()
    |> IO.iodata_to_binary()
  end

  defp style do
    ~S"""
    <style>
      :root { color-scheme: dark; font-family: Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; background: #050a14; color: #e7eef9; accent-color: #4f8cff; }
      * { box-sizing: border-box; }
      body { min-height: 100vh; margin: 0; background: radial-gradient(circle at 12% -8%, rgb(37 99 235 / 22%), transparent 34rem), radial-gradient(circle at 90% 12%, rgb(14 165 233 / 10%), transparent 28rem), #050a14; }
      header, main, footer { width: min(1120px, calc(100% - 40px)); margin: 0 auto; }
      header { padding: 42px 0 30px; border-bottom: 1px solid rgb(88 123 170 / 18%); }
      .brand-banner { display: block; width: 100%; height: auto; border: 1px solid #284665; border-radius: 24px; box-shadow: 0 24px 70px rgb(0 0 0 / 35%); }
      main:not(.result) { display: grid; grid-template-columns: repeat(auto-fit, minmax(min(100%, 500px), 1fr)); gap: 18px; padding-top: 30px; }
      section { background: linear-gradient(180deg, rgb(14 29 51 / 94%), rgb(9 20 37 / 94%)); border: 1px solid #1c3555; border-radius: 16px; padding: 26px; box-shadow: 0 18px 48px rgb(0 0 0 / 24%); }
      section:hover { border-color: #2d527c; }
      section h2 { margin: 0 0 9px; color: #f3f7fd; font-size: 1.22rem; font-weight: 720; letter-spacing: -.018em; }
      section p { margin: 0; color: #91a4bd; line-height: 1.58; }
      form { display: grid; gap: 15px; margin-top: 22px; }
      label { display: grid; gap: 7px; color: #c3d0e1; font-size: .84rem; font-weight: 680; }
      input, textarea, select, button { font: inherit; }
      input[type="text"], input[type="file"], textarea, select { width: 100%; border: 1px solid #284665; border-radius: 9px; outline: none; background: #071425; padding: 11px 12px; color: #e7eef9; box-shadow: inset 0 1px 0 rgb(255 255 255 / 3%); }
      input[type="text"]::placeholder, textarea::placeholder { color: #617791; }
      input[type="file"]::file-selector-button { margin: -7px 12px -7px -8px; border: 0; border-right: 1px solid #284665; background: #112945; padding: 8px 12px; color: #cfe0f7; font-weight: 700; cursor: pointer; }
      input[type="text"]:focus, input[type="file"]:focus, textarea:focus, select:focus { border-color: #5794ee; box-shadow: 0 0 0 3px rgb(59 130 246 / 16%); }
      textarea { resize: vertical; font-family: "SFMono-Regular", Consolas, "Liberation Mono", monospace; line-height: 1.5; }
      button { justify-self: start; border: 1px solid #5794ee; border-radius: 9px; background: linear-gradient(180deg, #377fe4, #2563c2); padding: 11px 17px; color: white; font-weight: 760; cursor: pointer; box-shadow: 0 8px 22px rgb(37 99 235 / 24%); }
      button:hover { background: linear-gradient(180deg, #4b91ef, #2f70d2); }
      button:focus-visible, a:focus-visible { outline: 3px solid rgb(96 165 250 / 45%); outline-offset: 3px; }
      .fields { display: grid; grid-template-columns: repeat(auto-fit, minmax(140px, 1fr)); gap: 10px; }
      .metadata { display: grid; gap: 10px; }
      .metadata label { grid-template-columns: minmax(120px, .6fr) 1fr auto; align-items: center; }
      .metadata label > span { display: flex; align-items: center; gap: 5px; color: #94a8c0; font-weight: 520; white-space: nowrap; }
      footer { padding: 34px 0 58px; }
      a { color: #72a7ff; text-underline-offset: 3px; }
      code, pre { font-family: ui-monospace, SFMono-Regular, Consolas, monospace; }
      code { border: 1px solid #1e3a5a; border-radius: 6px; background: #09182a; padding: 2px 6px; color: #cfe3ff; }
      pre { overflow: auto; padding: 20px; border: 1px solid #1c3555; border-radius: 12px; background: #07111f; color: #d6e3f5; line-height: 1.5; white-space: pre-wrap; word-break: break-word; box-shadow: 0 16px 44px rgb(0 0 0 / 22%); }
      .result { padding: 48px 0; }
      .result h1 { font-size: clamp(2rem, 5vw, 3.8rem); letter-spacing: -.045em; }
      .error { color: #ffb4b4; }
      @media (max-width: 650px) { header, main, footer { width: min(100% - 24px, 1120px); } header { padding-top: 24px; } .brand-banner { border-radius: 14px; } section { padding: 20px; } .metadata label { grid-template-columns: 1fr; } }
    </style>
    """
  end
end
