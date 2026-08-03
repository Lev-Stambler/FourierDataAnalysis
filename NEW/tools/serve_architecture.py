"""Serve ARCHITECTURE.md with live Markdown, Mermaid, and MathJax updates."""

from __future__ import annotations

import argparse
import hashlib
import json
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import urlsplit


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_DOCUMENT = ROOT / "ARCHITECTURE.md"

VIEWER = r"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Live Kronecker Architecture</title>
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css">
  <style>
    :root { color-scheme: light; --ink:#171717; --muted:#666; --line:#d8d8d8;
      --accent:#0755a5; --good:#267326; }
    * { box-sizing: border-box; }
    body { margin:0; background:#fff; color:var(--ink); font:16px/1.55
      ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif; }
    header { position:sticky; top:0; z-index:5; display:flex; gap:1rem;
      align-items:center; padding:.65rem max(1rem,calc((100% - 880px)/2));
      background:#fff; border-bottom:1px solid var(--line); }
    #status { margin-left:auto; color:var(--good); font-size:.88rem; }
    header a { color:var(--accent); }
    main { width:min(880px,calc(100% - 2rem)); margin:2.4rem auto 6rem; }
    h1,h2,h3 { line-height:1.22; scroll-margin-top:4rem; }
    h1 { font-size:2rem; margin-bottom:1.5rem; }
    h2 { margin-top:2.4rem; border-bottom:1px solid var(--line); padding-bottom:.3rem; }
    h3 { margin-top:1.7rem; }
    a { color:var(--accent); }
    blockquote { margin-left:0; padding:.4rem 1rem; border-left:3px solid #888;
      color:var(--muted); }
    code { background:#f3f3f3; padding:.1rem .28rem; }
    pre { overflow:auto; padding:1rem; background:#f7f7f7; border:1px solid var(--line); }
    pre code { padding:0; }
    table { width:100%; border-collapse:collapse; display:block; overflow:auto; }
    th,td { border-bottom:1px solid var(--line); padding:.45rem .65rem; text-align:left; }
    th { background:#f5f5f5; }
    .mermaid { margin:1.5rem 0; padding:1.25rem; background:#fff;
      border:1px solid var(--line); overflow:auto; text-align:center; }
    .mermaid foreignObject { overflow:visible; }
    #error { display:none; color:#a40000; white-space:pre-wrap; }
    @media (max-width:720px) { main { width:calc(100% - 1.5rem); }
      header { flex-wrap:wrap; } #status { width:100%; margin-left:0; } }
  </style>
  <script>
    window.MathJax = {
      tex: { inlineMath: [['$', '$'], ['\\(', '\\)']],
             displayMath: [['$$', '$$'], ['\\[', '\\]']] },
      startup: { typeset: false }
    };
  </script>
  <script defer src="https://cdn.jsdelivr.net/npm/marked@15.0.12/marked.min.js"></script>
  <script defer src="https://cdn.jsdelivr.net/npm/mermaid@11.4.1/dist/mermaid.min.js"></script>
  <script defer src="https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-mml-chtml.js"></script>
</head>
<body>
  <header><strong>Live architecture notebook</strong>
    <a href="/architecture.md" target="_blank">raw Markdown</a>
    <span id="status">connecting…</span></header>
  <main><div id="error"></div><article id="document">Loading…</article></main>
  <script>
    let previous = null;
    let rendering = false;
    function renderMarkdownWithoutTouchingMath(markdown) {
      const math = [];
      const protectedMarkdown = markdown.replace(
        /\$\$[\s\S]*?\$\$|\$(?!\$)(?:\\.|[^$\n])+\$/g,
        (source) => {
          const token = 'ARCHMATHPLACEHOLDER' + math.length + 'END';
          math.push(source);
          return token;
        }
      );
      let html = marked.parse(protectedMarkdown);
      return html.replace(/ARCHMATHPLACEHOLDER(\d+)END/g, (_, index) =>
        math[Number(index)]
          .replaceAll('&', '&amp;')
          .replaceAll('<', '&lt;')
          .replaceAll('>', '&gt;')
      );
    }
    async function renderIfChanged() {
      if (rendering || !window.marked || !window.mermaid || !window.MathJax) return;
      try {
        const response = await fetch('/architecture.md?now=' + Date.now(), {cache:'no-store'});
        if (!response.ok) throw new Error('HTTP ' + response.status);
        const markdown = await response.text();
        if (markdown === previous) return;
        rendering = true;
        previous = markdown;
        const article = document.getElementById('document');
        MathJax.typesetClear([article]);
        article.innerHTML = renderMarkdownWithoutTouchingMath(markdown);
        article.querySelectorAll('pre code.language-mermaid').forEach((code) => {
          const replacement = document.createElement('div');
          // Mermaid owns math inside the diagram. Prevent page-level MathJax
          // from re-typesetting KaTeX's hidden accessibility MathML.
          replacement.className = 'mermaid tex2jax_ignore';
          replacement.textContent = code.textContent;
          code.parentElement.replaceWith(replacement);
        });
        mermaid.initialize({
          startOnLoad:false,
          securityLevel:'strict',
          theme:'base',
          htmlLabels:true,
          forceLegacyMathML:true,
          flowchart:{curve:'linear', nodeSpacing:35, rankSpacing:42, wrappingWidth:480},
          themeVariables:{
            background:'#ffffff', primaryColor:'#ffffff', primaryTextColor:'#171717',
            primaryBorderColor:'#555555', lineColor:'#555555',
            secondaryColor:'#f5f5f5', tertiaryColor:'#ffffff', fontSize:'16px'
          }
        });
        await mermaid.run({nodes: article.querySelectorAll('.mermaid')});
        const prose = Array.from(article.children).filter(
          (node) => !node.classList.contains('mermaid')
        );
        await MathJax.typesetPromise(prose);
        document.getElementById('error').style.display = 'none';
        const stamp = response.headers.get('Last-Modified') || new Date().toLocaleTimeString();
        const sha = (response.headers.get('X-Architecture-SHA256') || '').slice(0, 12);
        document.getElementById('status').textContent = 'live · ' + stamp + (sha ? ' · ' + sha : '');
      } catch (error) {
        const node = document.getElementById('error');
        node.style.display = 'block'; node.textContent = 'Live render error: ' + error;
        document.getElementById('status').textContent = 'render error';
      } finally { rendering = false; }
    }
    window.addEventListener('load', () => { renderIfChanged(); setInterval(renderIfChanged, 1000); });
  </script>
</body>
</html>
"""


class ArchitectureHandler(BaseHTTPRequestHandler):
    document = DEFAULT_DOCUMENT

    def _send(
        self,
        body: bytes,
        content_type: str,
        *,
        extra: dict[str, str] | None = None,
        include_body: bool = True,
    ) -> None:
        self.send_response(HTTPStatus.OK)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store, max-age=0")
        for key, value in (extra or {}).items():
            self.send_header(key, value)
        self.end_headers()
        if include_body:
            self.wfile.write(body)

    def _serve(self, *, include_body: bool) -> None:
        route = urlsplit(self.path).path
        if route in ("/", "/index.html"):
            self._send(
                VIEWER.encode(), "text/html; charset=utf-8", include_body=include_body
            )
            return
        if route == "/architecture.md":
            try:
                body = self.document.read_bytes()
                stat = self.document.stat()
            except OSError as error:
                self.send_error(HTTPStatus.NOT_FOUND, str(error))
                return
            digest = hashlib.sha256(body).hexdigest()
            self._send(
                body,
                "text/markdown; charset=utf-8",
                extra={
                    "Last-Modified": self.date_time_string(stat.st_mtime),
                    "ETag": f'"{digest}"',
                    "X-Architecture-SHA256": digest,
                },
                include_body=include_body,
            )
            return
        if route == "/healthz":
            try:
                body = self.document.read_bytes()
                value = {
                    "status": "ok",
                    "document": str(self.document),
                    "sha256": hashlib.sha256(body).hexdigest(),
                    "bytes": len(body),
                }
                self._send(
                    json.dumps(value).encode(),
                    "application/json",
                    include_body=include_body,
                )
            except OSError as error:
                self.send_error(HTTPStatus.SERVICE_UNAVAILABLE, str(error))
            return
        self.send_error(HTTPStatus.NOT_FOUND)

    def do_GET(self) -> None:  # noqa: N802 - BaseHTTPRequestHandler API
        self._serve(include_body=True)

    def do_HEAD(self) -> None:  # noqa: N802 - BaseHTTPRequestHandler API
        self._serve(include_body=False)

    def log_message(self, format: str, *args: object) -> None:
        print(f"[architecture-live] {self.address_string()} {format % args}", flush=True)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8765)
    parser.add_argument("--document", type=Path, default=DEFAULT_DOCUMENT)
    args = parser.parse_args()
    document = args.document.resolve()
    if not document.is_file():
        raise FileNotFoundError(document)
    ArchitectureHandler.document = document
    server = ThreadingHTTPServer((args.host, args.port), ArchitectureHandler)
    print(f"Live architecture: http://{args.host}:{args.port}/", flush=True)
    print(f"Watching: {document}", flush=True)
    try:
        server.serve_forever(poll_interval=0.25)
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()


if __name__ == "__main__":
    main()
