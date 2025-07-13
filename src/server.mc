-- # Simple Local Doc Server
--
-- This file defines a small utility to preview the generated documentation.
-- 
-- It embeds a Python script (`pythonScript`) which:
-- - starts a local HTTP server on port 3000
-- - serves the generated outputs as HTML (using Python’s `markdown` module if output is markdown)
-- - auto-opens the browser
-- 
-- ## How it works:
-- 
-- - `startServer` writes the Python script to a temporary file
-- - launches it with:
--   ```bash
--   python3 script.py <output-folder dir> <initial object>
--   ```

include "extracting/objects.mc"
include "rendering/renderer.mc"

let pythonScript = use Renderer in lam fmt. concatAll ["
import os
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import unquote
import mimetypes
import webbrowser
import threading
",
match fmt with Md {} then
"import markdown"
else
"", "
import sys

DOC_DIR = sys.argv[1]

class Handler(BaseHTTPRequestHandler):
    def do_GET(self):
        path = unquote(self.path.lstrip('/'))
        
        file_path = os.path.join(DOC_DIR, path)
        
        if not os.path.isfile(file_path):
            self.send_response(404)
            self.send_header('Content-type', 'text/plain; charset=utf-8')
            self.end_headers()
            self.wfile.write(f\"File not found: {file_path}\".encode('utf-8'))
            return

        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()

        html_output = ", match fmt with Md {} then " markdown.markdown(content)" else "content", "

        full_html = ", match fmt with Md {} then "f\"\"\"<!DOCTYPE html>
<html>
    <head>
        <meta charset=\"utf-8\">
        <title>{os.path.basename(file_path)}</title>
        <style>
            body {{ font-family: sans-serif; max-width: 800px; margin: auto; padding: 2em; }}
            h1, h2, h3 {{ color: #444; }}
            pre, code {{ background: #f4f4f4; padding: 0.2em 0.4em; }}
        </style>
    </head>
    <body>{html_output}</body>
</html>\"\"\"" else "html_output", "

        self.send_response(200)
        self.send_header('Content-type', 'text/html; charset=utf-8')
        self.end_headers()
        self.wfile.write(full_html.encode('utf-8'))

    def serve_static_file(self, file_path):
        if not os.path.isfile(file_path):
            self.send_response(404)
            self.send_header('Content-type', 'text/plain; charset=utf-8')
            self.end_headers()
            self.wfile.write(f\"File not found: {file_path}\".encode('utf-8'))
            return
        
        content_type, _ = mimetypes.guess_type(file_path)
        content_type = content_type or 'application/octet-stream'

        with open(file_path, 'rb') as f:
            content = f.read()

        self.send_response(200)
        self.send_header('Content-type', content_type)
        self.end_headers()
        self.wfile.write(content)


server_address = ('127.0.0.1', 3000)
httpd = HTTPServer(server_address, Handler)
print(\"Server started on http://127.0.0.1:3000\")
def open_url():
    webbrowser.open('127.0.0.1:3000/' + sys.argv[2])

t = threading.Thread(target=open_url)
t.start()

try:
    httpd.serve_forever()
except KeyboardInterrupt:
    pass
finally:
    httpd.server_close()
"
]    
let startServer = lam obj.
    match obj with ObjectNode { obj = obj } then
    if opt.noOpen then () else
    let file = sysTempFileMake () in
    match fileWriteOpen file with Some wc then
        let write = fileWriteString wc in
        write (pythonScript opt.fmt);
        fileWriteFlush wc;
        fileWriteClose wc;
        let path = concatAll [sysGetCwd (), "/", opt.outputFolder] in
        let res = sysRunCommand ["python3", file, path, objLink obj] "" "/" in ()
        
    else error "Failed to open temporary file. The browser failed to start but the files have been generated."
    else error "Extraction failed: `extract` should always return a tree with a Program root."
