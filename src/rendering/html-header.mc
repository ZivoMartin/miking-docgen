let getHeader = lam title.
    concatAll [
"<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>", title, "</title>
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<style>
    body {
        font-family: system-ui, sans-serif;
        max-width: 900px;
        margin: auto;
        padding: 2em;
        line-height: 1.6;
        background-color: #fdfdfd;
        color: #333;
    }

    h1 {
        color: #2c3e50;
        border-bottom: 2px solid #eee;
        padding-bottom: 0.3em;
    }

    h2 {
        color: #2c3e50;
        font-size: 1.1em;
        margin-top: 3em;
        border-bottom: 1px solid #eee;
        padding-bottom: 0.3em;
    }

    a {
        color: #2980b9;
        text-decoration: none;
    }
    
    a:hover {
        text-decoration: underline;
    }

    pre.code {
        font-family: Consolas, \"Liberation Mono\", Menlo, Courier, monospace;
        background: #f5f5f5;
        border-left: 4px solid #2980b9;
        padding: 1em;
        overflow-x: auto;
        border-radius: 5px;
        font-size: 0.85em; 
        line-height: 1.4;
        margin-bottom: 2em;
    }

    pre a:hover {
        text-decoration: underline;
    }

    pre span {
        font-family: inherit;
        font-size: inherit;
    }

    pre.source {
        text-color: #24292e;
    }   
        
    pre.md {
        font-family: system-ui, sans-serif;
        font-size: 0.95em;
        line-height: 1.6;
        white-space: pre-wrap;
    }

    nav {
        background: #fafafa;
        padding: 0.8em 1em;
        border-bottom: 1px solid #ddd;
        margin-bottom: 2em;
        font-size: 0.9em;
        color: #999;
    }
    
    .inline-container {
      display: flex;
    }
    
    nav a {
        margin-right: 1em;
        color: #999;
        text-decoration: none;
        cursor: default; /* pas la main cliqueuse */
    }

    nav a:hover {
        text-decoration: none;
    }

    .toggle-btn {
        background: none;
        border: none;
        color: #888;
        font-family: monospace;
        font-size: 1em;
        cursor: pointer;
        padding: 0;
        margin: 0 4px;
        transition: color 0.2s;
    }
    
    .toggle-btn:hover {
        color: #444;
    }

    .gotoLink {
        position: absolute;
        bottom: 0.4em;
        right: 0.8em;
        font-size: 0.9em;
        color: #2980b9;
        text-decoration: none;
    }   
    
    .arg { color: #24292e; font-style: italic; }
    .kw  { color: #d73a49; }   
    .var { color: #005cc5; }                      
    .default { color: #24292e; font-style: italic; }
    .tp { color: #6f42c1; }    
    .comment { color: #22863a; }
    .string { color: #b36f00; }    


</style>
</head>
<body>
<nav>
    <a href=\"/\">Home (todo)</a>
    <a href=\"/Lang/\">Modules (todo)</a>
</nav>
"]
