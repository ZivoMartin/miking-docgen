let getHeader = lam title.
    join [
"<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>", title, "</title>
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<style>

    body {
        font-family: system-ui, sans-serif;
        width: 100%;
        margin: 0;
        display: flex;
        justify-content: center;
        background-color: #fdfdfd;
        color: #333;
    }

    .main-container {
        width: 100%;
        max-width: 1200px; /* ou 1000/1100 selon ton goût */
        padding: 0 2em;
        box-sizing: border-box;
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
        font-size: 1em; 
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
        font-size: 1em;
        text-color: #24292e;
    }   
        
    pre.md {
        font-family: system-ui, sans-serif;
        font-size: 1em;
        line-height: 1.6;
        white-space: pre-wrap;
    }

    nav {
        background: #fafafa;
        padding: 0.8em 1em;
        border-bottom: 1px solid #ddd;
        margin-bottom: 1em;
        font-size: 0.9em;
        color: #999;
    }

    .ObjectParent {
        border-bottom: 1px solid #eee;
        position: relative;
        padding-top: 1em;
        padding-bottom: 2em;
    }
        
    .inline-container {
        display: block;
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
        font-size: 0.8em;
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
        font-size: 1em;
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
    .weak { color: #b34f00 }
    .number { color: #0366d6; }

</style>
</head>
<body>
<div class=\"main-container\">    
<script>
function toggle(btn) {
    const scrollY = window.scrollY;
    const div = btn.nextElementSibling; 
    if (div.style.display === 'none') {
        div.style.display = 'inline';
    } else {
        div.style.display = 'none';
    }
    window.scrollTo({ top: scrollY }); // Preventing the screen to move
}
</script>
    
<nav>
    <a href=\"/\">Home (todo)</a>
    <a href=\"/Lang/\">Modules (todo)</a>
</nav>
"]
