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
        
    }

    .main-container {
        margin: 0 auto;
        width: 100%;
        max-width: 1200px;
        padding: 0 2em;
        box-sizing: border-box;
    }

    h1 {
        font-size: 2em;
        margin-top: 1em;
        color: #2c3e50;
        border-bottom: 2px solid #eee;
        padding-bottom: 0.3em;
    }

    h2 {
        font-size: 1.5em;
        margin-top: 2em;
        padding-bottom: 0.2em;
        border-left: 4px solid #2980b9;
        padding-left: 0.5em;
        color: #2c3e50;
    }

    h1 {
        color: #2c3e50;
        border-bottom: 2px solid #eee;
        padding-bottom: 0.3em;
    }


    a {
        color: #2980b9;
        text-decoration: none;
    }
    
    a:hover {
        text-decoration: underline;
    }

    .doc-signature {
        background: #f6f8fa;
        border-left: 4px solid #3b82f6;
        padding: 0.8em 1em;
        font-family: 'JetBrains Mono', monospace;
        font-size: 0.95em;
        color: #111827;
        border-radius: 6px;
        overflow-x: auto;
    }

    .doc-block {
        border: 1px solid #e5e7eb;
        border-radius: 8px;
        margin: 1.5em 0;
        background: #fefefe;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04);
        padding: 0;
        overflow: hidden;
        font-family: system-ui, sans-serif;
    }
        
    .inline-container {
        display: block;
    }

    nav a:hover {
        text-decoration: none;
    }

    .toggle-btn {
        color: #666;
        background: #f0f0f0;
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 0 0.5em;
        font-size: 0.8em;
        cursor: pointer;
        transition: background 0.2s, color 0.2s;
    }

    .toggle-btn:hover {
        background: #e0e0e0;
        color: #000;
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
"]
