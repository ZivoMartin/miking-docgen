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
    font-family: 'Segoe UI', Roboto, sans-serif;
    background-color: #f9fafb;
    color: #111827;
    margin: 2rem auto;
    max-width: 960px;
    padding: 0 1rem;
    font-size: 16px;
    line-height: 1.7;
    letter-spacing: 0.02em;
}

h1 {
    font-size: 2.4em;
    font-weight: 700;
    border-bottom: 3px solid #d1d5db;
    padding-bottom: 0.4em;
    margin-bottom: 1.5em;
    color: #111827;
}

h2 {
    font-size: 1.8em;
    font-weight: 600;
    color: #1f2937;
    margin-top: 2.5em;
    border-left: 4px solid #3b82f6;
    padding-left: 0.6em;
}

.doc-block {
    background-color: #ffffff;
    border-left: 3px solid #3b82f6;
    border: 1px solid #e5e7eb;
    border-radius: 4px;
    padding: 0.8rem 1rem;
    margin: 1.5rem 0;
    font-size: 15px;
    position: relative;    
}

.doc-signature {
    background-color: #f9f9f9;
    padding: 0.4em 0.8em;
    border-radius: 3px;
    font-weight: 600;
    display: inline-block;
    margin-bottom: 0.6em;
}

.doc-description {
    background-color: #f0f4ff;    
    border-left: 2px solid #d1d5db;
    padding: 0.5em 1em;
    color: #374151;
    font-style: italic;
    font-size: 0.95em;
    margin-bottom: 0.5em;
}

.code-block {
    background-color: #fafafa;
    border: 1px solid #e5e7eb;
    border-radius: 4px;
    padding: 0.5em 0.8em;
    margin-top: 0.5em;
    font-family: monospace;
    font-size: 0.9em;
}

.toggle-btn {
    all: unset;    
    border: none;
    padding: 0.2em 0.5em;
    font-size: 0.85em;
    font-family: monospace;
    cursor: pointer;
    transition: background-color 0.2s ease;
}

.toggle-btn:hover {
    background-color: #bae6fd;
}

.hiden-code {}

.gotoLink {
    font-size: 1.1em;
    color: #3b82f6;
    text-decoration: none;
    position: absolute;
    top: 0.6em;
    right: 1em;
    opacity: 0.7;
    font-weight: 500;
    transition: opacity 0.2s ease, transform 0.2s ease;
}


a {
    color: #2980b9;
    text-decoration: none;
}

a:hover {
    text-decoration: underline;
}

.gotoLink:hover {
    opacity: 1;
    transform: scale(1.1);
}


/* Highlighting stays the same (optional tweaks below) */
.kw { color: #dc2626; font-weight: 500; }
.var { color: #2563eb; }
.tp  { color: #7c3aed; }
.number { color: #0284c7; }
.comment { color: #16a34a; font-style: italic; }

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
