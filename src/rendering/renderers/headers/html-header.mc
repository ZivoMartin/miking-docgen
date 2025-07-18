include "string.mc"

type HtmlTheme = {
    bodyBGColor: String,                
    bodyTextColor: String,              
    h1BorderColor: String,              
    h1Color: String,                    
    h2Color: String,                    
    h2BorderColor: String,              
    docBlockBGColor: String,            
    docBlockBorderColor: String,        
    docBlockOutlineColor: String,       
    docSignatureBGColor: String,        
    docDescriptionBGColor: String,      
    docDescriptionBorderColor: String,  
    docDescriptionTextColor: String,    
    codeBlockBGColor: String,           
    codeBlockBorderColor: String,       
    toggleHoverBGColor: String,         
    gotoLinkColor: String,              
    aColor: String,                     
    keywordColor: String,               
    variableColor: String,              
    typeColor: String,                  
    numberColor: String,                
    commentColor: String
}

let getHeader : HtmlTheme -> String -> String = lam theme. lam title.
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
    background-color: ", theme.bodyBGColor, ";
    color: ", theme.bodyTextColor, ";
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
    border-bottom: 3px solid ", theme.h1BorderColor, ";
    padding-bottom: 0.4em;
    margin-bottom: 1.5em;
    color: ", theme.h1Color, ";
}

h2 {
    font-size: 1.8em;
    font-weight: 600;
    color: ", theme.h2Color, ";
    margin-top: 2.5em;
    border-left: 4px solid ", theme.h2BorderColor, ";
    padding-left: 0.6em;
}

.doc-block {
    background-color: ", theme.docBlockBGColor, ";
    border-left: 3px solid ", theme.docBlockBorderColor, ";
    border: 1px solid ", theme.docBlockOutlineColor, ";
    border-radius: 4px;
    padding: 0.8rem 1rem;
    margin: 1.5rem 0;
    font-size: 15px;
    position: relative;    
}

.doc-signature {
    background-color: ", theme.docSignatureBGColor, ";
    padding: 0.4em 0.8em;
    border-radius: 3px;
    font-weight: 600;
    display: inline-block;
    margin-bottom: 0.6em;
}

.doc-description {
    background-color: ", theme.docDescriptionBGColor, ";    
    border-left: 2px solid ", theme.docDescriptionBorderColor, ";
    padding: 0.5em 1em;
    color: ", theme.docDescriptionTextColor, ";
    font-style: italic;
    font-size: 0.95em;
    margin-bottom: 0.5em;
}

.code-block {
    background-color: ", theme.codeBlockBGColor, ";
    border: 1px solid ", theme.codeBlockBorderColor, ";
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
    background-color: ", theme.toggleHoverBGColor, ";
}

.hiden-code {}

.gotoLink {
    font-size: 1.1em;
    color: ", theme.gotoLinkColor, ";
    text-decoration: none;
    position: absolute;
    top: 0.6em;
    right: 1em;
    opacity: 0.7;
    font-weight: 500;
    transition: opacity 0.2s ease, transform 0.2s ease;
}

a {
    color: ", theme.aColor, ";
    text-decoration: none;
}

a:hover {
    text-decoration: underline;
}

.gotoLink:hover {
    opacity: 1;
    transform: scale(1.1);
}

/* Highlighting */
.kw      { color: ", theme.keywordColor, "; font-weight: 500; }
.var     { color: ", theme.variableColor, "; }
.tp      { color: ", theme.typeColor, "; }
.number  { color: ", theme.numberColor, "; }
.comment { color: ", theme.commentColor, "; font-style: italic; }

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
    window.scrollTo({ top: scrollY });
}
</script>"
]
