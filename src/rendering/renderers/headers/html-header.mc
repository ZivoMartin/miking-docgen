-- # HTML Theme Header
--
-- Provides a theme record (`HtmlTheme`) and a helper (`getHeader`) that returns
-- the full HTML `<head>`, embedded `<style>`, and opening `<body>` markup using
-- the theme’s colors. This is consumed by the HTML renderer.

include "./search.mc"
include "./html-script.mc"
include "./html-style.mc"
include "string.mc"

-- Build the HTML header + styles + opening body using a theme and a page title.
let getHeader : String -> String = lam title.
    join [
"<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>", title, "</title>
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<link rel=\"stylesheet\" href=\"/", htmlStylePath , "\">
</head>
<body>
<div class=\"main-container\">
<button class=\"theme-toggle\" id=\"themeButton\"></button>
<div id=\"themeMenu\" style=\"display:none;\">
  <button data-theme=\"htmlLight\">Light</button>
  <button data-theme=\"htmlDark\">Dark</button>
  <button data-theme=\"htmlWarm\">Warm</button>
  <button data-theme=\"htmlWarmDark\">Warm Dark</button>
</div>
", searchHtml, "
<script src=\"/", htmlScriptPath, "\"></script>
<script src=\"/", searchJsPath, "\"></script>
"]



