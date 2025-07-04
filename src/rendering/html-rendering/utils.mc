-- # HTML Helpers
--
-- Utility functions to generate basic HTML elements as strings.
-- These functions simplify HTML rendering in Miking-based tools.

include "../../util.mc"
include "../../extracting/objects.mc"

-- Wraps string `s` in an HTML tag `b`, with newlines.
let htmlBalise = lam s. lam b. concatAll ["<", b, ">\n", s, "\n</", b, ">"]

let htmlText = lam s. htmlBalise s "p"
let htmlPre = lam s. concatAll ["<pre>", s, "</pre>"]
let htmlCode = lam s. concatAll ["<pre class=code>", s, "</pre>"]
let htmlStrong = lam s. htmlText (htmlBalise s "strong")

-- Generates a link with path `l` and text `txt`.
let htmlGetLink = lam l. lam txt. concatAll ["<a href=\"/", l, "\">", txt, "</a>"]
let htmlGetLangLink = lam lng. htmlGetLink (concat (getLangLink lng) ".lang") lng

let htmlDoc = lam doc. concatAll ["<pre class=md>", doc, "</pre>"]                

let span = lam content. lam kind. concatAll ["<span class=\"", kind, "\">", content, "</span>"]
let kw = lam content. span content "kw"
let var = lam content. span content "var"
let tp = lam content. span content "tp"
let st = lam content. span content "string"
