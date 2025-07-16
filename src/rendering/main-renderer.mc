include "./html-rendering/renderer.mc"
include "./row-renderer.mc"
include "md-renderer.mc"
--include "mdx-rendering/renderer.mc"


lang Renderer =  RowRenderer + HtmlRenderer + MarkdownRenderer end
