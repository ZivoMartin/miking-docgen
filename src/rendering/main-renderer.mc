include "./html-rendering/renderer.mc"
--include "md-renderer.mc"
--include "mdx-rendering/renderer.mc"


-- Combines the Markdown and HTML renderers via language composition.    
lang Renderer = HtmlRenderer end
