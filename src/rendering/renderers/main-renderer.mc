include "./html-renderer.mc"
include "./row-renderer.mc"
include "./md-renderer.mc"
include "./mdx-renderer.mc"


lang Renderer =  RowRenderer + HtmlRenderer + MarkdownRenderer + MdxRenderer end
