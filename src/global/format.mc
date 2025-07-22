-- # Format Module
--
-- This module defines supported output formats for rendering  and utility
-- functions for converting to/from strings. It's used primarily by the CLI or renderer to
-- determine the output mode.

include "option.mc"
include "string.mc"

lang Formats

    syn Format =
    | Html {}
    | Md {}
    | Mdx {}
    | Row { fmt : Format }

    sem unwrapRow : Format -> Format
    sem unwrapRow =
    | Row { fmt = fmt } -> unwrapRow fmt
    | fmt -> fmt
        
    -- Converts a string into a `Format` if possible.
    -- Accepts various case-insensitive aliases and extensions.    
    sem formatFromStr : String -> Option Format
    sem formatFromStr =
     | "html" | "HTML" | "Html" | ".html" -> Some (Html {})
     | "md" | "Markdown" | "markdown" | "MARKDOWN" | "MD" | ".md" -> Some (Md {})
     | "mdx" | "MarkdownExtended" | "MarkdownExt" | "markdownest" | "MARKDOWNEXT" | "MDX" | ".mdx" -> Some (Mdx {})
     | _ -> None {}

    -- Converts a `Format` value back into a printable string.    
    sem formatToStr : Format -> String
    sem formatToStr =
    | Html {} -> "Html"
    | Md {} -> "Md"
    | Mdx {} -> "Mdx"
    | Row { fmt = fmt } -> join ["Row { fmt = ", formatToStr fmt, " }"]

    -- Converts a `Format` value into its file extention
    sem formatGetExtention : Format -> String
    sem formatGetExtention =
    | Html {} -> "html"
    | Md {} | Mdx {} -> "md"
    | Row { fmt = fmt } -> formatGetExtention fmt


    -- Returns the default rendering format to use when none is specified.    
    sem defaultFormat /- () -> Format -/ =
    | _ -> Html {}

    
end
