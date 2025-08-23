-- # Format Module
--
-- This module defines supported output formats for rendering and provides
-- utility functions for converting to/from strings. It is used primarily by the
-- CLI or renderer to determine the output mode.
--
-- We have 3 formats: mdx, md, and html. The Row format is a generic wrapper
-- around another format. Its purpose is to preserve the real format while
-- enabling temporary generic code, useful for factorization and composition.

include "option.mc"
include "string.mc"

lang Formats

    -- Represents the supported output formats.
    syn Format =
    | Html {}
    | Md {}
    | Mdx {}
    | Row { fmt : Format }

    -- Recursively unwraps a Row format to get the underlying format.
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
     | "mdx" | "MarkdownExtended" | "MarkdownExt" | "markdownext" | "MARKDOWNEXT" | "MDX" | ".mdx" -> Some (Mdx {})
     | _ -> None {}

    -- Converts a `Format` value back into a printable string.    
    sem formatToStr : Format -> String
    sem formatToStr =
    | Html {} -> "Html"
    | Md {} -> "Md"
    | Mdx {} -> "Mdx"
    | Row { fmt = fmt } -> join ["Row { fmt = ", formatToStr fmt, " }"]

    -- Returns the file extension associated with a given `Format`.
    sem formatGetExtension : Format -> String
    sem formatGetExtension =
    | Html {} -> "html"
    | Md {} | Mdx {} -> "md"
    | Row { fmt = fmt } -> formatGetExtension fmt


    -- Returns the default rendering format to use when none is specified.    
    sem defaultFormat /- () -> Format -/ =
    | _ -> Html {}

    
end
