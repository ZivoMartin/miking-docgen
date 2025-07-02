lang Formats

    syn Format =
    | Html {}
    | Md {}


    -- Parse format string (example: `html` or `md`), returns None if invalid
    sem formatFromStr : String -> Option Format
    sem formatFromStr =
     | "html" | "HTML" | "Html" | ".html" -> Some (Html {})
     | "md" | "Markdown" | "markdown" | "MARKDOWN" | "MD" | ".md" -> Some (Md {})

    -- Cast format to String
    sem formatToStr : Format -> String
    sem formatToStr =
    | Html {} -> "Html"
    | Md {} -> "Md"    

    -- Returns the default format if nothing is specified in the CLI
    sem defaultFormat /- () -> Format -/ =
        | _ -> Html {}

    
end
