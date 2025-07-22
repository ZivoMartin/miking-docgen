include "string.mc"

lang FormatLanguages

    syn FormatLanguage =
    | Js {}
    | Ts {}

    sem formatLanguageToStr : FormatLanguage -> String
    sem formatLanguageToStr =
    | Js {} -> "Js"
    | Ts {} -> "Ts"

    sem defaultFormatLanguage : () -> FormatLanguage
    sem defaultFormatLanguage =
    | _ -> Ts {}

    sem formatLanguageGetExt : FormatLanguage -> String
    sem formatLanguageGetExt =
    | Ts {} -> "tsx"
    | Js {} -> "jsx"

    
end
