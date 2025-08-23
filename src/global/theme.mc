-- # Themes Module
--
-- This module defines supported themes and provides conversion utilities
-- between strings and Theme values.

lang Themes
    -- Represents the supported themes.
    syn Theme =
    | ThDark {}
    | ThLight {}
    | ThWarm {}
    | ThWarmDark {}

    -- Converts a string into a `Theme` if possible.
    sem themeFromStr : String -> Option Theme
    sem themeFromStr =
    | "dark" -> Some (ThDark {})
    | "warm-dark" -> Some (ThWarmDark {})
    | "light" -> Some (ThLight {})
    | "warm" -> Some (ThWarm {})
    | _ -> None {} 

    -- Converts a `Theme` value into a string.
    sem themeToStr : Theme -> String
    sem themeToStr =
    | ThDark {} -> "Dark"
    | ThWarmDark {} -> "WarmDark"
    | ThLight {} -> "Light"
    | ThWarm {} -> "Warm"

    -- Returns the default theme.
    sem defaultTheme =
    | _ -> ThDark {}
    
end
