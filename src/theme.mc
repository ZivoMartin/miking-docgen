lang Themes

    syn Theme =
    | ThDark {}
    | ThLight {}
    | ThWarm {}
    | ThWarmDark {}

    sem themeFromStr : String -> Option Theme
    sem themeFromStr =
    | "dark" -> Some (ThDark {})
    | "warm-dark" -> Some (ThWarmDark {})
    | "light" -> Some (ThLight {})
    | "warm" -> Some (ThWarm {})
    | _ -> None {} 

    sem themeToStr : Theme -> String
    sem themeToStr =
    | ThDark {} -> "Dark"
    | ThWarmDark {} -> "WarmDark"
    | ThLight {} -> "Light"
    | ThWarm {} -> "Warm"
       
end 
