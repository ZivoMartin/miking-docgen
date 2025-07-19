include "./html-warm-dark.mc"
include "./html-dark.mc"
include "./html-light.mc"
include "./html-warm.mc"
include "../../../global/theme.mc"

let getHeader : use Themes in Theme -> String -> String = use Themes in lam theme. lam title.
    let theme = switch theme
    case ThDark {} then htmlDark
    case ThWarmDark {} then htmlWarmDark
    case ThWarm {} then htmlWarm
    case ThLight {} then htmlLight
    end in
    getHeader theme title
