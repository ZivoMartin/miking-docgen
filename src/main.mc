include "extracting/extracter.mc"
include "rendering/renderer.mc"
include "stdlib.mc"

mexpr
    switch parse "src/main.mc"
    case Some result then
        -- displayTree result
        let obj = extract result in
        render obj
    end
