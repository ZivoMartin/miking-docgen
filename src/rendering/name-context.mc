include "../global/util.mc"
include "./rendering-types.mc"
include "./renderers/objects-renderer.mc"


let nameContextInsertIfHas : NameContext -> Object -> String -> NameContext = use ObjectsRenderer in lam map. lam obj. lam link.
    match objNameIfHas obj with Some name then
          hmInsert name link map
    else map
