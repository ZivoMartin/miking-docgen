include "../extracting/objects.mc"
include "./renderers/objects-renderer.mc"
include "./rendering-options.mc"

include "ext/file-ext.mc"
    
type FileOpener = {
    depth: Option Int,
    currentDepth: Int,
    neverOpen: Bool    
}

let fileOpenerCreate : Option Int -> FileOpener = lam d.
    { depth = d, currentDepth = 1, neverOpen = false }

type FileOpenerResult = {
    wc: Option WriteChannel,
    write: String -> (),
    path: String,
    fileOpener: FileOpener,
    displaySons: Bool
}

let fileOpenerOpen : FileOpener -> Object -> RenderingOptions -> Option FileOpenerResult = use ObjectsRenderer in use ObjectKinds in lam opener. lam obj. lam opt.
    let empty = { wc = None {}, write = lam. (), path = "", fileOpener = opener, displaySons = false } in    
    let emptyNever = { empty with fileOpener = { opener with neverOpen = true } } in
    
    if opener.neverOpen then Some empty
    else

        let path = concat opt.outputFolder (objLink obj opt) in

        let open = lam depth. lam displaySons.
            match fileWriteOpen path with Some wc then
                Some {
                    wc = Some wc,
                    write = fileWriteString wc,
                    path = path,
                    fileOpener = { opener with currentDepth = depth },
                    displaySons = displaySons
                }
            else
                renderingWarn (concat "Failed to open " path); None {}
        in

        let checkAndOpen = lam d.
            if gti opener.currentDepth d then Some empty
            else open (addi 1 opener.currentDepth) (not (eqi d opener.currentDepth))
        in
    
        switch (opener.depth, obj.kind)
        case (_, ObjUtest {}) then Some emptyNever
        case (None {}, _) then open opener.currentDepth true
        case (Some d, ObjProgram {} | ObjInclude {} | ObjLang {}) then open opener.currentDepth (neqi d 0)
        case (Some d, _) then checkAndOpen d
            
        end 
