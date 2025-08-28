-- # file-opener.mc
--
-- This module provides an API to open documentation output files while keeping track of the **rendering depth**.  
--
-- Miking-doc-gen accepts a `depth` flag that specifies how deep into the object tree we want to render:
-- - With `depth = 1`, only **top-level objects** will be rendered.
-- - With higher depths, nested objects will also be processed.
--
-- The file opener enforces this policy:  
-- - It opens a file only if the object **should** be rendered at the current depth.
-- - Some objects, such as **unit tests (`utest`)**, are *never* rendered, nor are their children.
--
-- This ensures that documentation output remains concise and controllable.

include "../extracting/objects.mc"
include "./renderers/objects-renderer.mc"
include "./rendering-options.mc"

include "ext/file-ext.mc"

-- ## FileOpener
--
-- Tracks the state of file opening relative to depth constraints.
type FileOpener = {
    depth: Option Int,   -- Maximum rendering depth (None = unlimited)
    currentDepth: Int,   -- Current depth in the traversal
    neverOpen: Bool      -- If true, this node and its children should never be opened
}

-- Create a fresh `FileOpener` from an optional depth limit.
let fileOpenerCreate : Option Int -> FileOpener = lam d.
    { depth = d, currentDepth = 1, neverOpen = false }

-- ## FileOpenerResult
--
-- Result of opening a file for writing.
type FileOpenerResult = {
    wc: Option WriteChannel,   -- Optional write channel if the file is open
    write: String -> (),       -- Function to write a string to the file
    path: String,              -- Path of the opened file
    fileOpener: FileOpener,    -- Updated file opener state
    displaySons: Bool          -- Whether to display child nodes of this object
}

-- ## fileOpenerOpen
--
-- Attempts to open the output file for a given object, depending on the current depth
-- and the type of object.  
--
-- ### Rules:
-- - **Utests and Recursive blocks** are never opened (they are excluded entirely).
-- - If `depth = None`, files are always opened, with no restriction.
-- - If `depth = Some d`:
--   - `Program`, `Include`, and `Lang` objects are always opened, unless `d = 0`.
--   - Other objects are opened only if they are within the allowed depth.
--
-- The returned `FileOpenerResult` includes the write function, path, and an updated opener.
let fileOpenerOpen : FileOpener -> Object -> RenderingOptions -> Option FileOpenerResult =
    use ObjectsRenderer in use ObjectKinds in lam opener. lam obj. lam opt.

    -- Default empty result: no file opened
    let empty = { wc = None {}, write = lam. (), path = "", fileOpener = opener, displaySons = false } in
    let emptyNever = { empty with fileOpener = { opener with neverOpen = true } } in
    
    if opener.neverOpen then Some empty
    else
        let path = concat opt.outputFolder (objLink obj opt) in

        -- Helper to actually open the file
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

        -- Helper to check depth constraints
        let checkAndOpen = lam d.
            if gti opener.currentDepth d then Some empty
            else open (addi 1 opener.currentDepth) (not (eqi d opener.currentDepth))
        in
    
        -- Dispatch based on object kind and depth
        switch (opener.depth, obj.kind)
        case (_, ObjUtest {} | ObjRecursiveBloc {}) then Some emptyNever
        case (None {}, _) then open opener.currentDepth true
        case (Some d, ObjProgram {} | ObjInclude {} | ObjLang {}) then open opener.currentDepth (neqi d 0)
        case (Some d, _) then checkAndOpen d
        end
