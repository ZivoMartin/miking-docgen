-- # ObjectsRenderer utilities
--
-- Helpers to compute rendering-related data derived from extracted objects.
-- Provides link building, display titles, and optional name handling.

include "../../extracting/objects.mc"
include "../rendering-options.mc"
include "./headers/search.mc"
include "string.mc"

lang ObjectsRenderer = ObjectKinds + Formats
    -- Return the object name only for named kinds (let/type/sem/syn/lang/con).
    sem objNameIfHas : Object -> Option String
    sem objNameIfHas =
    | { kind = ObjLet {} | ObjType {} | ObjSem {} | ObjSyn {} | ObjLang {} | ObjCon {} } & obj -> Some (objName obj)
    | _ -> None {}

    -- Preserve the current name context only for Lang and Program roots.
    sem objPreserveNameCtx : Object -> Bool
    sem objPreserveNameCtx =
    | { kind = ObjLang {} | ObjProgram {} } -> true
    | _ -> false

    sem objUrlFetchFailed =
    | obj -> lam name. lam my.
      renderingWarn (join [
          "Failed to fetch ", if my then "my" else "the", " url with the name ", name, ".\n",
          "Here are the details of the fetcher object:\n",
          "namespace=", objNamespace obj, "\n",
          "name=", objName obj, "\n",
          "id=", int2string (objId obj), "\n"
      ])

    sem objGetMyLink : Object -> RenderingOptions -> String
    sem objGetMyLink =
    | obj -> lam opt.
      if not (objKindHasLink (objKind obj)) then ""
      else match nameContextFetchObjUrl opt.nameContext obj with Some res then res
      else objUrlFetchFailed obj (objName obj) true; ""

    sem objGetLink : Object -> RenderingOptions -> String -> String
    sem objGetLink =
    | obj -> lam opt. lam name.
      if not (objKindHasLink (objKind obj)) then ""
      else match nameContextFetchUrl opt.nameContext obj name with Some res then res
      else objUrlFetchFailed obj name false; ""
            
    -- Human-friendly display title; special-cases include/utest.
    sem objTitle : Object -> String
    sem objTitle =    
    | obj ->
        let name = head (reverse (strSplit "/" (objName obj))) in
        let kind = objKind obj in
        switch kind
        case ObjInclude { pathInFile = pathInFile } then pathInFile
        case ObjUtest {} then "utest"
        case _ then name
        end

    -- Debug logger for object rendering info.
    sem objLog : Object -> RenderingOptions -> ()
    sem objLog =
    | obj -> lam opt. opt.log (join [
        "Object ", objName obj, ":\n",
        "   kind: ", objKindToString (objKind obj), "\n",
        "   namespace: ", objNamespace obj, "\n",
        "   prefix: ", objPrefix obj, "\n",
        "   link: ", objGetMyLink obj opt, "\n",
        "   isStdlib: ", bool2string (objIsStdlib obj), "\n"
    ])

    sem objToJsDict : RenderingOptions -> ObjectTree -> [SearchDictObj]
    sem objToJsDict opt = 
    | tree ->
      recursive let objToJsDict = lam opt. lam tree. 
          let obj = objTreeObj tree in
          -- Recursive calls: render all children and transmit the name-context through the fold.
          let res =  foldl (lam arg. lam child.
              let obj = objTreeObj child in
              match (objTreeChildren child, obj.kind) with ([], ObjInclude {}) then arg else
              match objToJsDict opt child with { dicts = dicts, opt = opt } in
              { opt = opt, dicts = concat dicts arg.dicts }
              ) { dicts = [], opt = opt } (objTreeChildren tree)
          in
          let link = concat opt.urlPrefix (objGetMyLink obj opt) in
          let link = if strEndsWith ".md" link then subsequence link 0 (subi (length link) 3) else link in 
          {
             opt = if objPreserveNameCtx obj then res.opt else opt,
             dicts = if objRenderIt obj then cons { name = objNamespace obj, link = link } res.dicts else res.dicts
          }
      in (objToJsDict opt tree).dicts 

end
