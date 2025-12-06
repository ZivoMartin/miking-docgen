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

    sem objHasName : Object -> Bool
    sem objHasName =
    | obj -> optionIsSome (objNameIfHas obj)


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
      buildUrl opt.stdlibFolder opt.urlPrefix opt.fmt (objIsStdlib obj) (objNamespace obj)

    sem objGetLink : Object -> RenderingOptions -> String -> String
    sem objGetLink =
    | obj -> lam opt. lam name.
      let kind = objKind obj in
      if not (objKindHasLink kind) then ""
      else match nameContextFetch opt.nameContext obj name with Some res then res.url
      else objUrlFetchFailed obj name false; ""

    sem objTryFetch : Object -> RenderingOptions -> String -> Option NameMapValue
    sem objTryFetch =
    | obj -> lam opt. lam name.
      if not (objKindHasLink (objKind obj)) then None {}
      else nameContextFetch opt.nameContext obj name

    sem objGetMyLocation : Object -> RenderingOptions -> String
    sem objGetMyLocation =
    | obj -> lam opt.
      let name = objName obj in
      let link = objGetMyLink obj opt in
      let prefixLength = length opt.urlPrefix in
      subsequence link prefixLength (length link)
            
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
      recursive let objToJsDict = lam tree. 
          let obj = objTreeObj tree in
          -- Recursive calls: render all children and transmit the name-context through the fold.
          let dicts =  foldl (lam dicts. lam child.
              let obj = objTreeObj child in
              match (objTreeChildren child, obj.kind) with ([], ObjInclude {}) then dicts else
              let newDicts = objToJsDict child in
              concat newDicts dicts
              ) [] (objTreeChildren tree)
          in
          let link = objGetMyLink obj opt in
          let link = if strEndsWith ".md" link then subsequence link 0 (subi (length link) 3) else link in 
          if objRenderIt obj then
             cons { name = objNamespace obj, link = link } dicts
          else dicts
      in objToJsDict tree
end
