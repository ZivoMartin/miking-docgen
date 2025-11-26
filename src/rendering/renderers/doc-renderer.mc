include "string.mc"
include "common.mc"
include "../../global/logger.mc"
include "../../global/util.mc"
include "../rendering-options.mc"
include "./renderer-interface.mc"

lang DocContentInterface = RendererInterface
     
    syn DocContent = 

    sem docContentNext : String -> { stream: String, content: Option DocContent }
    sem docContentNext =
    | [] -> { stream = [], content = None {} }

    sem docContentIsHook : String -> Bool
    sem docContentIsHook =
    | _ -> false

    sem renderDocContent : Object -> DocContent -> RenderingOptions -> String
    sem renderDocContent (obj: Object) =
    | _ -> lam opt. renderingWarn "docContentStr is not fully implemented."; ""

end

lang DocContentRawTextLang = DocContentInterface

    syn DocContent =
    | DocContentRawText String

    sem renderDocContent (obj: Object) =
    | DocContentRawText s -> lam opt. renderRemoveDocForbidenChars s opt

    sem docContentNext =
    | ([c] ++ _) & s ->
      recursive let work = lam stream. lam acc.
         if docContentIsHook stream then { stream = stream, acc = reverse acc }
         else 
         match stream with [c] ++ stream then
              work stream (cons c acc)
         else { stream = [], acc = reverse acc}
      in
      match work s [] with { stream = stream, acc = acc } in
      { stream = stream, content = Some (DocContentRawText acc) }

end

lang DocContentArgHookLang = DocContentInterface

    syn DocContent =
    | DocContentArgHook String

    sem renderDocContent (obj: Object) =
    | DocContentArgHook s -> lam opt. renderItalic (renderRemoveDocForbidenChars s opt) opt

    sem docContentIsHook =
    | ['@'] ++ _ -> true

    sem docContentNext =
    | ['@'] ++ s ->
      match splitOnR (eqc ' ') s with { left = hook, right = stream } in
      { stream = stream, content = Some (DocContentArgHook hook)}
      

end

lang DocContentObjHookLang = DocContentInterface

    syn DocContent =
    | DocContentObjHook String

    sem renderDocContent (obj: Object) =
    | DocContentObjHook s -> lam opt.
      let doc = renderRemoveDocForbidenChars s opt in
      let link = objGetLink obj opt s in
      let link = renderLink doc link opt in
      renderBold link opt

    sem docContentIsHook =
    | ['#'] ++ _ -> true

    sem docContentNext =
    | ['#'] ++ s ->
      match splitOnR (eqc ' ') s with { left = hook, right = stream } in
      { stream = stream, content = Some (DocContentObjHook hook)}

end

lang DocContentLang = DocContentArgHookLang + DocContentObjHookLang + DocContentRawTextLang

    type DocContentText = [DocContent]

    sem renderDocContentText : Object -> DocContentText -> RenderingOptions -> String
    sem renderDocContentText (obj: Object) =
    | txt -> lam opt.
          foldl (lam acc. lam content. concat (renderDocContent obj content opt) acc) "" (reverse txt)

    sem docContentParse : String -> DocContentText
    sem docContentParse =
    | s ->
      recursive let parse = lam stream. lam acc.
          match docContentNext stream with { stream = stream, content = Some content } then
               parse stream (cons content acc)
          else acc
      in
      let parsed = parse s [] in
      reverse parsed

end

lang DocObjectInterface = DocContentLang

    syn DocObject =
    
    sem docObjectIsDirective : String -> Bool
    sem docObjectIsDirective =
    | _ -> false

    sem docObjectNext : [String] -> { stream: [String], obj: Option DocObject }
    sem docObjectNext =
    | _ -> { stream = [], obj = None {} }

    sem renderDocObject : Object -> DocObject -> RenderingOptions -> String
    sem renderDocObject (obj: Object) =
    | _ -> lam str. renderingWarn "One of the doc object does not implement docObjToStr."; ""

    sem docObjectFetchDocLines : [String] -> { doc: [String], rest: [String] }
    sem docObjectFetchDocLines =
    | [] -> { doc = [], rest = [] }
    | [line] ++ lines ->
          if docObjectIsDirective line then
             { doc = [], rest = cons line lines }
          else
             let res = docObjectFetchDocLines lines in
             { res with doc = cons line res.doc }

    sem docObjectFetchDoc : [String] -> { doc: DocContentText, rest: [String] }
    sem docObjectFetchDoc =
    | lines ->
      match docObjectFetchDocLines lines with { doc = doc, rest = rest } in
      let s = strJoin "\n" doc in
      let parsed = docContentParse s in
      { doc = parsed, rest = rest }
end

lang DocObjectArgLang = DocObjectInterface

    syn DocObject =
    | DocObjectArg { arg: String, doc: DocContentText }
 
    sem renderDocObject (obj: Object) =
    | DocObjectArg { arg = arg, doc = doc } -> lam opt.
      let arg = renderRemoveDocForbidenChars arg opt in
      join [arg, ":", renderDocContentText obj doc opt]

    sem docObjectIsDirective =
    | ".lam[" ++ _ -> true

    sem docObjectNext =
    | [".lam[" ++ line] ++ lines ->
      match strSplit "]" line with [arg] ++ line then
        let line = strJoin "]" line in
        let lines = cons line lines in
        match docObjectFetchDoc lines with { doc = doc, rest = rest } in
        { stream = rest, obj = Some (DocObjectArg { doc = doc, arg = arg }) }
      else
        renderingWarn "You have an incorrect .lam declaration in your code. Closing bracket is missing.";
        { stream = lines, obj =  None {}}
end

lang DocObjectReturnLang = DocObjectInterface

    syn DocObject =
    | DocObjectReturn { doc: DocContentText }

    sem renderDocObject (obj: Object) =
    | DocObjectReturn { doc = doc } -> lam opt. renderDocContentText obj doc opt
 
    sem docObjectIsDirective =
    | ".return " ++ _ -> true

    sem docObjectNext =
    | [".return " ++ line] ++ lines ->
      let lines = cons line lines in
      match docObjectFetchDoc lines with { doc = doc, rest = rest } in
      { stream = rest, obj = Some (DocObjectReturn { doc = doc }) }

end

lang DocObjectBriefLang = DocObjectInterface

    syn DocObject =
    | DocObjectBrief { doc: DocContentText }

    sem renderDocObject (obj: Object) =
    | DocObjectBrief { doc = doc } -> lam opt. renderDocContentText obj doc opt

    sem docObjectIsDirective =
    | ".brief " ++ _ -> true
 
    sem docObjectNext =
    | [".brief " ++ line] ++ lines ->
      let lines = cons line lines in
      match docObjectFetchDoc lines with { doc = doc, rest = rest } in
      { stream = rest, obj = Some (DocObjectBrief { doc = doc }) }

end

lang DocRenderer = DocObjectArgLang + DocObjectBriefLang + DocObjectReturnLang

    syn DocObjectParsed =
     | DocObjectRaw String 
     | DocObjectFormatted {
       brief: Option DocObject,
       return: Option DocObject,
       args: [DocObject]
     }

    sem renderDocObjectParse : String -> RenderingOptions -> DocObjectParsed
    sem renderDocObjectParse =
    | s -> lam opt.
       let sTrimmed = strTrim s in
       let beginDelimitor = "*-" in
       let endDelimitor = "-*" in
       let beginingOfLine = "*" in
       if and (strStartsWith beginDelimitor sTrimmed) (strEndsWith endDelimitor sTrimmed) then
          let lines = map strTrim (tail (init (strSplit "\n" sTrimmed))) in
          if any (lam l. not (strStartsWith beginingOfLine l)) lines then
             renderingWarn "One of the lines doesn't start with '*', the bloc will be treated as raw comment.";
             DocObjectRaw s
          else
             let lines = map (lam l. strTrim (tail l)) lines in
             recursive let parse = lam stream. lam acc.
                 match docObjectNext stream with { stream = stream, obj = obj} in
                 match obj with Some obj then
                 let acc = switch obj
                 case DocObjectArg {} then { acc with args = cons obj acc.args }
                 case DocObjectBrief { doc = doc } then
                      match acc.brief with Some brief then
                          match brief with DocObjectBrief brief in
                          { acc with brief = Some (DocObjectBrief { brief with doc = concat brief.doc doc }) }
                      else 
                          { acc with brief = Some obj }                      
                 case DocObjectReturn { doc = doc } then
                      match acc.return with Some return then
                          match return with DocObjectReturn return in
                          renderingWarn "You declared twice a return directive. This is probably unexpected. Docgen concatenated both descriptions.";
                          { acc with return = Some (DocObjectReturn { return with doc = concat return.doc doc }) }
                      else 
                          { acc with return = Some obj }
                 end in
                 parse stream acc
                 else acc
             in
             let parsed = parse lines { return = None {}, brief = None {}, args = [] } in
             DocObjectFormatted { parsed with args = reverse parsed.args }
       else
        DocObjectRaw s


    sem renderFormattedDoc (obj: Object) (objParsed: DocObjectParsed) =
    | opt -> let opt = fixOptFormat opt in
        let nl = renderNewLine opt in
        switch objParsed
        case DocObjectRaw s then renderRemoveDocForbidenChars s opt
        case DocObjectFormatted {
               brief = brief,
               args = args,
               return = return
             } then
             let brief = optionMapOr "" (lam brief. renderDocObject obj brief opt) brief in
             let briefTitle = renderBold "Description:\n" opt in
             let briefSection = if null brief then "" else
                                join [briefTitle, brief] in

             let args = strJoin "\n" (map (lam arg. renderDocObject obj arg opt) args) in
             let argsTitle = renderBold "Arguments:\n" opt in             
             let argSection = if null args then "" else
                              join [if null briefSection then "" else nl, argsTitle, args] in

             let return = optionMapOr "" (lam return. renderDocObject obj return opt) return in
             let returnTitle = renderBold "Returns:\n" opt in
             let returnSection = if null return then "" else
                                 join [if and (null argSection) (null briefSection) then "" else nl, returnTitle, return] in
             
             join [briefSection, argSection, returnSection]
        end 

end
