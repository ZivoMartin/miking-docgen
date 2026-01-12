include "./parsing-options.mc"
include "./ast-stream.mc"
include "./doc-parser.mc"

include "../global/util.mc"
include "../global/namespace-utils.mc"
include "../options/docgen-options.mc"
include "../extracting/objects.mc"
include "../extracting/source-code.mc"

include "seq.mc"
include "hashmap.mc"
include "fileutils.mc"
include "hashmap.mc"
include "sys.mc"

let parse : use Objects in ParsingOptions -> MAst -> Object =
    use Objects in
    use TokenReader in
    use AstStream in

    lam opt. lam ast.

    match opt with { log = log, basePath = basePath, longestPrefix = longestPrefix } in

    let openers =
        foldl
        (lam m. lam k. hmInsert k () m)
            (hashmapEmpty ())
        ["let", "lang", "type", "syn", "sem", "con", "mexpr", "use", "utest", "recursive"] in
    
    type ParseRes = { includeSet: IncludeSet (), astStream: AstStreamContext, obj: Object, newPos: Pos } in
    type ParseFileRes = { astStream: AstStreamContext, children: [Object], newPos: Pos } in

    recursive
    -- This function is parsing the text of the file without any includes
    let parseFile : AstStreamContext -> Pos -> String -> String -> ParseFileRes =
        lam astStream. lam pos. lam namespace. lam content.
        let isStdlib = pathIsInStdlib namespace in

        type CollectRes = { obj: Option Object, rest: String, astStream: AstStreamContext, newPos: Pos } in
        let collectOneNode : String -> CollectRes =
            lam content.
            let default = { obj = None {}, astStream = astStream, rest = "", newPos = pos } in

            match typeStreamNext astStream with Some {
                ctx = astStream,
                name = name,
                info = info,
                obj = obj
            } then

                let namespace = namespaceAdd namespace name in

                let obj = objWithName obj name in
                let obj = objWithIsStdlib obj isStdlib in
                let obj = objWithNamespace obj namespace in
                let obj = objWithPrefix obj longestPrefix in

                match info with Info { row1 = row1, col1 = col1, row2 = row2, col2 = col2 } then
                    -- printLn (int2string row1);
                    -- printLn (int2string col1);
                    -- printLn (int2string row2);
                    -- printLn (int2string col2);
                    match pos with { x = posX, y = posY } in
                    
                    recursive let gotoFirstWord : String -> [Token] -> Pos -> Option ([Token], Pos, String) =
                        lam rest. lam acc. lam pos.
                        switch next rest pos
                        case { token = TokenWord {} } then Some (reverse acc, pos, rest)
                        case { token = TokenEof {} } then None {}
                        case { token = token, pos = pos, stream = rest } then gotoFirstWord rest (cons token acc) pos
                        end
                    in

                    match gotoFirstWord content [] pos with Some (doc, newPos, rest) then
                        
                        let obj = match parseDoc doc with Some doc then objWithDoc obj doc else obj in

                        let dx = subi col2 col1 in
                        let dy = subi row2 row1 in

                        let newPos = { x = addi newPos.x dx, y = addi newPos.y dy } in

                        match strWalkTo rest dx dy with Some (code, rest) then
                            let code = strToSourceCode code in
                            let obj = objWithSourceCode obj code in
                            { obj = Some obj, astStream = astStream, rest = rest, newPos = newPos }
                        else default
                    else default
                else
                    parsingWarn "No info on the node, we can't create any object.";
                    default
            else default
                
        in

        match collectOneNode content with { obj = Some obj, astStream = astStream, rest = rest, newPos = newPos } then
             let res = parseFile astStream newPos namespace rest in
             { res with children = cons obj res.children }
        else { children = [], astStream = astStream, newPos = pos }

    -- Here we parse the include header of the file, jump in all the includes before processing the actual code.
    let parse: IncludeSet () -> AstStreamContext -> Pos -> String -> ParseRes =
        lam includeSet. lam astStream. lam pos. lam loc.

        let fileIsStdlib = pathIsInStdlib loc in

        match parsingOpenFile loc with
        Some { headerTokens = headerTokens, fileText = fileText } then

        recursive let lex : String -> Pos -> [Token] =
            lam s. lam pos.
            match next s pos with { token = token, stream = s, pos = pos } in
            match token with TokenEof {} then [] else cons token (lex s pos)
        in

        let fileContent = match fileReadOpen loc with Some rc then
            let s = fileReadString rc in
            fileReadClose rc;
            s
        else
            parsingWarn (join ["Failed to open ", loc, "."]);
            ""
        in

        let tokens = lex fileContent{ x = 0, y = 0 } in

        let getProgName : String -> String =
            lam loc.
            optionGetOrElse (lam. parsingWarn "Namespace is empty."; "") (namespaceLast loc)
        in
        let progName = getProgName loc in
        let progSourceCode = tokensToSourceCode tokens in
        let progDoc = optionGetOr (objDefaultDoc ()) (parseProgramDoc fileContent) in

        let progObj = ObjProgram { children = [], datas = objDefaultDatas () } in
        let progObj = objWithName progObj progName in

        let progObj = objWithIsStdlib progObj fileIsStdlib in
        let progObj = objWithDoc progObj progDoc in
        let progObj = objWithNamespace progObj loc in
        let progObj = objWithPrefix progObj longestPrefix in
        let progObj = objWithSourceCode progObj progSourceCode in
        
        let progNamespace = loc in

        let headerDocTree = foldl (lam arg: ParseRes. lam token.
            match arg with { includeSet = includeSet, obj = obj } in
            match token with { token = token, pos = pos } in

            let go : ParseRes -> Object -> ParseRes =
                lam arg. lam child.
                { arg with obj = objAddChild obj child }
            in
            
            match token with TokenInclude { content = content } then
                match includeSetInsert includeSet loc content () with
                { includeSet = includeSet, inserted = inserted, path = path } in
                
                let insertResult = if inserted then
                    match parse includeSet astStream pos path with
                    { includeSet = includeSet, obj = obj, astStream = astStream, newPos = newPos } in
                    ({ arg with includeSet = includeSet, astStream = astStream, newPos = newPos }, Some obj)
                else
                    (arg, None {}) in
                match insertResult with (arg, obj) in
                let child = ObjInclude { datas = objDefaultDatas (), pathInFile = content, child = obj } in
                let child = objWithIsStdlib child fileIsStdlib in
                let child = objWithNamespace child progNamespace in
                let child = objWithPrefix child longestPrefix in
                let child = objWithName child (getProgName path) in

                go arg child
            else
                arg
            ) { includeSet = includeSet, astStream = astStream, obj = progObj, newPos = pos } headerTokens
        in

        match headerDocTree with { includeSet = includeSet, astStream = astStream, obj = obj, newPos = newPos } in
        log (concat "Beginning of parsing stage on " loc);

        match parseFile astStream newPos progNamespace fileContent
            with { children = children, astStream = astStream, newPos = newPos }
        in

        let obj = objAddChildren obj (reverse children) in
        let obj = objReverseChildren obj in

        { includeSet = includeSet, astStream = astStream, obj = obj, newPos = newPos }
        else
            error (join ["Found an invalid path during parsing: ", loc, "."])
    in
    
    match goHere pwd basePath with { path = basePos } in

    let includeSet = includeSetNew () in

    match includeSetInsert includeSet "." basePath () with { includeSet = includeSet } in    

    match parse includeSet (buildAstStream ast) pos0 basePath with { includeSet = includeSet, obj = obj } in

    log "Parsing is over.";
    obj
