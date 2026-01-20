include "../global/objects.mc"
include "./ast-stream.mc"
include "./syn-variants.mc"

type ParseLangRes =  use Objects in { stream: String, obj: Object, newPos: Pos }

let parseLang : use AstStream in String -> Pos -> LangDatabase -> String -> Bool -> String -> ParseLangRes =
    lam stream. lam pos. lam database. lam longestPrefix. lam isStdlib. lam namespace.
    use AstStream in
    use TokenReader in

    recursive let splitStream =
        lam stream. lam acc. lam pos. lam switchCount.
        match next stream pos with { token = token, stream = stream, pos = newPos } in
        let return = lam.
            { stream = stream, newPos = newPos, tokens = reverse (cons (token, pos) acc) }
        in
        let continue = lam switchCount.
            splitStream stream (cons (token, pos) acc) newPos switchCount
        in

        switch token
        case TokenWord { content = "switch" } then continue (addi switchCount 1)
        case TokenWord { content = "end" } then
            if eqi switchCount 0 then return ()
            else continue (subi switchCount 1)
        case TokenEof {} then parsingWarn "Failed to get the end of the lang."; return ()
        case _ then continue switchCount
        end
    in

    -- We store the positions of all the occ of syn, sem, type
    let buildPosVec =
        lam tokens.
        let lastToken = last tokens in
        let tokens =
            filter (lam token. match token.0 with TokenWord { content = "syn" | "sem" | "type" } then true else false) tokens
        in
        let tokens = concat tokens [lastToken] in
        map (lam t. t.1) tokens
    in

    let parseHeader =
        lam stream.

        let skipLang =
            lam stream.
            match skipString "lang" stream with Some stream then stream
            else parsingWarn "The lang definition doesn't start with a lang keyword."; stream
        in        

        let getName = getNextWord in
        let skipEqual = skipString "=" in

        recursive let getParents =
            lam stream. lam acc.

            let skipPlus = skipString "+" in
            match next stream pos0 with { token = token, stream = stream } in -- we use pos0 because positions are not important here.
            match token with TokenWord { content = parent } then
                let acc = cons parent acc in
                match skipPlus stream with Some stream then getParents stream acc
                else { parents = reverse acc, stream = stream }
            else getParents stream acc                
        in

        let stream = skipLang stream in
        match getName stream with Some { word = name, stream = stream } then

            let parents =
                match skipEqual stream with Some stream then getParents stream []
                else { stream = stream, parents = [] }
            in
            match parents with { parents = parents, stream = stream } in

            let obj = ObjLang { parents = parents, datas = objDefaultDatas (), children = [] } in
            let obj = objWithName obj name in

            { obj = obj, stream = stream }
                
        else parsingWarn "Failed to reach the name of the language.";
             { obj = ObjLang { parents = [], datas = objDefaultDatas (), children = [] }, stream = stream }
    in

    recursive let collectChildren =
        lam stream. lam posVec. lam pos. lam acc. lam namespace. lam langName.
        let return = lam. reverse acc in

        match posVec with [] | [_] then return ()
        else match posVec with [p1, p2] ++ _ in
        
        match gotoFirstWord stream [] pos with Some { doc = doc, rest = rest, pos = pos, isLang = isLang } then -- Yet again, we don't need the position here.

            let objWithDoc =
                lam obj. lam doc.
                match parseDoc doc with Some doc then objWithDoc obj doc else obj
            in

            if isLang then parsingWarn "Detected a nested lang during lang parsing."; return () else

            match computeObjectSpanning rest pos p1 p2
            with { code = code, rest = rest, newPos = newPos } in

            match getNextWord code with Some { word = kind, stream = codeRest } then
            match getNextWord codeRest with Some { word = name, stream = codeRest } then

            let fetchObj =
                lam.
                match hmLookup name database with Some obj then obj
                else parsingWarn (join ["Failed to fetch ", name, " from the database"]);
                ObjType { t = None {}, datas = objDefaultDatas () } -- dummy node
            in

            let obj =
                switch kind
                case "type" then fetchObj ()
                case "syn" then
                    let code = map (lam t. t.0) (lex code pos0) in
                    let variants = synVariantParse code in
                    ObjSyn { langName = langName, variants = variants, datas = objDefaultDatas () }
                case "sem" then fetchObj ()
                case _ then
                    parsingWarn (join ["Unexpected word: ", kind, "."]); fetchObj ()
                end
            in

            let code = strToSourceCode code in
            
            let obj = objWithSourceCode obj code in
            let obj = objWithName obj name in
            let obj = objWithIsStdlib obj isStdlib in
            let obj = objWithNamespace obj (namespaceAdd namespace name) in
            let obj = objWithPrefix obj longestPrefix in
            let obj = objWithDoc obj doc in

            -- We consume one position, since the end position is the start position
            -- of the next node
            collectChildren rest (tail posVec) newPos (cons obj acc) namespace langName

            else parsingWarn "Failed to reach the kind word."; return ()
            else parsingWarn "Failed to reach the name word."; return ()
        else
            parsingWarn "Failed to reach the next word in the lang parsing."; return ()    
    in

    match splitStream stream [] pos 0 with { stream = rest, newPos = newPos, tokens = tokens } in
    let posVec = buildPosVec tokens in

    match parseHeader stream with { obj = obj, stream = stream } in
    let langName = objName obj in

    let namespace = namespaceAdd namespace langName in
    let children = collectChildren stream posVec pos [] namespace langName in
    let code = tokensToSourceCode (map (lam t. t.0) tokens) in

    let obj = objAddChildren obj children in
    let obj = objWithSourceCode obj code in
    { obj = obj, stream = rest, newPos = newPos }
