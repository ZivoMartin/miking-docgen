include "../parsing/doc-tree.mc"
include "../parsing/token-readers.mc"
include "../util.mc"
include "string.mc"
include "stdlib.mc"
include "sys.mc"

let getNamespace = lam namespace. lam name.
    concatAll [namespace, "/", name]

let extractLastNamespaceElement = lam namespace.
    match strSplit "/" namespace with ([_] ++ _) & namespace then head (reverse namespace) else ""


let normalizePath = lam path.
    let isAbsolute = match path with "/" ++ s then true else false in
    let components = strSplit "/" path in
    recursive let process = lam comps. lam stack.
        switch comps
        case [] then stack
        case ["."] ++ rest then process rest stack
        case [""] ++ rest then process rest stack  -- skip multiple slashes
        case [".."] ++ rest then
            (switch stack
             case ([] | [".."] ++ _) then process rest (cons ".." stack)
             case [_] ++ tl then process rest tl end)
        case [comp] ++ rest then process rest (cons comp stack) end
    in
    let cleaned = reverse (process components []) in
    let result = strJoin "/" cleaned in
    if isAbsolute then cons '/'  result
    else result

utest normalizePath "repo1/../repo2" with "repo2"
utest normalizePath "/repo1/../repo2" with "/repo2"
utest normalizePath "../../repo2" with "../../repo2"
utest normalizePath "./a/./b/../c" with "a/c"
utest normalizePath "/a/b/../../c" with "/c"

        
-- Takes in input the current location on the file system and a target path.
-- If the target is absolute, the function returns the target
-- Otherwise, will normalize the path, if the file exists it returns the fusion of current location and target
-- If the file doesnt exists, returns the target with the isStdlib as true.
let goHere : String -> String -> { path: String, isStdlib: Bool } = lam currentLoc. lam target.
    let path = if strStartsWith "/" target then target
               else concatAll [currentLoc, "/", target] in
    if sysFileExists path then
        { path = normalizePath path, isStdlib = false }
    else
        { path = concatAll [stdlibLoc, "/", target], isStdlib = true }        

        
let removeComments = use TokenReader in
    lam sons. filter (lam s. match s with Leaf { token = Comment {} } then false else true) sons

let nthWord = use TokenReader in lam sons. lam n.
    recursive
    let nthWord = lam sons. lam n.
        switch sons
        case [Leaf { token = Word { content = word } }] ++ rest then
            if eqi n 0 then Some { word = word, rest = rest }
            else nthWord rest (subi n 1)
        case [_] ++ rest then nthWord rest n
        case [] then None {}
        end in
    nthWord sons n

let strExtractType = use TokenReader in lam typedef.
    recursive let strExtractType = lam typedef.
        switch typedef
        case [] | ["in"] then ""
        case [x, "in"] | [x] then x
        case [current] ++ rest then
            let res = strExtractType rest in
            switch (current, res)
            case (_, "," ++ _) | ("{", "}" ++ _) | ("[", _) | (_, "]" ++ _) | ("(", _) | (_, ")" ++ _) | (_, ":" ++ _) then
                concat current res
            case (current, _) then concatAll [current, " ", res]
            end
        end in strExtractType (reverse typedef)
    
        

    

let extractType = use TokenReader in lam typedef.
    strExtractType (foldl
        (lam a. lam w.
            match w with Leaf { token = Word { content = content } } then cons content a
            else a
         ) [] typedef)


    
let extractParents = lam words.
    recursive let extractParents = lam words.
        match nthWord words 0 with Some { word = w, rest = words } then
            switch w
            case "end" | "type" | "sem" | "syn" | "con" then []
            case "=" | "+" then extractParents words
            case name then cons name (extractParents words)
            end
        else [] in extractParents words

-- Skip all the use name in pattern and returns the stream.
let skipUseIn : [DocTree] -> [DocTree] = lam sons.
    recursive let skipUseIn = lam sons.
        match nthWord sons 0 with
        Some { word = "use", rest = sons } then
            match (nthWord sons 1) with Some { rest = sons } then
                skipUseIn sons
            else
                warn "The last word of the stream is a use without any name after.";
                []
        else sons in skipUseIn sons

-- Takes a stream and returns the list of variant.
-- If the input is :
--  | x .... | y ... | z
-- The function will return [x, y, z].
-- Here ... involves anything, and the space between | and variant name can be any separator / comment
-- The input stream should be valid and have as first word '|', empty list is returned otherwise
let extractVariants : [DocTree] -> [String] = lam stream.
    recursive let extractVariants : [DocTree] -> Option [String] -> [String] = lam stream. lam typeAcc.
        switch (nthWord stream 0, typeAcc)
        case (Some { word = "|", rest = stream }, Some typeAcc) then cons (strExtractType typeAcc) (extractVariants stream (Some []))
        case (Some { word = "|", rest = stream }, None {}) then extractVariants stream (Some [])
        case (Some { word = "->", rest = stream }, Some typeAcc) then cons (strExtractType typeAcc) (extractVariants stream (None {}))
        case (Some { word = word, rest = stream }, Some typeAcc) then extractVariants stream (Some (cons word typeAcc))
        case (Some { rest = stream }, None {}) then extractVariants stream (None {})
        case (None {}, Some typeAcc) then [strExtractType typeAcc]
        case (None {}, None {}) then []
        end
        
    in extractVariants stream (None {})
