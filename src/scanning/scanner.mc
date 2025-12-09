include "../mast-gen/file-opener.mc"
include "../global/util.mc"

include "./scanning-options.mc"
include "./scanning-output.mc"

let scan : ScanningOptions -> ScanningOutput =
    lam opt.

    printLn "...";

    if null opt.files then { inputs = [], longestPrefix = "" } else

    let files =
        foldl (lam files: [String]. lam file: String.
            if isFolder file then
               let newFiles = folderFetchMcFiles file in
               concat newFiles files
            else if sysFileExists file then cons file files
            else error (join ["The file ", file, "doesn't exist."])
        ) [] opt.files
    in

    let files = cons "string.mc" files in
    
    let normalizeFiles : String -> [String] -> [String] = lam pos.
        map (lam f.
            if strStartsWith "/" f then f
            else
                let stdlibFile = normalizePath (join [stdlibLoc, "/", f]) in
                if sysFileExists stdlibFile then stdlibFile
                else normalizePath (join [pos, "/", f]))
    in

    let files = normalizeFiles pwd files in

    type InputFileSet = HashMap String () in
    type Visited = HashMap String () in
    type Ctx = { visited : Visited, set: InputFileSet } in


    recursive

    let go : Ctx -> [String] -> Ctx =
        lam ctx. lam files.
        foldl (
            lam ctx. lam f.
                if hmMem f ctx.visited then ctx
                else scanFile { ctx with visited = hmInsert f () ctx.visited } f
        ) ctx files

    let scanFile : Ctx -> String -> Ctx =
        lam ctx. lam pos.
        match parsingOpenFile pos with Some { includes = includes } then
            let pos = dirname pos in
            let includes = normalizeFiles pos includes in
            let set = foldl (lam set. lam i. hmRemove i set) ctx.set includes in
            go { ctx with set = set } includes
        else
            error (join ["Failed to open ", pos])
    in                           

    let filesSet = foldl (lam acc. lam f. hmInsert f () acc) (hashmapEmpty ()) files in
    let files = foldl scanFile { set = filesSet, visited = hashmapEmpty () } files in

    let visited = hmKeys files.visited in
    let files = hmKeys files.set in

    let nonStdlib = filter (lam f. not (pathIsInStdlib f)) visited in

    let commonPrefix = strLongestCommonPrefixArray (if null nonStdlib then visited else nonStdlib) in
    let commonPrefix = dirname commonPrefix in

    let commonPrefixLength = length commonPrefix in
    let stdlibLocLength = length stdlibLoc in

    let files = if pathIsInStdlib (head files) then reverse files else files in

    iter printLn files;

    let files =
        map (
            lam path.
            let getRelativeOutputFolder =
                lam outputFolder. lam commonPrefixLength.
                let f = subsequence path commonPrefixLength (length path) in
                let outputFolder = normalizePath (join [outputFolder, "/", f]) in
                let outputFolder = dirname outputFolder in
                concat outputFolder "/"
             in

            let outputFolder = if pathIsInStdlib path then
               getRelativeOutputFolder (join [opt.outputFolder, "/", opt.stdlibFolder]) stdlibLocLength
            else
               getRelativeOutputFolder opt.outputFolder commonPrefixLength
            in

            { path = path, outputFolder = dirname outputFolder }
        ) files
    in

    printLn "...";

    { inputs = files, longestPrefix = commonPrefix }
