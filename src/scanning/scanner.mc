include "../mast-gen/file-opener.mc"
include "../global/util.mc"

include "./scanning-options.mc"
include "./scanning-output.mc"


let scan : ScanningOptions -> ScanningOutput =
    lam opt.
    let files =
        foldl (lam files: [FileToProcess]. lam file: String.
            if isFolder file then
               let newFiles = folderFetchMcFiles file in
               let newFiles = map (lam path.
                   let t = tail (strSplit file path) in
                   let f = strJoin file t in
                   let outputFolder = normalizePath (join [opt.outputFolder, "/", f]) in
                   { path = path, outputFolder = dirname outputFolder }) newFiles
               in
               concat newFiles files
            else if sysFileExists file then
               cons { path = file, outputFolder = opt.outputFolder } files
            else error (join ["The file ", file, "doesn't exist."])
        ) [] opt.files
    in
    
    if null files then defaultScanningOutput () else

    let stdlibOutput = normalizePath (join [opt.outputFolder, "/", opt.stdlibFolder]) in
    let stringPath = normalizePath (join [stdlibLoc, "/", "string.mc"]) in
    let files = cons { path = stringPath, outputFolder = stdlibOutput } files in    

    { inputs = files }
