-- # Execution Context
--
-- This module defines the `ExecutionContext`, the central state threaded through all
-- stages of the documentation generation pipeline.
--
-- ## ExecutionContext fields
-- - `opt`      : Parsed CLI options.
-- - `mainFile` : Path of the main input file.
-- - `tokens`   : Tokens from the lexer (not always used directly).
-- - `docTree`  : Parsed documentation tree, if available.
-- - `ast`      : The Miking AST, if generated.
-- - `object`   : The extracted object tree, if built.
--
-- ## Step functions
-- Each stage of the pipeline is a `Step = ExecutionContext -> ExecutionContext`.
-- - `gen`     : Build MAst from the main file.
-- - `parse`   : Build DocTree from MAst.
-- - `extract` : Extract ObjectTree from DocTree.
-- - `label`   : Label ObjectTree with semantic metadata.
-- - `name`    : Create a Namespace to lookup url of a name depending on the context.
-- - `render`  : Generate documentation files.
-- - `serve`   : Start preview server.
--
-- If a step is called out of order, the `crash` function raises an error with details.
--
-- The ExecutionContext also provides a logger for each steps.

include "./options/docgen-options.mc"
include "./options/cast-options.mc"
include "./mast-gen/mast-generator.mc"
include "./parsing/parser.mc"
include "./extracting/extracter.mc"
include "./labeling/labeler.mc"
include "./naming/namer.mc"
include "./rendering/renderer.mc"
include "./server/server.mc"

type ExecutionContext =  use TokenReader in {    
    opt: DocGenOptions,
    userOutputFolder: String,
    currentFile: String,
    files: [{ path: String, outputFolder: String }],
    tokens: [Token],
    docTree : Option DocTree,
    ast: Option MAst,
    searchDatas: HashMap String String,
    object: Option ObjectTree,
    nameContext: Option NameContext
}

let buildLogger : ExecutionContext -> String -> Logger = lam ctx. lam step. if ctx.opt.debug then message "INFO" step else lam. ()

let execCtxNext : ExecutionContext -> Option ExecutionContext = use Renderer in lam ctx.
    match ctx.files with [{ path = path, outputFolder = outputFolder }] ++ files then
          Some { ctx with
              opt = { ctx.opt with outputFolder = outputFolder },
              currentFile = path,
              files = files,
              tokens = [],
              docTree = None {},
              ast = None {},
              object = None {},
              nameContext = None {}
          }
    else
        let log = buildLogger ctx "Rendering" in 
        let ropt = getRenderingOption ctx.opt log in
        let ropt = { ropt with outputFolder = ctx.userOutputFolder } in
        let searchDatas = map (lam entry. { name = entry.0, link = entry.1 })
                          (hashmap2seq ctx.searchDatas) in
        renderSearchFile searchDatas ropt;
        None {}

let execContextNew : DocGenOptions -> ExecutionContext = lam opt.
    let files = if isFolder opt.file then
       let files = folderFetchMcFiles opt.file in
       map (lam path.
           let t = tail (strSplit opt.file path) in
           let f = strJoin opt.file t in
           let outputFolder = normalizePath (join [opt.outputFolder, "/", f]) in
           { path = path, outputFolder = dirname outputFolder }) files
    else if sysFileExists opt.file then
       [{ path = opt.file, outputFolder = opt.outputFolder }]
    else error (join ["The file ", opt.file, "doesn't exist."]) in
    let ctx = {
        opt = opt,
        currentFile = "",
        userOutputFolder = opt.outputFolder,
        files = files,
        tokens = [],
        docTree = None {},
        object = None {},
        ast = None {},
        nameContext = None {},
        searchDatas = hashmapEmpty ()
    } in
    match execCtxNext ctx with Some ctx then ctx else
    error "Please provide a file to process."


let crash = lam miss. lam func. lam should.
    error (join ["Execution context: ", miss, " is missing in the exection context, ", func, " function should be called after having call the ", should, " function."])
    
type Step = ExecutionContext -> ExecutionContext

let gen : Step = lam ctx.
    let log = buildLogger ctx "MExpr Generation" in
    { ctx with ast = Some (buildMAstFromFile log ctx.currentFile) }

let parse : Step =  lam ctx.
    match ctx.ast with Some ast then
    let log = buildLogger ctx "Parsing" in
    { ctx with docTree = Some (parse log ctx.currentFile ast ) }
    else crash "ast" "parse" "gen"
    
let extract : Step =  lam ctx.
    match ctx.docTree with Some docTree then
    let log = buildLogger ctx "Extracting" in 
    { ctx with object = Some (extract log docTree ctx.opt.letDepth ) }
    else crash "doc tree" "extract" "parse"

let label : Step =  lam ctx.
    match (ctx.object, ctx.ast) with (Some object, Some ast) then
    let log = buildLogger ctx "Labeling" in    
    { ctx with object = Some (label log object ast) }
    else crash "object" "label" "extract"

let name : Step =  lam ctx.
    match ctx.object with Some object then
    let log = buildLogger ctx "Naming" in
    let opt = getNamingOption ctx.opt in
    { ctx with nameContext = Some (name log opt object) }
    else crash "object" "name" "extract"

let render : Step =  lam ctx.
    match ctx.object with Some obj then
    let log = buildLogger ctx "Rendering" in 
    let ropt = getRenderingOption ctx.opt log in
    let renderingRes = render ropt obj in

    let searchDatas = foldl (lam acc. lam arg.
        let prefix = normalizePath (join [ctx.opt.urlPrefix, "/", "Stdlib"]) in
        let isStdlib = strStartsWith prefix arg.link in

        let prefix = tail (strSplit ctx.userOutputFolder ctx.opt.outputFolder) in
        let prefix = if isStdlib then "" else join prefix in
        
        let link = normalizePath (join [prefix, "/", arg.link]) in
        let name = normalizePath (join [prefix, "/", arg.name]) in
        hmInsert name link acc
    ) ctx.searchDatas renderingRes.searchDatas in
    
    (if neqString ctx.opt.outputFolder ctx.userOutputFolder then    
        let newStdlibPath = normalizePath (join [ctx.opt.outputFolder, "/", "Stdlib"]) in
        let actualStdlibPath = normalizePath (join [ctx.userOutputFolder, "/", "Stdlib"]) in

        let code = sysRemoveSrcFiles ctx.opt.outputFolder in
        (if neqi code 0 then renderingWarn "Failed to clean source files." else ());

        if isFolder newStdlibPath then
            let code = sysMoveDirContents actualStdlibPath newStdlibPath in
            if neqi code 0 then renderingWarn "Failed to move Stdlib contents." else ()
        else ()
    else ());

    { ctx with searchDatas = searchDatas }
    else crash "object" "render" "label (or extract)"

let serve : Step = use ObjectsRenderer in lam ctx.
    match ctx.object with Some obj then
    let log = buildLogger ctx "Serving" in
    let opt = getRenderingOption ctx.opt log in
    let link = objLink (objTreeObj obj) opt in
    let opt = getServeOption ctx.opt link in    
    startServer opt; ctx
    else crash "object" "serve" "render"
