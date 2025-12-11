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
include "./scanning/scanner.mc"
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
    files: [FileToProcess],
    longestPrefix: String,

    tokens: [Token],
    docTree : Option DocTree,
    ast: Option MAst,
    searchDatas: HashMap String String,
    object: Option ObjectTree,
    nameContext: Option NameContext
}

let buildLogger : ExecutionContext -> String -> Logger =
    lam ctx. lam step.
    if ctx.opt.debug then message "INFO" step else lam. ()

let execCtxNext : ExecutionContext -> Option ExecutionContext = use Renderer in lam ctx.
    match ctx.files with [{ path = path, outputFolder = outputFolder }] ++ files then
          printLn outputFolder;
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

        -- Creating search engine
        let log = buildLogger ctx "Rendering" in 
        let ropt = getRenderingOption ctx.opt log (nameContextEmpty ()) in
        let ropt = { ropt with outputFolder = ctx.userOutputFolder } in
        let searchDatas = map (lam entry. { name = entry.0, link = entry.1 })
                          (hashmap2seq ctx.searchDatas) in
        renderSearchFile searchDatas ropt;

        None {}

let execContextNew : DocGenOptions -> Option ExecutionContext = lam opt.
    
    let scanningOptions = getScanningOptions opt in
    match scan scanningOptions with {
        inputs = files,
        longestPrefix = longestPrefix,
        onlyStdlib = onlyStdlib
    } in

    let opt = if onlyStdlib then { opt with stdlibFolder = "" } else opt in

    let ctx = {
        opt = opt,
        currentFile = "",
        userOutputFolder = opt.outputFolder,
        longestPrefix = longestPrefix,
        files = files,
        tokens = [],
        docTree = None {},
        object = None {},
        ast = None {},
        nameContext = None {},
        searchDatas = hashmapEmpty ()
    } in
    execCtxNext ctx

let crash = lam miss. lam func. lam should.
    error (join ["Execution context: ", miss, " is missing in the exection context, ", func, " function should be called after having call the ", should, " function."])
    
type Step = ExecutionContext -> ExecutionContext

let gen : Step = lam ctx.
    let log = buildLogger ctx "MExpr Generation" in
    let mast = buildMAstFromFile log ctx.currentFile in
    { ctx with ast = Some mast }

let parse : Step =  lam ctx.
    match ctx.ast with Some ast then
    let log = buildLogger ctx "Parsing" in
    { ctx with docTree = Some (parse log ctx.currentFile ast ) }
    else crash "ast" "parse" "gen"
    
let extract : Step =  lam ctx.
    match ctx.docTree with Some docTree then
    let log = buildLogger ctx "Extracting" in 
    let opt = getExtractingOption ctx.opt (pathIsInStdlib ctx.currentFile) ctx.longestPrefix log in
    { ctx with object = Some (extract opt docTree ) }
    else crash "doc tree" "extract" "parse"

let label : Step =  lam ctx.
    match (ctx.object, ctx.ast) with (Some object, Some ast) then
    let log = buildLogger ctx "Labeling" in    
    { ctx with object = Some (label log ctx.longestPrefix object ast) }
    else crash "object" "label" "extract"

let name : Step =  lam ctx.
    match ctx.object with Some object then
    let log = buildLogger ctx "Naming" in
    let opt = getNamingOption ctx.opt in
    match name log opt object with {
        annotatedObjTree = annotatedObjTree,
        nameContext = nameContext
    } in
    { ctx with nameContext = Some nameContext, object = Some annotatedObjTree }
    else crash "object" "name" "extract"

let render : Step =  lam ctx.
    match ctx.object with Some obj then
    match ctx.nameContext with Some nameContext then
    
    let log = buildLogger ctx "Rendering" in 
    let ropt = getRenderingOption ctx.opt log nameContext in
    let renderingRes = render ropt obj in

    let searchDatas = foldl (lam acc. lam arg.
        let prefix = normalizePath (join [ctx.opt.urlPrefix, "/", ctx.opt.stdlibFolder]) in
        let isStdlib = strStartsWith prefix arg.link in

        let prefix = tail (strSplit ctx.userOutputFolder ctx.opt.outputFolder) in
        let prefix = if isStdlib then "" else join prefix in
        
        let link = normalizePath (join [prefix, "/", arg.link]) in
        let name = normalizePath (join [prefix, "/", arg.name]) in
        hmInsert name link acc
    ) ctx.searchDatas renderingRes.searchDatas in
    
    (if neqString ctx.opt.outputFolder ctx.userOutputFolder then    
        let newStdlibPath = normalizePath (join [ctx.opt.outputFolder, "/", ctx.opt.stdlibFolder]) in
        let actualStdlibPath = normalizePath (join [ctx.userOutputFolder, "/", ctx.opt.stdlibFolder]) in

        let code = sysRemoveSrcFiles ctx.opt.outputFolder in
        (if neqi code 0 then renderingWarn "Failed to clean source files." else ());

        if isFolder newStdlibPath then
            let code = sysMoveDirContents actualStdlibPath newStdlibPath in
            if neqi code 0 then renderingWarn "Failed to move Stdlib contents." else ()
        else ()
    else ());

    { ctx with searchDatas = searchDatas }
    else crash "object" "render" "naming"
    else crash "name context" "render" "naming"

let serve : Step = use ObjectsRenderer in lam ctx.
    match ctx.object with Some obj then
    match ctx.nameContext with Some nameContext then
    let log = buildLogger ctx "Serving" in
    let opt = getRenderingOption ctx.opt log nameContext in
    let link = objGetMyLink (objTreeObj obj) opt in

    let opt = getServeOption ctx.opt link in    
    startServer opt; ctx
    else crash "object" "serve" "render"
    else crash "name context" "serve" "name"    
