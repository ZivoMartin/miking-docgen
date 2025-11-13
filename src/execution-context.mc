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
-- - `render`  : Generate documentation files.
-- - `serve`   : Start preview server.
--
-- If a step is called out of order, the `crash` function raises an error with details.


include "./options/options.mc"
include "./options/cast-options.mc"
include "./mast-gen/mast-generator.mc"
include "./parsing/parser.mc"
include "./extracting/extracter.mc"
include "./labeling/labeler.mc"
include "./rendering/renderer.mc"
include "./server/server.mc"

type ExecutionContext =  use TokenReader in {    
<<<<<<< Updated upstream
    opt: Options,
    mainFile: String,
=======
    opt: DocGenOptions,
    userOutputFolder: String,
    currentFile: String
    files: [{ path: String, outputFolder: String }],
>>>>>>> Stashed changes
    tokens: [Token],
    docTree : Option DocTree,
    ast: Option MAst,
    object: Option ObjectTree
}

<<<<<<< Updated upstream
let execContextNew : () -> ExecutionContext = lam.
    let opt = parseOptions argv in
    optLog opt;
    if sysFileExists opt.file then
    {
=======
let execCtxNext : ExecutionContext -> Option ExecutionContext = lam ctx.
    match ctx.files with [{ path = path, outputFolder = outputFolder }] ++ files then
          Some { ctx with
              opt = { ctx.opt with outputFolder = outputFolder }
              currentFile = path,
              files = files,
              tokens = [],
              docTree = None {},
              ast = None {},
              object = None {}
          }
    else None {}

let execContextNew : DocGenOptions -> ExecutionContext = lam opt.
    let files = if sysIfFolder opt.file then
       let files = folderFetchMcFiles opt.file in
       map (lam path.
           strSplit opt.file path with [_] ++ t then
           let f = strJoin opt.file t in
           let outputFolder = normalizePath (join [opt.outputFolder, "/", f]) in
           { path = path, outputFolder = dirname outputFolder }) files
    else if sysFileExists opt.file then
       [{ path = f, outputFolder = opt.outputFolder }]
    else error (join ["The file ", opt.file, "doesn't exist."]) in
    let ctx = {
>>>>>>> Stashed changes
        opt = opt,
        currentFile = "",
        userOutputFolder = opt.outputFolder,
        files = files,
        tokens = [],
        docTree = None {},
        object = None {},
        ast = None {}
    }
    match execCtxNext ctx with Some ctx then ctx else
    error "Please provide a file to process."
    

let crash = lam miss. lam func. lam should.
    error (join ["Execution context: ", miss, " is missing in the exection context, ", func, " function should be called after having call the ", should, " function."])
    
type Step = ExecutionContext -> ExecutionContext

let gen : Step = lam ctx.
    { ctx with ast = Some (buildMAstFromFile ctx.mainFile) }

let parse : Step =  lam ctx.
    match ctx.ast with Some ast then
    { ctx with docTree = Some (parse ctx.mainFile ast ) }
    else crash "ast" "parse" "gen"
    
let extract : Step =  lam ctx.
    match ctx.docTree with Some docTree then
    { ctx with object = Some (extract docTree ) }
    else crash "doc tree" "extract" "parse"

let label : Step =  lam ctx.
    match (ctx.object, ctx.ast) with (Some object, Some ast) then
    
    { ctx with object = Some (label object ast) }
    else crash "object" "label" "extract"

let render : Step =  lam ctx.
    match ctx.object with Some obj then
<<<<<<< Updated upstream
    let opt = getRenderingOption ctx.opt in
    render opt obj; ctx
=======
    let log = buildLogger ctx "Rendering" in 
    let opt = getRenderingOption ctx.opt log in
    let searchDatas  render opt obj in
    ctx
>>>>>>> Stashed changes
    else crash "object" "render" "label (or extract)"

let serve : Step = use ObjectsRenderer in lam ctx.
    match ctx.object with Some obj then
    let opt = getRenderingOption ctx.opt in
    let link = objLink (objTreeObj obj) opt in
    let opt = getServeOption ctx.opt link in    
    startServer opt; ctx
    else crash "object" "serve" "render"
