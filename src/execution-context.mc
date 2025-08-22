include "./options/options.mc"
include "./options/cast-options.mc"
include "./mast-gen/mast-generator.mc"
include "./parsing/parser.mc"
include "./extracting/extracter.mc"
include "./labeling/labeler.mc"
include "./rendering/renderer.mc"
include "./server/server.mc"

type ExecutionContext =  use TokenReader in {    
    opt: Options,
    mainFile: String,
    tokens: [Token],
    docTree : Option DocTree,
    ast: Option Ast,
    object: Option ObjectTree
}

let execContextNew : () -> ExecutionContext = lam.
    let opt = parseOptions argv in
    optLog opt;
    if sysFileExists opt.file then
    {
        opt = opt,
        mainFile = opt.file,
        tokens = [],
        docTree = None {},
        object = None {},
        ast = None {}
    }
    else error (join ["The file ", opt.file, "doesn't exist."])

let crash = lam miss. lam func. lam should.
    error (join ["Execution context: ", miss, " is missing in the exection context, ", func, " function should be called after having call the ", should, " function."])
    
type Step = ExecutionContext -> ExecutionContext

let gen : Step = lam ctx.
    { ctx with ast = Some (buildAstFromFile ctx.mainFile) }

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
    let opt = getRenderingOption ctx.opt in
    render opt obj; ctx
    else crash "object" "render" "label (or extract)"

let serve : Step = use ObjectsRenderer in lam ctx.
    match ctx.object with Some obj then
    let opt = getRenderingOption ctx.opt in
    let link = objLink (objTreeObj obj) opt in
    let opt = getServeOption ctx.opt link in    
    startServer opt; ctx
    else crash "object" "serve" "render"
