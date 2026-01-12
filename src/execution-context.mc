include "./options/docgen-options.mc"
include "./options/cast-options.mc"
include "./scanning/scanner.mc"
include "./mast-gen/mast-generator.mc"
include "./parsing/parser.mc"
include "./naming/namer.mc"
include "./rendering/renderer.mc"
include "./server/server.mc"

type ExecutionContext =
    use TokenReader in use Objects in {
    opt: DocGenOptions,
    userOutputFolder: String,
    currentFile: String,
    files: [FileToProcess],
    longestPrefix: String,
    renderedMap: RenderedMap,

    tokens: [Token],
    ast: Option MAst,
    object: Option Object,
    nameContext: Option NameContext,
    searchDatas: HashMap String String
}

let buildLogger : ExecutionContext -> String -> Logger =
    lam ctx. lam step.
    if ctx.opt.debug then message "INFO" step else lam. ()

let execCtxNext : ExecutionContext -> Option ExecutionContext = use Renderer in lam ctx.
    match ctx.files with [{ path = path, outputFolder = outputFolder }] ++ files then
          printLn (join ["Processing file ", path, "..."]);
          Some { ctx with
              opt = { ctx.opt with outputFolder = outputFolder },
              currentFile = path,
              files = files,
              tokens = [],
              ast = None {},
              object = None {},
              nameContext = None {}
          }
    else

        -- Creating search engine
        let log = buildLogger ctx "Rendering" in 
        let ropt = getRenderingOption ctx.opt log (nameContextEmpty ()) (hashmapEmpty ()) in
        let ropt = { ropt with outputFolder = ctx.userOutputFolder } in
        let searchDatas = map (lam entry. { name = entry.0, link = entry.1 })
                          (hashmap2seq ctx.searchDatas) in
        renderSearchFile searchDatas ropt;
        printLn "Done!";
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
        renderedMap = renderedMapEmpty (),

        tokens = [],
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

let parse : Step = lam ctx.
    match ctx.ast with Some ast then
    let log = buildLogger ctx "Parsing" in
    let opt = getParsingOptions log ctx.currentFile ctx.longestPrefix in
    let obj = parse opt ast in
    { ctx with object = Some obj }
    else crash "ast" "parse" "gen"

let name : Step =  lam ctx.
    match ctx.object with Some object then
    let log = buildLogger ctx "Naming" in
    let opt = getNamingOption ctx.opt in
    match name log opt object with {
        annotatedObj = annotatedObj,
        nameContext = nameContext
    } in
    { ctx with nameContext = Some nameContext, object = Some annotatedObj }
    else crash "object" "name" "extract"

let render : Step =  lam ctx.
    match ctx.object with Some obj then
    match ctx.nameContext with Some nameContext then
    
    let log = buildLogger ctx "Rendering" in 
    let ropt = getRenderingOption ctx.opt log nameContext ctx.renderedMap in
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

    { ctx with searchDatas = searchDatas, renderedMap = renderingRes.renderedMap }
    else crash "object" "render" "naming"
    else crash "name context" "render" "naming"

let serve : Step = use ObjectsRenderer in lam ctx.
    match ctx.object with Some obj then
    match ctx.nameContext with Some nameContext then
    let log = buildLogger ctx "Serving" in
    let opt = getRenderingOption ctx.opt log nameContext (hashmapEmpty ()) in
    let link = objGetMyLink obj opt in

    let opt = getServeOption ctx.opt link in    
    startServer opt; ctx
    else crash "object" "serve" "render"
    else crash "name context" "serve" "name"    
