include "mexpr/keywords.mc"
include "pmexpr/demote.mc"
include "ocaml/external.mc"
include "mexpr/type-check.mc"
include "mexpr/ast.mc"
include "mexpr/info.mc"
include "sys.mc"

include "./include-set.mc"
include "./file-opener.mc"
include "./ast.mc"

include "../global/util.mc"
include "../global/logger.mc"

include "../execution-context.mc"


let buildAstFromFile: String -> Ast = use PMExprDemote in use BootParser in use TokenReader in lam file.

    let externalsExclude = mapKeys (externalGetSupportedExternalImpls ()) in
    let parseOpt = {{{{{{{ defaultBootParserParseMCoreFileArg
          with keepUtests = true }
          with allowFree = true }
          with pruneExternalUtests = false }
          with externalsExclude = externalsExclude }
          with pruneExternalUtestsWarning = true }
          with eliminateDeadCode = false }
          with keywords = mexprExtendedKeywords } in
    
    let pos0 = { x = 0, y = 0 } in

    type Arg = { acc: [String], includeSet: IncludeSet String } in

    recursive let work : Arg -> String -> Arg = lam arg. lam file.
        parsingLog (join ["Generating ast for the file ", file, "."]);

        match arg with { acc = acc, includeSet = includeSet } in

        let removeMexpr : String -> String = lam s.
            recursive let removeMexpr : String -> String -> String = lam s. lam acc.
                match next s pos0 with { stream = stream, token = token } in
                switch token
                case Eof {} then acc
                case Word { content = "mexpr"} then removeMexpr stream (concat (reverse (join ["let #var\"\" = "])) acc)
                case _ then removeMexpr stream (concat (reverse (lit token)) acc)
                end
            in
            removeMexpr s ""
        in        

        match parsingOpenFile file with { includes = topIncludes, fileText = rest } in
        
        let rest = removeMexpr rest in

        let arg = foldl (lam arg. lam topInclude.
            
            match includeSetInsert arg.includeSet file topInclude "" with
            { includeSet = includeSet, inserted = inserted, path = path } in
            if inserted then work { arg with includeSet = includeSet} path
            else arg
        ) arg topIncludes in

        { arg with acc = cons rest arg.acc } 

    in

    let includeSet = includeSetNew (dirname file) in

    let unit = TmSeq {
      tms = [],
      ty = TyUnknown { info = NoInfo () },
      info = NoInfo ()
    } in

    match work { acc = [], includeSet = includeSet } file with { acc = code, includeSet = includeSet } in
    let code = reverse (join code) in
    let tmpFile = sysTempFileMake () in
    match fileWriteOpen tmpFile with Some wc then
        fileWriteString wc code;
        fileWriteFlush wc;

        parsingLog "Parsing final ast";
        let ast = parseMCoreFile parseOpt tmpFile in
        -- printLn (expr2str ast);
        parsingLog "Symbolizing final ast";
        let ast = symbolize ast in

        parsingLog "Type checking final ast";
        let ast = typeCheckExpr { typcheckEnvDefault with disableConstructorTypes = true} ast in

        { expr = ast, includeSet = includeSet }
    else error "Failed to create temporary file."


let gen : ExecutionContext -> ExecutionContext = lam execCtx.
    { execCtx with ast = Some (buildAstFromFile execCtx.mainFile) }    
