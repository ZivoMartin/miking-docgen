include "mexpr/keywords.mc"
include "pmexpr/demote.mc"
include "ocaml/external.mc"
include "mexpr/type-check.mc"
include "mexpr/ast.mc"
include "mexpr/info.mc"
include "sys.mc"

include "./include-set.mc"
include "./token-readers.mc"

include "../global/util.mc"
include "../global/logger.mc"

type Ast = use MExprAst in {
     expr: Expr
}


let buildAstFromFile: String -> Ast = use PMExprDemote in use BootParser in use TokenReader in lam file.

    recursive let astConcat : Expr -> Expr -> Expr = lam left. lam right.
        switch left
        case TmLet d then TmLet { d with inexpr = astConcat d.inexpr right }
        case TmRecLets d then TmRecLets { d with inexpr = astConcat d.inexpr right }
        case TmType d then TmType { d with inexpr = astConcat d.inexpr right }
        case TmConDef d then TmConDef { d with inexpr = astConcat d.inexpr right }
        case TmExt d then TmExt { d with inexpr = astConcat d.inexpr right }
        case TmUtest d then TmUtest { d with next = astConcat d.next right }
        case TmRecord {} then right
        case _ then parsingWarn (join ["The leaf of the given ast is not a unit, ", expr2str left]); right
        end
    in

    let externalsExclude = mapKeys (externalGetSupportedExternalImpls ()) in
    let parseOpt = {{{{{{ defaultBootParserParseMCoreFileArg
          with keepUtests = true }
          with pruneExternalUtests = false }
          with externalsExclude = externalsExclude }
          with pruneExternalUtestsWarning = true }
          with eliminateDeadCode = false }
          with keywords = mexprExtendedKeywords } in

    let pos0 = { x = 0, y = 0 } in

    type Arg = { acc: Expr, includeSet: IncludeSet } in

    recursive let work : Arg -> String -> Arg = lam arg. lam file.
        parsingLog (join ["Generating ast for the file ", file, "."]);

        match arg with { acc = acc, includeSet = includeSet } in
        let s = readOrNever file in
        
        type SplitRes = { topIncludes: [String], rest: String } in
        
        recursive let split : String -> SplitRes = lam s.
            match next s pos0 with { stream = stream, token = token } in
            switch token
            case Comment {} | MultiLigneComment {} | Separator {} then split stream
            case Include { content = content} then
                 let res = split stream in
                 { res with topIncludes = cons content res.topIncludes }
            case _ then { rest = s, topIncludes = [] }
            end
        in

        match split s with { topIncludes = topIncludes, rest = rest } in

        let tmpFile = sysTempFileMake () in
        match fileWriteOpen tmpFile with Some wc then
            fileWriteString wc rest;
            fileWriteFlush wc;

            let ast = parseMCoreFile parseOpt tmpFile in
            printLn (expr2str ast);
            
            sysDeleteFile tmpFile;

            let arg = { arg with acc = astConcat ast acc } in
    
            foldl (lam arg. lam topInclude.
                match includeSetInsert arg.includeSet file topInclude with
                { includeSet = includeSet, inserted = inserted, path = path } in
                if inserted then work { arg with includeSet = includeSet} path
                else arg
            ) arg topIncludes

        else error "Failed to create temporary file."

    in

    match goHere (sysGetCwd()) file with { path = basePos } in
    let includeSet = includeSetNew (dirname basePos) in

    let unit = TmSeq {
      tms = [],
      ty = TyUnknown { info = NoInfo () },
      info = NoInfo ()
    } in

    match work { acc = unit, includeSet = includeSet } basePos with { acc = ast } in
    let ast = symbolize ast in
    let ast = typeCheckExpr { typcheckEnvDefault with disableConstructorTypes = true} ast in

    { expr = ast }
