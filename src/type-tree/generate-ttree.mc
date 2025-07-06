include "mexpr/keywords.mc"
include "parse.mc"
include "mexpr/ast.mc"
include "../util.mc"
include "map.mc"
include "pmexpr/demote.mc"
include "mexpr/boot-parser.mc"
include "ocaml/external.mc"

type TTree
con TTreeNode : { sons: [TTree], tp: String } -> TTree

recursive let displayAst = use MExprAst in lam ast.
    switch ast
    case TmVar { ident = ident } then printLn (concat "var " ident.0)
    case TmLet { ident = ident, body = body, inexpr = inexpr} then
        printLn (concat "let " ident.0);
        displayAst body;
        displayAst inexpr
    case TmApp { lhs = lhs, rhs = rhs } then
        printLn "app";
        displayAst lhs;
        displayAst rhs
    case TmLam { ident = ident, body = body } then
        printLn (concat "lam " ident.0);
        displayAst body
    case TmRecLets {} then error "rec"
    case TmConst {} then printLn "const"
    case TmSeq { tms = tms } then
        print "[ ";
        iter displayAst tms;
        printLn " ]"
    case TmRecord {bindings = bindings } then iter displayAst (mapValues bindings)
    case TmRecordUpdate {} then error "req update"
    case TmType {} then printLn "type"
    case TmConDef {} then error "con def"
    case TmConApp {} then error "con app"
    case TmMatch { target = target, thn = thn, els = els } then
        displayAst target;
        displayAst thn;
        displayAst els        
    case TmUtest {} then error "utest"
    case TmPlaceholder {} then error "plaecholder"
    case TmExt {} then error "ext"
    case TmNever {} then error "never"
    end 
end

    
let generateTypeTree = use PMExprDemote in use BootParser in lam file.
    let externalsExclude = mapKeys (externalGetSupportedExternalImpls ()) in
    let ast = parseMCoreFile {{{{{{ defaultBootParserParseMCoreFileArg
      with keepUtests = true }
      with pruneExternalUtests = true }
      with externalsExclude = externalsExclude }
      with pruneExternalUtestsWarning = false }
      with eliminateDeadCode = false }
      with keywords = mexprExtendedKeywords } file in
    
    let ast = makeKeywords ast in
    let ast = demoteParallel ast in
    let ast =
      removeMetaVarExpr
        (typeCheckExpr
           {typcheckEnvDefault with
            disableConstructorTypes = false}
           ast) in
    displayAst ast
