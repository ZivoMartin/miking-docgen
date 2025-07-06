include "mexpr/keywords.mc"
include "parse.mc"
include "mexpr/ast.mc"
include "../util.mc"
include "map.mc"
include "pmexpr/demote.mc"
include "mexpr/boot-parser.mc"
include "ocaml/external.mc"
include "mexpr/type-check.mc"

type TTree
con TTreeNode : { sons: [TTree], tp: String } -> TTree

recursive let displayAst = use MExprAst in lam ast.
    switch ast 
    case TmVar { ident = ident } then
        printLn (concat "var " ident.0)
    case TmLet { ident = ident, body = body, inexpr = inexpr } then
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
    case TmRecLets { bindings = bindings, inexpr = inexpr } then
        printLn "rec";
        iter (lam b. displayAst b.body) bindings;
        displayAst inexpr
    case TmConst {} then
        printLn "const"
    case TmRecord { bindings = bindings } then
        printLn "record";
        iter displayAst (mapValues bindings)
    case TmRecordUpdate { rec = rec, value = value } then
        printLn "record update";
        displayAst rec;
        displayAst value
    case TmSeq { tms = tms } then
      printLn "seq ";
      iter displayAst tms
    case TmType { ident = ident, ty = _ty, inexpr = inexpr } then
        printLn (concat "type " ident.0);
        displayAst inexpr
    case TmConDef { ident = ident, ty = _ty, inexpr = inexpr } then
        printLn (concat "con def " ident.0);
        displayAst inexpr
    case TmConApp { ident = ident, body = body } then
        printLn (concat "con app " ident.0);
        displayAst body
    case TmMatch { target = target, thn = thn, els = els } then
        printLn "match";
        displayAst target;
        displayAst thn;
        displayAst els
    case TmUtest { test = test, expected = expected, next = next } then
        printLn "utest";
        displayAst test;
        displayAst expected;
        displayAst next
    case TmPlaceholder {} then
        printLn "placeholder"
    case TmExt {inexpr = inexpr } then
        printLn "external";
        displayAst inexpr
    case TmNever {} then
        printLn "never"
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
    let ast = symbolize ast in
    displayAst ast
