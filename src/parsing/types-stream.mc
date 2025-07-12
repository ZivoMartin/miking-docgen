include "mexpr/keywords.mc"
include "mexpr/ast.mc"
include "../util.mc"
include "map.mc"
include "pmexpr/demote.mc"
include "ocaml/external.mc"
include "mexpr/type-check.mc"
include "map.mc"
include "../logger.mc"

lang TypeStreamInterface = MExprAst
    
    type TypeStreamContext = { stack: [Expr] }
    type TypeStreamNextResult = { ctx: TypeStreamContext, t: Option Type, skiped: [{ name: String, t: Type }] }
    
        
    sem typeStreamNext : String -> TypeStreamContext -> TypeStreamNextResult
    sem typeStreamNext name =
    | { stack = [_] ++ stack } ->
        typeStreamNext name { stack = stack }
    | { stack = [] } & ctx -> { t = None {}, ctx = ctx, skiped = [] }

    sem checkAndEnd name ident t =
    | ctx -> if eqString name ident.0 then
            { t = Some t, ctx = ctx, skiped = [] }
        else
             printLn (concatAll ["skiped: ", ident.0, " ", name]);
            let res = typeStreamNext name ctx in
            { res with skiped = cons { name = ident.0, t = t } res.skiped }
end


-- TmLet --
lang LetTypeStream = TypeStreamInterface
  sem typeStreamNext name =
  | { stack = [TmLet { ident = ident, body = body, ident = ident, inexpr = inexpr }] ++ stack } & ctx ->
        let ctx = { ctx with stack = concat [body, inexpr] stack } in
        checkAndEnd name ident (tyTm body) ctx

end


-- TmRecLets --
lang RecLetsTypeStream = TypeStreamInterface

    sem typeStreamNext name =
    | { stack = [TmRecLets { bindings = [], inexpr = inexpr }] ++ stack } & ctx ->
        typeStreamNext name { stack = cons inexpr stack }
    | { stack = ([TmRecLets { bindings = [b] ++ bindings }] ++ stack) & ([TmRecLets tm] ++ stack) } & ctx ->
        let ctx = { ctx with stack = concat [b.body, TmRecLets { tm with bindings = bindings } ] stack } in
        checkAndEnd name b.ident (b.tyBody) ctx
end

    
lang AppTypeStream = TypeStreamInterface

  sem typeStreamNext name =
  | { stack = [TmApp { lhs = lhs, rhs = rhs }] ++ stack } ->

    typeStreamNext name { stack = concat [lhs, rhs] stack }

end


lang SeqTypeStream = TypeStreamInterface

  sem typeStreamNext name =
  | { stack = [TmSeq { tms = tms }] ++ stack } ->

    typeStreamNext name { stack = concat tms stack }

end


lang RecordTypeStream = TypeStreamInterface
    
    sem typeStreamNext name =
      | { stack = [TmRecord { bindings = bindings }] ++ stack } ->

        typeStreamNext name { stack = concat (mapValues  bindings) stack }
      | { stack = [TmRecordUpdate { rec = rec, value = value }] ++ stack } ->

        typeStreamNext name { stack = concat [rec, value] stack }

end


lang MatchTypeStream = TypeStreamInterface

  sem typeStreamNext name =
  | { stack = [TmMatch { target = target, thn = thn, els = els }] ++ stack } ->

    typeStreamNext name { stack = concat [target, thn, els] stack }
    
end


-- TmUtest --
lang UtestTypeStream = TypeStreamInterface
    
  sem typeStreamNext name =
  | { stack = [TmUtest { next = next }] ++ stack }  ->

    typeStreamNext name { stack = cons next stack }

end    
    
-- TmConDef and TmConApp --
lang SimpleSkip = TypeStreamInterface
    
  sem typeStreamNext name =
  | { stack =
          [TmConDef { inexpr = inexpr } 
        | TmConApp { body = inexpr }
        | TmLam { body = inexpr }
        | TmType { inexpr = inexpr }
        | TmExt { inexpr = inexpr }] ++ stack } & ctx ->

            typeStreamNext name { ctx with stack = cons inexpr stack }

end
    

lang TypeStream = AppTypeStream + LetTypeStream + RecLetsTypeStream + SeqTypeStream + RecordTypeStream + MatchTypeStream + UtestTypeStream + PMExprDemote + BootParser + SimpleSkip

    
    sem buildTypeStream : String -> TypeStreamContext
    sem buildTypeStream = | file ->
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
        let ast =
          removeMetaVarExpr
            (typeCheckExpr
               {typcheckEnvDefault with
                disableConstructorTypes = true}
               ast)
        in
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
in
--displayAst ast;
    printLn (expr2str ast);
    { stack = [ast] }

end 
    
