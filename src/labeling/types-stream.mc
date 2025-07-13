include "mexpr/keywords.mc"
include "map.mc"
include "pmexpr/demote.mc"
include "ocaml/external.mc"
include "mexpr/type-check.mc"
include "map.mc"

include "../logger.mc"
include "../util.mc"
    
lang TypeStreamInterface = MExprAst
    
    type TypeStreamContext = { stack: [Expr] }
    type TypeStreamNextResult = { ctx: TypeStreamContext, t: Option Type, skiped: [{ name: String, body: Expr, t: Type }] }
        
    sem typeStreamNext : String -> TypeStreamContext -> TypeStreamNextResult
    sem typeStreamNext name =
    | { stack = [_] ++ stack } ->
        typeStreamNext name { stack = stack }
    | { stack = [] } & ctx -> { t = None {}, ctx = ctx, skiped = [] }

    sem typeStreamPop : TypeStreamContext -> { ctx: TypeStreamContext, ast: Expr }
    sem typeStreamPop =
        | { stack = [ast] ++ stack } & ctx -> { ast = ast, ctx = { ctx with stack = stack }}
    
    sem checkAndEnd name ident t body =
    | ctx -> if eqString name ident.0 then
            { t = Some t, ctx = ctx, skiped = [] }
        else
            let res = typeStreamNext name ctx in
            { res with skiped = cons { name = ident.0, body = body, t = t } res.skiped }
end


-- TmLet --
lang LetTypeStream = TypeStreamInterface
  sem typeStreamNext name =
  | { stack = [TmLet { ident = ident, body = body, ident = ident, inexpr = inexpr, tyBody = tyBody }] ++ stack } & ctx ->
        let ctx = { ctx with stack = concat [body, inexpr] stack } in
        checkAndEnd name ident tyBody body ctx

end


-- TmRecLets --
lang RecLetsTypeStream = TypeStreamInterface

    sem typeStreamNext name =
    | { stack = [TmRecLets { bindings = [], inexpr = inexpr }] ++ stack } & ctx ->
        typeStreamNext name { stack = cons inexpr stack }
    | { stack = ([TmRecLets { bindings = [b] ++ bindings }] ++ stack) & ([TmRecLets tm] ++ stack) } & ctx ->
        let ctx = { ctx with stack = concat [b.body, TmRecLets { tm with bindings = bindings } ] stack } in
        checkAndEnd name b.ident (b.tyBody) b.body ctx
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
    sem typeStreamFromExpr : Expr -> TypeStreamContext 
    sem typeStreamFromExpr =
        | ast -> { stack = [ast] }
    
    sem buildTypeStream : String -> TypeStreamContext
    sem buildTypeStream = | file ->
        let externalsExclude = mapKeys (externalGetSupportedExternalImpls ()) in
        labelingLog "Genarating ast...";
        let ast = parseMCoreFile {{{{{{ defaultBootParserParseMCoreFileArg
          with keepUtests = false }
          with pruneExternalUtests = true }
          with externalsExclude = externalsExclude }
          with pruneExternalUtestsWarning = false }
          with eliminateDeadCode = false }
          with keywords = mexprExtendedKeywords } file in
        
        labelingLog "Symbolizing ast...";
        let ast = symbolize ast in

        labelingLog "Typing ast...";
        let ast = typeCheckExpr { typcheckEnvDefault with disableConstructorTypes = true} ast in

--        printLn (expr2str ast);
        { stack = [ast] }
end 
    
