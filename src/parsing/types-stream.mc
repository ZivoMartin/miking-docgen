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
    type TypeStreamNextResult = { ctx: TypeStreamContext, t: Option Type }
    
        
    sem typeStreamNext : TypeStreamContext -> TypeStreamNextResult
    sem typeStreamNext =
    | { stack = [_] ++ stack } ->
        typeStreamNext { stack = stack }
    | { stack = [] } & ctx -> { t = None {}, ctx = ctx }
end


-- TmLet --
lang LetTypeStream = TypeStreamInterface
  sem typeStreamNext =
  | { stack = [TmLet { body = body, ident = ident, inexpr = inexpr }] ++ stack } & ctx ->

        { t = Some (tyTm body), ctx = { ctx with stack = concat [body, inexpr] stack } }

end


-- TmRecLets --
lang RecLetsTypeStream = TypeStreamInterface

    sem typeStreamNext =
    | { stack = [TmRecLets { bindings = [], inexpr = inexpr }] ++ stack } & ctx ->

    typeStreamNext { stack = cons inexpr stack }
    | { stack = ([TmRecLets { bindings = [b] ++ bindings }] ++ stack) & ([TmRecLets tm] ++ stack) } & ctx ->

        { t = Some b.tyBody, ctx = { ctx with stack = concat [b.body, TmRecLets { tm with bindings = bindings } ] stack } }

end

    
lang AppTypeStream = TypeStreamInterface

  sem typeStreamNext =
  | { stack = [TmApp { lhs = lhs, rhs = rhs }] ++ stack } ->

    typeStreamNext { stack = concat [lhs, rhs] stack }

end


lang SeqTypeStream = TypeStreamInterface

  sem typeStreamNext =
  | { stack = [TmSeq { tms = tms }] ++ stack } ->

    typeStreamNext { stack = concat tms stack }

end


lang RecordTypeStream = TypeStreamInterface
    
    sem typeStreamNext =
      | { stack = [TmRecord { bindings = bindings }] ++ stack } ->

        typeStreamNext { stack = concat (mapValues  bindings) stack }
      | { stack = [TmRecordUpdate { rec = rec, value = value }] ++ stack } ->

        typeStreamNext { stack = concat [rec, value] stack }

end


lang MatchTypeStream = TypeStreamInterface

  sem typeStreamNext =
  | { stack = [TmMatch { target = target, thn = thn, els = els }] ++ stack } ->

    typeStreamNext { stack = concat [target, thn, els] stack }
    
end


-- TmUtest --
lang UtestTypeStream = TypeStreamInterface
    
  sem typeStreamNext =
  | { stack = [TmUtest { next = next }] ++ stack }  ->

    typeStreamNext { stack = cons next stack }

end    
    
-- TmConDef and TmConApp --
lang SimpleSkip = TypeStreamInterface
    
  sem typeStreamNext =
  | { stack =
          [TmConDef { inexpr = inexpr } 
        | TmConApp { body = inexpr }
        | TmLam { body = inexpr }
        | TmType { inexpr = inexpr }
        | TmExt { inexpr = inexpr }] ++ stack } & ctx ->

            typeStreamNext { ctx with stack = cons inexpr stack }

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
        { stack = [ast] }

end 
    
