include "mexpr/keywords.mc"
include "mexpr/ast.mc"
include "../util.mc"
include "map.mc"
include "pmexpr/demote.mc"
include "mexpr/boot-parser.mc"
include "ocaml/external.mc"
include "mexpr/type-check.mc"
include "map.mc"

lang TypeStreamInterface = MExprAst

    
    
    type TypeStreamContext = { stack: [Expr] }
    type TypeStreamNextResult = { ctx: TypeStreamContext, t: Option Type }
    
        
    sem typeStreamNext : TypeStreamContext -> TypeStreamNextResult
    sem typeStreamNext =
    | { stack = [_] ++ stack } -> typeStreamNext { stack = stack }
    | { stack = [] } & ctx -> { t = None {}, ctx = ctx }
end


-- TmLet --
lang LetTypeStream = TypeStreamInterface
  sem typeStreamNext =
  | { stack = [TmLet { ty = ty, body = body, inexpr = inexpr }] ++ stack } & ctx -> { t = Some ty, ctx = { ctx with stack = concat [body, inexpr] stack } }

end


-- TmRecLets --
lang RecLetsTypeStream = TypeStreamInterface

    sem typeStreamNext =
    | { stack = [TmRecLets { bindings = bindings, inexpr = inexpr, ty = ty }] ++ stack } & ctx -> error "todo"

end

    
lang AppTypeStream = TypeStreamInterface

  sem typeStreamNext =
  | { stack = [TmApp { lhs = lhs, rhs = rhs }] ++ stack } -> typeStreamNext { stack = concat [lhs, rhs] stack }

end


lang SeqTypeStream = TypeStreamInterface

  sem typeStreamNext =
  | { stack = [TmSeq { tms = tms }] ++ stack } -> typeStreamNext { stack = concat tms stack }

end


lang RecordTypeStream = TypeStreamInterface
    
    sem typeStreamNext =
      | { stack = [TmRecord { bindings = bindings }] ++ stack } -> typeStreamNext { stack = concat (mapValues  bindings) stack }
      | { stack = [TmRecordUpdate { rec = rec, value = value }] ++ stack } -> typeStreamNext { stack = concat [rec, value] stack }

end


lang MatchTypeStream = TypeStreamInterface

  sem typeStreamNext =
  | { stack = [TmMatch { target = target, thn = thn, els = els }] ++ stack } -> typeStreamNext { stack = concat [target, thn, els] stack }
    
end


-- TmUtest --
lang UtestTypeStream = TypeStreamInterface
    
  sem typeStreamNext =
  | { stack = [TmUtest { test = test, expected = expected, next = next, tusing = tusing, tonfail = tonfail }] ++ stack }  ->
    typeStreamNext { stack = concat [test, expected, next] (concat (switch (tusing, tonfail)
    case (None {}, None {}) then []
    case (Some a, Some b) then [a, b]    
    case (Some a, None {}) | (None {}, Some a) then [a]
    end) stack) }

end    
    
-- TmConDef and TmConApp --
lang SimpleSkip = TypeStreamInterface
    
  sem typeStreamNext =
  | { stack =
          [TmConDef { inexpr = inexpr } 
        | TmConApp { body = inexpr }
        | TmLam { body = inexpr }
        | TmType { inexpr = inexpr }
        | TmExt { inexpr = inexpr }] ++ stack } & ctx -> typeStreamNext { ctx with stack = cons inexpr stack }

end

    
lang TypeStream = AppTypeStream + LetTypeStream + RecLetsTypeStream + SeqTypeStream + RecordTypeStream + MatchTypeStream + UtestTypeStream + PMExprDemote + BootParser

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
    
