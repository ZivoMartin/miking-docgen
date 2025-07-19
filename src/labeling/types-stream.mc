-- # TypeStream Interface
-- 
-- This module defines the `TypeStreamInterface`, an API for managing type information traversal over a stack of MExpr expressions.  
-- Its main purpose is to simulate a stream-like behavior for typed `let`-bindings and to support recursive parsing of complex syntactic constructs such as `Lang` and `Sem`.
-- 
-- It exposes an abstract `TypeStreamContext` that maintains a stack of expressions, and two main operations:
-- 
-- - `typeStreamNext`: Iteratively searches for a `let`-binding matching a given name and returns its type and context, while skipping unrelated ones.
-- - `typeStreamPop`: Pops the top expression from the context stack, useful for recursively analyzing inner expressions.
--
-- This interface supports scenarios where expressions are not encountered in their declaration order, and enables deferred or conditional type lookups, crucial for features like `Lang` and `Sem` handling in Miking's semantics.

include "mexpr/keywords.mc"
include "map.mc"
include "pmexpr/demote.mc"
include "ocaml/external.mc"
include "mexpr/type-check.mc"

include "../global/logger.mc"
include "../global/util.mc"
    
lang TypeStreamInterface = MExprAst

    -- A context holding a stack of expressions yet to be processed.    
    type TypeStreamContext = { stack: [Expr] }

    -- The result of a typeStreamNext query.
    -- Includes:
    -- - The updated context
    -- - The optional type found
    -- - A list of skipped bindings that were not relevant to the query.
    type TypeStreamNextResult = { ctx: TypeStreamContext, t: Option Type, skipped: [{ name: String, body: Expr, t: Type }] }

    -- Searches the top of the context stack for a let-binding whose identifier matches the provided name.
    -- - If the top does not match, it is skipped and added to the skipped list.
    -- - If the stack is empty, returns None for the type and an unchanged context.        
    sem typeStreamNext : String -> TypeStreamContext -> TypeStreamNextResult
    sem typeStreamNext name =
    | { stack = [_] ++ stack } ->
        typeStreamNext name { stack = stack }
    | { stack = [] } & ctx -> { t = None {}, ctx = ctx, skipped = [] }

    -- Removes and returns the top expression (Expr) from the stack, updating the context accordingly.
    sem typeStreamPop : TypeStreamContext -> { ctx: TypeStreamContext, ast: Expr }
    sem typeStreamPop =
        | { stack = [ast] ++ stack } & ctx -> { ast = ast, ctx = { ctx with stack = stack }}

    -- Utility function that:
    -- - Immediately returns the type if the provided identifier matches the target name.
    -- - Otherwise, recursively invokes typeStreamNext, adding the current item to the skipped list.
    sem checkAndEnd name ident t body =
    | ctx -> if eqString name ident.0 then
            { t = Some t, ctx = ctx, skipped = [] }
        else
            let res = typeStreamNext name ctx in
            { res with skipped = cons { name = ident.0, body = body, t = t } res.skipped }
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

    -- Builds a TypeStream, creates an AST via the compiler's parser. Then types this AST via compiler's typer.
    -- Note that meta vars are not removed here    
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
        { stack = [ast] }
end 
    
