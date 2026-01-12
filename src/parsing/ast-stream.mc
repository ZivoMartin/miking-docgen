include "map.mc"

include "./utils.mc"

include "../global/logger.mc"
include "../global/util.mc"
include "../extracting/objects.mc"

lang AstStreamInterface = MExprAst + MExprPrettyPrint + Objects

    type AstStreamContext = { stack: [Expr] }

    type AstStreamNextResult = {
        ctx: AstStreamContext,
        name: String,
        info: Info,
        obj: Object
    }

    sem typeStreamHandleDecl : Decl -> AstStreamContext -> AstStreamNextResult

    sem typeStreamNext : AstStreamContext -> Option AstStreamNextResult
    sem typeStreamNext =
    | { stack = [TmDecl ({ decl = decl, inexpr = inexpr } & tm)] ++ stack } & ctx ->
        let ctx = { ctx with stack = stack } in
        let res = typeStreamHandleDecl decl ctx in
        Some { res with ctx = { res.ctx with stack = concat res.ctx.stack [inexpr] } }
    | { stack = [_] ++ stack } ->
        typeStreamNext { stack = stack }
    | { stack = [] } & ctx -> None {}

end

lang LetAstStream = AstStreamInterface

  sem typeStreamHandleDecl =
  | DeclLet { ident = ident, body = body, tyBody = tyBody, info = info } & decl ->
      lam ctx.
      let obj = ObjLet { ty = Some tyBody, datas = objDefaultDatas () } in
      let info = concatInfos info (infoTm (getLastNode body)) in
      { ctx = ctx, name = ident.0, info = info, obj = obj }

end

lang TypeAstStream = AstStreamInterface

  sem typeStreamHandleDecl =
  | DeclType { ident = ident, params = params, tyIdent = tyIdent, info = info } ->
      lam ctx.
      let t = if eqString "<>" (type2str tyIdent) then None {} else Some tyIdent in
      let obj = ObjType { t = t, datas = objDefaultDatas () } in
      { ctx = ctx, name = ident.0, info = info, obj = obj }

end

lang ConAstStream = AstStreamInterface

  sem typeStreamHandleDecl =
  | DeclConDef { ident = ident, tyIdent = tyIdent, info = info } ->
      lam ctx.      
      let parentType = type2str (getReturnType tyIdent) in
      let obj = ObjCon { t = tyIdent, parentType = parentType , datas = objDefaultDatas () } in
      let info = concatInfos info (infoTy tyIdent) in
      { ctx = ctx, name = ident.0, info = info, obj = obj }
end

lang UtestAstStream = AstStreamInterface

  sem typeStreamHandleDecl =
  | DeclUtest { info = info } ->
      lam ctx.      
      let obj = ObjUtest (objDefaultDatas ()) in
      { ctx = ctx, name = "utest", info = info, obj = obj }
end

lang RecursiveAstStream = AstStreamInterface

  sem typeStreamHandleDecl =
  | DeclRecLets { bindings = bindings, info = info } ->
      lam ctx.
      
      let dummyNode = TmNever { ty = TyUnknown { info = NoInfo () }, info = NoInfo () } in
      let decls = map (lam binding.
          let decl = DeclLet binding in
          TmDecl { decl = decl, inexpr = dummyNode , info = infoDecl decl, ty = binding.tyBody }          
          ) bindings
      in

      let ctx = { ctx with stack = concat decls ctx.stack } in
      optionGetOrElse
          (lam.
              parsingWarn "Parsing the first recursive binding failed.";
              let dummyObject = ObjUtest (objDefaultDatas ()) in
              { ctx = ctx, name = "", info = info, obj = dummyObject })
          (typeStreamNext ctx)
end


-- lang DeclAstStream = AstStreamInterface
    
--   sem typeStreamNext name =
--   | { stack = [TmDecl ({ decl = decl, inexpr = inexpr } & tm)] ++ stack } & ctx ->
--             switch decl
--             case DeclRecLets { bindings = [] } then
--                 typeStreamNext name { stack = cons inexpr stack }
--             case DeclRecLets ({ bindings = [b] ++ bindings } & dl) then
--                 let ctx = { ctx with stack = concat [b.body, TmDecl { tm with decl = DeclRecLets { dl with bindings = bindings } } ] stack } in
--                 checkAndEnd name b.ident (b.tyBody) b.body ctx
--             case DeclLet { ident = ident, body = body, tyBody = tyBody } then
--                 let ctx = { ctx with stack = concat [body, inexpr] stack } in
--                 checkAndEnd name ident tyBody body ctx
--             case DeclUtest { test = test, expected = expected, tusing = tusing, tonfail = tonfail }  then
--                 typeStreamNext name { stack = cons inexpr stack }
--             case _ then typeStreamNext name { ctx with stack = cons inexpr stack }
--             end

-- end

lang AstStream =
    LetAstStream + TypeAstStream + ConAstStream + UtestAstStream + RecursiveAstStream

    sem typeStreamFromExpr : Expr -> AstStreamContext 
    sem typeStreamFromExpr =
        | ast -> { stack = [ast] }

    -- Builds a AstStream, creates an AST via the compiler's parser. Then types this AST via compiler's typer.
    -- Note that meta vars are not removed here    
    sem buildAstStream : MAst -> AstStreamContext
    sem buildAstStream = | ast -> typeStreamFromExpr ast
end
