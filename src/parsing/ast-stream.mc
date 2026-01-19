include "map.mc"

include "./utils.mc"

include "../global/logger.mc"
include "../global/util.mc"
include "../extracting/objects.mc"

lang AstStreamInterface = MExprPrettyPrint  + Objects

    type AstStreamContext = Expr 
    type LangDatabase = HashMap String Object

    type AstStreamNextResult = {
        ctx: AstStreamContext,
        name: String,
        info: Info,
        obj: Object
    }

    sem typeStreamHandleDecl : Decl -> AstStreamContext -> AstStreamNextResult

    sem typeStreamCreateLangDatabase : AstStreamContext -> String -> { database: LangDatabase, ctx: AstStreamContext }
    sem typeStreamCreateLangDatabase =
    | ctx -> lam. { database = hashmapEmpty (), ctx = ctx }

    sem getNextInfo : AstStreamContext -> Info
    sem getNextInfo =
    | TmDecl ({ info = info } & tm) -> info
    | _ -> NoInfo ()

    sem typeStreamNext : AstStreamContext -> Option AstStreamNextResult
    sem typeStreamNext =
    | TmDecl { decl = decl, inexpr = inexpr } -> Some (typeStreamHandleDecl decl inexpr)
    | _ -> None {}

end

lang ExternalAstStream = AstStreamInterface

  sem typeStreamHandleDecl =
  | DeclExt {ident = ident, tyIdent = tyIdent, info = info} ->
      lam ctx.
      let obj = ObjLet { ty = Some tyIdent, datas = objDefaultDatas () } in
      let info = concatInfos info (getNextInfo ctx) in
      { ctx = ctx, name = ident.0, info = info, obj = obj }

end

lang LetAstStream = AstStreamInterface

  sem typeStreamHandleDecl =
  | DeclLet { ident = ident, body = body, tyBody = tyBody, info = info } & decl ->
      lam ctx.
      let obj = ObjLet { ty = Some tyBody, datas = objDefaultDatas () } in
      let info = concatInfos info (getNextInfo ctx) in
      { ctx = ctx, name = ident.0, info = info, obj = obj }

end

lang TypeAstStream = AstStreamInterface

  sem getOptionalType : Type -> Option Type
  sem getOptionalType =
  | t -> if eqString "<>" (type2str t) then
         None {} else Some t

  sem typeStreamCreateLangDatabase =
  | TmDecl { decl = DeclType { tyIdent = tyIdent, ident = ident }, inexpr = inexpr } & ctx ->
      lam langName.
      if not (belongToTheLang langName ident.0) then { database = hashmapEmpty (), ctx = ctx } else
      
      match decomposeLangItemName ident.0 with Some (actualLangName, itemName) in

      let obj =
          match getOptionalType tyIdent
          with Some t then ObjType { t = Some t, datas = objDefaultDatas () } 
          else ObjSyn { langName = langName, variants = [], datas = objDefaultDatas () }
      in

      let res = typeStreamCreateLangDatabase inexpr langName in
      { res with database = hmInsert itemName obj res.database }

  sem typeStreamHandleDecl =
  | DeclType { ident = ident, tyIdent = tyIdent, info = info } ->
      lam ctx.
      let t = getOptionalType tyIdent in
      let obj = ObjType { t = t, datas = objDefaultDatas () } in
      let info = concatInfos info (getNextInfo ctx) in      
      { ctx = ctx, name = ident.0, info = info, obj = obj }

end

lang ConAstStream = AstStreamInterface

  sem typeStreamCreateLangDatabase =
  | TmDecl { decl = DeclConDef { ident = ident, tyIdent = tyIdent }, inexpr = inexpr } & ctx ->
      lam langName.

      if not (belongToTheLang langName ident.0) then { database = hashmapEmpty (), ctx = ctx } else
    
      match decomposeLangItemName ident.0 with Some (actualLangName, itemName) in
      typeStreamCreateLangDatabase inexpr langName
      
  sem typeStreamHandleDecl =
  | DeclConDef { ident = ident, tyIdent = tyIdent, info = info } ->
      lam ctx.      
      let parentType = getParentType tyIdent in
      let obj = ObjCon { t = tyIdent, parentType = parentType , datas = objDefaultDatas () } in
      let info = concatInfos info (getNextInfo ctx) in      
      { ctx = ctx, name = ident.0, info = info, obj = obj }
end

lang UtestAstStream = AstStreamInterface

  sem typeStreamHandleDecl =
  | DeclUtest { info = info } ->
      lam ctx.      
      let obj = ObjUtest (objDefaultDatas ()) in
      let info = concatInfos info (getNextInfo ctx) in
      { ctx = ctx, name = "utest", info = info, obj = obj }
end

lang RecursiveAstStream = AstStreamInterface


  sem typeStreamCreateLangDatabase =
  | TmDecl { decl = DeclRecLets { bindings = bindings, info = info }, inexpr = inexpr } & ctx ->
      lam langName.
      let ident = tail (head bindings).ident.0 in -- sem name always start with a v, so we remove it.

      if not (belongToTheLang langName ident) then { database = hashmapEmpty (), ctx = ctx } else
      match decomposeLangItemName ident with Some (actualLangName, itemName) in
  
      let database = foldl (
          lam acc. lam binding.
             match decomposeLangItemName binding.ident.0 with Some (langName, itemName) in
             let obj = ObjSem { langName = langName, ty = Some binding.tyBody, datas = objDefaultDatas () } in
             hmInsert itemName obj acc
         ) (hashmapEmpty ()) bindings
      in
      { database = database, ctx = inexpr }
      


  sem typeStreamHandleDecl =
  | DeclRecLets { bindings = bindings, info = info } ->
      lam ctx.
      recursive let createDecl =
          lam bindings.
          match bindings with [binding] ++ bindings then
              let decl = DeclLet binding in
              TmDecl {
                  decl = decl,
                  inexpr = createDecl bindings,
                  info = infoDecl decl,
                  ty = binding.tyBody
              }
          else ctx
      in

      let ctx = createDecl bindings in
      optionGetOrElse
          (lam.
              parsingWarn "Parsing the first recursive binding failed.";
              let dummyObject = ObjUtest (objDefaultDatas ()) in
              { ctx = ctx, name = "", info = NoInfo (), obj = dummyObject })
          (typeStreamNext ctx)
end

lang AstStream =
    LetAstStream + TypeAstStream + ConAstStream + UtestAstStream + RecursiveAstStream + ExternalAstStream

    sem typeStreamFromExpr : Expr -> AstStreamContext 
    sem typeStreamFromExpr =
    | ast -> ast

    -- Builds a AstStream, creates an AST via the compiler's parser. Then types this AST via compiler's typer.
    -- Note that meta vars are not removed here    
    sem buildAstStream : MAst -> AstStreamContext
    sem buildAstStream = | ast ->
        typeStreamFromExpr ast
end
