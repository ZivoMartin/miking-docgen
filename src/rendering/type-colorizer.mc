include "mexpr/type-check.mc"
include "../logger.mc"
include "../util.mc"
include "mexpr/pprint.mc"

    
lang TypeColorizerInterface = MExprAst + PrettyPrint
    sem getTypeStringCode (indent : Int) (env : PprintEnv) =
    
    sem typeColorize =
    | ty ->
        match getTypeStringCode 0 pprintEnvEmpty ty with (_,str) in str
    
    sem formatTypeName : String -> String
    sem formatEntryName : String -> String
    sem unknownDisplay : () -> String

end



lang UnknownTypeColorizer = TypeColorizerInterface
  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TyUnknown _ -> (env, unknownDisplay ())
end

lang BoolTypeColorizer = TypeColorizerInterface
  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TyBool _ -> (env, formatTypeName "Bool")
end

lang IntTypeColorizer = TypeColorizerInterface
  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TyInt _ -> (env, formatTypeName "Int")
end

lang FloatTypeColorizer = TypeColorizerInterface
  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TyFloat _ -> (env, formatTypeName "Float")
end

lang CharTypeColorizer = TypeColorizerInterface
  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TyChar _ -> (env, formatTypeName "Char")
end

lang FunTypeColorizer = TypeColorizerInterface
  sem typePrecedence =
  | TyArrow _ -> 0

  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TyArrow t ->
    match printTypeParen indent 1 env t.from with (env, from) in
    match getTypeStringCode indent env t.to with (env, to) in
    (env, join [from, " -> ", to])
end

lang SeqTypeColorizer = TypeColorizerInterface
  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TySeq t ->
    match getTypeStringCode indent env t.ty with (env, ty) in
    (env, join ["[", ty, "]"])
end

lang TensorTypeColorizer = TypeColorizerInterface
  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TyTensor t ->
    match getTypeStringCode indent env t.ty with (env, ty) in
    (env, join ["Tensor[", ty, "]"])
end

lang RecordTypeColorizer = TypeColorizerInterface + RecordTypeUtils
  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | (TyRecord t) & ty ->
    if mapIsEmpty t.fields then (env,"()") else
      let orderedFields = tyRecordOrderedFields ty in
      let tuple =
        let seq = map (lam b : (SID,Type). (sidToString b.0, b.1)) orderedFields in
        if forAll (lam t : (String,Type). stringIsInt t.0) seq then
          let seq = map (lam t : (String,Type). (string2int t.0, t.1)) seq in
          let seq : [(Int,Type)] = sort (lam l : (Int,Type). lam r : (Int,Type). subi l.0 r.0) seq in
          let fst = lam x: (Int, Type). x.0 in
          let first = fst (head seq) in
          let last = fst (last seq) in
          if eqi first 0 then
            if eqi last (subi (length seq) 1) then
              Some (map (lam t : (Int,Type). t.1) seq)
            else None ()
          else None ()
        else None ()
      in
      match tuple with Some tuple then
        match mapAccumL (getTypeStringCode indent) env tuple with (env, tuple) in
        let singletonComma = match tuple with [_] then "," else "" in
        (env, join ["(", strJoin ", " tuple, singletonComma, ")"])
      else
        let f = lam env. lam field.
          match field with (sid, ty) in
          match getTypeStringCode indent env ty with (env, tyStr) in
          (env, (sid, tyStr))
        in
        match mapAccumL f env orderedFields with (env, fields) in
        let fields =
          map (lam b : (SID,String). (pprintLabelString b.0, b.1)) fields in
        let conventry = lam entry : (String,String). join [formatEntryName entry.0, ": ", formatTypeName entry.1] in
        (env,join ["{", strJoin ", " (map conventry fields), "}"])
end

lang VariantTypeColorizer = TypeColorizerInterface
  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TyVariant t ->
    if eqi (mapLength t.constrs) 0 then (env,"<>")
    else (env, join ["Variant<", strJoin ", " (map nameGetStr (mapKeys t.constrs)), ">"])
end

lang ConTypeColorizer = TypeColorizerInterface
  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TyCon t ->
    let idstr = formatTypeName t.ident.0 in
    let d = unwrapType t.data in
    match d with TyUnknown _ then (env, idstr) else
      match getTypeStringCode indent env t.data with (env, datastr) in
      match d with TyData _ then (env, concat idstr datastr) else
        (env, join [idstr, "{", datastr, "}"])
end

lang DataTypeColorizer = TypeColorizerInterface
  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TyData t ->
    match
      mapFoldWithKey
        (lam acc. lam. lam ks.
          if setIsEmpty ks then acc
          else
            match mapAccumL pprintConName acc.0 (setToSeq ks)
            with (env, kstr) in
            (env, snoc acc.1 (strJoin " " kstr)))
        (env, [])
        (computeData t)
    with (env, consstr) in
    (env, join ["{", strJoin " " consstr, "}"])
end

lang VarTypeColorizer = TypeColorizerInterface
  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TyVar t ->
    (env, formatEntryName t.ident.0)
end

lang AllTypeColorizer = TypeColorizerInterface
  sem typePrecedence =
  | TyAll _ -> 0

  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TyAll t ->
    let idstr = formatTypeName t.ident.0 in
    match
      match t.kind with Mono () | Poly () then (env, "") else
        match getKindStringCode indent env t.kind with (env, kistr) in
        (env, concat "::" kistr)
    with (env, kistr) in
    match getTypeStringCode indent env t.ty with (env, tystr) in
    (env, join ["all ", idstr, kistr, ". ", tystr])
end

lang AppTypeColorizer = TypeColorizerInterface
  sem typePrecedence =
  | TyApp _ -> 1

  sem getTypeStringCode (indent : Int) (env: PprintEnv) =
  | TyApp t ->
    match printTypeParen indent 1 env t.lhs with (env,lhs) in
    match printTypeParen indent 2 env t.rhs with (env,rhs) in
    (env, join [lhs, " ", rhs])
end

lang AliasTypeColorizer = TypeColorizerInterface
  sem typePrecedence =
  | TyAlias t -> typePrecedence t.display

  sem getTypeStringCode (indent : Int) (env : PprintEnv) =
  | TyAlias t -> getTypeStringCode indent env t.display
end

lang TyWildColorizer = TypeColorizerInterface + TyWildAst
  sem typePrecedence =
  | TyWild _ -> 0

  sem getTypeStringCode indent env =
  | TyWild _ -> (env, "_")
end

lang ReprTypeColorizer = TypeColorizerInterface + ReprTypeAst
  sem typePrecedence =
  | TyRepr _ -> 1

  sem getTypeStringCode indent env =
  | TyRepr x ->
    let repr = switch deref (botRepr x.repr)
      case BotRepr repr then join [int2string repr.scope, ", ", int2string (sym2hash repr.sym)]
      case UninitRepr _ then "uninit"
      case _ then "impossible"
      end in
    match printTypeParen indent 2 env x.arg with (env, arg) in
    (env, join ["Repr[", repr, "] ", arg])
end

lang TypeColorizerTemplate = UnknownTypeColorizer + BoolTypeColorizer + IntTypeColorizer +
  FloatTypeColorizer + CharTypeColorizer + FunTypeColorizer +
  SeqTypeColorizer + RecordTypeColorizer + VariantTypeColorizer +
  ConTypeColorizer + DataTypeColorizer + VarTypeColorizer +
  AppTypeColorizer + TensorTypeColorizer + AllTypeColorizer +
  AliasTypeColorizer end
