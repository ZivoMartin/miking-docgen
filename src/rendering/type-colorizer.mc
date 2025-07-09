include "mexpr/type-check.mc"
include "../logger.mc"
include "../util.mc"

lang TypeColorizerInterface = MExprAst
    sem typeColorize : Type -> String
    sem typeColorize =
    | t -> error (infoErrorString (infoTy t) "")
    
    sem formatTypeName : String -> String
    sem unknownDisplay : () -> String

end

lang UnknownTypeColorizer = TypeColorizerInterface
  sem typeColorize =
  | TyUnknown {} -> unknownDisplay ()

end

lang BoolTypeColorizer = TypeColorizerInterface
  sem typeColorize =
  | TyBool {} -> formatTypeName "Bool"

end

lang IntTypeColorizer = TypeColorizerInterface
  sem typeColorize =
  | TyInt {} -> formatTypeName "Int"

end

lang FloatTypeColorizer = TypeColorizerInterface
  sem typeColorize =
  | TyFloat {} -> formatTypeName "Float"

end

lang CharTypeColorizer = TypeColorizerInterface
  sem typeColorize =
  | TyChar {} -> formatTypeName "Char"

end

lang FunTypeColorizer = TypeColorizerInterface
  sem typeColorize =
  | TyArrow { from = left, to = right} -> concatAll [typeColorize left, " -> ", typeColorize right]

end

lang SeqTypeColorizer = TypeColorizerInterface
  sem typeColorize =
  | TySeq { ty = TyChar {} } -> formatTypeName "String"
  | TySeq { ty = t} -> concatAll ["[", typeColorize t, "]"]


end

lang TensorTypeColorizer = TypeColorizerInterface

  sem typeColorize =
  | TyTensor { ty = t } -> error "tensor"

end

lang RecordTypeColorizer = TypeColorizerInterface
  sem typeColorize =
  | TyRecord { fields = fields } -> concatAll ["{ ", strJoin ", " (map typeColorize (mapValues fields)), " }"]

end

lang VariantTypeColorizer = TypeColorizerInterface
  sem typeColorize =
  | TyVariant { constrs = variants } -> error "variants"

end

lang ConTypeColorizer = TypeColorizerInterface
  sem typeColorize =
  | TyCon { ident = ident, data = ty} -> concatAll [ident.0, " ", typeColorize ty]

end

lang DataTypeColorizer = TypeColorizerInterface

  sem typeColorize =
  | TyData d -> error "data"


end


lang VarTypeColorizer = TypeColorizerInterface
  sem typeColorize =
  | TyVar  { ident = ident } -> formatTypeName ident.0

end

lang AllTypeColorizer = TypeColorizerInterface
  sem typeColorize =
  | TyAll {} -> error "all"


end

lang AppTypeColorizer = TypeColorizerInterface
  sem typeColorize =
  | TyApp { lhs = left, rhs  = right} -> concatAll [typeColorize left, " -> ", typeColorize right]


end

lang AliasTypeColorizer = TypeColorizerInterface
  sem typeColorize =
  | TyAlias {display = d, content = content} -> error "alias"
    
end

    
lang TypeColorizerTemplate = UnknownTypeColorizer + BoolTypeColorizer + IntTypeColorizer + FloatTypeColorizer + CharTypeColorizer + FunTypeColorizer + SeqTypeColorizer + TensorTypeColorizer + RecordTypeColorizer + VariantTypeColorizer + ConTypeColorizer + DataTypeColorizer + VarTypeColorizer + AllTypeColorizer + AppTypeColorizer + AliasTypeColorizer  end

