include "mexpr/ast.mc"
include "mexpr/uncurried.mc"
include "mexpr/type-check.mc"
include "mexpr/type-lift.mc"
include "ocaml/ast.mc"
include "../logger.mc"
include "../util.mc"
include "mlang/ast.mc"
include "mexpr/info.mc"

lang TypeColorizerInterface = MExprAst + UncurriedAst + PprintTyAnnot + TyWildAst + ReprTypeAst + ReprSubstAst + VariantNameTypeAst + OCamlTypeAst + TyUseAst

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

lang TyUncurriedArrowColorizer = TypeColorizerInterface

   sem typeColorize =
  | TyUncurriedArrow {} -> error "uncurried"


end

lang FakeTypeColorizer = TypeColorizerInterface

   sem typeColorize =
  | FakeType {} -> error "fake"
  | TyRepr {} -> error "repr"
  | TyWild {} -> error "wild"
  | TySubst {} -> error "subs"
  | TyVariantName {} -> error "variant"
  | OTyList {} -> error "ocaml"
  | OTyArray {} -> error "ocaml"
  | OTyTuple {} -> error "ocaml"
  | OTyBigarrayGenarray {} -> error "ocaml"
  | OTyBigarrayArray {} -> error "ocaml"
  | OTyBigarrayFloat64Elt {} -> error "ocaml"
  | OTyBigarrayIntElt {} -> error "ocaml"
  | OTyBigarrayClayout {} -> error "ocaml"
  | OTyLabel {} -> error "ocaml"
  | OTyVar {} -> error "ocaml"
  | OTyVarExt {} -> error "ocaml"
  | OTyParam {} -> error "ocaml"
  | OTyRecord {} -> error "ocaml"
  | OTyRecordExt {} -> error "ocaml"
  | OTyString {} -> error "ocaml"
  | OTyInlinedRecord {} -> error "ocaml"
  | TyUse {} -> error "use"


end
    
lang TypeColorizerTemplate = UnknownTypeColorizer + BoolTypeColorizer + IntTypeColorizer + FloatTypeColorizer + CharTypeColorizer + FunTypeColorizer + SeqTypeColorizer + TensorTypeColorizer + RecordTypeColorizer + VariantTypeColorizer + ConTypeColorizer + DataTypeColorizer + VarTypeColorizer + AllTypeColorizer + AppTypeColorizer + AliasTypeColorizer + FakeTypeColorizer + TyUncurriedArrowColorizer end

