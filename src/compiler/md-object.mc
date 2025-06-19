include "../util.mc"

lang MdObject

    syn Object = 
    | MdProgram { filePath: String, doc: [String], link : String }
    | MdLet { name : String, doc: [String], link : String }
    | MdLang { name : String, parents : [String] , doc: [String], link : String }
    | MdType { name: String, t: Option String, doc: [String], link : String }
    | MdUse { l: String, link : String }
    | MdSem { name: String, langName: String, doc: [String], link : String }
    | MdSyn { name: String, langName: String, doc: [String], link : String }
    | MdCon { name: String, t: String, doc: [String], link : String }
    | MdMexpr { doc: [String], link : String }
    | MdWord { word: String }
    | MdInclude { filePath: String, link : String }

    sem getMdDoc = 
    | MdLet { name = name, doc = doc} -> concatAll ["let ", name]
    | MdLang {} -> error "TODO: MdLang getMdDoc"
    | MdType {} -> error "TODO: MdType getMdDoc"
    | MdUse {} -> error "TODO: MdUse getMdDoc"
    | MdSem {} -> error "TODO: MdSem getMdDoc"
    | MdSyn {} -> error "TODO: MdSyn getMdDoc"
    | MdCon {} -> error "TODO: MdCon getMdDoc"
    | MdMexpr {} -> error "TODO: MdMexpr getMdDoc"
    
    sem getWord =
    | MdProgram { } -> ""
    | MdLet {} -> "let"
    | MdLang {} -> "lang"
    | MdType {} -> "type"
    | MdUse {} -> "use"
    | MdSem {} -> "sem"
    | MdSyn {} -> "syn"
    | MdCon {} -> "con"
    | MdMexpr {} -> "mexpr"
    | MdWord { word = word } -> word

end
