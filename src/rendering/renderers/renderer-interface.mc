include "../../theme.mc"
include "../../extracting/objects.mc"
include "../../format.mc"
include "../../theme.mc"    
include "../rendering-types.mc"

include "mexpr/pprint.mc"

lang RendererInterface = Formats + Themes + ObjectKinds + Formatter + MExprPrettyPrint + MetaVarTypePrettyPrint

    sem renderTopPageDoc : RenderingData -> Format -> String
    
    sem renderDocBloc : RenderingData -> Format -> String
    
    sem renderDocSignature : Object -> Format -> String

    sem renderDocDescription : Object -> Format -> String    

    sem renderLinkList : [Object] -> Format -> String

    sem renderGotoLink : String -> Format -> String

    sem renderStringColorized : Object -> Format -> String  

    sem renderCodeWithoutPreview : RenderingData -> Format -> String

    sem renderCodeWithPreview : RenderingData -> Format -> String

    sem renderHidenCode : String -> Bool -> Format -> String

    sem renderHeader : Object -> Theme -> Format -> String

    sem renderFooter : Object -> Format -> String

    sem renderSectionTitle : String -> Format -> String

    sem renderBold : String -> Format -> String

    sem renderRemoveForbidenChars : String -> Format -> String

    sem renderSourceCodeStr : String -> Format -> String
    
    sem renderSourceCode : SourceCode -> Format -> String
    
    sem renderWord : SourceCodeWord -> Format -> String

    sem renderTreeSourceCode : [TreeSourceCode] -> Object -> Format -> RenderingData

    sem renderTitle : Int -> String -> Format -> String

    sem renderObjTitle : Int -> Object -> Format -> String
    
    sem renderText : String -> Format -> String

    sem renderLink : String -> String -> Format -> String
    
    sem renderType : String -> Format -> String

    sem renderVar : String -> Format -> String
    
    sem renderKeyword : String -> Format -> String
    
    sem renderComment : String -> Format -> String
    
    sem renderString : String -> Format -> String

    sem renderNumber : String -> Format -> String
    
    sem renderDefault : String -> Format -> String
    
    sem renderWeakComment : String -> Format -> String

    sem renderNewLine : Format -> String
    
end
