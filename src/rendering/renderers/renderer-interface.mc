include "../../extracting/objects.mc"
include "../../global/format.mc"
include "../../global/theme.mc"
include "../../global/format-language.mc"    
include "../rendering-types.mc"
include "../rendering-options.mc"

include "./objects-renderer.mc"

include "mexpr/type-check.mc"
include "mexpr/pprint.mc"

lang RendererInterface = Formats + Themes + ObjectsRenderer + Formatter + MExprPrettyPrint + MetaVarTypePrettyPrint + FormatLanguages

    sem renderSetup : RenderingOptions -> ()
     
    sem renderTopPageDoc : RenderingData -> RenderingOptions -> String
    
    sem renderDocBloc : RenderingData -> Bool -> RenderingOptions -> String
    
    sem renderDocSignature : Object -> RenderingOptions -> String

    sem renderDocDescription : Object -> RenderingOptions -> String    

    sem renderLinkList : [Object] -> RenderingOptions -> String

    sem renderGotoLink : String -> RenderingOptions -> String

    sem renderStringColorized : Object -> RenderingOptions -> String  

    sem renderCodeWithoutPreview : RenderingData -> RenderingOptions -> String

    sem renderCodeWithPreview : RenderingData -> RenderingOptions -> String

    sem renderHidenCode : String -> Bool -> RenderingOptions -> String

    sem renderHeader : Object -> RenderingOptions -> String

    sem renderFooter : Object -> RenderingOptions -> String

    sem renderSectionTitle : String -> RenderingOptions -> String

    sem renderBold : String -> RenderingOptions -> String

    sem renderRemoveDocForbidenChars : String -> RenderingOptions -> String

    sem renderRemoveCodeForbidenChars : String -> RenderingOptions -> String    

    sem renderSourceCodeStr : String -> RenderingOptions -> String
    
    sem renderSourceCode : SourceCode -> RenderingOptions -> String
    
    sem renderWord : SourceCodeWord -> RenderingOptions -> String

    sem renderTreeSourceCode : [TreeSourceCode] -> Object -> RenderingOptions -> RenderingData

    sem renderTitle : Int -> String -> RenderingOptions -> String

    sem renderObjTitle : Int -> Object -> RenderingOptions -> String
    
    sem renderText : String -> RenderingOptions -> String

    sem renderLink : String -> String -> RenderingOptions -> String
    
    sem renderType : String -> RenderingOptions -> String

    sem renderVar : String -> RenderingOptions -> String
    
    sem renderKeyword : String -> RenderingOptions -> String
    
    sem renderComment : String -> RenderingOptions -> String
    
    sem renderString : String -> RenderingOptions -> String

    sem renderNumber : String -> RenderingOptions -> String
    
    sem renderDefault : String -> RenderingOptions -> String
    
    sem renderMultiLigneComment : String -> RenderingOptions -> String

    sem renderNewLine : RenderingOptions -> String
    
end
