-- # Renderer Interface
--
-- This module defines the interface for a renderer used in **mi-doc-gen**.
--
-- Any renderer (MarkdownRenderer, HTMLRenderer, etc.) must implement this interface.

include "../extracting/objects.mc"
include "./rendering-types.mc"
include "../format.mc"

lang RendererInterface = Formats
    
    -- Returns the header of the page for an object
    sem objFormatHeader : (Format, Object) -> String
    
    -- Render an object as child (inside another page)
    sem objFormat : (Format, RenderingData) -> String

    sem objGetSpecificDoc : (Format, RenderingData) -> String

    -- Render the title of an object on its page
    sem objFormatedTitle : (Format, Object) -> String

    -- Render a section title
    sem getFormatedSectionTitle : (Format, String) -> String

    -- Render a list of objects as a link list, separated by commas
    sem getFormatedLinkList : (Format, [Object]) -> String

    -- Render the footer of the page for an object
    sem objFormatFooter : (Format, Object) -> String

    -- Returns a function taking a source code word and returning the formated source code word
    sem getWordRenderer : Format -> WordRenderer

    -- Returns a function taking a a formated source code, and returned it wrapped into a toggle button
    sem getCodeHider : Format -> CodeHider

end
