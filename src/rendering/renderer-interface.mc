-- # Renderer Interface
--
-- This module defines the interface for a renderer used in **mi-doc-gen**.
--
-- Any renderer (MarkdownRenderer, HTMLRenderer, etc.) must implement this interface.

include "../extracting/objects.mc"

lang RendererInterface

    -- Different formats supported by mi-doc-gen
    syn Format =

    -- Parse format string (example: "html" or "md"), returns None if invalid
    sem formatFromStr /- String -> Option Format -/ = 
        | _ -> None {}

    -- Returns the header of the page for an object
    sem objFormatHeader /- (Format, Object) -> String -/ =
    
    -- Render an object as child (inside another page)
    sem objFormat /- (Format, Object) -> String -/ =

    -- Render an object on its own page (full documentation)
    sem objGetSpecificDoc /- (Format, Object) -> String -/ =

    -- Render the title of an object on its page
    sem objFormatedTitle /- (Format, Object) -> String -/ =

    -- Render a section title
    sem getFormatedSectionTitle /- (Format, String) -> String -/ =

    -- Render a list of objects as a link list, separated by commas
    sem getFormatedLinkList /- (Format, [Object]) -> String -/ =

    -- Render the footer of the page for an object
    sem objFormatFooter /- (Format, Object) -> String -/ =
end
