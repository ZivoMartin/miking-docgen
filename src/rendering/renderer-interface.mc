include "../extracting/objects.mc"

lang RendererInterface

    -- Differents format supported by mi-doc-gen
    syn Format =

    -- Take in input a String representing a format and output the corresponding format.
    -- Returns None if the format is invalid.
    sem formatFromStr /- String -> Option Format -/ = 
        | _ -> None {}

    -- Takes a format and an object, returns the String representing the object on a page as a son.
    sem objFormat /- (Format, Object), String -/ =

    -- Takes a format and an object, returns the String representing the object on its own page.
    sem objGetSpecificDoc /- (Format, Object) -> String -/ = 

    -- Takes a format and an object, returns the String representing the title on its own page.
    sem objFormatedTitle /- (Format, Object) -> String -/ =

    -- Takes a format and an object, returns the link of the object formated.
    sem objGetFormatedLink /- (Format, Object) -> String -/ =

    -- Takes a format and a section's title, returns the section title formated.
    sem getFormatedSectionTitle /- (Format, String) -> String -/ =

    -- Takes a format and a list of object, returns a String representing all the objects displayed as a list of link separated with a comma
    sem getFormatedLinkList /- (Format, [Object]) -> String -/ =
end
