include "./token-readers.mc"
include "./include-set.mc"

-- This token is not readable but is at the root of a DocTree, the content is the name of the file and the includeSet a set will all the files.
lang ProgramTokenReader = TokenReaderInterface
    syn Token =
        | ProgramToken { content: String, includeSet: IncludeSet }

    sem lit =
        | ProgramToken {} -> ""

    sem tokenToString =
        | ProgramToken {} -> "Program"
end
