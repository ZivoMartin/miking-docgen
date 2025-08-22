include "./options.mc"
include "../server/server-options.mc"
include "../rendering/rendering-options.mc"
include "hashmap.mc"

let getServeOption : Options -> String -> ServerOptions  = lam opt. lam link.
    {
        fmt = opt.fmt,
        folder = opt.outputFolder,
        firstFile = opt.file,
        noOpen = opt.noOpen,
        link = link
    }

let getRenderingOption : Options -> RenderingOptions = use FormatLanguages in lam opt.
    {
        theme = opt.theme,
        fmt = opt.fmt,
        noStdlib = opt.noStdlib,
        outputFolder = opt.outputFolder,
        urlPrefix = opt.urlPrefix,
        fmtLang = opt.fmtLang,
        letDepth = opt.letDepth,
        keepTestsDoc = opt.keepTestsDoc,
        mdDoc = opt.mdDoc,
        nameContext = hashmapEmpty ()
    }
