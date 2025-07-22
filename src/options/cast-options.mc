include "./options.mc"
include "../server/server-options.mc"
include "../rendering/rendering-options.mc"

let getServeOption : () -> ServerOptions  = lam.
    {
        fmt = opt.fmt,
        folder = opt.outputFolder,
        firstFile = opt.file,
        noOpen = opt.noOpen
    }

let getRenderingOption : () -> RenderingOptions = use FormatLanguages in lam.
    {
        theme = opt.theme,
        fmt = opt.fmt,
        noStdlib = opt.noStdlib,
        outputFolder = opt.outputFolder,
        urlPrefix = opt.urlPrefix,
        fmtLang = opt.fmtLang,
        letDepth = opt.letDepth
    }
