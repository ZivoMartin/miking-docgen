include "./options.mc"
include "../server/server-options.mc"
include "../rendering/renderer.mc"

let getServeOption : () -> ServerOptions  = lam.
    {
        fmt = opt.fmt,
        folder = opt.outputFolder,
        firstFile = opt.file,
        noOpen = opt.noOpen
    }

let getRenderingOption : () -> RenderingOptions  = lam.
    {
        theme = opt.theme,
        format = opt.fmt,
        noStdlib = opt.noStdlib,
        outputFolder = opt.outputFolder    
    }
