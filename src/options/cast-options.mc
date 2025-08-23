-- # Step-Specific Options
--
-- Some steps in the documentation pipeline require a set of arguments
-- that are usually a subset of the full `Options` type.
-- Instead of passing the entire `Options` record, or a large number of parameters,
-- we define helper functions that convert `Options` into step-specific option types.
--
-- This file provides basic utilities to cast `Options` into
-- `ServerOptions` and `RenderingOptions`.

include "./options.mc"
include "../server/server-options.mc"
include "../rendering/rendering-options.mc"
include "hashmap.mc"

-- Convert a global `Options` record and a link string representing the URL of the opening file.
-- into a `ServerOptions` record used by the server.
let getServeOption : Options -> String -> ServerOptions  = lam opt. lam link.
    {
        fmt = opt.fmt,
        folder = opt.outputFolder,
        firstFile = opt.file,
        noOpen = opt.noOpen,
        link = link
    }

-- Convert a global `Options` record into a `RenderingOptions` record
-- used by the rendering step.
let getRenderingOption : Options -> RenderingOptions = use FormatLanguages in lam opt.
    {
        theme = opt.theme,
        fmt = opt.fmt,
        noStdlib = opt.noStdlib,
        outputFolder = opt.outputFolder,
        urlPrefix = opt.urlPrefix,
        fmtLang = opt.fmtLang,
        letDepth = opt.letDepth,
        nameContext = hashmapEmpty ()
    }
