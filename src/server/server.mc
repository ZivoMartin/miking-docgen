include "./server-options.mc"
include "./python-server.mc"
include "./server-options.mc"
include "../options/cast-options.mc"
include "../execution-context.mc"
include "../rendering/renderers/objects-renderer.mc"

let startServer : ServerOptions -> ExecutionContext -> () = use Formats in use ObjectsRenderer in lam opt. lam execCtx.
    
    match execCtx.object with Some obj then
        let link = objLink (objTreeObj obj) (getRenderingOption ()) in
        if opt.noOpen then () else
           switch opt.fmt
           case Md {} then pythonServerStart true opt link
           case Html {} then pythonServerStart false opt link 
           case Mdx {} then printLn (join ["Mdx generated in ", opt.folder, "."])
           end
    else ()
