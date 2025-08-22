include "./server-options.mc"
include "./python-server.mc"
include "./server-options.mc"
include "../options/cast-options.mc"

let startServer : ServerOptions -> () = use Formats in use ObjectsRenderer in lam opt.
    if opt.noOpen then () else
       switch opt.fmt
       case Md {} then pythonServerStart true opt
       case Html {} then pythonServerStart false opt 
       case Mdx {} then printLn (join ["Mdx generated in ", opt.folder, "."])
       end
