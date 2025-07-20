include "./server-options.mc"
include "./python-server.mc"
include "./server-options.mc"

let startServer : ServerOptions -> () = use Formats in lam opt.
    if sysFileExists opt.firstFile then    
     if opt.noOpen then () else
        switch opt.fmt
        case Md {} then pythonServerStart true opt
        case Html {} then pythonServerStart false opt
        case Mdx {} then printLn (join ["Mdx generated in ", opt.folder, "."])
        end
    else error (join ["Failed to start server, file ", opt.firstFile, " doesn't exist."])
