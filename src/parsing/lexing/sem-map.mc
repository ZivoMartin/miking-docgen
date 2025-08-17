include "mexpr/ast.mc"
include "./token-readers.mc"
include "../../global/util.mc"

type SemMap = HashMap String Int

let getSemMap : String -> SemMap = use TokenReader in lam s.
    recursive let nthWord : String -> Int -> String = lam stream. lam n.
        match next stream pos0 with { stream = stream, token = token } in
        switch token
        case Eof {} then parsingWarn "EOF detected during nthWord."; ""
        case Word { content = content } then
             if eqi 0 n then content
             else nthWord stream (subi n 1)
        case _ then nthWord stream n
        end
    in

    recursive let work : String -> SemMap -> Int -> Int -> String -> SemMap  = lam s. lam acc. lam count. lam switchCount. lam currentSem.
        match next s pos0 with { token = token, stream = stream } in
        let finish : () -> SemMap = lam.
            if eqi -1 count then acc else
                let count: Int = match hmLookup currentSem acc with Some n then addi n count else count in
                hmInsert currentSem count acc
        in
        switch token
        case Eof {} then parsingWarn "EOF reached in getSemStream"; acc
        case Word { content = "switch" } then work stream acc count (addi switchCount 1) currentSem
        case Word { content = "end" } then
             if eqi switchCount 0 then
                finish ()
             else
                work stream acc count (subi switchCount 1) currentSem
        case Word { content = "sem" } then work stream (finish ()) 0 switchCount (nthWord stream 0)
        case Word { content = "recursive" } then work stream acc (addi 1 count) switchCount currentSem
        case _ then work stream acc count switchCount currentSem
        end
    in
    work s (hashmapEmpty ()) -1 0 ""

