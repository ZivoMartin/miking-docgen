include "../parsing/token-readers.mc"
include "../parsing/doc-tree.mc"
include "./util.mc"

type SynVariant = {
    name: String,
    vtype: String,
    doc: String
}

-- Extracts variant names from a stream of syntax tree nodes starting with '|'.
-- Returns a list of the variants.
let synVariantParse : [DocTree] -> [SynVariant] = lam stream.

    recursive let work : [DocTree] -> Option [String] -> String -> [String] -> [SynVariant] =
        use TokenReader in
        lam stream. lam typeAcc. lam nextDoc. lam commentBuffer.

        let strExtractType = lam acc.
            let t = strExtractType acc in
            match strSplitOnce t ' ' with Some { left = name, right = vtype } then
                { name = name, vtype = vtype, doc = nextDoc }
            else
                { name = t, vtype = "", doc = nextDoc }
        in

        let joinComments : () -> String = lam. strJoin "\n" (reverse commentBuffer) in

        switch (stream, typeAcc)
        case ([DocTreeLeaf { token = TokenWord { content = "|" } }] ++ stream, None {}) then work stream (Some []) (joinComments ()) []
        case ([DocTreeLeaf { token = TokenWord { content = "|" } }] ++ stream, Some typeAcc) then
            let t = strExtractType typeAcc in
            let nextDoc = joinComments () in
            let tail = work stream (Some []) nextDoc [] in
            cons t tail
        case ([DocTreeLeaf { token = TokenWord { content = word } }] ++ stream, Some typeAcc) then work stream (Some (cons word typeAcc)) nextDoc commentBuffer
        case ([DocTreeLeaf { token = TokenWord {} }] ++ stream, None {}) then work stream (None {}) nextDoc commentBuffer
        
        case ([DocTreeLeaf { token = TokenComment { content = comment } | TokenMultiLineComment { content = comment} }] ++ stream, _) then work stream typeAcc nextDoc (cons (strTrim comment) commentBuffer)

        case ([_] ++ stream, _) then work stream typeAcc nextDoc commentBuffer

        case ([], Some typeAcc) then [strExtractType typeAcc]
        case ([], None {}) then []
        end
    in
    work stream (None {}) "" []

