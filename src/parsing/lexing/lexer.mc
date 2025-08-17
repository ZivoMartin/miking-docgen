/-

# Lexer
This module defines the `lex` function, which tokenizes a Miking source string.
It produces a `TokenStream`, which is a list of `(Token, Pos)` pairs.
The lexer uses the `next` function from `token-readers.mc` to scan the input string
and records the position of each token.
The main challenge of our lexer is that we would like to perform preprocessing to replace the closers of recursive blocks with a specific token.
Indeed, the syntax of recursive blocks is highly ambiguous, which causes problems for our parser, whose goal is not to fully parse the Miking program but only to annotate it.

For example, the following code is very ambiguous, since it is not possible to determine in advance whether the first in closes the recursive block or the let y, without doing a complete parse:

let z =
    recursive
    let a = 3
    let x = let y = 3 in 2
    in
    2

To address this, we will use the compiler’s AST by establishing a recursive stream.
The idea is that we will traverse the compiler’s AST and count how many ins are expected in each recursive block.
With this simple piece of information, it becomes straightforward to disambiguate recursive blocks by determining which in closes them.

The small subtlety that makes this a bit more difficult lies in the languages: when a language is assembled, all semantic patterns are merged, including those of parent languages, which may introduce recursive blocks in the AST that are absent from the source code. This desynchronizes the recursive stream and the lexer.

To handle this, we use a sem-stream. The idea is simple: whenever we encounter the keyword lang in the code, we traverse the entire language definition to check how many recursives each semantic introduces. The recursive stream then adapts accordingly.

For more details on the internal workings of the recursive stream, see the documentation in recursive-stream.mc.

-/


include "mexpr/ast.mc"
include "mexpr/pprint.mc"

include "./token-readers.mc"
include "./sem-map.mc"
include "./recursive-stream.mc"

include "../../mast-gen/ast.mc"

type LexingCtx = { ast: Ast, recursiveDataStream: RecursiveDataStream }

let lexingCtxNew: Ast -> LexingCtx = lam ast.
    { ast = ast, recursiveDataStream = createRecursiveDataStream ast.expr }

-- A list of tokens paired with their position in the source file.
type TokenStream = use TokenReader in [(Token, Pos)]

type LexOutput = { stream: TokenStream, ctx: LexingCtx }


-- Lexical analysis: converts a raw source string into a stream of (token, position) pairs.
-- The lexer uses a recursive helper that scans the input until EOF is reached.
let lex : use TokenReader in LexingCtx -> String -> LexOutput = use TokenReader in lam ctx. lam stream.
    recursive let nthWord : String -> Int -> String = lam stream. lam n.
        match next stream pos0 with { stream = stream, token = token } in
        switch token
        case Eof {} then parsingWarn "EOF detected during lexing."; ""
        case Word { content = content } then
             if eqi 0 n then content
             else nthWord stream (subi n 1)
        case _ then nthWord stream n
        end
    in

    recursive let lex : LexingCtx -> [Int] -> String -> Pos -> TokenStream -> Int -> LexOutput =
        lam ctx. lam stack. lam stream. lam pos. lam acc. lam switchCount.
        switch next stream pos
        case { token = Eof {} & token } then { stream = reverse (cons (token, pos) acc), ctx = ctx }
        
        case { token = token, stream = stream, pos = newPos } then
             let enderCandidate = RecursiveEnderToken { ender = lit token } in
             let default = (stack, token, ctx, switchCount) in
             let switchRes = switch token
                 case Word { content = "recursive" } then
                     match recursiveDataStreamNext ctx.recursiveDataStream with
                     { inCount = inCount, stream = newStream } in
                         (cons inCount stack, token, { ctx with recursiveDataStream = newStream }, switchCount)
                 case Word { content = "lang" } then
                     match nthWord stream 1 with "end" then default else
                     let semStream = getSemMap stream in
                     match nthWord stream 0 with langName in
                     let stream = recursiveDataStreamLang ctx.recursiveDataStream langName semStream in
                     (stack, token, { ctx with recursiveDataStream = stream }, switchCount)
                 case Word { content = "sem" } then
                     match nthWord stream 0 with semName in
                     let stream = recursiveDataStreamSem ctx.recursiveDataStream semName in
                     (stack, token, { ctx with recursiveDataStream = stream }, switchCount)
                 case Word { content = "match" | "case" | "if" | "use" } then
                      let stack = match stack with [h] ++ t then cons (addi 1 h) t else stack in
                      (stack, token, ctx, switchCount)
                 case Word { content = ("in" | "then" | ";" | "switch") & content } then 
                     match stack with [c] ++ stack then
                         let switchCount = addi switchCount (match content with "switch" then 1 else 0) in
                         if eqi 0 c then (stack, enderCandidate, ctx, switchCount)
                         else (cons (subi c 1) stack, token, ctx, switchCount)
                     else default
                 case Word { content = "end" } then
                     switch stack
                     case [_] ++ rest then
                         if eqi switchCount 0 then (rest, enderCandidate, ctx, switchCount)
                         else (stack, token, ctx, subi switchCount 1)
                     case _ then default -- We are closing a lang
                     end
                 case _ then default
                 end
             in
             match switchRes with (stack, token, ctx, switchCount) in
             lex ctx stack stream newPos (cons (token, pos) acc) switchCount
        end
    in
    lex ctx [] stream { x = 0, y = 0 } [] 0
    
let lexFromFile : LexingCtx -> String -> Option LexOutput = lam ctx. lam path.
    match includeSetGetValue ctx.ast.includeSet path with Some s then
        Some (lex ctx s)
     else
        None {}
