# Intrinsics (Not Public)
This is as of writing a not public page on intrinsic documentation.

## Integer Intrinsics

| Name       | <nobr>Brief Description</nobr> | Type Signature      | Description |
| :----------| :---------------- | :------------------ | :---------- |
| **`addi`** | Addition          | <nobr>`Int -> Int -> Int`</nobr> | `addi x y` computes x + y. |
| **`subi`** | Subtraction       | `Int -> Int -> Int` | `subi x y` computes x - y. |
| **`muli`** | Multiplication    | `Int -> Int -> Int` | `muli x y` computes x * y. |
| **`divi`** | Division          | `Int -> Int -> Int` | `divi x y` computes x / y. |
| **`modi`** | Modulus           | `Int -> Int -> Int` | `modi x y` computes x % y. |
| **`negi`** | Negation          | `Int -> Int`        | `negi x` computes -x. |
| **`slli`** | Logical Shift Left    | `Int -> Int -> Int` | `slli x y` computes x \<< y, disregarding any information about the sign of x. Example: `slli 5 2` returns `20`. |
| **`srli`** | Logical Shift Right   | `Int -> Int -> Int` | `srli x y` computes x >> y, disregarding any information about the sign of x. Example: `srli 5 2` returns `1`. |
| **`srai`** | Arithmetic Shift Right   | `Int -> Int -> Int` | `srai x y` computes x >> y, retaining information about the sign of x. Example: `srli (negi 5) 2` returns `-2`. |
| **`lti`** | Less-Than (\<) | <nobr>`Int -> Int -> Bool`</nobr> | `lti x y` returns `true` if x < y, otherwise `false`. |
| **`leqi`** | Less or Equal (\<=) | `Int -> Int -> Bool` | `leqi x y` returns `true` if x \<= y, otherwise `false`. |
| **`gti`** | Greater-Than (>) | `Int -> Int -> Bool` | `gti x y` returns `true` if x > y, otherwise `false`. |
| **`geqi`** | <nobr>Greater or Equal (>=)</nobr> | `Int -> Int -> Bool` | `geqi x y` returns `true` if x >= y, otherwise `false`. |
| **`eqi`** | Equal (==) | `Int -> Int -> Bool` | `eqi x y` returns `true` if x == y, otherwise `false`. |
| **`neqi`** | Not Equal (!=) | `Int -> Int -> Bool` | `neqi x y` returns `true` if x != y, otherwise `false`. |

## Float Intrinsics

| Name       | <nobr>Brief Description</nobr> | Type Signature      | Description |
| :----------| :---------------- | :------------------ | :---------- |
| **`addf`** | Addition          | <nobr>`Float -> Float -> Float`</nobr> | `addf x y` computes x + y. |
| **`subf`** | Subtraction       | `Float -> Float -> Float` | `subf x y` computes x - y. |
| **`mulf`** | Multiplication    | `Float -> Float -> Float` | `mulf x y` computes x * y. |
| **`divf`** | Division          | `Float -> Float -> Float` | `divf x y` computes x / y. |
| **`negf`** | Negation          | `Float -> Float -> Float` | `negf x y` computes -x. |
| **`ltf`** | Less-Than (\<\) | <nobr>`Float -> Float -> Bool`</nobr> | `ltf x y` returns `true` if x < y, otherwise `false`. |
| **`leqf`** | Less or Equal (\<=) | `Float -> Float -> Bool` | `leqf x y` returns `true` if x \<= y, otherwise `false`. |
| **`gtf`** | Greater-Than (>) | `Float -> Float -> Bool` | `gtf x y` returns `true` if x > y, otherwise `false`. |
| **`geqf`** | <nobr>Greater or Equal (>=)</nobr> | `Float -> Float -> Bool` | `geqf x y` returns `true` if x >= y, otherwise `false`. |
| **`eqf`** | Equal (==) | `Float -> Float -> Bool` | `eqf x y` returns `true` if x == y, otherwise `false`. |
| **`neqf`** | Not Equal (!=) | `Float -> Float -> Bool` | `neqf x y` returns `true` if x != y, otherwise `false`. |

## Float-Integer Conversion Intrinsics

| Name       | <nobr>Brief Description</nobr> | Type Signature      | Description |
| :----------| :---------------- | :------------------ | :---------- |
| **`floorfi`** | Floor          | <nobr>`Float -> Int`</nobr> | `floorfi x` rounds x down the to the nearest integer. Example: `floorfi 2.7` returns `2`. |
| **`ceilfi`** | Ceil       | `Float -> Int` | `ceilfi x` rounds x up the to the nearest integer. Example: `ceilfi 2.1` returns `3`. |
| **`roundfi`** | Rounding    | `Float -> Int` | `roundfi x` rounds x the to the nearest integer. Example: `roundfi 2.1` returns `2`, and `roundfi 2.7` returns `3`. |
| **`int2float`** | To Float Type | `Int -> Float` | `int2float x` converts x to a Float type, potentially losing precision for very large integers. |

## Character and String Intrinsics

| Name       | <nobr>Brief Description</nobr> | Type Signature      | Description |
| :----------| :---------------- | :------------------ | :---------- |
| **`eqc`**  | Equal (==)          | <nobr>`Char -> Char -> Bool`</nobr> | `eqc a b` returns `true` if a == b, otherwise `false`. |
| **`char2int`** | To Unicode          | `Char -> Int` | `char2int c` returns the unicode encoding for the character c. |
| **`int2char`** | From Unicode          | `Int -> Char` | `int2char i` returns a character with the unicode encoding i. |
| **`stringIsFloat`** | Check if String is Formatted as a Float | `String -> Bool` | `stringIsFloat s` returns `true` if s is a properly formatted string representing a float, otherwise `false`. If this returns `true`, then it means that `float2string` is safe to use on s. |
| **`string2float`** | To Float          | `String -> Float` | `string2float s` returns the Float represented by s. Use `stringIsFloat` prior to this if unsure whether about the format of s properly follows that for a Float. |
| **`float2string`** | From Float     | `Float -> String` | `float2string x` returns the string representation for x. |

## Sequence Intrinsics

| Name       | <nobr>Brief Description</nobr> | Type Signature      | Description |
| :----------| :---------------- | :------------------ | :---------- |
| **`create`**  | Create a Sequence          | <nobr>`all a. Int -> (Int -> a) -> [a]`</nobr> | `create n f` creates a sequence of length n where an element at index i is instantiated as `f i`. Example: `create 5 (lam i. muli i 3)` returns the sequence `[0, 3, 6, 9, 12]`. |
| **`createRope`**  | Create a Rope Sequence          | <nobr>`all a. Int -> (Int -> a) -> [a]`</nobr> | Same interface as for `create`, but ensures that the underlying sequence representation is a rope. |
| **`createList`**  | Create a List Sequence          | <nobr>`all a. Int -> (Int -> a) -> [a]`</nobr> | Same interface as for `create`, but ensures that the underlying sequence representation is a list. |
| **`isRope`**  | Check if Rope          | <nobr>`all a. [a] -> Bool`</nobr> | `isRope l` returns `true` if the underlying representation of l is a rope, otherwise `false`. |
| **`isList`**  | Check if List          | <nobr>`all a. [a] -> Bool`</nobr> | `isList l` returns `true` if the underlying representation of l is a list, otherwise `false`. |
| **`length`**  | Sequence Length          | <nobr>`all a. [a] -> Int`</nobr> | `length l` returns the length of the sequence l. |
| **`concat`**  | Concatenate Sequences | <nobr>`all a. [a] -> [a] -> [a]`</nobr> | `concat l1 l2` returns the concatenation of l1 and l2. Example: `concat [4,5,6] [1,2,3]` returns `[4,5,6,1,2,3]`. |
| **`get`**  | Get Element          | <nobr>`all a. [a] -> Int -> a`</nobr> | `get l i` returns the element at index i in sequence l. Example: `get [10,11,12] 1` returns `11`. |
| **`set`**  | Set Element          | <nobr>`all a. [a] -> Int -> a -> [a]`</nobr> | `set l i e` returns a copy of l where the element at index i has been set to e. Example: `set [10,11,12] 1 777` returns `[10,777,12]`. |
| **`cons`**  | Prepend Element          | <nobr>`all a. a -> [a] -> [a]`</nobr> | `cons e l` prepends e to l. Example: `cons 5 [10,11]` returns `[5,10,11]`. |
| **`snoc`**  | Append Element          | <nobr>`all a. [a] -> a -> [a]`</nobr> | Reverse cons. `snoc l e` appends e to l. Example: `snoc [10,11] 5` returns `[10,11,5]`. |
| **`splitAt`**  | Split Sequence          | <nobr>`all a. [a] -> Int -> ([a], [a])`</nobr> | `splitAt l i` splits the sequence at l at index i, returning a tuple with each split. Example: `splitAt [1,2,3] 2` returns `([1,2],[3])`, and `splitAt [1,2,3] 0` returns `([],[1,2,3])`. |
| **`reverse`**  | Reverse Sequence       | <nobr>`all a. [a] -> [a]`</nobr> | `reverse l` reverses the sequence l. Example: `reverse [1,2,3]` returns `[3,2,1]`. |



## Not yet documented

```mc
let builtin = use MExprAst in
  [ ("unsafeCoerce", CUnsafeCoerce ())
  -- Sequences
  , ("head", CHead ())
  , ("tail", CTail ())
  , ("null", CNull ())
  , ("map", CMap ())
  , ("mapi", CMapi ())
  , ("iter", CIter ())
  , ("iteri", CIteri ())
  , ("foldl", CFoldl ())
  , ("foldr", CFoldr ())
  , ("subsequence", CSubsequence ())
  -- Random numbers
  , ("randIntU", CRandIntU ())
  , ("randSetSeed", CRandSetSeed ())
  -- MCore intrinsics: Time
  , ("wallTimeMs", CWallTimeMs ())
  , ("sleepMs", CSleepMs ())
  -- MCore intrinsics: Debug and I/O
  , ("print", CPrint ())
  , ("printError", CPrintError ())
  , ("dprint", CDPrint ())
  , ("flushStdout", CFlushStdout ())
  , ("flushStderr", CFlushStderr ())
  , ("readLine", CReadLine ())
  , ("readBytesAsString", CReadBytesAsString ())
  , ("argv", CArgv ())
  , ("readFile", CFileRead ())
  , ("writeFile", CFileWrite ())
  , ("fileExists", CFileExists ())
  , ("deleteFile", CFileDelete ())
  , ("command", CCommand ())
  , ("error", CError ())
  , ("exit", CExit ())
  -- Constructor tags
  , ("constructorTag", CConstructorTag ())
  -- Symbols
  , ("eqsym", CEqsym ())
  , ("gensym", CGensym ())
  , ("sym2hash", CSym2hash ())
  -- References
  , ("ref", CRef ())
  , ("deref", CDeRef ())
  , ("modref", CModRef ())
  -- NOTE: I will not include documentation for the tensor and boot parser things yet...
  -- Tensors
  , ("tensorCreateUninitInt", CTensorCreateUninitInt ())
  , ("tensorCreateUninitFloat", CTensorCreateUninitFloat ())
  , ("tensorCreateCArrayInt", CTensorCreateInt ())
  , ("tensorCreateCArrayFloat", CTensorCreateFloat ())
  , ("tensorCreateDense", CTensorCreate ())
  , ("tensorGetExn", CTensorGetExn ())
  , ("tensorSetExn", CTensorSetExn ())
  , ("tensorLinearGetExn", CTensorLinearGetExn ())
  , ("tensorLinearSetExn", CTensorLinearSetExn ())
  , ("tensorRank", CTensorRank ())
  , ("tensorShape", CTensorShape ())
  , ("tensorReshapeExn", CTensorReshapeExn ())
  , ("tensorCopy", CTensorCopy ())
  , ("tensorTransposeExn", CTensorTransposeExn ())
  , ("tensorSliceExn", CTensorSliceExn ())
  , ("tensorSubExn", CTensorSubExn ())
  , ("tensorIterSlice", CTensorIterSlice ())
  , ("tensorEq", CTensorEq ())
  , ("tensor2string", CTensorToString ())
  -- Boot parser
  , ("bootParserParseMExprString", CBootParserParseMExprString ())
  , ("bootParserParseMCoreFile", CBootParserParseMCoreFile ())
  , ("bootParserGetId", CBootParserGetId ())
  , ("bootParserGetTerm", CBootParserGetTerm ())
  , ("bootParserGetType", CBootParserGetType ())
  , ("bootParserGetString", CBootParserGetString ())
  , ("bootParserGetInt", CBootParserGetInt ())
  , ("bootParserGetFloat", CBootParserGetFloat ())
  , ("bootParserGetListLength", CBootParserGetListLength ())
  , ("bootParserGetConst", CBootParserGetConst ())
  , ("bootParserGetPat", CBootParserGetPat ())
  , ("bootParserGetInfo", CBootParserGetInfo ())
  ]
```
