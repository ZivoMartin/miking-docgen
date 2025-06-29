-- ## SourceCodeWord
--
-- Represents a single word from the source code.
-- - `Some word` -> an actual token string
-- - `None {}`   -> a placeholder for a child block's source code
type SourceCodeWord = String
