{
module Main where
import Types
}

%wrapper "monad"

@reservedWords = "where"
@identifiers = [a-zA-Z][a-zA-Z0-9]*\'?
@constants = [0-9]+
@operators = \+
$delimiter = [\; \( \) \[ \] \; \, ]
$separator = $white
@inlineComment = "--".*

tokens :-

@reservedWords { tokenize ReservedWord }
@identifiers { tokenize Identifier }
@constants { tokenize Constant }
@operators { tokenize Operator }
$delimiter { tokenize DelimiterSymbol }
$separator { tokenize SeparatorSymbol }
@inlineComment { tokenize Comment }

{

tokenize :: Token -> AlexAction Token
tokenize t = token (\_ _ -> t)

scanner str = runAlex str $ loop (Result 0 0 0 0 0 0 0)

loop :: Result -> Alex Result
loop tok = do
  someToken <- alexMonadScan
  (if someToken == EOFToken
    then return tok
    else do loop $ updateResult tok someToken)



alexEOF = return EOFToken

main = do
  s <- getContents
  print $ scanner s
}
