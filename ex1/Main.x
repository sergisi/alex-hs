{
module Main where
import Types
}

%wrapper "monad"

@reservedWords = "where" | "as" | "case of" | "class" | "data" | "data family" | "data instance" | "default" | "deriving" | "deriving instance" | "do" | "forall" | "foreing" | "hiding" | "if" | "then" | "else" | "import" | "infix" | "infixl" | "infixr" | "instance" | "let" | "in" | "mdo" | "module" | "newtype" | "proc" | "qualified" | "rec" | "type" | "type family" | "type instance" | "#"
@identifiers = [_a-zA-Z][_a-zA-Z0-9]*\'?
@constants = ([0-9]+ |\"([^\"]|\\\")*[^\\]\")
$operatorsChar = [\- \+ \* \/ \^ & \| > \< \= \\ \. \! : @ \_ \~ \$]
@operators = $operatorsChar+ |`@identifiers`
$delimiter = [\( \) \[ \] \; \, \{ \} ]
@separator = $white+
@inlineComment = "--".*

@multilineCommentStart = "{-"
@multilineCommentEnd = "-}"

tokens :-

<0> {
^$white        { tokenize DelimiterSymbol }
@reservedWords { tokenize ReservedWord }
@identifiers   { tokenize Identifier }
@constants     { tokenize Constant }
@operators    { tokenize Operator }
$delimiter     { tokenize DelimiterSymbol }
@separator     { tokenize SeparatorSymbol }
@inlineComment { tokenize Comment }

@multilineCommentStart { tokenize Comment `andBegin` comment_code}
}
<comment_code> {
@multilineCommentEnd { begin 0 }
$white+ ;
$printable+ ;
}

{

tokenize :: Token -> AlexAction Token
tokenize t = token (\_ _ -> t)

scanner str = runAlex str $ loop (Result 0 0 0 0 0 0 0)

loop :: Result -> Alex Result
loop tok = do
  someToken <- alexMonadScan
  if someToken == EOFToken
    then return $ makeStats tok
    else do
      loop $ updateResult tok someToken



alexEOF = return EOFToken

main = do
  s <- getContents
  print $ scanner s

}
