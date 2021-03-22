{
module Main (main) where
}

%wrapper "monad"

$complementari = \!
$id = A-Z
@impl = "->"
@dobleimpl = "<->"
$parentesis = [\(\)]
$andOperator = ∧
$orOperator = ∨
@validChars = $complementari | $id | @impl | @dobleimpl | $parentesis | $andOperator | $orOperator
@multilineCommentStart = "{-"
@multilineCommentEnd = "-}"
@inlineComment = "--" .*

tokens :-
<0> {
$white { skip }
@validChars { token (\_ _ -> SomeToken) }
@multilineCommentStart { begin state_comment }
@inlineComment { skip }
}
<state_comment> {
    @multilineCommentEnd { begin 0 }
    -- without the next two rules it would break the comments if
    -- it werent at the start of a new line.
    [^\- \}] { skip }
    "-" { skip }
    "}" { skip }
    \n { skip }
}
{

data Token = SomeToken | EOFToken deriving (Eq, Show)


scanner :: String -> Either String [Token]
scanner str = runAlex str $ do
  let loop i = do
      someToken <- alexMonadScan
      (if someToken == EOFToken
         then return i
         else do loop $! (someToken:i))
  loop []



alexEOF :: Alex Token
alexEOF = return EOFToken

main :: IO ()
main = do
  s <- getContents
  print $ scanner s
}
