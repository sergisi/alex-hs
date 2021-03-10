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
    .* { skip }
    \n { skip }
}
{

data Token = SomeToken | EOFToken deriving (Eq, Show)


scanner str = runAlex str $ do
  let loop i = do
      someToken <- alexMonadScan
      (if someToken == EOFToken
         then return i
         else do loop $! (i+1))
  loop 0



alexEOF = return EOFToken

main = do
  s <- getContents
  print $ scanner s
}
