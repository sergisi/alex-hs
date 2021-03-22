{
module Main (main) where
}

%wrapper "monad"

$chars = [a-d]
$operators = [\| \. \* \+ \? \( \) ]
$validChars = [$operators $chars]
@buida = "BUIDA" | "buida"
@valid = $validChars | @buida
tokens :-

$white { skip }
@valid { token (\_ _ -> SomeToken) }

{

data Token = SomeToken | EOFToken deriving (Eq, Show)


scanner :: Num a => String -> Either String a
scanner str = runAlex str $ do
  loop 0

loop :: Num a => a -> Alex a
loop i = do
  someToken <- alexMonadScan
  if someToken == EOFToken
    then return i
    else do
      loop $! (i + 1)



alexEOF = return EOFToken

main = do
  s <- getContents
  print $ scanner s

}
