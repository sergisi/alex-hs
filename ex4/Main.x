{
module Main (main) where

import Types
}

%wrapper "monad"

$idStart = [a-zA-Z]
$idContinue = [$idStart 0-9 \_ ]
@id = $idStart $idContinue*
@macroArgs = \| ( " "*@id" "* (","" "*@id" "*)*)* \|

  
tokens :-
"define " { token (\_ _ = TStartMacro) `andBegin` macro_definition }
  
$white { skip }

<macro_definition> {
@id { token (\(_, _, _, s) len -> TMacroDef)}

}

{


getArgsToken :: Alex [String]
getArgsToken = do
  let loop acc = do
                  maybeString <- alexMonadScan
                  case maybeString of
                    Nothing -> return acc
                    Just x -> loop $! x:acc

getDefinitionToken = alexMonadScan
        
  
scanner :: String -> Either String ([Result], [Token])
scanner str = runAlex str $ do
  let loop acc xs = do
                someToken <- alexMonadScan
                case someToken of
                  TEOF -> return (acc, xs)
                  TStartMacro -> do
                      idToken <- alexMonadScan
                      argsToken <- getArgsToken
                      definitionToken <- getDefinitionToken
                      loop (ReadMacro (idToken, argsToken, definitionToken):acc) xs
                (if someToken == TEOF
                   then return xs
                   else do loop acc $! (someToken:xs))
  loop [] []



alexEOF = return TEOF

main = do
  s <- getContents
  print $ scanner s
}
