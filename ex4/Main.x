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
$white+ {skip}
@id \| { token (\(_, _, _, s) len -> TMacroId s) `andBegin` macro_args}

}

<macro_args> {
\| { token (\s len -> TEndMacroArgs) `andBegin` macro_def}
@id" "*"," { token (\(_, _, _, s) len -> TMoreArgs s}
$white+ { skip }
@id { token(\(_, _, _, s) len -> TLastArg s) }
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
  loop [] []


loop :: MacroAcc -> Token -> [Result]
loop acc xs = do
                someToken <- alexMonadScan
                case someToken of
                  TEOF -> return xs
                  TStartMacro -> do
                      idToken <- alexMonadScan
                      argsToken <- getArgsToken
                      definitionToken <- getDefinitionToken
                      loop ((idToken, argsToken, definitionToken):acc) xs
                  _ -> do loop acc $! (someToken:xs))

alexEOF = return TEOF

main = do
  s <- getContents
  print $ scanner s
}
