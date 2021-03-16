{
module Main (main) where

import Types
}

%wrapper "monad"

$idStart = [a-zA-Z]
$idContinue = [$idStart 0-9 \_ ]
@id = $idStart $idContinue*
@reservedWords = "where" | "as" | "case of" | "class" | "data" | "data family" | "data instance" | "default" | "deriving" | "deriving instance" | "do" | "forall" | "foreing" | "hiding" | "if" | "then" | "else" | "import" | "infix" | "infixl" | "infixr" | "instance" | "let" | "in" | "mdo" | "module" | "newtype" | "proc" | "qualified" | "rec" | "type" | "type family" | "type instance" | "#"
@identifiers = [_a-zA-Z][_a-zA-Z0-9]*\'?
@constants = ([0-9]+ |\"([^\"]|\\\")*[^\\]\")
$operators = [\- \+ \* \/ \^ & \| > \< \= \\ \. \! : @ \_ \~ ]
$delimiter = [\( \) \[ \] \; \, \{ \} ]
@inlineComment = "--".*
@validTokens = @reservedWords | @identifiers | @constants | $operators | $delimiter
@macroArgs = \| ( " "*@id" "* (","" "*@id" "*)*)* \|

  
tokens :-
"define " { token (\_ _ = TMacro) `andBegin` start_macro }
$white { skip }
@validTokens {token (\_ s) = SomeToken s}

<start_macro> {
$white+ {skip}
@id \| { token (\(_, _, _, s) len -> TMacroId s) `andBegin` macro_args}

}

<macro_args> {
@id" "*\|" "*{ { token(\(_, _, _, s) len -> TLastArg s) `andBegin` macro_def}
@id" "*","  { token (\(_, _, _, s) len -> TMoreArgs s}
$white+ { skip }
}

<macro_def>{
\} { token (\s len -> TEndMacroDef)}
$white+ {skip}
@validTokens { token (\s len -> TMacroDef s) } 
}

{

parse :: String -> String
parse s = takeWhile (\c -> and [c != " ", c != ",", c != "|"]) s

getIdMacro :: Alex String
getIdMacro = do
              id <- alexMonadScan
              case id of
                TMacroId s -> return $ parse s

getArgsToken :: Alex [String]
getArgsToken = do
  let loop acc = do
                  arg <- alexMonadScan
                  case arg of
                    TMoreArgs s -> loop $! (parse s):acc
                    TLastArg s -> return s:acc

getDefinitionToken :: Alex [String]
getDefinitionToken = do
        let loop acc = do
                        tok <- alexMonadScan
                        case arg of
                          TMacroDef s -> loop $! s:acc
                          TEndMacroDef -> return acc
  
scanner :: String -> Either String File
scanner str = runAlex str $ do
  loop [] [] []


loop :: MacroAcc -> ImportAcc -> TokenAcc -> File
loop macors imports code = do
                someToken <- alexMonadScan
                case someToken of
                  TEOF -> return (macros, imports, code)
                  TMacro -> do
                      idToken <- getIdMacro
                      argsToken <- getArgsToken
                      definitionToken <- getDefinitionToken
                      loop ((idToken, argsToken, definitionToken):macros) imports code
                  TImport s -> do
                  _ -> do loop macros imports $! (someToken:code))

alexEOF = return TEOF

main = do
  s <- getContents
  print $ scanner s
}
