{
module Main (main) where

import Types

import Macros

import qualified Data.Map.Strict as Map
}

%wrapper "monad"

$idStart = [a-zA-Z]
$idContinue = [$idStart 0-9 \_ ]
@id = $idStart $idContinue*
@reservedWords = "where" | "as" | "case of" | "class" | "data" | "data family" | "data instance" | "default"
                 | "deriving" | "deriving instance" | "do" | "forall" | "foreing" | "hiding" | "if" | "then"
                 | "else" | "import" | "infix" | "infixl" | "infixr" | "instance" | "let" | "in" | "mdo"
                 | "module" | "newtype" | "proc" | "qualified" | "rec" | "type" | "type family" | "type instance" | "#"
@identifiers = [_a-zA-Z][_a-zA-Z0-9]*\'?
@constants = ([0-9]+ |\"([^\"]|\\\")*[^\\]\")
$operators = [\- \+ \* \/ \^ & \| > \< \= \\ \. \! : @ \_ \~ ]
$delimiter = [\( \) \[ \] \; \, \{ \} ]
@inlineComment = "--".*
@fileName = [a-zA-Z0-9\_\-]+\.[a-z]{3}
@validTokens = @reservedWords | @identifiers | @constants | $operators | $delimiter
@macroArgs = \| ( " "*@id" "* (","" "*@id" "*)*)* \|

  
tokens :-
<0> {
"define " { token (\_ _ -> TMacro) `andBegin` start_macro }
"import " { token (\_ _ -> TImport) `andBegin` start_import}
$white { skip }
^.*$ { token (\(_, _, _, s) len -> SomeToken (take len s)) }

}
<start_macro> {
$white+ {skip}
@id \| { token (\(_, _, _, s) len -> TMacroId (take len s)) `andBegin` macro_args}

}

<macro_args> {
$white+ { skip }
"," { skip }
@id {  token (\(_, _, _, s) len -> TMoreArgs (take len s)) }
\| $white* \{ {  token(\_ len -> TLastArg) `andBegin` macro_def }
}

<macro_def> {
$printable*\} { token (\(_, _, _, s) len -> TMacroDef (take len s)) `andBegin` 0}

}

<start_import> {
$white+ { skip }
@fileName { token (\(_, _, _, s) len -> TFile (take len s)) `andBegin` 0 }
}

{

getIdMacro :: Alex String
getIdMacro = do
              id <- alexMonadScan
              case id of
                TMacroId s -> return $! s
                x          -> alexError $! "Expected a macro id, got: " ++ show x 

getArgsToken :: Alex [String]
getArgsToken = do
  let loop' acc = do
                  arg <- alexMonadScan
                  case arg of
                    TMoreArgs s -> loop' $! s:acc
                    TLastArg -> return acc
                    x          -> alexError $! "Expected a macro argument, got: " ++ show x 
  loop' []

-- Ens ho petem
getDefinitionToken :: Alex String
getDefinitionToken = do
                        tok <- alexMonadScan
                        case tok of
                          TMacroDef s  -> return $! trim s
                          x            -> alexError $! "Expected a macro definition, got: " ++ show x
                     where trim s = reverse . tail . dropWhile (/='}') $ reverse s


scanner :: String -> Either String [Token]
scanner str = runAlex str $ do
   loop Map.empty []


loop :: MacroAcc -> TokenAcc -> Alex [Token]
loop macros code = do
                someToken <- alexMonadScan
                case someToken of
                  TEOF -> return code
                  TMacro -> do
                      idToken <- getIdMacro
                      argsToken <- getArgsToken
                      definitionToken <- getDefinitionToken
                      loop (Map.insert idToken (createDef argsToken definitionToken) macros) code
                  TImport -> do
                      file <- alexMonadScan
                      loop macros (file:code)
                  _ -> do loop macros (someToken:code)

alexEOF = return TEOF

main = do
  s <- getContents
  print $ scanner s
}
