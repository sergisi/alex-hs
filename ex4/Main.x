{
module Main (main) where

import System.Environment (getArgs)
  
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Types

import Macros

}

%wrapper "monad"

$idStart = [a-zA-Z]
$idContinue = [$idStart 0-9 \_ ]
@id = $idStart $idContinue*
@fileName = [a-zA-Z0-9\_ \- \. \/]+

  
tokens :-
<0> {
^"import"$white+ { token (\_ _ -> TImport) `andBegin` start_import}
^"define"$white+ { token (\_ _ -> TMacro) `andBegin` start_macro }
-- Macro Application
@id \| { token (\(_, _, _, s) len -> TMacroUse $ take (len - 1) s) `andBegin` macro_application}

$white+ { token (\(_, _, _, s) len -> SomeToken (take len s)) }
[^$white \|]+ { token (\(_, _, _, s) len -> SomeToken (take len s)) }
\| { token $ \_ _ -> SomeToken "|" }

}
<start_macro> {
$white+ {skip}
@id \| { token (\(_, _, _, s) len -> TMacroId (take (len - 1) s)) `andBegin` macro_args}

}

<macro_application> {
$white+ { skip }
"," { skip }
[^ $white \| \,]+ {  token (\(_, _, _, s) len -> TMoreArgs $ take len s) }
\| {  token(\_ len -> TLastArg) `andBegin` 0 }
}

<macro_args> {
$white+ { skip }
"," { skip }
@id {  token (\(_, _, _, s) len -> TMoreArgs (take len s)) }
\| $white* \{ {  token (\_ len -> TLastArg) `andBegin` macro_def }
}

<macro_def> {
$printable*\} { token (\(_, _, _, s) len -> TMacroDef $ take len s) `andBegin` 0}

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
          TMoreArgs s -> loop' $! s : acc
          TLastArg -> return acc
          x -> alexError $! "Expected a macro argument, got: " ++ show x
  loop' []

-- Ens ho petem
getDefinitionToken :: Alex String
getDefinitionToken = do
  tok <- alexMonadScan
  case tok of
    TMacroDef s -> return $! trim s
    x           -> alexError $! "Expected a macro definition, got: " ++ show x
  where trim s = reverse . tail . dropWhile (/= '}') $ reverse s


scanner :: String -> Either String [Token]
scanner str = runAlex str $ do
  loop Map.empty []


loop :: MacroAcc -> TokenAcc -> Alex [Token]
loop macros code = do
  someToken <- alexMonadScan
  case someToken of
    TEOF   -> return code
    TMacro -> do
      idToken         <- getIdMacro
      argsToken       <- getArgsToken
      definitionToken <- getDefinitionToken
      loop (Map.insert idToken (createDef argsToken definitionToken) macros)
           code
    TImport -> do
      file <- alexMonadScan
      loop macros (file : code)
    TMacroUse id -> do
      argsToken <- getArgsToken
      if id `Map.member` macros
        then
          let var = macros Map.! id $ argsToken
          in  case var of
                Left err ->
                  alexError $ "Macro " ++ id ++ " produced error" ++ err
                Right s -> loop macros $ (SomeToken s) : code
        else
          alexError
          $  "Macro "
          ++ id
          ++ " is not defined.\n"
          ++ "All macros defined are: "
          ++ show (Map.keys macros)
    _ -> do
      loop macros (someToken : code)

alexEOF = return TEOF

importerFunction :: Set.Set String -> Token -> ExceptT String IO TokenAcc
importerFunction set (TFile file) = scanFile set file
importerFunction _   t            = liftEither (Right [t])

tokenToContent :: Token -> String
tokenToContent (SomeToken s) = s
tokenToContent _             = ""

toString :: TokenAcc -> String
toString = concatMap tokenToContent

scanFile :: Set.Set String -> String -> ExceptT String IO TokenAcc
scanFile set file
  | file `Set.member` set
  = liftEither
    .  Left $ "Circular dependency at file: "
    ++ file
    ++ "\n With Set: \n\t"
    ++ show set
  | otherwise
  = do
    s    <- liftIO $ readFile file
    toks <- liftEither . fmap reverse . scanner $ s
    fmap concat $ traverse (importerFunction set') toks
  where set' = file `Set.insert` set

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then error
      "To use the program execute `cabal v2-run <input-file> <output-file>`"
    else do
      result <- runExceptT . fmap toString . scanFile (Set.empty) $ args !! 0
      case result of
        Left  err  -> putStrLn err
        Right toks -> writeFile (args !! 1) toks

}
