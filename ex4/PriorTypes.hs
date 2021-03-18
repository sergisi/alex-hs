-- |

module Types where

data Lexer = LInclude String | LMacro String | LMacroApplication String | LOtherToken String | LEOF deriving (Show, Eq, Ord, Read)


data Token = TInclude String | Processed String deriving (Show, Eq, Ord, Read)
-- scanner s -> [Token]
-- TInclude str -> scanner $ getFileContents str

-- id|(.*)*|

-- #define Nom|a, b, c, d| a + b + c + d
-- Nom|2, 3, 4, 5| -> xs@["2", "3", "4", "5"] -> Map.lookup (4 , "Nom") $ xs
-- Map (M, Nom) ([Stings] -> String)

-- extractInclude Token -> IO String
-- case token of
-- LInclude str-> do
--                s  <- getFileContents str
--                let toks = scanner s
--                fmap unlines $ traverse extractInclude toks
-- Processed str -> return str

-- (Token -> IO String) -> [Token] -> IO String

-- | From Extra library. We only needed this, so it felt wrong to install
-- | all the library. It elevates our extractInclude to a list of tokens
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (pure [])
 where
  f x xs = do
    y <- op x
    if null y
      then xs
      else do
        ys <- xs
        pure $ y ++ ys

type Result = String
