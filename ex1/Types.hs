-- | Types for exercice 1

{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Result(..)
  , Token(..)
  , updateResult
  , makeStats
  )
where
import           Lens.Micro
import           Lens.Micro.TH


data Result = Result { _reservedWords :: Double
                   , _identifiers :: Double
                   , _constants :: Double
                   , _operators :: Double
                   , _delimiterSymbols :: Double
                   , _separatorSymbols :: Double
                   , _comments :: Double
                   } deriving (Show, Read, Eq, Ord)

makeLenses ''Result

data Token = ReservedWord | Identifier | Constant | Operator | DelimiterSymbol | SeparatorSymbol | EOFToken | Comment
  deriving (Eq, Show, Ord, Read)

updateResult :: Result -> Token -> Result
updateResult res tok = case tok of
  ReservedWord    -> over reservedWords (+ 1) res
  Identifier      -> over identifiers (+ 1) res
  Constant        -> over constants (+ 1) res
  Operator        -> over operators (+ 1) res
  DelimiterSymbol -> over delimiterSymbols (+ 1) res
  SeparatorSymbol -> over separatorSymbols (+ 1) res
  Comment         -> over comments (+ 1) res
  EOFToken        -> res

makeStats :: Result -> Result
makeStats res@(Result 0 0 0 0 0 0 0) = res 
makeStats (Result a b c d e f g) = Result (a/total*100) (b/total*100) (c/total*100) (d/total*100) (e/total*100) (f/total*100) (g/total*100)
            where total = a + b + c + d + e + f + g
