-- | Types for exercice 1

{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Result(..)
  , Token(..)
  , updateResult
  )
where
import           Lens.Micro
import           Lens.Micro.TH


data Result = Result { _reservedWords :: Int
                   , _identifiers :: Int
                   , _constants :: Int
                   , _operators :: Int
                   , _delimiterSymbols :: Int
                   , _separatorSymbols :: Int
                   , _comments :: Int
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
