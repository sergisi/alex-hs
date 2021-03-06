-- * Types Module
-- | This module contains the types used by this exercice.

module Types
  ( Result(..)
  , Token(..)
  , Macro
  , MacroAcc
  , TokenAcc
  ) where

import qualified Data.Map.Strict               as Map

data Result = Processed String
            | LImport String
            | LEOF
            deriving (Show, Eq, Ord, Read)

data Token = TImport
           | TFile String
           | TMacro
           | TMacroId String
           | TMoreArgs String
           | TLastArg
           | TMacroDef String
           | TMacroUse String
           | TEOF
           | SomeToken String
           deriving (Show, Eq, Ord, Read)

type Macro = [String] -> Either String String

type MacroAcc = Map.Map String Macro

type TokenAcc = [Token]
