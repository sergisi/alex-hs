-- * Types Module
-- | This module contains the types used by this exercice.

module Types (Result(..), Token(..)) where

data Result = Processed String
            | LImport String
            | LEOF
            deriving (Show, Eq, Ord, Read)

data Token = TImport
           | TFile String
           | TMacroId String
           | TMacro
           | TMacroUse String
           | TMoreArgs String
           | TLastArg String
           | TMacroDef String
           | TEndMacroDef
           | TEOF
           | SomeToken String
           deriving (Show, Eq, Ord, Read)

type Macro = (String, [String], [String])

type MacroAcc = [Macro]

type TokenAcc = [Token]
