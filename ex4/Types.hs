-- * Types Module
-- | This module contains the types used by this exercice.

module Types (Result(..), Token(..)) where

data Result = Processed String
            | LImport String
            | LEOF
            | ReadMacro Macro
            deriving (Show, Eq, Ord, Read)

data Token = TImport String
           | TMacroId String
           | TMacro
           | TMacroUse String
           | TOther String
           | TEOF
           deriving (Show, Eq, Ord, Read)

type Macro = (String, [String], String)
