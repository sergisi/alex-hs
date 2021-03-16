-- * Types Module
-- | This module contains the types used by this exercice.

module Types (Result(..), Token(..)) where

data Result = Processed String
            | LImport String
            | LEOF
            deriving (Show, Eq, Ord, Read)

data Token = TImport String
           | TMacroId String
           | TMacro
           | TMacroUse String
           | TOther String
           | TEndMacroArgs String
           | TLastArg String
           | TEOF
           deriving (Show, Eq, Ord, Read)

type Macro = (String, [String], String)

type MacroAcc = [Macro]

-- @id|arg1, arg2, arg3, ..., argN| { def }
-- define @id|argsN| def
