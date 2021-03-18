-- | 

module Macros (createDef) where

import qualified Data.Map.Strict as Map



createDef :: [String] -> String -> [String] -> Either String String
createDef as d xs | length as >= length xs = Left "Definition has more arguments than provided"
                  | otherwise = Right . unwords . map f $ words d
                  where f df = if df `Map.member` m then m Map.! df else df
                        m = Map.fromList $ zip as xs
                        
