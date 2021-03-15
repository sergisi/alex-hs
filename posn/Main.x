{
module Main (main) where
}

%wrapper "posn"

@aba = "aba"
@newline = "\n"

tokens :-

@aba { token (\p s-> SomeToken p) }
$white+  ;
@newline ;

{

token f p s = f p s

data Token = SomeToken AlexPosn deriving (Eq, Show) 
 
main = do
    s <- getContents
    print $ alexScanTokens s
}