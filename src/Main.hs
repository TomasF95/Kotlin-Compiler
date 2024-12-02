module Main where
import           Lexer              (alexScanTokens)
import           Parser             (parse)

main :: IO ()
main = do

  let inputKT = "input.kt"
  input <- readFile inputKT
  let file = "output.txt"
  writeFile file "" 
  let tokens = alexScanTokens input
  printTokens tokens file
  let ast = parse tokens
  printTree ast file

printTokens :: Show a => a -> FilePath -> IO ()
printTokens tokens file = do
  appendFile file "Tokens:\n"
  appendFile file (show tokens)
  appendFile file "\n"


printTree :: Show a => a -> FilePath -> IO ()
printTree ast file = do
  appendFile file "Tree:\n"
  appendFile file (show ast)
  
