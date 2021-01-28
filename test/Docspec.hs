module Main (main) where

main :: IO ()
main = do
  putStrLn "This test-suite exists only to add dependencies"
  putStrLn "To run docspecs:"
  putStrLn "    cabal build --enable-tests"
  putStrLn "    ./docspec.sh"
