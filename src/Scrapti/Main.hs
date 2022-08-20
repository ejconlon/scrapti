module Scrapti.Main (main) where
import System.Environment (getArgs)
import System.Exit (die)

convert :: FilePath -> FilePath -> IO ()
convert _aiffPath _wavPath = do
  putStrLn "TODO"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["convert-aif", aifPath, wavPath] ->
      convert aifPath wavPath
    _ -> die "no dice"
