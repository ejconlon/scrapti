module Scrapti.Main (main) where
import System.Exit (die)
import System.Environment (getArgs)

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
