module Scrapti.Main
  ( main
  ) where

import Options.Applicative

data Action =
    ActionStart !String
  | ActionStop
  deriving stock (Eq, Show)

run :: Action -> IO ()
run = print

parser :: Parser Action
parser = subparser
  ( command "start" (info (ActionStart <$> argument str idm) idm)
 <> command "stop"  (info (pure ActionStop) idm) )

parserInfo :: ParserInfo Action
parserInfo = info (parser <**> helper)
  ( fullDesc
  <> progDesc "Some program"
  <> header "More about my program" )

main :: IO ()
main = do
  a <- execParser parserInfo
  run a
