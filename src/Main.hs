module Main where

import Control.Concurrent.Thread.Delay ( delay )
import Control.Monad ( guard, join, when )
import Control.Concurrent.Async ( race_ )

import Data.Char ( isDigit )
import Data.Foldable ( fold, forM_ )
import Data.List ( genericReplicate )
import Data.Maybe ( isJust )
import Data.Natural

import System.Clock ( Clock(Monotonic), TimeSpec(..), getTime )
import System.Console.Terminal.Size ( size, width )
import System.Environment ( getArgs )
import System.Exit ( exitSuccess, exitFailure )
import System.IO ( hSetBuffering, stdin, stdout, BufferMode(NoBuffering) )

-------------------------------------------------------------------------------
-- Parsing command line arguments

-- We recognize hours, minutes and seconds. Defaulting to seconds.
-- The modifier returns a number of milliseconds.
modifier :: String -> Maybe (Natural -> Natural)
modifier str = case str of
  []  -> Just $ unsafeNatural . (1000*)      . getNatural
  "s" -> Just $ unsafeNatural . (1000*)      . getNatural
  "m" -> Just $ unsafeNatural . (1000*60*)   . getNatural
  "h" -> Just $ unsafeNatural . (1000*3600*) . getNatural
  _ -> Nothing

-- An option is a natural number together with a modifier
option :: String -> Maybe Natural
option str = modifier mod <*> readNatural n
  where (n, mod) = span isDigit str

-- Options are summed to get the total length of time
-- If we are about to fail because of an unrecognized option, we
-- highlight it in red.
options :: [String] -> Either String Natural
options strs =
  let mopts = option <$> strs in
  case sequence mopts of
    Just tm -> pure (fold tm)
    Nothing -> Left $ unwords $ zipWith warn mopts strs

  where warn mopt str = if isJust mopt then str
                        else "\x1b[31m" ++ str ++ "\x1b[0m"

getOptions :: IO Natural
getOptions = do
  args <- getArgs
  when (null args) $ errorAndOut "empty option list"
  case options args of
    Right tm -> pure tm
    Left str -> errorAndOut $ concat
               [ "invalid options ("
               , str
               , ")"
               ]

-------------------------------------------------------------------------------
-- Error

errorAndOut :: String -> IO a
errorAndOut str = do
  putStrLn $ "\x1b[31mError\x1b[0m: " ++ str
  exitFailure

-------------------------------------------------------------------------------
-- Measure the width of the terminal

getWidth :: IO Integer
getWidth = do
  msz <- size
  case msz of
    Just sz -> pure (width sz)
    Nothing -> errorAndOut "this can only be called from a terminal"

-------------------------------------------------------------------------------
-- Progress bar

progress :: Natural -- ^ Amount of time currently spent (< total)
         -> Natural -- ^ Total amount of time to wait
         -> Integer -- ^ Size of the terminal
         -> String  -- ^ Progress bar
progress curr tot sz = concat
  [ genericReplicate width '…'
  , genericReplicate (sz - 4 - width) ' '
  , percent
  ] where

  ratio r = (r * getNatural curr) `div` getNatural tot

  width   = ratio (sz - 4)

  percent :: String -- ^ 4 character long % of total time spent so far
  percent = let pc = ratio 100 in
    if pc == 0  then " 00%" else
    if pc < 10  then concat [ " 0", show pc, "%" ] else
    if pc < 100 then concat [ " ", show pc, "%" ]
    else show pc ++ "%"

-------------------------------------------------------------------------------
-- Measuring time

-- @getTime@ returns a @TimeSpec@. We compare them and get a result in milliseconds
diffTimeSpec :: TimeSpec -> TimeSpec -> Integer
diffTimeSpec a b = toms a - toms b where
  toms a = 1000 * fromIntegral (sec a) + fromIntegral (nsec a) `quot` 10^6

-------------------------------------------------------------------------------
-- Main function

loop :: Natural  -- Total amount of time to wait
     -> TimeSpec -- Starting time
     -> IO ()
loop tot start = do
  wd  <- getWidth
  clk <- getTime Monotonic
  let curr = unsafeNatural $ diffTimeSpec clk start
  putStr $ "\r" ++ progress curr tot wd
  if tot < curr
    then exitSuccess
    else do
      delay (10 * getNatural tot)
      loop tot start

sleeping :: IO ()
sleeping = do
  opts <- getOptions
  time <- getTime Monotonic
  loop opts time

dying :: IO ()
dying = do
  c <- getChar
  case c of
    'q'    -> return ()
    '\ESC' -> return ()
    _ -> dying

main :: IO ()
main = do
  -- Because we only @putStr@, we need to turn stdout buffering off
  hSetBuffering stdout NoBuffering
  -- Because we read single characters, we need to turn stdin bufferin off
  hSetBuffering stdin NoBuffering
  race_ sleeping dying
