module Data.Natural
       ( Natural
       , getNatural
       , mkNatural
       , unsafeNatural
       , readNatural
       ) where

import Data.Function

newtype Natural = Natural { getNatural :: Integer }
  deriving (Show, Eq, Ord)

unsafeNatural :: Integer -> Natural
unsafeNatural = Natural

mkNatural :: Integer -> Maybe Natural
mkNatural i
  | 0 <= i    = Just $ Natural i
  | otherwise = Nothing

readNatural :: String -> Maybe Natural
readNatural str = case reads str of
  [(a, [])] -> mkNatural a
  _ -> Nothing

instance Monoid Natural where
  mempty = Natural 0
  mappend m n = Natural $ ((+) `on` getNatural) m n
