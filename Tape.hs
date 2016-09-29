module Tape
  ( Tape(..)
  , tapeFromList
  , prev
  , next
  ) where

data Tape a = Tape { beginning :: [a]
                   , current :: a
                   , end :: [a]
                   } deriving (Show)

tapeFromList (x:xs) = Tape [] x xs

prev :: Tape a -> Tape a
prev (Tape (a:as) b c) = Tape as a (b:c)

next :: Tape a -> Tape a
next (Tape a b (c:cs)) = Tape (b:a) c cs


data Event = Joy
             { xaxis :: Int
             , yaxis :: Int
             }
           | Keyboard
             { keycode :: Int
             , duration :: Int
             } deriving (Show)

