module AST where

import qualified Data.Set as S

type CharSet = S.Set Char

data RE =
    RClass Bool{-complemented?-} CharSet
  | RSeq [RE]
  | RAlt RE RE
  | RRepeat RE (Int,Int)
  | RCapture RE
  | RBackref Int
  | RConj RE RE
  | RNeg RE
  deriving (Show, Eq)
