-- Put your matcher implementation in this file
module MatcherImpl where

import qualified Data.Set as S
import AST
import Control.Monad

-- Generic string-matching monad. Do not change anything in
-- the following definitions.

newtype Matcher d a =
  Matcher {runMatcher :: String -> Int -> d -> [(a, Int, d)]}

instance Monad (Matcher d) where
  return a = Matcher (\_s _i d -> return (a, 0, d))
  m >>= f =
    Matcher (\s i d -> do (a, j, d') <- runMatcher m s i d
                          (b, j', d'') <- runMatcher (f a) s (i + j) d'
                          return (b, j + j', d''))

instance Functor (Matcher d) where fmap = liftM
instance Applicative (Matcher d) where pure = return; (<*>) = ap

-- Associated operations to implement. Their definitions may freely use
-- the Matcher term constructor. Do not change the types!

nextChar :: Matcher d Char
nextChar = undefined

getData :: Matcher d d
getData = undefined

putData :: d -> Matcher d ()
putData = undefined

mfail :: Matcher d a
mfail = undefined

pick :: [Matcher d a] -> Matcher d a
pick = undefined

grab :: Matcher d a -> Matcher d (String, a)
grab = undefined

both :: Matcher d a -> (a -> Matcher d b) -> Matcher d b
both = undefined

neg :: Matcher d a -> Matcher d ()
neg = undefined

-- APREL-specific functions to implement. `matchRE` should NOT use the Matcher
-- term constructor or the `runMatcher` projection, only the above functions.

type Captures = [String]

matchRE :: RE -> Matcher Captures ()
matchRE = undefined

matchTop :: RE -> String -> Maybe Captures
matchTop = undefined
