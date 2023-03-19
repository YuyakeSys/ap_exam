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
nextChar = Matcher(\s _ d -> case s of
                              [] -> []
                              (x:_) -> [(x, 1, d)])

getData :: Matcher d d
getData = Matcher(\_ _ d -> [(d,0,d)])

putData :: d -> Matcher d ()
putData d'= Matcher(\_ _ _ -> [((), 0, d')])

mfail :: Matcher d a
mfail = Matcher(\_ _ _ -> [])


pick :: [Matcher d a] -> Matcher d a
pick [] = mfail
pick xs = Matcher(\s i d -> case xs of
                            (c:cs) -> case runMatcher c s i d of
                                      [] -> runMatcher (pick cs) s i d
                                      (a,j,d'):_ -> [(a,j,d')])

-- grab m succeeds whenever m mathches consumes what m consumes and does any state
-- modifications m does, in addition to the result from m, grab m also returns the
-- a copy of the input string that m consumed. 
grab :: Matcher d a -> Matcher d (String, a)
grab m = Matcher(\s i d -> case runMatcher m s i d of
                           [] -> []
                           [(a,j,d')] -> [((take j s, a), j, d')]) -- take j s is the string m consumed

--both 𝑚1 𝑚2 succeeds whenever m1 and m2 a(a is the result from m1) both match
-- the same prefix of the input string.
both :: Matcher d a -> (a -> Matcher d b) -> Matcher d b
both m1 f = Matcher(\s i d -> case runMatcher m1 s i d of
                              [] -> []
                              (a,j,d'):_ -> runMatcher (f a) s (i+j) d')

-- neg m succeeds whenever m fails
neg :: Matcher d a -> Matcher d ()
neg m = Matcher(\s i d -> case runMatcher m s i d of
                          [] -> [((),0,d)]
                          _ -> [])

-- APREL-specific functions to implement. `matchRE` should NOT use the Matcher
-- term constructor or the `runMatcher` projection, only the above functions.

type Captures = [String]

matchRE :: RE -> Matcher Captures ()
matchRE (RClass b s) = do c <- nextChar
                          if (b && c `elem` s) || (not b && c `notElem` s)
                            then mfail
                            else return ()
matchRE (RSeq []) = return ()
matchRE (RSeq (x:xs)) = do matchRE x
                           matchRE (RSeq xs)
matchRE (RAlt r1 r2) = do pick [matchRE r1, matchRE r2]
matchRE (RConj r1 r2) = do both (matchRE r1) (\_ -> matchRE r2)
matchRE (RCapture r) = do (s, _) <- grab (matchRE r)
                          d <- getData
                          putData (s:d)
matchRE (RNeg r) = do neg (matchRE r)
matchRE (RBackref _) = undefined
-- replicate :: Int -> a -> [a]
matchRE (RRepeat r (a,b)) = do matchRE (RSeq (replicate a r))
                               matchRE (RSeq (replicate b (RAlt r (RSeq []))))
matchTop :: RE -> String -> Maybe Captures
matchTop r s = case runMatcher (matchRE r) s 0 [] of
                [] -> Nothing
                ((),_,x):_ -> Just x