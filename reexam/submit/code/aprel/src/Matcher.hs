-- Do not modify this file. Put all your Matcher code in MatcherImpl.hs
module Matcher (
  Matcher(..),
  nextChar,
  getData, putData,
  mfail, pick,
  grab,
  both, neg,
  matchRE, matchTop) where

import MatcherImpl

