-- This is a suggested skeleton for your main black-box tests. You are not
-- required to use Tasty, but be sure that your test suite can be build
-- and run against any implementation of the APREL APIs.

import AST
import Parser
import Matcher
-- Do not import from the XXXImpl modules here!

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set as S

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = rudimentary -- replace this

testCaseBad :: Show a => String -> Either String a -> TestTree
testCaseBad s t =
  testCase ("*" ++ s) $
    case t of
      Left e -> return ()
      Right a -> assertFailure $ "Unexpected success: " ++ show a

rudimentary :: TestTree
rudimentary =
  testGroup "Rudimentary tests"
    [testCase "parse1" $
       parseRE "a(#b*)" @?= Right re1,
     testCaseBad "parse2" $
       parseRE "(#*b)a",
     testCase "match1" $
       matchTop re1 "abb" @?= Just ["bb"],
     testCase "*match2" $
       matchTop re1 "bba" @?= Nothing]
  where
    re1 = RSeq [rChar 'a', RCapture (rStar (rChar 'b'))]
    rChar c = RClass False (S.singleton c)
    rStar r = RRepeat r (0,maxBound)
