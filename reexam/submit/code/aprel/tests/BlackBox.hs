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
       matchTop re1 "bba" @?= Nothing,
     testCase "Test Single Char1" $ parseRE "a" @?= Right re2,
     testCase "Test Single Char1'" $ parseRE "a" @?= parseRE "(a)",
     testCase "Test Single class1" $ parseRE "[^a-z0-2]" @?= Right re3,
     testCase "Test Single class2" $ parseRE "." @?= Right re4, 
     testCase "Test Single escape number" $ parseRE str5 @?= Right re5, 
     testCase "Test Seq escape number" $ parseRE "\n\t" @?= Right re6, 
     testCase "Test Single capture" $ parseRE str7 @?= Right re7,
     testCase "Test Single neg" $ parseRE "a!!!!!" @?= Right re8,
     testCase "lowerlehigher" $ parseRE "[9-0]" @?= Right re9,
     testCase "wrongclass" $ parseRE "[z-a]" @?= Right re27,
     testCase "allchar" $ parseRE "." @?= Right re10,
     testCase "2sequence" $ parseRE "ap" @?= Right re11,
     testCase "empty1" $ parseRE "" @?= Right re12,
     testCase "empty2" $ parseRE "()" @?= Right re12,
     testCase "empty3" $ parseRE "((()))" @?= Right re12,
     testCase "3sequence" $ parseRE "a[0-9]b" @?= Right re13,
     testCase "sequencing4" $ parseRE "[^a-z][a-z]" @?= Right re14, 
     testCase "alternation1" $ parseRE "a|bb" @?= Right re16,
     testCase "alternation2" $ parseRE "a|b|c" @?= parseRE "(a|b)|c",
     testCase "conjunction1" $ parseRE "a&[a-z]" @?= Right re18,
     testCase "conjunction2" $ parseRE "[abcd]&[ab]&[bc]" @?= parseRE "([abcd]&[ab])&[bc]",
     testCase "Test Complex" $ parseRE "(#a)b{1,2}c+[^A-Z]\\1" @?= Right re20,
     testCase "alt&conj1" $ parseRE "a|b|c&de" @?= parseRE "a|b|(c&de)",
     testCase "alt&conj2" $ parseRE "[abc]&[ab]|[a]" @?= parseRE "([abc]&[ab])|[a]",
     testCase "alt&conj3" $ parseRE "a|b|c&[a-c]" @?= parseRE "a|b|(c&[a-c])",
     testCase "repetition1" $ parseRE "a{2}" @?= Right re24,
     testCase "repetition2" $ parseRE "a{1,4}" @?= Right re25,
     testCase "repetition3" $ parseRE "a{4,}" @?= Right re26,
     testCaseBad "badrepe1" $ parseRE "a?+*" ,
     testCaseBad "badrepe2" $ parseRE "a{10,1}"
     ]
  where
    re1 = RSeq [rChar 'a', RCapture (rStar (rChar 'b'))]
    rChar c = RClass False (S.singleton c)
    rStar r = RRepeat r (0,maxBound)
    --Test Single Char1
    re2 = rChar 'a'
    re3 = RClass True (S.fromList "012abcdefghijklmnopqrstuvwxyz")
    re4 = RClass True (S.fromList "")
    --"Test Single escape number" 
    str5 = "\\%"
    re5 = RClass False (S.fromList "%")
    --"Test Seq escape number" 
    re6 = RSeq [RClass False (S.fromList "\n"),RClass False (S.fromList "\t")]
    str7 = "(#a)"
    re7 = RCapture (RClass False (S.fromList "a"))
    re8 = (RNeg (RNeg (RNeg (RNeg (RNeg (RClass False (S.fromList "a")))))))
    re9 = RClass False (S.fromList "")
    re10 = RClass True (S.fromList "")
    re11 = RSeq [RClass False (S.fromList "a"),RClass False (S.fromList "p")]
    re12 = RSeq []
    re13 = RSeq [RClass False (S.fromList "a"),RClass False (S.fromList "0123456789"),RClass False (S.fromList "b")]
    re14 = RSeq [RClass True (S.fromList "abcdefghijklmnopqrstuvwxyz"),RClass False (S.fromList "abcdefghijklmnopqrstuvwxyz")]
    re15 = RClass False (S.fromList "()abcde")
    re16 = (RAlt (RClass False (S.fromList "a")) (RSeq [RClass False (S.fromList "b"),RClass False (S.fromList "b")]))
    re17 = (RAlt (RClass False (S.fromList "a")) (RAlt (RClass False (S.fromList "b")) (RClass False (S.fromList "c"))))
    re18 = (RConj (RClass False (S.fromList "a")) (RClass False (S.fromList "abcdefghijklmnopqrstuvwxyz")))
    re19 = RConj (RClass False (S.fromList "abcd")) (RConj (RClass False (S.fromList "ab")) (RClass False (S.fromList "bc")))
    re20 = RSeq [RCapture (RClass False (S.fromList "a")),RRepeat (RClass False (S.fromList "b")) (1,2),RRepeat (RClass False (S.fromList "c")) (1,9223372036854775807),RClass True (S.fromList "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),RBackref 1]
    re21 = RAlt (RClass False (S.fromList "a")) (RAlt (RClass False (S.fromList "b")) (RConj (RClass False (S.fromList "c")) (RSeq [RClass False (S.fromList "d"),RClass False (S.fromList "e")])))
    re23 = RAlt (RClass False (S.fromList "a")) (RAlt (RClass False (S.fromList "b")) (RConj (RClass False (S.fromList "c")) (RClass False (S.fromList "abc"))))
    re24 = RRepeat (RClass False (S.fromList "a")) (2,2)
    re25 = RRepeat (RClass False (S.fromList "a")) (1,4)
    re26 = RRepeat (RClass False (S.fromList "a")) (4,maxBound)
    re27 = RClass False (S.fromList "")

