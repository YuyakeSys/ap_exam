-- Put your parser implementation in this file
module ParserImpl where

import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import qualified Data.Set as S
import AST
-- import ReadP or Parsec, as relevant

max1 :: Int
max1 = maxBound
allLetterDigit = ['a'..'z']++['A'..'Z']++['0'..'9']
reserved13 = "!#&()*+.?[\\{|"
reserved4 = "-\\]^"
reserved62 = ["0···9A···Za···z"]
-- Do not change the type!
parseRE :: String -> Either String RE
parseRE str = case readP_to_S (do
    result <- pRE
    eof
    return result) str of
    [] -> Left "Parse error"
    x -> case last x of
      (x, "") -> return x
      _ -> Left "Parse error"



-- RE ::= RESeq| conjuct
pRE :: ReadP RE
pRE = pRESeq
    <|> do
    a <- pRESeq
    pConjuct a


pConjuct :: RE -> ReadP RE 
pConjuct a = do
    char '&'
    b<- pRESeq
    pConjuct (RConj a b)
    <|> do
    char '|'
    b <- pRESeq
    pAlt a b
    <++ do
        return a

pAlt :: RE -> RE -> ReadP RE
pAlt a b = do
    char '|'
    c <- pRESeq
    pAlt (RAlt a b) c
    <|> do
    char '&'
    c <- pRESeq
    pAlt a (RConj b c)
    <++ do
        return $ RAlt a b

-- RESeq ::= pREElt RESeq' RESeq''| RESeq' RESeq''|  RESeq' RESeq''
pRESeq :: ReadP RE
pRESeq = do
    a <- pREElt
    return a
    <|> do
    -- a <- pREElt
    b <- pRESeq'
    return (RSeq b)
    <|> return (RSeq [])

-- RESeq' = pREElt | em
pRESeq' :: ReadP [RE]
pRESeq' = do
    a <- pREElt
    b <- many1 pREElt
    return $ a:b

-- REElt ::= RERep Elt'
pREElt :: ReadP RE
pREElt = do
    many skipEmpty
    r <- pRERep
    s <- pElt'
    return $ dealNeg s r

dealNeg :: [Char] -> RE -> RE
dealNeg [] a = a
dealNeg [x] a = RNeg a
dealNeg (x:xs) a = dealNeg xs (RNeg a)

-- Elt' = 𝜖 | ‘!’ Elt'
pElt' :: ReadP [Char]
pElt' = do
    a <- munch (\x -> x == '!')
    return $ a
    <++ return []

-- RERep ::= REAtom| REAtom ‘{’ Count ‘}’| REAtom ‘?’| REAtom ‘*’| REAtom ‘+’
pRERep :: ReadP RE
pRERep = do
    a <- pREAtom
    char '{'
    b <- pCount
    char '}'
    return (RRepeat a b)
    <|> do
    a <- pREAtom
    char '?'
    return (RRepeat a (0,1))
    <|> do
    a <- pREAtom
    char '*'
    return (RRepeat a (0,max1))
    <|> do
    a <- pREAtom
    char '+'
    return (RRepeat a (1,max1))
    <|> pREAtom


-- REAtom ::= RChar| Class| ‘\’ Number| pREAtom'
pREAtom :: ReadP RE
pREAtom = pRChar
    <|> pClass 
    <|> pRef  
    <|> pREAtom'

pRef = do
    string "\\"
    n <- pNumber
-- help define the back ref and simply number
    string "()"
    return $ RBackref n
    <++ do
    string "\\"
    n <- pNumber
    return $ RBackref n  
-- pREAtom'    ‘(’ RE ‘)’| ‘(’ ‘#’ RE ‘)’
pREAtom' :: ReadP RE
pREAtom' = do
    string "(#"
    n <- pRE
    char ')'
    return (RCapture n)
    <++ do
    char '('
    n <- pRE
    char ')'
    return n

-- Count ::= ::= Number| Number ‘,’| Number ‘,’ Number
pCount :: ReadP (Int,Int)
pCount = do
    a <- pNumber
    char ','
    b <- pNumber
    case a > b of
        True ->
            pfail 
        False ->
            return ((a,b))
    <++ do
    a <- pNumber
    char ','
    return ((a,max1))
    <++ do
    a <- pNumber
    return ((a,a))


-- Class ::= ‘[’ ClassItemz ‘]’| ‘[’ ‘^’ ClassItemz ‘]’| ‘.’
pClass :: ReadP RE
pClass = do
    char '['
    char '^'
    x <- pClassItemz
    char ']'
    return $ RClass True (S.fromList x)
    <|> do
    char '['
    x <- pClassItemz
    char ']'
    return $ RClass False (S.fromList x)
    <|> do
    char '.'
    return $ RClass True (S.fromList [])
    
-- ClassItemz ::= 𝜖 | ClassItem ClassItemz
pClassItemz :: ReadP [Char]
pClassItemz = do
    c <- pClassItem
    ds <- pClassItemz
    return $ c++ds
    <|> return []

-- ClassItem ::= CChar | CChar ‘-’ CChar
pClassItem :: ReadP [Char]
pClassItem = do
    x <- pCChar
    char '-'
    y <- pCChar
    if x < y
        then return [x..y]
        else return []
    <|> do
    x <- pCChar
    return [x]
    
-- RChar ::= [any character except the following 13: “!#&()*+.?[\{|”] | EscChar
pRChar :: ReadP RE
pRChar = do
    x <- satisfy (`notElem` reserved13)
    return (RClass False (S.singleton x))
    <|> do
    a <- pEscChar
    return $ (RClass False (S.singleton a))

-- CChar ::= [any character except the following 4: “-\]^”] | EscChar
pCChar :: ReadP Char
pCChar = do
    x <- satisfy (`notElem` reserved4)
    return x
    <|> pEscChar

-- EscChar ::= ‘\’ [any character except the following 62: “0···9A···Za···z”]| ‘\’ ‘n’| ‘\’ ‘t’
pEscChar :: ReadP Char
pEscChar = do
    string "\n"
    return '\n'
    <++ do
    string "\t"
    return '\t'
    <++ do
    char '\\'
    s <- satisfy(`notElem` allLetterDigit)
    return $ s


-- Number ::= Digit Number'
-- | empty
pNumber :: ReadP Int
pNumber = do
    x <- munch1 isDigit
    return $ read x 

-- helper function to delete single empty bracket
skipEmpty :: ReadP ()
skipEmpty = do
    a <- many1 (char '(')
    b <- many1 (char ')')
    return ()
    <++ do
    char '('
    char ')'
    return ()