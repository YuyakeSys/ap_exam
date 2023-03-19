module Main where

import AST
import Parser
import Matcher
import qualified Data.Set as S

import System.Environment (getArgs)
import System.IO (Handle, IOMode(..), stdin, stderr, hPutStr, withFile,
                  hGetContents)

doFile :: RE -> String -> Handle -> IO ()
doFile r pref h =
  do fc <- hGetContents h
     mapM_ (\s -> case matchTop r s of
                    Nothing -> return ()
                    Just _ -> putStr $ pref ++ s ++ "\n")
           (lines fc)

doFiles :: RE -> [String] -> IO ()
doFiles r fns = 
  if null fns then
    doFile r "" stdin
  else
    mapM_ (\fn -> let pref = if length fns > 1 then fn ++ ":" else ""
                  in withFile fn ReadMode (doFile r pref))
          fns

rDotStar = RRepeat (RClass True S.empty) (0,maxBound)

main =
  do as <- getArgs
     let (xopt, as1) = case as of "-x":as1 -> (True,as1); _ -> (False,as)
     case as1 of
       [] -> hPutStr stderr "Usage: aprel [-x] REGEXP [FILE]...\n"
       pat:files ->
         case parseRE pat of
           Left e -> hPutStr stderr $ "aprel: " ++ e ++ "\n"
           Right r ->
             let r1 = if xopt then r else RSeq [rDotStar, r, rDotStar]
             in doFiles r1 files
