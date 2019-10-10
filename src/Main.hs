{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void
import Control.Monad
import Data.Graph.Inductive
import qualified Data.Text.Lazy as T
import qualified Data.GraphViz as G
import Data.Graph.Inductive.Example
import Data.GraphViz.Printing
import qualified Data.Set as Set
import qualified Data.List as List

type Parser = Parsec Void String

main :: IO ()
main = do
  putStr "hello, world\n"

  return ()

ghci :: IO ()
ghci = do
  z <- readFile "in.txt"
  case parse dumpP "" z of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right dump -> do
      writeFile "out.dot" $ T.unpack $ renderDot $ toDot $ dumpToDot (shorterFiles dump)

shorterFiles :: Dump -> Dump
shorterFiles = mapFiles (\file -> maybe file id (List.stripPrefix "github.com/sourcegraph/sourcegraph@/" file))

dumpToDot :: Dump -> G.DotGraph String
dumpToDot (Dump goroutines) =
  let
    unique :: Ord a => [a] -> [a]
    unique = Set.toList . Set.fromList
    frameID (Frame _ (name, line)) = name ++ ":" ++ show line
    allnames = unique $ concatMap (\(Goroutine _ _ frames) -> map frameID frames) goroutines
    alledges = concatMap (\(Goroutine _ _ frames) -> zipWith (\f1 f2 -> (frameID f2, frameID f1, ())) frames (tail frames)) goroutines
  in
    G.graphElemsToDot G.nonClusteredParams (map (\x -> (x, x)) allnames) alledges

mapFiles :: (String -> String) -> Dump -> Dump
mapFiles f (Dump goroutines) = Dump (map (\(Goroutine number reason frames) -> Goroutine number reason $ map (\(Frame name (file, line)) -> Frame name (f file, line)) frames) goroutines)

lean :: Dump -> String
lean (Dump goroutines) =
  let
    leanFrame (Frame name (file, line)) = "  - " ++ file ++ ":" ++ show line
    leanGoroutine (Goroutine number reason frames) = unlines $
      ("goroutine " ++ show number ++ " [" ++ reason ++ "]:") :
      map leanFrame frames
  in
    unlines $ map leanGoroutine goroutines

dumpP :: Parser Dump
dumpP = do
  let
    frameP = do
      name <- notFollowedBy "goroutine" >> manyTill anySingle eol
      "\t"
      file <- manyTill anySingle ":"
      line <- decimal
      manyTill anySingle (void eol <|> eof)
      return $ Frame name (file, line)
    goroutineP = do
      number <- "goroutine " *> decimal <* " "
      reason <- "[" *> many (anySingleBut ']') <* "]:" <* eol
      frames <- many frameP
      return $ Goroutine number reason frames
  goroutines <- many goroutineP
  return $ Dump goroutines

-- name, (file, lineno)
data Frame = Frame String (String, Int) deriving (Show)
-- number, reason, stack
data Goroutine = Goroutine Int String [Frame] deriving (Show)

data Dump = Dump [Goroutine] deriving (Show)
