{-# LANGUAGE OverloadedStrings #-}

module Main where

import BaseConvert.Bases
import BaseConvert.JSON

import Paths_baseconvert

import Options.Applicative

data BCOpt = BCOpt
  { baseFrom :: String
  , baseTo   :: String
  , inputNum :: String
  }

bcOpt :: Parser BCOpt
bcOpt =  BCOpt
  <$> (argument str (metavar "fromBase"))
  <*> (argument str (metavar "toBase"))
  <*> (argument str (metavar "input"))

baseConvert :: BCOpt -> IO ()
baseConvert (BCOpt fro to num) = do
  bases <- loadBases
  case bases of
    Left msg ->
      putStrLn msg
    Right bs ->
      case getFields bs of
        Left msg ->
          putStrLn msg
        Right (fro' , to') ->
          putStrLn $ convertBases fro' to' num
  where
    loadBases :: IO (Either String Bases)
    loadBases =  do
      fileName <- getDataFileName "data/bases.json"
      text <- readFile fileName
      return $ readBasesJSON text
    
    getFields :: Bases -> Either String (Alphabet , Alphabet)
    getFields bases = do
      fro' <- lookupBase bases fro
      to'  <- lookupBase bases to
      _    <- validateNumber fro' num
      return (alphabet fro' , alphabet to')

main :: IO ()
main = execParser opts >>= baseConvert
  where
    opts = info (helper <*> bcOpt)
      ( fullDesc
        <> progDesc "Convert between arbitrary bases"
        <> header "baseConvert - convert between arbitrary bases"
      )
