{-# LANGUAGE OverloadedStrings #-}

module Main where

import Paths_baseconvert

import Control.Error.Util
import Options.Applicative

import Data.List as List
import Data.Maybe
import Data.Map.Lazy as Map
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson

data Base = Base
  {  name     :: String
  ,  alphabet :: String
  }
  deriving Show

data Bases = Bases [Base]
  deriving Show

instance FromJSON Base where
  parseJSON (Object v) = Base <$>
    (v .: "name") <*>
    (v .: "alphabet")

  parseJSON _ = error "Not an object"

instance FromJSON Bases where
  parseJSON (Object v) = Bases <$> (v .: "bases")

  parseJSON _ = error "Not an object"

basesToMap :: Bases -> Map String String
basesToMap (Bases bs) = fromList . List.map (\b -> (name b , alphabet b)) $ bs

fromBase :: String -> String -> Integer
fromBase base num = sum $
  zipWith (*) placeValues digitValues

  where
    placeValues :: [Integer]
    placeValues =  iterate (\p -> (p *) . toInteger . length $ base) 1

    digitValues :: [Integer]
    digitValues =  List.map (toInteger . fromJust . (`elemIndex` base)) (reverse num)

toBase :: String -> Integer -> String
toBase base num = go num ""
  where
    baseLen = toInteger . length $ base

    go :: Integer -> String -> String
    go 0 accum = accum
    go n accum =
      let c = base !! lsd n
      in  go (n `div` baseLen) (c:accum)

    lsd :: Integer -> Int
    lsd n = fromInteger . (n `mod`) $ baseLen

convertBases :: String -> String -> String -> String
convertBases from to num =
  toBase to . fromBase from $ num

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
  bases <- getBases
  case bases of
    Left msg ->
      putStrLn msg
    Right bs ->
      case getFields (basesToMap bs) of
        Left msg ->
          putStrLn msg
        Right (fro' , to') ->
          putStrLn $ convertBases fro' to' num
  where
    getBases :: IO (Either String Bases)
    getBases =  do
      fileName <- getDataFileName "data/bases.json"
      text <- readFile fileName
      return $ (eitherDecode . B.pack $ text :: Either String Bases)
    
    getFields :: Map String String -> Either String (String , String)
    getFields bases = do
      fro' <- note ("Invalid base: " ++ fro) $ Map.lookup fro bases
      to'  <- note ("Invalid base: " ++ to)  $ Map.lookup to bases
      if not (validInput fro' num)
        then
          Left $ "Invalid input \""
          ++ num
          ++ "\" for base "
          ++ fro
          ++ " (" ++ fro' ++ ")"
        else
          return (fro' , to')

    validInput :: String -> String -> Bool
    validInput fro inp = all (\c -> c `elem` fro) inp

main :: IO ()
main = execParser opts >>= baseConvert
  where
    opts = info (helper <*> bcOpt)
      ( fullDesc
        <> progDesc "Convert between arbitrary bases"
        <> header "baseConvert - convert between arbitrary bases"
      )
