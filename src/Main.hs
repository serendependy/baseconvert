module Main where

import Control.Error.Util
import Options.Applicative

import Data.List as List
import Data.Maybe
import Data.Map.Lazy as Map

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

bases :: Map String String
bases = fromList
  [ ("dec"
    , "0123456789")
  , ("hex"
    , "0123456789abcdef")
  , ("base62"
    , "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
  ]

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
baseConvert (BCOpt fro to num) =
  case getFields of
    Left msg ->
      putStrLn msg
    Right (fro' , to') ->
      putStrLn $ convertBases fro' to' num
  where
    getFields :: Either String (String , String)
    getFields = do
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
        <> footer ("where bases can be " ++ (show . Map.keys $ bases))
      )
