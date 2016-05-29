module BaseConvert.Bases where

import Control.Error.Util

import Data.List as List
import Data.Map.Lazy as Map
import Data.Maybe

type BaseName = String
type Alphabet = String

data Base = Base
  {  name     :: BaseName
  ,  alphabet :: Alphabet
  }
  deriving Show

newtype Bases = Bases [Base]
  deriving Show

basesToMap :: Bases -> Map BaseName Base
basesToMap (Bases bs) = fromList . List.map (\b -> (name b , b)) $ bs

lookupBase :: Bases -> BaseName -> Either String Base
lookupBase bases baseName =
  note ("Invalid base: " ++ baseName) $ Map.lookup baseName (basesToMap bases)

validateBase :: Base -> Either String Alphabet
validateBase base =
  let alph = alphabet base in
  if length (nub alph) == length alph
  then
    Right alph
  else
    Left $
     "Base " ++ (name base)
     ++ " has duplicate digits"
     ++ " (" ++ alph ++ ")"

validateNumber :: Base -> String -> Either String String
validateNumber base num =
  let alph = alphabet base in
  if all (\c -> c `elem` alph) num
  then
    Right num
  else
    Left $
      num ++ " is not a valid number in base " ++ (name base)
      ++ " (" ++ alph ++ ")"

fromBase :: Alphabet -> String -> Integer
fromBase alph num = sum $
  zipWith (*) placeValues digitValues

  where
    placeValues :: [Integer]
    placeValues =  iterate (\p -> (p *) . toInteger . length $ alph) 1

    digitValues :: [Integer]
    digitValues =  List.map (toInteger . fromJust . (`elemIndex` alph)) (reverse num)

toBase :: Alphabet -> Integer -> String
toBase alph num = go num ""
  where
    alphLen = toInteger . length $ alph

    go :: Integer -> String -> String
    go 0 accum = accum
    go n accum =
      let c = alph !! lsd n
      in  go (n `div` alphLen) (c:accum)

    lsd :: Integer -> Int
    lsd n = fromInteger . (n `mod`) $ alphLen

convertBases :: Alphabet -> Alphabet -> String -> String
convertBases from to num =
  toBase to . fromBase from $ num
