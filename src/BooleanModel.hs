{- |
Copyright: (c) 2022 Iván Molina Rebolledo

SPDX-License-Identifier: GPL-3.0-only
Maintainer: Iván Molina Rebolledo <ivanmolinarebolledo@gmail.com>

See README for more info
-}

{-# LANGUAGE OverloadedStrings #-}

module BooleanModel
  ( someFunc, genMatrix, search
  )
where

import qualified Data.ByteString as BS
import           Data.List ((\\), delete, intersect, nub, sort, union)
import qualified Data.Map as Dm
import           Data.Map (Map, delete, fromList, insert, insertWith, (!))
import           Data.Serialize
import           Data.Serialize.Text ()
import           Data.Text (Text, pack, replace, splitOn)
import           Data.Text.IO as DT (readFile)
import           Debug.Trace (trace)
import           Query

rone :: [Text]
rone = ["$user tweeted ", " $ht", "$ht ", " $user", "$user "]

fixWords :: Text -> [Text] -> Text
fixWords xs [] = xs
fixWords xs (l : ls) = fixWords (replace l (pack "") xs) ls

getWords :: Text -> Map Text (Map Int Bool)
getWords l =
  fromList
    ( map
        (\x -> (x, fromList [(0, False)]))
        (Data.List.delete "" (nub (splitOn " " (replace "\n" " " l))))
    )

emptymatrix :: Map Text (Map Int Bool) -> Int -> Map Text (Map Int Bool)
emptymatrix m c = Dm.map (\o -> fromList (map (\x -> (x, False)) [0 .. c])) m

aa :: Map Text (Map Int Bool) -> Text -> Int -> Map Text (Map Int Bool)
aa m t i = let r = m ! t in insert t (insert i True r) m

--mLine ::
byLine :: [Text] -> Map Text (Map Int Bool) -> Int -> Map Text (Map Int Bool)
byLine [] m _ = m
byLine (l : ls) m c =
  byLine
    ls
    (foldr (\x y -> aa y x c) m (Data.List.delete "" (nub (splitOn " " l))))
    (c + 1)

genMatrix t =
  let ff = fixWords t rone
      gw = getWords ff
      ts = (splitOn "\n" ff)
      em = emptymatrix gw (length ts)
   in byLine ts em 0


readd :: Text -> Map Text (Map Int Bool) -> [Int]
readd t m = case Dm.lookup t m of
  Just v -> map fst (Dm.toList (Dm.filter (== True) v))
  Nothing -> []



pw :: Query -> Int -> Map Text (Map Int Bool) -> [Int]
pw (QAnd l r) t m = sort $ (pw l t m) `intersect` (pw r t m)
pw (QOr l r) t m = sort $ (pw l t m) `union` (pw r t m)
pw (QNot e) t m = [0..t] \\ (pw e t m)
pw (QP v) t m = readd v m

search :: Query -> Map Text (Map Int Bool) -> [Int]
search q m = pw q (Dm.size (snd (Dm.elemAt 0 m))) m





someFunc :: IO ()

someFunc = putStrLn ("someFunc" :: String)
