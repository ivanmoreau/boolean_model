{- |
Copyright: (c) 2022 Iván Molina Rebolledo
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Iván Molina Rebolledo <ivanmolinarebolledo@gmail.com>

See README for more info
-}

{-# LANGUAGE OverloadedStrings #-}

module BooleanModel
  ( someFunc,
  )
where

import Data.List (delete, nub)
import Data.Map (Map, delete, fromList, insert, insertWith, (!))
import qualified Data.Map as Dm
import Data.Text (Text, pack, replace, splitOn)
import Data.Text.IO as DT (readFile)



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

readTest name = do
  c <- DT.readFile name
  let r = genMatrix c
  return r


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
