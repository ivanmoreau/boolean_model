
   
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import           BooleanModel (someFunc)
import qualified Brick.AttrMap as A
import Brick
import qualified Brick.Focus as F
import           Brick.Forms ((@@=))
import qualified Brick.Main as M
import qualified Brick.Types as T
import           Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B

import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , updateAttrMap, vLimit
  , str
  , withAttr)
import qualified Brick.Widgets.Edit as E
import           Data.Text
import qualified Graphics.Vty as V
import           Lens.Micro
import           Lens.Micro
import           Lens.Micro.TH
import           Lens.Micro.TH

data Name = Edit1
          | Edit2
          | Cleaned
          | Matrix
          | Docs
          | CleanedButton
          | MatrixButton
          | DocsButton
          deriving (Ord, Show, Eq)

data St =
    St { _focusRing :: F.FocusRing Name
       , _edit1 :: E.Editor String Name
       , _edit2 :: E.Editor String Name
       , _cleaned :: E.Editor String Name
       , _matrix :: E.Editor String Name
       , _docs :: E.Editor String Name
       }

makeLenses ''St

titleAttr :: A.AttrName
titleAttr = "title"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
    [ (B.borderAttr,         fg V.white )
    , (titleAttr,            fg V.white)
    ]

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
    where
        e1 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . Prelude.unlines)) (st^.edit1)
        e2 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . Prelude.unlines)) (st^.cleaned)
        e3 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . Prelude.unlines)) (st^.matrix)
        e4 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . Prelude.unlines)) (st^.docs)

        ui = updateAttrMap (A.applyAttrMappings borderMappings) $
            B.borderWithLabel (withAttr titleAttr $ str "Boolean Model") $
            vBox [opt, B.hBorder, browser]
        browser = str "Press Tab to switch between editors, Esc to quit."
        opt = hBox [input, B.vBorder, loader]
        input = C.center $ str "Query: " <+> (vLimit 1 e1) 
        loader = hLimit 30 $ vBox [
            str "Prep file:  " <+> (vLimit 1 e2),
            vLimit 1 $ C.center $ clickable CleanedButton $ str "[[Load]]",
            str "Matrix dile:" <+> (vLimit 1 e3),
            vLimit 1 $ C.center $ clickable MatrixButton $ str "[[Load]]",
            str "Og file:    " <+> (vLimit 1 e4),
            vLimit 1 $ C.center $ clickable DocsButton $ str "[[Load]]"
            ]
        old = C.center $
            (str "Input 1 (unlimited): " <+>  (hLimit 30 $ vLimit 5 e1)) <=>
            str " " <=>
            (str "Input 2 (limited to 2 lines): " <+> (hLimit 30 e2)) <=>
            str " " <=>
            str "Press Tab to switch between editors, Esc to quit."

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
        V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev

        _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
               Just Edit1 -> T.handleEventLensed st edit1 E.handleEditorEvent ev
               Just Edit2 -> T.handleEventLensed st edit2 E.handleEditorEvent ev
               Just Cleaned -> T.handleEventLensed st cleaned E.handleEditorEvent ev
               Just Matrix -> T.handleEventLensed st matrix E.handleEditorEvent ev
               Just Docs -> T.handleEventLensed st docs E.handleEditorEvent ev
               Nothing -> return st
appEvent st _ = M.continue st

initialState :: St
initialState =
    St (F.focusRing [Edit1, Cleaned, CleanedButton, Matrix, MatrixButton, Docs, DocsButton])
       (E.editor Edit1 Nothing "")
       (E.editor Edit2 (Just 2) "")
       (E.editor Cleaned (Just 1) "")
       (E.editor Matrix (Just 1) "")
       (E.editor Docs (Just 1) "")

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.brightBlack)
    , (E.editFocusedAttr,            V.black `on` V.white)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

theApp :: M.App St e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do
    st <- M.defaultMain theApp initialState
    putStrLn "In input 1 you entered:\n"
    putStrLn $ Prelude.unlines $ E.getEditContents $ st^.edit1
    putStrLn "In input 2 you entered:\n"
    putStrLn $ Prelude.unlines $ E.getEditContents $ st^.edit2