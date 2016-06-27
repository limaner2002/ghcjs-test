{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Common
    ( BtnClass (..)
    , ButtonConfig (..)
    , button
    , Component (..)
    , TblClass (..)
    , Table (..)
    , dispCells
    )where

import Prelude ()
import ClassyPrelude
import qualified Data.Map as Map
import Reflex
import Reflex.Dom hiding (button)
import Data.Default

data BtnClass = BtnPrimary
              | BtnDefault
              | BtnSuccess
              | BtnInfo
              | BtnWarning
              | BtnDanger
              | BtnLink
              | BtnDisabled
              | BtnActive

data ButtonConfig = ButtonConfig (Map.Map String String)
  deriving Show

-- instance Default ButtonConfig where
--     def = ButtonConfig ("type" =: ("button" <> " " <> btnClass BtnDefault))

button :: MonadWidget t m => String -> ButtonConfig -> m (Event t ())
button label (ButtonConfig attrs) = do
    (e, _) <- elAttr' label attrs $ text label
    return $ domEvent Click e

class Component b a where
    data CompBackend b a :: *
    wrapComponent :: a -> CompBackend b a
    getComponent :: CompBackend b a -> String
    renderComponent :: MonadWidget t m => CompBackend b a -> String -> m (Event t())

data TblClass = TblDefault
              | TblStriped
              | TblBordered
              | TblHover
              | TblCondensed

class Table b where
    renderTable :: (Traversable t, MonadWidget t1 m) => CompBackend b TblClass -> t (m a) -> t (t (m a)) -> m ()

-- Takes in a collection of cells and displays them
dispCells :: (Traversable t, MonadWidget t1 m) => String -> t (m b) -> m (t b)
dispCells label cells = el "tr" $ mapM dispCell cells
    where
      dispCell = el label