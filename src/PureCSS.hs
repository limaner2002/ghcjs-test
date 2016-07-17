{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PureCSS
    ( B
    , header
    ) where

import Prelude ()
import ClassyPrelude
import Reflex
import Reflex.Dom hiding (button)
import Data.Default
import qualified Data.Map as Map

import Common

data PureCSS

type B = PureCSS

instance Component PureCSS BtnClass where
    data CompBackend PureCSS BtnClass = PureCSSBtnClass BtnClass
    wrapComponent btn = PureCSSBtnClass btn

    getComponent (PureCSSBtnClass v) = unBtnClass v
        where
          unBtnClass BtnPrimary = "pure-button-primary"
          unBtnClass BtnDefault = mempty
          unBtnClass BtnSuccess = "button-success"
          unBtnClass BtnInfo = "button-secondary"
          unBtnClass BtnWarning = "button-warning"
          unBtnClass BtnDanger = "button-error"
          unBtnClass BtnLink = fail "BtnLink not implemented"
          unBtnClass BtnActive = fail "BtnActive not implemented"
          unBtnClass BtnDisabled = fail "BtnDisabled not implemented"

    renderComponent btn label = button label buttonConfig
        where
          buttonConfig = ButtonConfig (  "class" =: ("pure-button" <> " " <> getComponent btn))

instance Component PureCSS TblClass where
    data CompBackend PureCSS TblClass = PureCSSTblClass TblClass
    wrapComponent tbl = PureCSSTblClass tbl

    getComponent (PureCSSTblClass tbl) = unTblClass tbl
        where
          unTblClass TblDefault = mempty
          unTblClass TblStriped = "pure-table-odd"
          unTblClass TblBordered = "pure-table-bordered"
          unTblClass TblHover = undefined
          unTblClass TblCondensed = undefined

header :: (MonadWidget t m) => m ()
header = elAttr "link" (Map.fromList [ ("rel", "stylesheet")
                                     , ("href", "http://yui.yahooapis.com/pure/0.6.0/pure-min.css")
                                     , ("type", "text/css")
                                     ]
                       ) blank
