{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bootstrap
    ( bootstrapFileInput
    , header
    , B
    ) where

import Prelude ()
import ClassyPrelude
import Reflex
import Reflex.Dom hiding (button)
import Data.Default
import qualified Data.Map as Map

import Common

data Bootstrap = Bootstrap

type B = Bootstrap

instance Component Bootstrap BtnClass where
    data CompBackend Bootstrap BtnClass = BootstrapBtnClass BtnClass
    wrapComponent btn = BootstrapBtnClass btn

    getComponent (BootstrapBtnClass v) = unBtnClass v
        where
          unBtnClass BtnPrimary = "btn-primary"
          unBtnClass BtnDefault = "btn-default"
          unBtnClass BtnSuccess = "btn-success"
          unBtnClass BtnInfo = "btn-info"
          unBtnClass BtnWarning = "btn-warning"
          unBtnClass BtnDanger = "btn-danger"
          unBtnClass BtnLink = "btn-link"

    renderComponent btn label = button label bootstrapConfig
        where
          bootstrapConfig = ButtonConfig (  "class" =: ("btn" <> " " <> getComponent btn))

instance Table Bootstrap where
    renderTable tbl header rows = renderIt
        where
          comp = getComponent tbl
          renderIt = do
            elAttr "table" attrs $ do
                el "thead" $ dispCells "th" header
                el "tbody" $ do
                    mapM (dispCells "td") rows
                    blank
          attrs = ("class" =: ("table" <> " " <> comp))

bootstrapFileInput :: MonadWidget t m => String -> m (FileInput t)
bootstrapFileInput label =
    elAttr label ("class" =: "btn btn-default btn-file") $ do
      text label
      fileInput (FileInputConfig $ constDyn ("id" =: "fileUpload" <> "multiple" =: "multiple" <> "style" =: "display: none"))

header :: (MonadWidget t m) => m ()
header = elAttr "link" (Map.fromList [ ("rel", "stylesheet")
                                              , ("href", "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css")
                                              , ("type", "text/css")
                                              ]
                                ) blank

data BootstrapTableConfig = BootstrapTableConfig (Map.Map String String)

instance Default BootstrapTableConfig where
    def = BootstrapTableConfig ("class" =: "table")

instance Component Bootstrap TblClass where
    data CompBackend Bootstrap TblClass = BootstrapTblClass TblClass
    wrapComponent tbl = BootstrapTblClass tbl

    getComponent (BootstrapTblClass tbl) = unTblClass tbl
        where
          unTblClass TblDefault = mempty
          unTblClass TblStriped = "table-striped"
          unTblClass TblBordered = "table-bordered"
          unTblClass TblHover = "table-hover"
          unTblClass TblCondensed = "table-condensed"


-- -- This Takes in a collection of rows and displays the table
-- bootstrapTableWithConfig :: (Traversable t, MonadWidget t2 m) => BootstrapTableConfig -> t (m b) -> t (t (m b)) -> m ()
-- bootstrapTableWithConfig (BootstrapTableConfig attrs) header rows = do
--   elAttr "table" attrs $ do
--     el "thead" $ do
--       dispCells "th" header
--     el "tbody" $ do
--       mapM (dispCells "td") rows
--       return ()

-- data BootstrapTableStyle = TblDefault
--                 | TblStriped
--                 | TblBordered
--                 | TblHover
--                 | TblCondensed

-- getTableClass :: BootstrapTableStyle -> String
-- getTableClass TblDefault = mempty
-- getTableClass TblStriped = "table-striped"
-- getTableClass TblBordered = "table-bordered"
-- getTableClass TblHover = "table-hover"
-- getTableClass TblCondensed = "table-condensed"

-- bootstrapTable :: (Traversable t, MonadWidget t2 m) => BootstrapTableStyle -> t (m b) -> t (t (m b)) -> m ()
-- bootstrapTable tblStyle = bootstrapTableWithConfig cfg
--     where
--       cfg = BootstrapTableConfig ("class" =: ("table" <> " " <> getTableClass tblStyle))

