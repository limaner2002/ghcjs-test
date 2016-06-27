{-# LANGUAGE JavaScriptFFI #-}

module FileUploader where

import Prelude ()
import ClassyPrelude

import Reflex
import Reflex.Dom hiding (button)
import GHCJS.Types
import GHCJS.DOM.Types hiding (Event)

foreign import javascript unsafe
            "var x = document.getElementById('fileUpload');\
             \   files = []\
             \ if ('files' in x) {\
             \     if (x.files.length > $1){\
             \         $r = x.files[$1].name \
             \     }\
             \ else { $r = 'index too big!' }\
             \ }" getFileName :: Int -> JSString

displayFileNames :: MonadWidget t m => [JSString] -> m ()
displayFileNames fNames = do
  el "ul" $
     mapM_ (el "li" . text . showFName) fNames

showFName :: JSString -> String
showFName = show

getFileNames :: [File] -> [JSString]
getFileNames [] = mempty
getFileNames files = go $ (length files) - 1
    where
      go (-1) = []
      go n = getFileName n : go (n - 1)


uploader :: MonadWidget t m => m ()
uploader = uploaderWith displayFileNames

uploaderWith :: MonadWidget t m => ([JSString] -> m ()) -> m ()
uploaderWith f = do
  filesDyn <- elAttr "label" ("class" =: "btn btn-default btn-file") $ do
      text "Add Files"
      value <$> fileInput (FileInputConfig $ constDyn ("id" =: "fileUpload" <> "multiple" =: "multiple" <> "style" =: "display: none"))

  el "div" $ do
      dyn =<< mapDyn (f . getFileNames) filesDyn
      blank
