{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

import Reflex
import Reflex.Dom
import Data.Monoid

import Common
import Bootstrap
-- import PureCSS

toComponent :: Component B a => a -> CompBackend B a
toComponent comp = wrapComponent comp

fontAwesome :: (MonadWidget t m) => m ()
fontAwesome = elAttr "link" (  "rel" =: "stylesheet"
                            <> "href" =: "font-awesome-4.6.3/css/font-awesome.css"
                            ) blank

createItem :: MonadWidget t m => (Int, String) -> m (Event t Int)
createItem (idx, item) = do
  (e, _) <- el' "div" $ text item
  return $ pushAlways (const (return idx)) $ domEvent Click e

dispSortedCol :: MonadWidget t m => m [Event t Int] -> m ()
dispSortedCol mEvts = do
  el "div" $ text "Sorted by"

  _ <- el "div" . holdDyn 0 . leftmost =<< mEvts
  return ()

thead :: MonadWidget t m => [m (Event t Int)]
thead = fmap createItem $ zip [1..] ["First", "Second"]

tbody :: MonadWidget t m => [[m ()]]
tbody = (fmap . fmap) text [["entry1", "entry2"], ["entry3", "entry4"]]

main :: IO ()
main = mainWidgetWithHead (fontAwesome >> header) $ do
         renderTable (toComponent TblStriped) thead tbody
         dispSortedCol $ sequence thead