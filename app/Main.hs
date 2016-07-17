{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

import Prelude ()
import ClassyPrelude

import Reflex
import Reflex.Dom

-- import Common
-- -- import Bootstrap
import PureCSS

-- toComponent :: Component B a => a -> CompBackend B a
-- toComponent comp = wrapComponent comp

fontAwesome :: (MonadWidget t m) => m ()
fontAwesome = elAttr "link" (  "rel" =: "stylesheet"
                            <> "href" =: "font-awesome-4.6.3/css/font-awesome.css"
                            ) blank

data Sorting = Ascending | Descending | NotSorted

createSortHeadCell :: MonadWidget t m => (Int, String, Sorting) -> m (Event t Int)
createSortHeadCell (idx, item, sorting) = do
  (e, _) <- el' "div" $ do
              text item
              showArrow
              el "span" blank
  return $ pushAlways (const (return idx)) $ domEvent Click e

showArrow :: MonadWidget t m => Sorting -> m ()
showArrow Ascending = elAttr "i" ("class" =: "fa fa-long-arrow-down")
showArrow Descending = elAttr "i" ("class" =: "fa fa-long-arrow-up")
showArrow NotSorted = blank

thead :: MonadWidget t m => [String] -> [m (Event t Int)]
thead cts = fmap createSortHeadCell $ zip3 [0..] cts $ repeat False

-- thead :: [String]
-- thead = ["First", "Second"]

tbody :: MonadWidget t m => [[Int]] -> [[m ()]]
tbody contents = do
  (fmap . fmap) (text . show) contents

bodyContents :: [[Int]]
bodyContents = [ [1, 3]
               , [4, 2]
               , [7, 5]
               , [4, 1]
               , [1, 9]
               , [2, 14]
               , [13, 93]
               ]

-- -- Takes in a collection of cells and displays them
-- dispCells :: (Traversable t, MonadWidget t1 m) => String -> t (m b) -> m (t b)
-- dispCells label cells = el "tr" $ mapM dispCell cells
--     where
--       dispCell = el label

dispTable :: MonadWidget t m => m ()
dispTable = elAttr "table" ("class" =: "pure-table pure-table-striped") $ do
              evts <- el "thead" $ dispCells "th" thead
              let leftEvent = leftmost evts

              folded <- foldDyn (\idx (idxOld, revBool) -> case idx == idxOld of
                                                             True -> (idx, not revBool)
                                                             False -> (idx, False)
                                ) (0, False) leftEvent

              sorted <- mapDyn (\(idx, revBool) -> shouldReverse revBool $ sortTable idx) folded

              el "tbody" $ 
                 simpleList sorted (\x -> el "tr" $
                                          simpleList x (\y -> el "td" $ dynText =<< mapDyn show y)
                                   )
              return ()      

mainView :: MonadWidget t m => m ()
mainView = do
  dispTable

sortTable :: Int -> [[Int]]
sortTable idx = sortOn (flip indexEx idx) bodyContents

items :: [Int]
items = [1,5,3,4]

shouldReverse :: Bool -> [a] -> [a]
shouldReverse False items = items
shouldReverse True items = reverse items

main :: IO ()
main = mainWidgetWithHead (fontAwesome >> header) $ do
             dispTable
