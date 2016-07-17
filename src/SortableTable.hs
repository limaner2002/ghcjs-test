{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module SortableTable
    ( testTable
    ) where

import Prelude ()
import ClassyPrelude

import Reflex
import Reflex.Dom

type DispHead = forall a c m t. (MonoFoldable c, MonadWidget t m, Monoid a) => (a -> Element c -> m a) -> c -> m a
type DispBody = forall a c m t. (MonoFoldable c, MonoFoldable (Element c), MonadWidget t m, Monoid a) => (a -> Element (Element c) -> m a) -> c -> m a
type DispCell = forall a b m t. (MonadWidget t m, Monoid b) => a -> m b

header :: [String]
header = ["First", "Second"]

bodyContents :: [[Int]]
bodyContents = [ [1, 3]
               , [4, 2]
               , [7, 5]
               , [4, 1]
               , [1, 9]
               , [2, 14]
               , [13, 93]
               ]

dispTablePart :: (MonoFoldable c, MonadWidget t m, Monoid a) => (a -> Element c -> m a) -> String -> c -> m a
dispTablePart f tablePart cts =
    el tablePart $ foldM f mempty cts

dispHeader :: DispHead
dispHeader f cts = dispTablePart f "thead" cts

dispBody :: DispBody
dispBody f cts = foldM (\_ -> dispTablePart f "tbody") mempty cts

dispCell :: MonadWidget t m => String -> m ()
dispCell content = el "th" $ text content

dispCellEvt :: MonadWidget t m => (Int, String) -> m (Event t Int)
dispCellEvt (idx, item) = do
  (e, _) <- el' "th" $ text item
  return $ pushAlways (const $ return idx) $ domEvent Click e

-- dispTable :: (MonoFoldable a, MonoFoldable b) => (a -> m c) -> (b -> m c) -> DispHead -> a -> DispBody -> b -> m c
-- dispTable dispCellHead dispCellBody dispHead head dispBody body =
--   elAttr "table" ("class" =: "pure-table pure-table-striped") $ do
--     dispHeader dispCellHead head
--     dispBody dispCellBody body

testHeader :: MonadWidget t m => m ()
testHeader = dispHeader (\_ -> dispCell) header

testHeaderEvt :: MonadWidget t m => m [Event t Int]
testHeaderEvt = dispHeader dispCellEvts $ zip [0..] header
  where
    dispCellEvts evts x = do
      r <- dispCellEvt x
      return $ r : evts

testTable :: MonadWidget t m => m ()
testTable =
  elAttr "table" ("class" =: "pure-table pure-table-striped") $ do
    evts <- testHeaderEvt
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
    blank
    -- dispBody (\_ -> dispCell . show) bodyContents

sortTable :: Int -> [[Int]]
sortTable idx = sortOn (flip indexEx idx) bodyContents

shouldReverse :: Bool -> [a] -> [a]
shouldReverse False items = items
shouldReverse True items = reverse items
