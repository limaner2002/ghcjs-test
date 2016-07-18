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

dispCellEvt :: MonadWidget t m => (Int -> String -> m ()) -> (Int, String) -> m (Event t Int)
dispCellEvt f (idx, item) = do
  (e, _) <- el' "th" $ f idx item
  return $ pushAlways (const $ return idx) $ domEvent Click e

handleSortUpdate :: Int -> (Int, Bool) -> (Int, Bool)
handleSortUpdate idx (idxOld, revBool) =
    case idx == idxOld of
      True -> (idx, not revBool)
      False -> (idx, False)

dispDynamicBody :: (Show a, MonadWidget t m) => Dynamic t [[a]] -> m ()
dispDynamicBody dynBody = simpleList dynBody dispRows >> blank
    where
      dispRows x = el "tr" $ simpleList x dispDynCell
      dispDynCell x = el "td" $ dynText =<< mapDyn show x

testHeader :: MonadWidget t m => m ()
testHeader = dispHeader (\_ -> dispCell) header

testHeaderEvt :: MonadWidget t m => (Int -> String -> m ()) -> m [Event t Int]
testHeaderEvt f = dispHeader dispCellEvts $ zip [0..] header
  where
    dispCellEvts evts x = do
      r <- dispCellEvt f x
      return $ r : evts

-- Maybe using attributes to display the sort arrow will work
testTable :: MonadWidget t m => m ()
testTable =
  elAttr "table" ("class" =: "pure-table pure-table-striped") $ do
    rec evts <- testHeaderEvt (\_ -> text)

        let leftEvent = leftmost evts
        folded <- foldDyn handleSortUpdate (0, False) leftEvent

        sorted <- mapDyn (\(idx, revBool) -> shouldReverse revBool $ sortTable idx) folded

        el "tbody" $ dispDynamicBody sorted

    blank

sortTable :: Int -> [[Int]]
sortTable idx = sortOn (flip indexEx idx) bodyContents

shouldReverse :: Bool -> [a] -> [a]
shouldReverse False items = items
shouldReverse True items = reverse items
