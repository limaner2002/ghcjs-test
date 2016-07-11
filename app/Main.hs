{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

import Prelude ()
import ClassyPrelude

import Reflex
import Reflex.Dom

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

-- dispSortedCol :: MonadHold t m => [Event t Int] -> m (Dynamic t Int)
-- dispSortedCol evts = do
--   el "div" $ text "Sorted by"

--   d <- holdDyn 0 . leftmost $ evts
--   el "div" $ display d
--   return d

thead :: MonadWidget t m => [m (Event t Int)]
thead = fmap createItem $ zip [0..] ["First", "Second"]

tbody :: MonadWidget t m => [[Int]] -> [[m ()]]
tbody contents = do
  (fmap . fmap) (text . show) contents

bodyContents :: [[Int]]
bodyContents = [[1, 3], [4, 2]]

dispTable :: MonadWidget t m => [[m c]] -> m ([Event t Int], [[c]])
dispTable = renderTable (toComponent TblStriped) thead

-- dispTable' :: MonadWidget t m => Dynamic t [[Int]] -> m ([Event t Int], [[Int]])
-- dispTable' sorted = do
--   r <- mapDyn

mainView :: MonadWidget t m => m ()
mainView = do
  (evts, _) <- dispTable $ tbody bodyContents
  dynIdx <- holdDyn 0 $ leftmost evts
  sorted <- mapDyn sortTable dynIdx
  elAttr "table" ("class" =: "table table-striped") $ el "tbody" $ 
     simpleList sorted (\x -> el "tr" $
                           simpleList x (\y -> el "td" $ dynText =<< mapDyn show y)
                       )
  blank

         -- clickE <- Reflex.Dom.button "Reverse it!"
         -- reversed <- toggle False clickE
         -- dyns <- mapDyn shouldReverse reversed
         
         -- simpleList dyns (\x -> el "li" $ dynText =<< mapDyn show x)
         -- return ()
  

sortTable :: Int -> [[Int]]
sortTable idx = sortOn (flip indexEx idx) bodyContents

-- dynList :: MonadHold t m => [a] -> m (Dynamic t [a])
-- dynList =

items :: [Int]
items = [1,5,3,4]

shouldReverse :: Bool -> [Int]
shouldReverse False = items
shouldReverse True = reverse items

main :: IO ()
main = mainWidgetWithHead (fontAwesome >> header) $ do
         -- dispTable $ tbody bodyContents
             mainView
             
         -- r <- mapDyn (dispTable . tbody) dn
