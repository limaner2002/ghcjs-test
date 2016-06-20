{-# LANGUAGE RecursiveDo #-}

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.DynamicList
import qualified Data.Map as Map
import Data.Monoid
import Reflex.Dom.Contrib.Widgets.Modal

app :: MonadWidget t m => m ()
app = do
  rec _ <- elAttr "form" ("class" =: "form-horizontal") $ dynamicList single snd (const never) (Left "edit me" <$ addItem)
                      [Right "alpha", Right "bravo", Right "charlie"]
      addItem <- addButton "Add Item"

  return ()

single :: MonadWidget t1 m =>
          t -> Either String String -> Event t1 (Either String String) -> m ((), Event t1 ())
single _ v _ = do
    elAttr "div" ("class" =: "form-group") $ do
      -- del <- button "remove"
      e <- elAttr "div" ("class" =: "col-md-4") $ -- do
         -- rec val <- holdDyn v $ leftmost [ups, edits]
         --     edits <- editInPlace (constant True) val
         -- return edits
         bootInput v
      del <- elAttr "label" ("class" =: "col-md-1 control-label") $ delButton

      return (e, del)

addButton :: MonadWidget t m => String -> m (Event t ())
addButton label = do
  (e, _) <- elAttr' "input" (  "class" =: "btn btn-primary"
                               <> "type" =: "button"
                               <> "value" =: label
                               ) blank
  return $ domEvent Click e

delButton :: MonadWidget t m => m (Event t ())
delButton = do
  (e, _) <- elAttr' "i" (  "class" =: "fa fa-remove text-danger pull-right"
                        <> "style" =: "font-size: 24px; vertical-align: middle"
                        ) blank
  return $ domEvent Click e

bootInput :: MonadWidget t m => Either String String -> m ()
bootInput input = do
  let v = case input of
         Left val -> "placeholder" =: val
         Right val -> "value" =: val
  elAttr "input" (  "class" =: "form-control"
                 <> "placeholder" =: "Edit me"
                 <> v
                 ) blank

bootTable :: MonadWidget t m => m ()
bootTable = do
  rec _ <- elAttr "table" ("class" =: "table table-striped") $ el "tbody" $ dynamicList row snd (const never) ("New plugin!" <$ addItem) ["Magic Deploy", "Business Charts", "Get All Apps"]
      addItem <- addButton "Add Item"
  elAttr "label" ("class" =: "btn btn-default btn-file") $ do
         text "Browse"
         elAttr "input" (  "type" =: "file"
                        <> "multiple" =: "multiple"
                        <> "style" =: "display: none"
                        ) blank

  return ()

confirmModal :: MonadWidget t m => m (Event t (Either e a), Event t ())
confirmModal = mkModalBody (const never) modalFooter body
    where
      body = el "div" (text "An 'el' should be able to be a Dynamic")

modalFooter :: MonadWidget t m => Dynamic t (Either e a) -> m (Event t (), Event t ())
modalFooter _ = do
  (e1, _) <- elAttr' "button" (  "type" =: "button"
                              <> "class" =: "btn btn-success pull-left"
                              ) blank
  (e2, _) <- elAttr' "button" (  "type" =: "button"
                              <> "class" =: "btn btn-danger pull-right"
                              ) blank
  
  return (domEvent Click e1, domEvent Click e2)

row :: MonadWidget t1 m =>
       t -> String -> Event t1 String -> m ((), Event t1())
row _ v _ = do
  el "tr" $ do
    e <- el "td" (text v)
    del <- el "td" delButton
    return (e, del)

main :: IO ()
main = mainWidgetWithHead (fontAwesome >> bootstrap) $ elAttr "div" ("style" =: "padding: 25px") bootTable


-- import Safe (readMay)
-- import Data.Monoid

-- -- main = mainWidget $ el "div" $ do
-- --          nx <- numberInput
-- --          d <- dropdown "*" (constDyn ops) def
-- --          ny <- numberInput
-- --          values <- combineDyn (,) nx ny
-- --          result <- combineDyn (\o (x,y) -> stringToOp o <$> x <*> y) (_dropdown_value d) values
-- --          resultString <- mapDyn show result
-- --          text " = "
-- --          dynText resultString
-- --          el "br" blank
-- --          evt <- button "Press me!"
-- --          nPressed <- count evt
-- --          nPressedString <- mapDyn show nPressed
-- --          dynText nPressedString

-- -- fileUpload :: (MonadWidget t m) => m (Dynamic t (Maybe String))
-- -- fileUpload = do
-- --   fi <- fileInput def

-- -- numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Double))
-- -- numberInput = do
-- --   let errorState = Map.singleton "style" "border-color: red"
-- --       validState = Map.singleton "style" "border-color: green"
-- --   rec n <- textInput $ def & textInputConfig_inputType .~ "number"
-- --                            & textInputConfig_initialValue .~ "0"
-- --                            & textInputConfig_attributes .~ attrs
-- --       result <- mapDyn readMay $ _textInput_value n
-- --       attrs <- mapDyn (\r -> case r of
-- --                                   Just _ -> validState
-- --                                   Nothing -> errorState) result
-- --   return result

-- -- ops = Map.fromList [("+", "+"), ("-", "-"), ("*", "*"), ("/", "/")]

-- -- stringToOp s = case s of
-- --                  "-" -> (-)
-- --                  "*" -> (*)
-- --                  "/" -> (/)
-- --                  _ -> (+)

bootstrap :: (MonadWidget t m) => m ()
bootstrap = elAttr "link" (Map.fromList [ ("rel", "stylesheet")
                                        , ("href", "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css")
                                        , ("type", "text/css")
                                        ]
                          ) blank

fontAwesome :: (MonadWidget t m) => m ()
fontAwesome = elAttr "link" (  "rel" =: "stylesheet"
                            <> "href" =: "font-awesome-4.6.3/css/font-awesome.css"
                            ) blank

-- pillItem component = do
--   let active = "class" =: "active"
--       inactive = mempty
--   rec (e, item) <- elDynAttr' "li" activeDyn $
--                   -- elAttr "a" ("href" =: "#") component
--                      el "a" component
--       activeE <- toggle False $ domEvent Click e
--       activeDyn <- mapDyn (\r -> case r of
--                                    True -> active
--                                    False -> inactive
--                           ) activeE
--   return item

-- panel :: MonadWidget t m => m a -> m a
-- panel = elAttr "div" (Map.fromList [ ("class", "panel-group")
--                                    , ("id", "accordion")
--                                    , ("role", "tablist")
--                                    , ("aria-multiselectable", "true")
--                                    ]
--                      )

-- panelItem :: (MonadWidget t m) => m a -> m a -> m a
-- panelItem title body = do
--   let collapsed = "class" =: "collapsed"
--       open = mempty
--       bodyCollapsed = "class" =: "pannel-collapse collapse" <> "role" =: "tabpanel"
--       bodyOpened = "class" =: "pannel-collapse collapse in" <> "role" =: "tabpanel"
--   rec (e, item) <- elAttr' "div" (Map.fromList [ ("class", "panel panel-default")]
--                                       ) $ do
--                          elAttr "div" (  "class" =: "panel-heading"
--                                        <> "role" =: "tab"
--                                        ) $
--                              elAttr "h4" (  "class" =: "panel-title" ) $
--                                  elDynAttr "a" dynCollapse $ title
--                          elDynAttr "div" ( dynBodyCollapse
--                                          ) $
--                              elAttr "div" ( "class" =: "panel-body") body

--       collapseE <- toggle True $ domEvent Click e
--       dynCollapse <- mapDyn (\r -> case r of
--                                      True -> collapsed
--                                      False -> open
--                             ) collapseE
--       dynBodyCollapse <- mapDyn (\r -> case r of
--                                          True -> bodyCollapsed
--                                          False -> bodyOpened
--                                 ) collapseE
--   return item

-- parentLi :: MonadWidget t m => m a -> m a
-- parentLi component = elAttr "div" ("class" =: "tree") $ elAttr "ul" ("class" =: "list-unstyled") component

-- collapsibleList :: (MonadWidget t m) => String -> m a -> m a
-- collapsibleList title items = do
--   let collapsed = "+"
--       open = "-"
--       hidden = "class" =: "hidden"
--       shown = mempty
--   rec (e, item) <- elAttr' "li" ("href" =: "#") $ do
--                      el "span" (dynText dynCollapse)
--                      el "span" $ text title
--                      elDynAttr "ul" dynHidden items

--       collapseE <- toggle True $ domEvent Click e
--       dynCollapse <- mapDyn (\r -> case r of
--                                      True -> collapsed
--                                      False -> open
--                             ) collapseE
--       dynHidden <- mapDyn (\r -> case r of
--                                    True -> hidden
--                                    False -> shown
--                           ) collapseE

--   return $ item
                     

-- main = mainWidgetWithHead bootstrap $ do -- el "div" $ text "Reflex!"
--          -- elAttr "ul" (Map.singleton "class" "nav nav-pills nav-stacked") $ do
--          --              pillItem $ text "Home"
--          --              pillItem $ text "Profile"
--          --              pillItem $ text "Messages"
--          -- panel $ do
--          --   panelItem (text "First panel!") (text "Here is the body!")
--          --   panelItem (text "Second panel!") (text "Here is the body of the second panel!")
--          parentLi $ do
--            collapsibleList "US Dev 1" $ do
--              el "li" (text "First")
--              el "li" (text "Second")
--            collapsibleList "US Dev 2" $ do
--              el "li" (text "Dev2 First")
--              el "li" (text "Dev2 Second")