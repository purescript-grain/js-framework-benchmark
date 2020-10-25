module Main where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (delete, length, snoc, unsafeIndex, updateAtIndices, (!!))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt)
import Grain (class GlobalGrain, GProxy(..), VNode, fromConstructor, mount, useFinder, useUpdater, useValue)
import Grain.Markup as H
import Partial.Unsafe (unsafePartial)
import Web.DOM.Element (toNode)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  maybeEl <- window >>= document <#> toParentNode >>= querySelector (QuerySelector "#main")
  case maybeEl of
    Nothing -> pure unit
    Just el ->
      mount view $ toNode el

view :: VNode
view =
  H.div # H.className "container" # H.kids
    [ jumbotronView
    , rowsView
    , H.span
        # H.className "preloadicon glyphicon glyphicon-remove"
        # H.prop "aria-hidden" "true"
    ]

jumbotronView :: VNode
jumbotronView = H.component do
  findState <- useFinder (GProxy :: _ State)
  updateState <- useUpdater (GProxy :: _ State)

  let run_ count = do
        State s <- findState
        state <- addRows count $ State s { rows = [], selectedId = 0 }
        updateState \_ -> state

      run = run_ 1000

      runLots = run_ 10000

      add = do
        state <- findState >>= addRows 1000
        updateState \_ -> state

      updateEveryTenth i rows =
        case rows !! i of
          Just row ->
            updateEveryTenth (i + 10) $ updateAtIndices
              [ Tuple i row { label = row.label <> " !!!" } ]
              rows
          Nothing -> rows

      update_ = updateState $ over State \s -> s
        { rows = updateEveryTenth 0 s.rows
        }

      clear =
        updateState $ over State _ { rows = [], selectedId = 0 }

      swapRows =
        updateState $ over State \s ->
          case s.rows !! 1, s.rows !! 998 of
            Just row1, Just row2 ->
              s { rows =
                    updateAtIndices
                      [ Tuple 1 row2, Tuple 998 row1 ]
                      s.rows
                }
            _, _ -> s

  pure $ H.div # H.className "jumbotron" # H.kids
    [ H.div # H.className "row" # H.kids
        [ H.div # H.className "col-md-6" # H.kids
            [ H.h1 # H.kids [ H.text "Grain keyed" ]
            ]
        , H.div # H.className "col-md-6" # H.kids
            [ H.div # H.className "row" # H.kids
                [ buttonView { id: "run", label: "Create 1,000 rows", onClick: run }
                , buttonView { id: "runlots", label: "Create 10,000 rows", onClick: runLots }
                , buttonView { id: "add", label: "Append 1,000 rows", onClick: add }
                , buttonView { id: "update", label: "Update every 10th row", onClick: update_ }
                , buttonView { id: "clear", label: "Clear", onClick: clear }
                , buttonView { id: "swaprows", label: "Swap Rows", onClick: swapRows }
                ]
            ]
        ]
    ]

rowsView :: VNode
rowsView = H.component do
  State { rows, selectedId } <- useValue (GProxy :: _ State)
  updateState <- useUpdater (GProxy :: _ State)

  let select row = updateState $ over State _ { selectedId = row.id }
      remove row = updateState $ over State \s -> s
                      { rows = delete row s.rows
                      }

  pure $ H.table
    # H.className "table table-hover table-striped test-data"
    # H.kids
        [ H.tbody # H.kids
            (rows <#> \row -> rowView { row, selected: row.id == selectedId, onSelect: select row, onRemove: remove row })
        ]

rowView :: { row :: Row, selected :: Boolean, onSelect :: Effect Unit, onRemove :: Effect Unit } -> VNode
rowView { row, selected, onSelect, onRemove } =
  let idTxt = show row.id
   in H.tr
        # H.key idTxt
        # H.fingerprint (row.label <> if selected then "t" else "f")
        # H.className (if selected then "danger" else "")
        # H.kids
            [ H.td # H.className "col-md-1" # H.kids [ H.text idTxt ]
            , H.td # H.className "col-md-4" # H.kids
                [ H.a # H.onClick (\_ -> onSelect) # H.kids [ H.text row.label ]
                ]
            , H.td # H.className "col-md-1" # H.kids
                [ H.a # H.onClick (\_ -> onRemove) # H.kids [ iconView ]
                ]
            , H.td # H.className "col-md-6"
            ]

buttonView :: { id :: String, label :: String, onClick :: Effect Unit } -> VNode
buttonView { id, label, onClick } =
  H.div # H.className "col-sm-6 smallpad" # H.kids
    [ H.button
        # H.id id
        # H.type_ "button"
        # H.className "btn btn-primary btn-block"
        # H.onClick (\_ -> onClick)
        # H.kids [ H.text label ]
    ]

iconView :: VNode
iconView =
  H.fingerprint "remove-icon" $ H.span
    # H.className "glyphicon glyphicon-remove"
    # H.prop "aria-hidden" "true"

type Row =
  { id :: Int
  , label :: String
  }

newtype State = State
  { nextId :: Int
  , rows :: Array Row
  , selectedId :: Int
  }

derive instance newtypeState :: Newtype State _

instance globalGrainState :: GlobalGrain State where
  typeRefOf _ = fromConstructor State
  initialState _ = pure $ State
    { nextId: 1
    , rows: []
    , selectedId: 0
    }

addRows :: Int -> State -> Effect State
addRows count (State s) =
  tailRecM go { state: s, addedCount: 0 }
  where
    go { state, addedCount }
      | addedCount >= count =
          pure $ Done $ State state
      | otherwise = do
          a <- sample adjectives
          c <- sample colours
          n <- sample nouns
          let id = state.nextId
              label = a <> " " <> c <> " " <> n
              state' = state
                { nextId = state.nextId + 1
                , rows = snoc state.rows { id, label }
                }
          pure $ Loop
            { state: state'
            , addedCount: addedCount + 1
            }

sample :: forall a. Array a -> Effect a
sample xs = do
  i <- randomInt 0 $ length xs - 1
  pure $ unsafePartial $ unsafeIndex xs i

adjectives :: Array String
adjectives =
  [ "pretty"
  , "large"
  , "big"
  , "small"
  , "tall"
  , "short"
  , "long"
  , "handsome"
  , "plain"
  , "quaint"
  , "clean"
  , "elegant"
  , "easy"
  , "angry"
  , "crazy"
  , "helpful"
  , "mushy"
  , "odd"
  , "unsightly"
  , "adorable"
  , "important"
  , "inexpensive"
  , "cheap"
  , "expensive"
  , "fancy"
  ]

colours :: Array String
colours =
  [ "red"
  , "yellow"
  , "blue"
  , "green"
  , "pink"
  , "brown"
  , "purple"
  , "brown"
  , "white"
  , "black"
  , "orange"
  ]

nouns :: Array String
nouns =
  [ "table"
  , "chair"
  , "house"
  , "bbq"
  , "desk"
  , "car"
  , "pony"
  , "cookie"
  , "sandwich"
  , "burger"
  , "pizza"
  , "mouse"
  , "keyboard"
  ]
