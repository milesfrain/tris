module Main where

import Prelude
import Data.Array ((..), index)
import Data.Either (Either(..))
import Data.Foldable (all, any, foldl, length, null)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (toNumber)
import Data.Map (Map, insert, union, intersection, keys, empty)
import Data.Maybe (Maybe(..))
import Data.Number.Format (toString)
import Data.Set (size, filter)
import Data.Traversable (sequence)
import Effect.Aff (Aff, delay, Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect (Effect)
import Effect.Random (randomInt)
import Effect.Ref (Ref, new, read, write)
import Graphics.Canvas (Context2D, TextAlign(..), fillPath, fillText, getCanvasElementById, getContext2D, rect, setFillStyle, setTextAlign)
import Web.Event.Event (EventType(..), preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.DOM.Element (toEventTarget)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (Window, document, requestAnimationFrame)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, key, toEvent)

type Point
  = { x :: Int, y :: Int }

type Color
  = String

type Board
  = Map Point Color

type Matrix
  = Point -> Point

type GameEnv
  = { pieces :: Array Board
    , width :: Int
    , height :: Int
    }

type Player
  = { offset :: Point
    , orient :: Matrix
    , tile :: Board
    , nextTile :: Board
    }

type Running
  = { player :: Player
    , board :: Board
    , score :: Int
    }

type GameOver
  = { board :: Board
    , score :: Int
    }

type GameState
  = Either GameOver Running

transform :: Point -> Point -> Point
transform i j = { x: i.x + j.x, y: i.y + j.y }

rotate90d :: Matrix
rotate90d p = { x: -p.y, y: p.x }

randomTile :: GameEnv -> Effect Board
randomTile env = do
  i <- randomInt 0 (length env.pieces - 1)
  case index env.pieces i of
    Just tile -> pure tile
    Nothing -> pure empty

initPlayer :: GameEnv -> Board -> Effect Player
initPlayer env tile = do
  nextTile <- randomTile env
  pure
    { offset: { x: 6, y: 0 }
    , orient: (\i -> i)
    , tile: tile
    , nextTile: nextTile
    }

initGame :: GameEnv -> Effect Running
initGame env = do
  player <- randomTile env >>= initPlayer env
  pure { player: player, board: empty, score: 0 }

stepShift :: Int -> GameEnv -> Running -> Running
stepShift x env prev =
  let
    run = prev { player = prev.player { offset = transform { x: x, y: 0 } prev.player.offset } }
  in
    if affordable env run then run else prev

stepRotate :: GameEnv -> Running -> Running
stepRotate env prev =
  let
    run = prev { player = prev.player { orient = prev.player.orient >>> rotate90d } }
  in
    if affordable env run then run else prev

stepDown :: GameEnv -> Running -> Either GameOver (Effect Running)
stepDown env prev =
  let
    run =
      { player:
          { offset: transform { x: 0, y: 1 } prev.player.offset
          , orient: prev.player.orient
          , tile: prev.player.tile
          , nextTile: prev.player.nextTile
          }
      , board: prev.board
      , score: prev.score
      }
  in
    (if affordable env run then pure (pure run) else nextTurn)
  where
  nextTurn =
    let
      fl =
        flushBoard env.width env.height
          $ { board: union (currentTile prev.player) prev.board
            , score: 1
            }
    in
      if any (\p -> p.y <= 2) (keys fl.board) then
        (Left { board: fl.board, score: prev.score + fl.score })
      else
        pure
          $ do
              player <- initPlayer env prev.player.nextTile
              (pure { player: player, board: fl.board, score: prev.score + fl.score })

flushBoard ::
  Int ->
  Int ->
  { board :: Board, score :: Int } ->
  { board :: Board, score :: Int }
flushBoard w h = foldl (>>>) (\a -> a) (map (flushRow w) (2 .. (h - 1)))

flushRow ::
  Int ->
  Int ->
  { board :: Board, score :: Int } ->
  { board :: Board, score :: Int }
flushRow w y bs =
  if w == countRow y bs.board then
    { board: mapKeys' (collapse y) bs.board, score: bs.score + w }
  else
    bs

countRow :: Int -> Board -> Int
countRow y board = size (filter (\p -> p.y == y) (keys board))

collapse :: Int -> Point -> Maybe Point
collapse y p
  | (p.y == y) = Nothing

collapse y p
  | (p.y < y) = Just { x: p.x, y: p.y + 1 }

collapse y p = Just p

affordable :: GameEnv -> Running -> Boolean
affordable env run =
  let
    tile = currentTile run.player
  in
    null (intersection tile run.board)
      && all (in_board env.width env.height) (keys tile)

in_board :: Int -> Int -> Point -> Boolean
in_board w h p =
  (0 <= p.x) && (p.x < w)
    && (0 <= p.y)
    && (p.y < h)

currentTile :: Player -> Board
currentTile player = mapKeys (pt player) player.tile

pt :: Player -> Point -> Point
pt player = transform player.offset <<< player.orient

initGameEnv :: GameEnv
initGameEnv = { pieces: tetrominoes, width: 14, height: 25 }

tetrominoes :: Array Board
tetrominoes =
  [ piece [ p 0 0, p 1 0, p 0 1, p 1 1 ] "#080"
  , piece [ p (-1) 0, p 0 0, p 1 0, p 2 0 ] "#800"
  , piece [ p (-1) 0, p 0 0, p 1 0, p 1 1 ] "#00C"
  , piece [ p (-1) 0, p 0 0, p 1 0, p 0 1 ] "#088"
  , piece [ p (-1) 0, p 0 0, p 1 1, p 0 1 ] "#808"
  ]
  where
  p x y = { x, y }

piece :: Array { x :: Int, y :: Int } -> Color -> Board
piece indices color =
  foldl
    (\m p -> insert p color m)
    empty
    indices

main :: Effect Unit
main = do
  w <- window
  d <- document w
  let
    env = initGameEnv
  gameRef <- initGame env >>= (new <<< Right)
  keyEvent <- eventListener (onKeyEvent env gameRef <<< fromEvent)
  canvas_elem <- getCanvasElementById "tris"
  case canvas_elem of
    Just canvas -> do
      ctx <- getContext2D canvas
      _ <- requestAnimationFrame (read gameRef >>= display w ctx gameRef) w
      launchAff_ (gameStep env gameRef)
      pure unit
    Nothing -> do
      pure unit
  kbElem <- getElementById "tris" (toNonElementParentNode (toDocument d))
  case kbElem of
    Just kb -> do
      addEventListener (EventType "keydown") keyEvent true (toEventTarget kb)
    Nothing -> do
      pure unit

onKeyEvent :: GameEnv -> Ref GameState -> Maybe KeyboardEvent -> Effect Unit
onKeyEvent env gameRef (Just ev) = case key ev of
  "ArrowLeft" -> do
    game <- read gameRef
    write (map (stepShift (-1) env) game) gameRef
    preventDefault (toEvent ev)
  "ArrowRight" -> do
    game <- read gameRef
    write (map (stepShift 1 env) game) gameRef
    preventDefault (toEvent ev)
  "ArrowUp" -> do
    game <- read gameRef
    write (map (stepRotate env) game) gameRef
    preventDefault (toEvent ev)
  "ArrowDown" -> do
    game <- read gameRef
    game' <- sequence (game >>= stepDown env)
    write game' gameRef
    preventDefault (toEvent ev)
  _ -> pure unit

onKeyEvent env gameRef Nothing = pure unit

gameStep :: GameEnv -> Ref GameState -> Aff Unit
gameStep env gameRef = do
  speed <- liftEffect $ map gameSpeed (read gameRef)
  delay $ Milliseconds $ toNumber speed
  game <- liftEffect $ read gameRef
  game' <- liftEffect $ sequence (game >>= stepDown env)
  liftEffect $ write game' gameRef
  case game' of
    Left _ -> pure unit
    Right _ -> gameStep env gameRef

gameSpeed :: GameState -> Int
gameSpeed (Right g)
  | g.score < 140 = 100 * (7 - g.score / 20)
  | g.score < 220 = 10 * (16 - g.score / 20)

gameSpeed _ = 50

display ::
  Window ->
  Context2D ->
  Ref GameState ->
  GameState ->
  Effect Unit
display w ctx game (Left over) = do
  displayBackground ctx over.board
  setTextAlign ctx AlignLeft
  setFillStyle ctx "#FFF"
  fillText ctx (toString (toNumber over.score)) (3.0 * 16.0) 425.0
  setTextAlign ctx AlignLeft
  fillText ctx ("game over") (3.0 * 16.0) 440.0

display w ctx game (Right run) = do
  displayBackground ctx run.board
  displayPlayer ctx run.player
  displayNextTile ctx run.player.nextTile
  setFillStyle ctx "#FFF"
  setTextAlign ctx AlignLeft
  fillText ctx (toString (toNumber run.score)) (3.0 * 16.0) 425.0
  _ <- requestAnimationFrame (read game >>= display w ctx game) w
  pure unit

displayBackground :: Context2D -> Board -> Effect Unit
displayBackground ctx board = do
  setFillStyle ctx "#333"
  fillPath ctx
    $ rect ctx
        { x: 0.0
        , y: 0.0
        , width: 320.0
        , height: 480.0
        }
  setFillStyle ctx "#000"
  fillPath ctx
    $ rect ctx
        { x: 3.0 * 16.0
        , y: 0.0
        , width: 16.0 * 14.0
        , height: 16.0 * 25.0
        }
  setFillStyle ctx "#000"
  fillPath ctx
    $ rect ctx
        { x: 12.0 * 16.0
        , y: 26.0 * 16.0
        , width: 16.0 * 5.0
        , height: 16.0 * 2.0
        }
  foldlWithIndex
    (\p m c -> m >>= \_ -> plot ctx p c)
    (pure unit)
    board

displayPlayer :: Context2D -> Player -> Effect Unit
displayPlayer ctx player = do
  foldlWithIndex
    (\p m c -> m >>= \_ -> plot ctx (pt player p) c)
    (pure unit)
    player.tile

displayNextTile :: Context2D -> Board -> Effect Unit
displayNextTile ctx tile = do
  foldlWithIndex
    (\p m c -> m >>= \_ -> plot ctx (transform { x: 11, y: 26 } p) c)
    (pure unit)
    tile

plot :: Context2D -> Point -> Color -> Effect Unit
plot ctx p color = do
  setFillStyle ctx color
  fillPath ctx
    $ rect ctx
        { x: toNumber (p.x + 3) * 16.0 + 1.0
        , y: toNumber p.y * 16.0 + 1.0
        , width: 14.0
        , height: 14.0
        }

mapKeys :: forall j k v. Ord j => Ord k => (k -> j) -> Map k v -> Map j v
mapKeys f m =
  foldlWithIndex
    (\k out v -> insert (f k) v out)
    empty
    m

mapKeys' :: forall j k v. Ord j => Ord k => (k -> Maybe j) -> Map k v -> Map j v
mapKeys' f m =
  foldlWithIndex
    ( \k out v -> case f k of
        Just k' -> insert k' v out
        Nothing -> out
    )
    empty
    m
