module Main where

import Data.Array ((..), index)
import Data.Either (Either(..))
import Data.Foldable
import Data.FoldableWithIndex
import Data.Int (toNumber)
import Data.Map (Map, insert, union, intersection, keys, empty)
import Data.Maybe (Maybe(..))
import Data.Number.Format (toString)
import Data.Set (size, filter)
import Data.Traversable (sequence, sequence_)
import Effect.Aff (Aff, delay, Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect (Effect)
import Effect.Random (randomInt)
import Effect.Ref (Ref, new, read, write)
import Graphics.Canvas
import Prelude
import Web.Event.Event
import Web.Event.EventTarget
import Web.DOM.Element (toEventTarget)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (Window, document, requestAnimationFrame)
import Web.UIEvent.KeyboardEvent

type Point = {x :: Int, y :: Int}
type Board a = Map Point a

type Matrix = Point -> Point

type GameEnv a =
    { pieces :: Array (Board a)
    , width :: Int
    , height :: Int }

type Player a =
    { offset :: Point
    , orient :: Matrix
    , tile :: Board a
    , nextTile :: Board a }
type Running a =
    { player :: Player a
    , board :: Board a
    , score :: Int }
type GameOver a =
    { board :: Board a
    , score :: Int }
type GameState a = Either (GameOver a) (Running a)

transform :: Point -> Point -> Point
transform i j = {x: i.x + j.x, y: i.y + j.y}

rotate90d :: Matrix
rotate90d p = {x: -p.y, y: p.x}
 
randomTile :: forall a. GameEnv a -> Effect (Board a)
randomTile env = do
    i <- randomInt 0 (length env.pieces - 1)
    case index env.pieces i of
        Just tile -> pure tile
        Nothing   -> pure empty

initPlayer :: forall a. GameEnv a -> Board a -> Effect (Player a)
initPlayer env tile = do
    nextTile <- randomTile env
    pure
      { offset: {x: 6, y: 0}
      , orient: (\i -> i)
      , tile: tile
      , nextTile: nextTile }

initGame :: forall a. GameEnv a -> Effect (Running a)
initGame env = do
    player <- randomTile env >>= initPlayer env
    pure { player: player, board: empty, score: 0 }
 
stepShift :: forall a. Int -> GameEnv a -> Running a -> Running a
stepShift x env prev = let
    run = prev { player = prev.player { offset = transform {x:x,y:0} prev.player.offset } }
    in if affordable env run then run else prev
 
stepRotate :: forall a. GameEnv a -> Running a -> Running a
stepRotate env prev = let
    run = prev { player = prev.player { orient = prev.player.orient >>> rotate90d } }
    in if affordable env run then run else prev

stepDown :: forall a. GameEnv a -> Running a -> Either (GameOver a) (Effect (Running a))
stepDown env prev = let
    run = {
        player: { offset: transform {x:0,y:1} prev.player.offset
                , orient: prev.player.orient
                , tile: prev.player.tile
                , nextTile: prev.player.nextTile }
        , board: prev.board
        , score: prev.score }
    in (if affordable env run then pure (pure run) else nextTurn)
    where nextTurn = let
             fl = flushBoard env.width env.height
                $ { board: union (currentTile prev.player) prev.board
                  , score: 1 }
             in if any (\p -> p.y <= 2) (keys fl.board)
                then (Left {board: fl.board, score: prev.score + fl.score})
                else pure $ do
                  player <- initPlayer env prev.player.nextTile
                  (pure {player: player, board: fl.board, score: prev.score + fl.score})

flushBoard :: forall a. Int -> Int
           -> {board::Board a, score::Int}
           -> {board::Board a, score::Int}
flushBoard w h = foldl (>>>) (\a -> a) (map (flushRow w) (2 .. (h-1)))

flushRow :: forall a. Int -> Int
         -> {board::Board a, score::Int}
         -> {board::Board a, score::Int}
flushRow w y bs = if w == countRow y bs.board
    then {board: mapKeys' (collapse y) bs.board, score: bs.score+w}
    else bs

countRow :: forall a. Int -> Board a -> Int
countRow y board = size (filter (\p -> p.y == y) (keys board))

collapse :: Int -> Point -> Maybe Point
collapse y p | (p.y == y) = Nothing
collapse y p | (p.y < y)  = Just {x: p.x, y: p.y + 1}
collapse y p              = Just p

affordable :: forall a. GameEnv a -> Running a -> Boolean
affordable env run = let
    tile = currentTile run.player
    in null (intersection tile run.board)
    && all (in_board env.width env.height) (keys tile)

in_board :: Int -> Int -> Point -> Boolean
in_board w h p =
  (0 <= p.x) && (p.x < w)
  && (0 <= p.y) && (p.y < h)
 
currentTile :: forall a. Player a -> Board a
currentTile player = mapKeys (pt player) player.tile

pt :: forall a. Player a -> Point -> Point
pt player = transform player.offset <<< player.orient


initGameEnv :: GameEnv String 
initGameEnv = { pieces: tetrominoes, width: 14, height: 25 }

tetrominoes :: Array (Board String)
tetrominoes =
  [ piece
    [ {x: 0, y: 0}, {x:  1, y: 0}, {x: 2, y: 0}, {x: -1, y: 0}
    ] "#800"
  , piece
    [ {x: 0, y: 0} , {x: 1, y: 0} , {x: 0, y: 1} , {x: 1, y: 1}
    ] "#080"
  , piece
    [ {x: -1, y: 0} , {x: 0, y: 0} , {x: 1, y: 0} , {x: 1, y: 1}
    ] "#00C"
  , piece
    [ {x: -1, y: 0} , {x: 0, y: 0} , {x: 1, y: 0} , {x: 0, y: 1}
    ] "#088"
  , piece
    [ {x: -1, y: 0} , {x: 0, y: 0} , {x: 1, y: 1} , {x: 0, y: 1}
    ] "#808"
  ]

piece :: Array {x :: Int, y :: Int} -> String -> Board String
piece indices color = foldl
  (\m p -> insert p color m)
  empty
  indices

main :: Effect Unit
main = do
  w <- window
  d <- document w
  let env = initGameEnv
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

onKeyEvent :: GameEnv String -> Ref (GameState String) -> Maybe KeyboardEvent -> Effect Unit
onKeyEvent env gameRef (Just ev) = case key ev of
    "ArrowLeft"  -> do
        game <- read gameRef
        write (map (stepShift (-1) env) game) gameRef
        preventDefault (toEvent ev)
    "ArrowRight" -> do
        game <- read gameRef
        write (map (stepShift 1 env) game) gameRef
        preventDefault (toEvent ev)
    "ArrowUp"    -> do
        game <- read gameRef
        write (map (stepRotate env) game) gameRef
        preventDefault (toEvent ev)
    "ArrowDown"  -> do
        game <- read gameRef
        game' <- sequence (game >>= stepDown env)
        write game' gameRef
        preventDefault (toEvent ev)
    _ -> pure unit
onKeyEvent env gameRef Nothing = pure unit

gameStep :: GameEnv String -> Ref (GameState String) -> Aff Unit
gameStep env gameRef = do
    speed <- liftEffect $ map gameSpeed (read gameRef)
    delay $ Milliseconds speed
    game <- liftEffect $ read gameRef
    game' <- liftEffect $ sequence (game >>= stepDown env)
    liftEffect $ write game' gameRef
    case game' of
      Left _ -> pure unit
      Right _ -> gameStep env gameRef

gameSpeed :: forall a. GameState a -> Number
gameSpeed (Right g) | g.score < 20  = 700.0
gameSpeed (Right g) | g.score < 40  = 600.0
gameSpeed (Right g) | g.score < 60  = 500.0
gameSpeed (Right g) | g.score < 80  = 400.0
gameSpeed (Right g) | g.score < 100 = 300.0
gameSpeed (Right g) | g.score < 120 = 200.0
gameSpeed (Right g) | g.score < 140 = 100.0
gameSpeed (Right g) | g.score < 160 = 90.0
gameSpeed (Right g) | g.score < 180 = 80.0
gameSpeed (Right g) | g.score < 200 = 70.0
gameSpeed (Right g) | g.score < 220 = 60.0
gameSpeed g                         = 50.0

display :: Window
        -> Context2D
        -> Ref (GameState String)
        -> GameState String
        -> Effect Unit
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
  fillText ctx (toString (toNumber  run.score)) (3.0 * 16.0) 425.0
  _ <- requestAnimationFrame (read game >>= display w ctx game) w
  pure unit

displayBackground :: Context2D -> Board String -> Effect Unit
displayBackground ctx board = do
  setFillStyle ctx "#333"
  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: 320.0
    , height: 480.0
    }
  setFillStyle ctx "#000"
  fillPath ctx $ rect ctx
    { x: 3.0 * 16.0
    , y: 0.0
    , width: 16.0 * 14.0
    , height: 16.0 * 25.0
    }
  setFillStyle ctx "#000"
  fillPath ctx $ rect ctx
    { x: 12.0 * 16.0
    , y: 26.0 * 16.0
    , width: 16.0 * 5.0
    , height: 16.0 * 2.0
    }
  foldlWithIndex
    (\p m c -> m >>= \_ -> plot ctx p c)
    (pure unit)
    board

displayPlayer :: Context2D -> Player String -> Effect Unit
displayPlayer ctx player = do
  foldlWithIndex
    (\p m c -> m >>= \_ -> plot ctx (pt player p) c)
    (pure unit)
    player.tile

displayNextTile :: Context2D -> Board String -> Effect Unit
displayNextTile ctx tile = do
  foldlWithIndex
    (\p m c -> m >>= \_ -> plot ctx (transform {x:11,y:26} p) c)
    (pure unit)
    tile

plot :: Context2D -> Point -> String -> Effect Unit
plot ctx p color = do
  setFillStyle ctx color
  fillPath ctx $ rect ctx
    { x: toNumber (p.x+3) * 16.0 + 1.0
    , y: toNumber p.y     * 16.0 + 1.0
    , width: 14.0
    , height: 14.0
    }

mapKeys :: forall j k v. Ord j => Ord k => (k -> j) -> Map k v -> Map j v
mapKeys f m = foldlWithIndex
    (\k out v -> insert (f k) v out)
    empty
    m

mapKeys' :: forall j k v. Ord j => Ord k => (k -> Maybe j) -> Map k v -> Map j v
mapKeys' f m = foldlWithIndex
    (\k out v -> case f k of
        Just k' -> insert k' v out
        Nothing -> out)
    empty
    m
