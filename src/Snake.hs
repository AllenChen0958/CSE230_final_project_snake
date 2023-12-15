{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Snake
  ( initGame,
    initGame2p,
    step,
    updateFoodLogic,
    changeUpdateFoodLogic,
    turn,
    turn2,
    Game (..),
    Command (..),
    Coord (..),
    Direction (..),
    Camera (..),
    Color (..),
    Options (..),
    Name (..),
    UpdateFoodLogicType (..),
    dead,
    food,
    foodType,
    currentFoods,
    score,
    score2,
    snake,
    snake2,
    height,
    width,
    twoPlayer,
    winner,
    resetGame2p,
    dir,
    degree,
    degree2,
    color,
    targetDegree,
    targetDegree2,
    rotationProgress,
    renderTick,
    camera,
    dialog,
    updateFoodLogicType,
    bgmVolume
  )
where


import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Control.Monad.Trans.State (execState, get, State, put, runState, state)
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as S
import Control.Monad.Trans.Cont (reset)
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as C
import Linear.V2 (V2 (..), _x, _y)
import System.Random (Random(..), newStdGen, RandomGen, randomRs, randomRIO, StdGen)
import Data.Maybe (mapMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (replicateM, when, unless)
import Data.Foldable (toList)
import Sound.ProteaAudio (volume)
-- import Debug.Trace (trace)



-- Types

data Game = Game -- With named field
  { -- | snake as a sequence of points in N2
    _snake :: Seq Coord,
    -- | direction
    _dir :: Direction,
    -- | location of the food
    _food :: Coord,
    -- | type of the food
    _foodType :: Char,
    -- | infinite list of random next food locations
    _foods :: [Coord],
    -- | infinite list of random next food types
    _foodTypes :: [Char],
    -- | counter for food updates
    _foodUpdateCounter :: Int,
    -- | multiple food showing concurrently (Coordinate, Food Type, Time Remaining) for MultiFood
    _currentFoods :: [(Coord, Char, Int)],
    -- | game over flag
    _dead :: Bool,
    -- | paused flag
    _paused :: Bool,
    -- | score
    _score :: Int,
    -- | lock to disallow duplicate turns between time steps
    _locked :: Bool,
    -- set two player mode
    _twoPlayer :: Bool,
    -- | snake2 as a sequence of points in N2
    _snake2 :: Seq Coord,
    -- | direction2
    _dir2 :: Direction,
    -- | game over flag for snake2
    _dead2 :: Bool,
    -- | score2 for accumulating
    _score2 :: Int,
    -- | winnder flag
    _winner :: Int,
    _camera :: Camera,
    _rotationProgress :: Int,
    _degree :: Double,
    _targetDegree :: Double,
    _degree2 :: Double,
    _targetDegree2 :: Double,
    _color :: Color,
    _dialog :: D.Dialog Options Name,    -- Names: button str names, Options: values for corresponding button
    _updateFoodLogicType ::UpdateFoodLogicType,
    _rng :: StdGen, -- for MultiFood
    _bgmVolume :: Float
  }

data Options = Red | Blue | Green deriving (Show)

data Name =
    RedButton
    | BlueButton
    | GreenButton
    deriving (Show, Eq, Ord)



data UpdateFoodLogicType
  = FixFood
  | RandFood
  | MultiFood
    deriving (Eq)

data Color = BLUE | MAGENTA | CYAN | WHITE

-- type Coord = V2 Int
newtype Coord = Coord (Int, Int)
              deriving (Eq, Ord, Show)

data Camera
  = Fixed
  | Moving
  | FPV
  deriving (Eq)

data Command
  = CUp
  | CDown
  | CLeft
  | CRight
  deriving (Eq)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq)

makeLenses ''Game -- reduce unpacking and packing

-- Constants

height, width :: Int
height = 30
width = 30

renderTick :: Int
renderTick = 25

-- Functions


step :: Game -> Game
step = execState sta

updateFoodLogic :: Game -> Game
updateFoodLogic g = case g ^. updateFoodLogicType of
                    FixFood -> g
                    RandFood -> execState nextFood g
                    MultiFood -> execState nextFoodMulti g

changeUpdateFoodLogic :: Game -> Game
changeUpdateFoodLogic g = case g ^. updateFoodLogicType of
                      FixFood -> g & updateFoodLogicType .~ RandFood
                      RandFood -> g & updateFoodLogicType .~ MultiFood
                      MultiFood -> g & updateFoodLogicType .~ FixFood

sta :: State Game ()
sta = do
  g <- get
  locked .= False
  rotationProgress .= 0
  if not (_twoPlayer g) then
    let goOn = let ss S.:> _ = S.viewr (_snake g)
                in
                snake .= nextHead g <| ss 
      in
    if 
      | _paused g || _dead g -> return ()
      | nextHead g `elem` _snake g -> do
          dead .= True 
      | otherwise -> case _updateFoodLogicType g of
        FixFood -> if _food g == nextHead g then case _foodType g of
          'a' -> do 
            modifying score (+100)
            modifying snake (nextHead g <|)
            nextFoodFixed
          'b' -> do 
            modifying score (subtract 50)
            nextFoodFixed
           else
            goOn
        RandFood -> if _food g == nextHead g then case _foodType g of
          'a' -> do 
            modifying score (+100)
            modifying snake (nextHead g <|)
            nextFood
          'b' -> do 
            modifying score (subtract 50)
            nextFood
           else
            goOn
        MultiFood -> case findFood (nextHead g) (g ^. currentFoods) of
          Just 'a' -> do
            modifying score (+100)
            modifying snake (nextHead g <|)
            currentFoods .= filter (\(pos, _, _) -> pos /= nextHead g) (g ^. currentFoods) -- update the currentFoods by removing the consumed food
          Just 'b' -> do
            modifying score (subtract 50)
            currentFoods .= filter (\(pos, _, _) -> pos /= nextHead g) (g ^. currentFoods) -- update the currentFoods by removing the consumed food
          otherwise   -> goOn
      -- (_updateFoodLogicType g)
      --   FixFood -> return ()
      -- FixFood == _updateFoodLogicType g && _food g == nextHead g -> do
      --                           case _foodType g of
      --                             'a' -> modifying score (+100)
      --                             'b' -> modifying score (subtract 50)
      --                           modifying snake (nextHead g <|)
      --                           nextFood
      --                           return ()
      -- | MultiFood == _updateFoodLogicType g  -> case findFood (nextHead g) (g ^. currentFoods) of
      --                                         Just foodType -> do
      --                                           return ()
      --                                           case foodType of
      --                                             'a' -> modifying score (+100)
      --                                             'b' -> modifying score (subtract 50)
      --                                           currentFoods .= filter (\(pos, _, _) -> pos /= nextHead g) (g ^. currentFoods) -- update the currentFoods by removing the consumed food
      --                                           modifying snake (nextHead g <|)
      --                                         _     -> return ()
      -- | otherwise -> do
      --                           let ss S.:> _ = S.viewr (_snake g)
      --                             in
      --                               snake .= nextHead g <| ss
  else
    if  | _paused g || _dead g || _dead2 g || _winner g > 0 -> return ()
      --  | elem (nextHead g) (_snake2 g) || elem (nextHead2 g) (_snake g) -> do -- add another game over state where two snakes head collide
                                  -- dead .= True
        | nextHead g == nextHead2 g -> do
                                  -- dead .= True
                                  -- dead2 .= True
                                  winner .= 0
        | elem (nextHead g) (_snake2 g) -> do
                                  dead .= True
                                  winner .= 2
                                  modifying score2 (+150)
                                  -- modifying score (subtract 100)
        | elem (nextHead2 g) (_snake g) -> do
                                  dead2 .= True
                                  winner .= 1
                                  modifying score (+150)
                                  -- modifying score2 (subtract 100)
        | _food g == nextHead g -> do
                                  case _foodType g of
                                    'a' -> modifying score (+100)
                                    'b' -> modifying score (subtract 50)
                                  modifying snake (nextHead g <|)
                                  nextFood
        | _food g == nextHead2 g -> do
                                  case _foodType g of
                                    'a' -> modifying score2 (+100)
                                    'b' -> modifying score2 (subtract 50)
                                  modifying snake2 (nextHead2 g <|)
                                  nextFood2
        | Just foodType <- findFood (nextHead g) (g ^. currentFoods) -> do
                                  case foodType of
                                    'a' -> modifying score (+100)
                                    'b' -> modifying score (subtract 50)
                                  -- update the currentFoods by removing the consumed food
                                  currentFoods .= filter (\(pos, _, _) -> pos /= nextHead g) (g ^. currentFoods)
                                  modifying snake (nextHead g <|)
        | Just foodType <- findFood (nextHead2 g) (g ^. currentFoods) -> do
                                  case foodType of
                                    'a' -> modifying score2 (+100)
                                    'b' -> modifying score2 (subtract 50)
                                  -- update the currentFoods by removing the consumed food
                                  currentFoods .= filter (\(pos, _, _) -> pos /= nextHead2 g) (g ^. currentFoods)
                                  modifying snake2 (nextHead2 g <|)
        | otherwise           -> do
                                  let ss S.:> _ = S.viewr (_snake g)
                                    in
                                      snake .= nextHead g <| ss

                                  let ss S.:> _ = S.viewr (_snake2 g)
                                    in
                                      snake2 .= nextHead2 g <| ss

        where
          findFood coord foods = lookup coord [(pos, t) | (pos, t, _) <- foods]

-- generate one new food every 5 seconds
nextFood :: State Game ()
nextFood = do
  g <- get
  let counter = g ^. foodUpdateCounter
  when (counter `mod` 5 == 0) $ do
    let s = _snake g
    let f : fs = _foods g
    let t : ts = _foodTypes g
    foods .= fs
    foodTypes .= ts
    unless (f `elem` s) $ do
      food .= f
      foodType .= t
  -- Increment the counter
  foodUpdateCounter .= counter + 1

nextFoodFixed :: State Game ()
nextFoodFixed = do
  g <- get
  let
    s = _snake g
    f : fs = _foods g
    in
      do
        foods .= fs
        if f `elem` s then nextFoodFixed else food .= f

nextFoodMulti :: State Game ()
nextFoodMulti = do
  g <- get
  let updatedFoods = mapMaybe updateFoodTime (g ^. currentFoods)
  -- trace (show updatedFoods) $ return ()  -- Print for debugging
  -- ensure 'updatedFoods' only contains foods that are still in the game
  currentFoods .= updatedFoods
  -- then add new foods if necessary
  when (length updatedFoods < 10) $
    addNewFoods (10 - length updatedFoods) updatedFoods

updateFoodTime :: (Coord, Char, Int) -> Maybe (Coord, Char, Int)
updateFoodTime (coord, typ, time) = if time <= 0
                                      then Nothing
                                      else Just (coord, typ, time - 1)

-- Helper function to generate a list of all possible coordinates
allCoords :: [Coord]
allCoords = [Coord (x, y) | x <- [11..11+width-1], y <- [0..height-1]]

-- Function to add 'n' new foods
addNewFoods :: Int -> [(Coord, Char, Int)] -> State Game ()
addNewFoods n existingFoods = do
  g <- get
  let snakeCoords = toList (g ^. snake)
  let availableCoords = filter (`notElem` snakeCoords) $ filter (\(Coord coord) -> not $ any ((== coord) . (\(Coord c, _, _) -> c)) existingFoods) allCoords
  let (newFoods, newGen) = runState (generateNewFoods n availableCoords) (g ^. rng)
  currentFoods .= existingFoods ++ map (\(coord, typ, time, _) -> (coord, typ, time)) newFoods
  rng .= newGen

-- Recursive function to generate new foods
generateNewFoods :: Int -> [Coord] -> State StdGen [(Coord, Char, Int, StdGen)]
generateNewFoods 0 _ = return []
generateNewFoods n availableCoords = do
  gen <- get
  let ((coord, foodType, foodDuration), newGen) = generateFood availableCoords gen
  put newGen
  let remainingCoords = filter (/= coord) availableCoords
  remainingFoods <- generateNewFoods (n - 1) remainingCoords
  return ((coord, foodType, foodDuration, newGen) : remainingFoods)

-- Helper function to generate a single new food item
generateFood :: [Coord] -> StdGen -> ((Coord, Char, Int), StdGen)
generateFood availableCoords gen =
  let (coordIndex, newGen1) = randomR (0, length availableCoords - 1) gen
      coord = availableCoords !! coordIndex
      (foodType, newGen2) = randomR ('a', 'b') newGen1
      foodDuration = 5
  in ((coord, foodType, foodDuration), newGen2)

-- Helper function to get the first element of a tuple
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- Helper function to remove a coordinate from the list
filterOutCoord :: Coord -> [Coord] -> IO [Coord]
filterOutCoord coord coords = return $ filter (/= coord) coords

nextHead :: Game -> Coord
nextHead Game {_dir =  d, _snake = ((Coord (x, y)) :<| _)} -- A bidirectional pattern synonym viewing the front of a non-empty sequence.
  | d == North = Coord (x, (y+1) `mod` height)
  | d == South = Coord (x, (y-1) `mod` height)
  | d == East = Coord ((x+1) `mod` height, y)
  | d == West = Coord ((x-1) `mod` height, y)
nextHead _ = error "Snakes can't be empty!"

nextHead2 :: Game -> Coord
nextHead2 Game {_dir2 =  d, _snake2 = ((Coord (x, y)) :<| _)} -- A bidirectional pattern synonym viewing the front of a non-empty sequence.
  | d == North = Coord (x, (y+1) `mod` height)
  | d == South = Coord (x, (y-1) `mod` height)
  | d == East = Coord ((x+1) `mod` height, y)
  | d == West = Coord ((x-1) `mod` height, y)
nextHead2 _ = error "Snakes can't be empty!"

nextFood2 :: State Game ()
nextFood2 = do
  g <- get
  let counter = g ^. foodUpdateCounter
  when (counter `mod` 5 == 0) $ do
    let s = _snake g
    let f : fs = _foods g
    let t : ts = _foodTypes g
    foods .= fs
    foodTypes .= ts
    unless (f `elem` s) $ do
      food .= f
      foodType .= t
  -- Increment the counter
  foodUpdateCounter .= counter + 1

turn :: Command -> Game -> Game
turn d g
  | g ^. locked = g
  | g ^. camera == FPV && (d `elem` [CLeft, CRight])
  = g & dir %~ turnDir (g ^. camera) d & paused .~ False
      & locked .~ True
      & targetDegree %~ turnDegree d
  | g ^. camera == FPV && (d `elem` [CUp, CDown])
  = g
  | otherwise
  = g & dir %~ turnDir (g ^. camera) d
      & paused .~ False
      & locked .~ True

turn2 :: Command -> Game -> Game
turn2 d g
  | g ^. locked = g
  | g ^. camera == FPV && (d `elem` [CLeft, CRight])
  = g & dir2 %~ turnDir (g ^. camera) d & paused .~ False
      & locked .~ True
      & targetDegree2 %~ turnDegree d
  | g ^. camera == FPV && (d `elem` [CUp, CDown])
  = g
  | otherwise
  = g & dir2 %~ turnDir (g ^. camera) d
      & paused .~ False
      & locked .~ True


turnDir :: Camera -> Command -> Direction -> Direction
-- FPV
turnDir FPV CLeft  North =  West
turnDir FPV CLeft  West  =  South
turnDir FPV CLeft  South =  East
turnDir FPV CLeft  East  =  North
turnDir FPV CRight North =  East
turnDir FPV CRight East  =  South
turnDir FPV CRight South =  West
turnDir FPV CRight West  =  North
-- Fixed/Moving Camera
turnDir _ CUp d
  | d `elem` [West, East] =  North
  | otherwise =  d
turnDir _ CDown d
  | d `elem` [West, East] =  South
  | otherwise =  d
turnDir _ CLeft d
  | d `elem` [North, South] =  West
  | otherwise =  d
turnDir _ CRight d
  | d `elem` [North, South] =  East
  | otherwise =  d

turnDegree :: Command -> Double -> Double
turnDegree CLeft = (+ (-pi / 2))
turnDegree CRight = (+ (pi / 2))
turnDegree CUp = (+ (0))
turnDegree CDown = (+ (0))

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  rand_gen <- newStdGen
  let (f : fs) = randomRs ((0,0), (width - 1, height - 1)) rand_gen
      (t : ts) = (randomRs ('a', 'b') rand_gen)
      xm = width `div` 2
      ym = height `div` 2
      xm2 = -1
      ym2 = -1
      -- for MultiFood
      gridCoords = [(x, y) | x <- [0..width-1], y <- [0..height-1]]
      randomCoords = take 10 $ randomRs ((0,0), (width - 1, height - 1)) rand_gen
      randomFoodTypes = take 10 $ randomRs ('a', 'b') rand_gen
      foodLifetime = 5
      initialFoods = zipWith3 (\coord typ time -> (Coord coord, typ, time)) randomCoords randomFoodTypes (repeat foodLifetime)
      g =
        Game
          { _snake = S.fromList [Coord (xm, ym), Coord (xm, ym-1), Coord (xm, ym-2), Coord (xm, ym-3), Coord (xm, ym-4)], -- snake at the center
            _food = Coord f,
            _foods = Coord <$> fs,
            _foodType = t,
            _foodTypes = ts,
            _foodUpdateCounter = 0,
            _score = 0,
            _dir =  North,
            _dead = False,
            _paused = True,
            _locked = False,
            _twoPlayer = False,
            _snake2 = S.fromList [Coord (xm2, ym2), Coord (xm2, ym2-1), Coord (xm2, ym2-2), Coord (xm2, ym2-3), Coord (xm2, ym2-4)],
            _dir2 = North,
            _dead2 = False,
            _score2 = 0,
            _winner = -1,
            _degree = 0,
            _degree2 = 0,
            _rotationProgress = 24,
            _targetDegree = 0,
            _targetDegree2 = 0,
            _camera = Fixed,
            _color = BLUE,
            _dialog = D.dialog (Just $ C.str "Title") (Just (RedButton, [ ("Red",   RedButton,   Red), ("Blue",  BlueButton,  Blue), ("Green", GreenButton, Green)])) 50,
            _updateFoodLogicType = FixFood,
            _currentFoods = initialFoods,
            _rng = rand_gen,
            _bgmVolume = 1
          }
  return $ updateFoodLogic g

-- generateInitPos :: Int -> Seq(V2 Int)
generateInitPos :: Int -> Seq Coord
generateInitPos p = do
  let
      xm = width `div` 4
      ym = height `div` 4
      xm2 = width - xm
      ym2 = height - ym
  if p == 1
    then S.fromList [Coord (xm, ym), Coord (xm, ym-1), Coord (xm, ym-2), Coord (xm, ym-3), Coord (xm, ym-4)]
    else S.fromList [Coord (xm2, ym2), Coord (xm2, ym2-1), Coord (xm2, ym2-2), Coord (xm2, ym2-3), Coord (xm2, ym2-4)]


initGame2p :: IO Game
initGame2p = do
  rand_gen <- newStdGen
  -- let (f : fs) = (randomRs (V2 0 0, V2 (width - 1) (height - 1)) rand_gen)
  let (f : fs) = randomRs ((0,0), (width - 1, height - 1)) rand_gen
      (t : ts) = (randomRs ('a', 'b') rand_gen)
      xm = width `div` 4
      ym = height `div` 4
      xm2 = width - xm
      ym2 = height - ym
      -- for MultiFood
      gridCoords = [(x, y) | x <- [0..width-1], y <- [0..height-1]]
      randomCoords = take 10 $ randomRs ((0,0), (width - 1, height - 1)) rand_gen
      randomFoodTypes = take 10 $ randomRs ('a', 'b') rand_gen
      foodLifetime = 5
      initialFoods = zipWith3 (\coord typ time -> (Coord coord, typ, time)) randomCoords randomFoodTypes (repeat foodLifetime)
      g =
        Game
          { _snake = generateInitPos 1, -- snake at the center
            _food = Coord f,
            _foods = Coord <$> fs,
            _foodType = t,
            _foodTypes = ts,
            _foodUpdateCounter = 0,
            _score = 0,
            _dir = North,
            _dead = False,
            _paused = True,
            _locked = False,
            _twoPlayer = True,
            _snake2 = generateInitPos 2,
            _dir2 = North,
            _dead2 = False,
            _score2 = 0,
            _winner = -1,
            _degree = 0,
            _degree2 = 0,
            _rotationProgress = 24,
            _targetDegree = 0,
            _targetDegree2 = 0,
            _camera = Fixed,
            _color = BLUE,
            _dialog = D.dialog (Just $ C.str "Title") (Just (RedButton, [ ("Red",   RedButton,   Red), ("Blue",  BlueButton,  Blue), ("Green", GreenButton, Green)])) 50,
            _updateFoodLogicType = FixFood,
            _currentFoods = initialFoods,
            _rng = rand_gen,
            _bgmVolume = 1
          }
  return $ updateFoodLogic g

resetGame2p :: Game -> Game
resetGame2p g =
  if g ^. twoPlayer
    then g & dead .~ False & dead2 .~ False & dir .~ North & dir2 .~ North  & snake .~ generateInitPos 1 & snake2 .~ generateInitPos 2 & winner .~ -1  & paused .~ True -- set dir to be turnDir d
    else g