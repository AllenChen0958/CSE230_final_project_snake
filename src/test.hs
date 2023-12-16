{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Test.QuickCheck
import Snake
import UI hiding (main)
import Control.Lens hiding (elements, elements, (:<), (:>), (<|), (|>))
import Test.QuickCheck.Monadic

import Control.Lens hiding (elements, (:<), (:>), (<|), (|>))
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

import Brick.BChan (newBChan, writeBChan, BChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as C
import qualified Brick.Widgets.Dialog as D
import Brick.Focus as BF
import Data.Text(Text, unpack)
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((^.), (.~), (&), (%~), use, (%=))
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify, put, MonadState (get))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import System.Random.Stateful

initStateGame :: Game
initStateGame =
  let rand_gen = mkStdGen 1
      (f : fs) = randomRs ((0,0), (width - 1, height - 1)) rand_gen
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
            _rotationProgress2 = 24,
            _targetDegree = 0,
            _targetDegree2 = 0,
            _camera = Snake.Fixed,
            _color = BLUE,
            _dialog = D.dialog (Just $ C.str "Title") (Just (RedButton, [ ("Red",   RedButton,   Red), ("Blue",  BlueButton,  Blue), ("Green", GreenButton, Green)])) 50,
            _updateFoodLogicType = FixFood,
            _currentFoods = initialFoods,
            _rng = rand_gen,
            _bgmVolume = 1
          }
  in
    g

data GameTops c
    = STEP
    | TURN c

ofGameTops ops = foldr doOp initStateGame ops
  where
      doOp STEP       = step
      doOp (TURN c)   = turn c

genStep :: Gen (GameTops Command)
genStep = do
    return STEP

commands = [CUp, CDown, CLeft, CRight]

genTurn :: Gen (GameTops Command)
genTurn = do
    c <- elements commands
    return (TURN c)

genGameTops ::  Gen (GameTops Command)
genGameTops  = resize 10000 $ frequency [(5, genStep), (1, genTurn)]  -- maximum 1,000 ops game

-- Test step function, single player

get_snake_property :: (Game -> Bool) -> Property
get_snake_property f = forAllBlind (listOf genGameTops) $ \ops ->
                    f (ofGameTops ops)

get_score_food :: Game -> Bool
get_score_food g = _twoPlayer g || ((_food g /= nextHead g) || (case _foodType g of
    'a' -> 100 + _score g == _score (step g)
    'b' -> -50 + _score g == _score (step g)))


hit_self :: Game -> Bool
hit_self g = _twoPlayer g || (notElem (nextHead g) (_snake g) || _dead (step g))

same_length :: Game -> Bool
same_length g = (_food g == nextHead g) || (length (_snake g) == length (_snake (step g)))

increment_length :: Game -> Bool
increment_length g = (_food g /= nextHead g) || (case _foodType g of
    'a' -> 1 + length (_snake g) == length (_snake (step g))
    'b' -> length (_snake g) == length (_snake (step g)))

never_empty :: Game -> Bool
never_empty g = not (null (_snake g))

main :: IO ()
main = do
    quickCheck (withMaxSuccess 10000 $ get_snake_property get_score_food)
    quickCheck (withMaxSuccess 10000 $ get_snake_property hit_self)
    quickCheck (withMaxSuccess 10000 $ get_snake_property same_length)
    quickCheck (withMaxSuccess 10000 $ get_snake_property increment_length)
    quickCheck (withMaxSuccess 10000 $ get_snake_property never_empty)
