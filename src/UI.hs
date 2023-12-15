{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (..),
    EventM,
    Padding (..),
    Widget,
    attrMap,
    attrName,
    customMain,
    emptyWidget,
    fg,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    on,
    padAll,
    padLeft,
    padRight,
    padTop,
    padBottom,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
    showFirstCursor
  )

import Sound.ProteaAudio
import qualified Data.ByteString as SB
import System.Environment
import System.FilePath
import Control.Concurrent

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
import Linear.V2 (V2 (..))
import Snake
    ( Command(..),
      Coord(..),
      Game (..),
      Direction (..),
      Camera(..),
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
      dir,
      color,
      degree,
      targetDegree,
      degree2,
      targetDegree2,
      rotationProgress,
      renderTick,
      camera,
      height,
      width,
      step,
      updateFoodLogic,
      changeUpdateFoodLogic,
      turn,
      turn2,
      initGame,
      initGame2p,
      twoPlayer,
      winner,
      resetGame2p,
      dialog, updateFoodLogicType,
      bgmVolume
    )
import Control.Monad (unless)
import Control.Lens.Setter (assign)

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data CustomEvent = Tick Int | UpdateFood



-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
--type Name =  ()


data Cell = Snake | Snake2 | SnakeHead | SnakeHead2 | Empty | MagentaSnake | CyanSnake | WhiteSnake | FoodA | FoodB

-- App definition

app :: App Game CustomEvent Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,  -- neverShowCursor
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

main :: IO ()
main = do
  result <- initAudio 64 44100 1024
  unless result $ fail "failed to initialize the audio system"
  sampleA <- sampleFromFile "./src/bgm.mp3" 0.1 -- volume
  sndTrkA <- soundLoop sampleA 1 1 0 1

  chan <- newBChan 10

  -- New food update thread
  forkIO $ forever $ do
    writeBChan chan UpdateFood
    threadDelay 1000000 -- updates food every 5 seconds

  forkIO (countThread chan 0)

  g <- initGame

  let builder = mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

countThread :: Brick.BChan.BChan CustomEvent -> Int -> IO b
countThread c n = do
    writeBChan c (Tick n)
    threadDelay 5000
    countThread c ((n+1) `mod` renderTick)

-- Handling events

handleEvent :: BrickEvent Name CustomEvent -> EventM Name Game ()
handleEvent (AppEvent (Tick n)) = do
  modify render
  g@(Game {_rotationProgress = rp}) <- get
  when (renderTick - 1 == rp) $ modify step
handleEvent (AppEvent UpdateFood) = modify updateFoodLogic
handleEvent (VtyEvent (V.EvKey V.KUp [])) = modify $ turn CUp
handleEvent (VtyEvent (V.EvKey V.KDown [])) = modify $ turn CDown
handleEvent (VtyEvent (V.EvKey V.KRight [])) = modify $ turn CRight
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = modify $ turn CLeft
handleEvent (VtyEvent (V.EvKey (V.KChar 'w') [])) = modify $ turn2 CUp
handleEvent (VtyEvent (V.EvKey (V.KChar 's') [])) = modify $ turn2 CDown
handleEvent (VtyEvent (V.EvKey (V.KChar 'd') [])) = modify $ turn2 CRight
handleEvent (VtyEvent (V.EvKey (V.KChar 'a') [])) = modify $ turn2 CLeft
handleEvent (VtyEvent (V.EvKey (V.KChar '6') [])) = modify $ turn CRight
handleEvent (VtyEvent (V.EvKey (V.KChar '4') [])) = modify $ turn CLeft
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO initGame >>= put
handleEvent (VtyEvent (V.EvKey (V.KChar '1') [])) = liftIO Snake.initGame >>= put -- 1p
handleEvent (VtyEvent (V.EvKey (V.KChar '2') [])) = liftIO Snake.initGame2p >>= put --2p
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = modify Snake.resetGame2p --reset snake to initial position
handleEvent (VtyEvent (V.EvKey (V.KChar 'm') [])) = modify changeCamera
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [])) = modify changeColor
handleEvent (VtyEvent (V.EvKey (V.KChar 'f') [])) = modify changeUpdateFoodLogic
handleEvent (VtyEvent (V.EvKey (V.KChar ',') [])) = do
                                                      v <- use bgmVolume
                                                      if v > 0 then assign bgmVolume (v-0.1) else assign bgmVolume v
                                                      v <- use bgmVolume
                                                      liftIO $ do
                                                        volume v v
                                                      return ()
handleEvent (VtyEvent (V.EvKey (V.KChar '.') [])) = do
                                                      v <- use bgmVolume
                                                      if v < 2 then assign bgmVolume (v+0.1) else assign bgmVolume v
                                                      v <- use bgmVolume
                                                      liftIO $ do
                                                        volume v v
                                                      return ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent _ = return ()


changeColor :: Game -> Game
changeColor g@(Game {_color = BLUE}) = g & color .~ MAGENTA
changeColor g@(Game {_color = MAGENTA}) = g & color .~ CYAN
changeColor g@(Game {_color = CYAN}) = g & color .~ WHITE
changeColor g@(Game {_color = WHITE}) = g & color .~ BLUE

changeCamera :: Game -> Game
changeCamera g@(Game {_camera = Fixed}) = g & camera .~ Moving & bgmVolume .~ 2.0
changeCamera g@(Game {_camera = Moving, _dir = North}) = g & camera .~ FPV & targetDegree .~ 0  & targetDegree2 .~ 0
changeCamera g@(Game {_camera = Moving, _dir = East}) = g & camera .~ FPV & targetDegree .~ (pi/2) & targetDegree2 .~ (pi/2)
changeCamera g@(Game {_camera = Moving, _dir = South}) = g & camera .~ FPV & targetDegree .~ pi & targetDegree2 .~ pi
changeCamera g@(Game {_camera = Moving, _dir = West}) = g & camera .~ FPV & targetDegree .~ (-pi/2) & targetDegree2 .~ (-pi/2)
changeCamera g@(Game {_camera = FPV}) = g & camera .~ Fixed


-- changeCamera2 :: Game -> Game
-- changeCamera2 g@(Game {_camera = Fixed}) = g & camera .~ Moving
-- changeCamera2 g@(Game {_camera = Moving, _dir2 = North}) = g & camera .~ FPV & targetDegree2 .~ 0
-- changeCamera2 g@(Game {_camera = Moving, _dir2 = East}) = g & camera .~ FPV & targetDegree2 .~ (pi/2)
-- changeCamera2 g@(Game {_camera = Moving, _dir2 = South}) = g & camera .~ FPV & targetDegree2 .~ pi
-- changeCamera2 g@(Game {_camera = Moving, _dir2 = West}) = g & camera .~ FPV & targetDegree2 .~ (-pi/2)
-- changeCamera2 g@(Game {_camera = FPV}) = g & camera .~ Fixed



render :: Game -> Game
render g@(Game {_degree = d, _targetDegree = td, _rotationProgress = rp}) = if renderTick - 1 == rp then g else g & degree .~ (1.0 - p) * d + p * td & rotationProgress %~ (+ 1)
  where
    p :: Double
    p = fromIntegral rp / (fromIntegral renderTick - 1)

drawUI :: Snake.Game -> [Widget Name]
drawUI g =
    --[ padLeft (Pad 5) $ padRight (Pad 5) (drawStats g) <+>  padRight (Pad 5) (drawGrid g) <+> instr]  -- <+> instr
    if g ^. Snake.twoPlayer
      then
        case g ^. camera of
          Fixed  -> [C.center $ padRight (Pad 5) (drawStats g) <+> padRight (Pad 10) (drawGrid g 0) <+> instr]  -- <+> instr
          Moving -> [C.center $ padRight (Pad 5) (drawStats g) <+> padRight (Pad 10) (drawGrid g 1) <+> padRight (Pad 10) (drawGrid g 2) <+> instr]  -- <+> instr
          FPV    -> [C.center $ padRight (Pad 5) (drawStats g) <+> padRight (Pad 10) (drawGrid g 0) <+> instr]  -- <+> instr
      else
        [C.center $ padRight (Pad 5) (drawStats g) <+> padRight (Pad 10) (drawGrid g 1) <+> instr]




instr :: Widget Name
instr = 
  C.hLimit 70 $
  C.vLimit 20 $
   withBorderStyle BS.unicodeBold $
    B.borderWithLabel (withAttr titleAttr $ str "Game Instruction") $
        C.center $ padAll 1 $ (withAttr cameraAttr $ C.str "Press `key Up/Down/Left/Right`: move snake up/down/left/right \n\nPress `key w/s/a/d`: move snake up/down/left/right  \n\nPress `r`: reset initial game  \n\nPress `c`: change snake color \n\nPress `m`: change camera viewpoint  \n\nPress `1`: single-player game mode \n\nPress `2`: multi-player game mode  \n\nPress `f`: change food mode (good food: 🍑 \x08, bad food: 💩 \x08)   \n\nPress `q`: quit game")




drawDialog :: Game -> [Widget Name]
drawDialog g = [ui]
    where
        ui = D.renderDialog (g ^. dialog) $ C.hCenter $ padAll 1 $ str "This is the dialog body."

      
drawStatsHelper :: Game -> Widget Name
drawStatsHelper g = 
  if g ^. Snake.twoPlayer
    then
      C.hLimit 30 $
      withBorderStyle BS.unicodeBold $
        B.borderWithLabel (withAttr titleAttr $ str "Score") $
          C.hCenter $
          padAll 1 $
            C.vLimit 20 $ vBox [mid2, mid3]
    else
      C.hLimit 30 $
      withBorderStyle BS.unicodeBold $
        B.borderWithLabel (withAttr titleAttr $ str "Score") $
          C.hCenter $
          padAll 1 $
            C.vLimit 10 $ vBox [mid1, mid4]
            where
              mid1 = C.center $ drawScore (g ^. score)
              mid2 = C.center $ drawPlayer g
              mid3 = C.center $ drawWinner g
              mid4 = C.center $ drawGameOver (g ^. dead)



drawStats :: Snake.Game -> Widget Name
drawStats g =
  if g ^. Snake.twoPlayer
       then
        case g ^. camera of
          Fixed  -> C.hLimit 30 $
                        withBorderStyle BS.unicodeBold $
                          B.borderWithLabel (withAttr titleAttr $ str "Score") $
                            C.hCenter $
                            padAll 1 $
                              C.vLimit 20 $ vBox [mid2, midFixed, mid3]
          Moving -> C.hLimit 30 $
                          withBorderStyle BS.unicodeBold $
                            B.borderWithLabel (withAttr titleAttr $ str "Score") $
                              C.hCenter $
                              padAll 1 $
                                C.vLimit 20 $ vBox [mid2, midMoved, mid3]
          FPV    -> C.hLimit 30 $
                        withBorderStyle BS.unicodeBold $
                          B.borderWithLabel (withAttr titleAttr $ str "Score") $
                            C.hCenter $
                            padAll 1 $
                              C.vLimit 20 $ vBox [mid2, midFPV, mid3]
    else
      case g ^. camera of
           Fixed  ->  C.hLimit 30 $
                        withBorderStyle BS.unicodeBold $
                          B.borderWithLabel (withAttr titleAttr $ str "Score") $
                            C.hCenter $
                            padAll 1 $
                              C.vLimit 10 $ vBox [mid1, midFixed, mid4]
           Moving ->  C.hLimit 30 $
                        withBorderStyle BS.unicodeBold $
                          B.borderWithLabel (withAttr titleAttr $ str "Score") $
                            C.hCenter $
                            padAll 1 $
                              C.vLimit 10 $ vBox [mid1, midMoved, mid4]
           FPV    -> C.hLimit 30 $
                      withBorderStyle BS.unicodeBold $
                        B.borderWithLabel (withAttr titleAttr $ str "Score") $
                          C.hCenter $
                          padAll 1 $
                            C.vLimit 10 $ vBox [mid1, midFPV, mid4]
  where
              mid1 = C.center $ drawScore (g ^. score)
              mid2 = C.center $ drawPlayer g
              mid3 = C.center $ drawWinner g
              mid4 = C.center $ drawGameOver (g ^. dead)
              midFixed = drawCameraMode Fixed
              midMoved = drawCameraMode Moving
              midFPV = drawCameraMode FPV



drawPlayer :: Snake.Game -> Widget Name
drawPlayer g =
        (withAttr snakeAttr (str "  ")  <+> (withAttr titleAttr $ str " Player 1: " <+> str (show (g ^. Snake.score))) <+> str "\n\n\n\n" )
        C.<=>
        (withAttr snake2Attr (str "  ") <+> (withAttr titleAttr $ str " Player 2: " <+> str (show (g ^. Snake.score2))) ) 


drawWinner :: Snake.Game -> Widget Name
drawWinner g
  | g ^. Snake.winner == 1 = C.hCenter $ (withAttr gameOverAttr $ str "Player 1 kills Player 2! \n\n Game Over")
  | g ^. Snake.winner == 2 = C.hCenter $ (withAttr gameOverAttr $ str "Player 2 kills Player 1! \n\n Gamer Over")
  | g ^. Snake.winner == 0 = C.hCenter $ (withAttr gameOverAttr $ str "Collision! \n\n Game Over")
  | otherwise = emptyWidget

drawScore :: Int -> Widget Name
drawScore n = withAttr titleAttr $ str $ show n
          

drawCameraMode :: Camera -> Widget Name
drawCameraMode Fixed  = withAttr cameraAttr $ C.hCenter $ str "Fixed Camera \n\n"
drawCameraMode Moving = withAttr cameraAttr $ C.hCenter $ str "Moving Camera \n\n"
drawCameraMode FPV    = withAttr cameraAttr $ C.hCenter $ str "FPV Camera \n\n"

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
    then withAttr gameOverAttr $ C.hCenter $ str "Collision! \n\nGAME OVER"
    else emptyWidget

custom :: BS.BorderStyle
custom =
    BS.BorderStyle { 
                    BS.bsCornerTL = '🐍'
                   , BS.bsCornerTR = '🐍'
                   , BS.bsCornerBR = '🐍'
                   , BS.bsCornerBL = '🐍'
                   , BS.bsIntersectFull = '🐍'
                   , BS.bsIntersectL = '🐍'
                   , BS.bsIntersectR = '🐍'
                   , BS.bsIntersectT = '🐍'
                   , BS.bsIntersectB = '🐍'
                   , BS.bsHorizontal = '𓆗'
                   , BS.bsVertical = '🐍'
                   }
-- TODO: change snake2 color
drawGrid :: Game -> Int -> Widget Name
drawGrid g@(Game {_dir = _}) player = -- player ==0 : show both snake, player==1: show snake1, player==2: show snake2
  withBorderStyle custom $
    B.borderWithLabel (withAttr titleAttr $ str "Snake") $
      vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height - 1, height - 2 .. 0]]
    cellsInRow y = [drawCoord (Coord (x, y)) | x <- [0 .. width - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | relativeCoord cameraC currentDir mainSnakeHead c == snakeHead      = SnakeHead
      | relativeCoord cameraC currentDir mainSnakeHead c == snakeHead2      = SnakeHead2
      | relativeCoord cameraC currentDir mainSnakeHead c `elem` g ^. snake = case g ^. color of
                                                                          BLUE -> Snake
                                                                          MAGENTA -> MagentaSnake
                                                                          CYAN -> CyanSnake
                                                                          WHITE -> WhiteSnake
      | relativeCoord cameraC currentDir mainSnakeHead c `elem` g ^. snake2 = Snake2 -- TODO feature: change snake2 color
      | relativeCoord cameraC currentDir mainSnakeHead c == (g ^. food) && (g ^. updateFoodLogicType) `elem` [FixFood, RandFood] = case g ^. foodType of
                                                                          'a' -> FoodA
                                                                          'b' -> FoodB
      | ((g ^. updateFoodLogicType) == MultiFood) && any (\(pos, _, _) -> relativeCoord cameraC currentDir mainSnakeHead c == pos) (g ^. currentFoods) =
          maybe Empty foodTypeToCell (lookupFood (relativeCoord cameraC currentDir mainSnakeHead c) (g ^. currentFoods))
      | otherwise           = Empty
        where
          snakeHead S.:<| _ = g ^. snake
          snakeHead2 S.:<| _ = g ^. snake2
          mainSnakeHead = if player==1 then snakeHead else snakeHead2
          currentDir        = if player==1 then g ^. degree else g ^. degree2
          cameraC           = g ^. camera
          lookupFood coord foods = lookup coord [(pos, t) | (pos, t, _) <- foods]
          foodTypeToCell 'a' = FoodA
          foodTypeToCell 'b' = FoodB
          foodTypeToCell _   = Empty

relativeCoord :: (Num p, RealFrac p, Floating p) => Camera -> p -> Coord -> Coord -> Coord
relativeCoord Fixed _ (Coord (a, b)) (Coord (x, y)) = Coord (x `mod` width, y `mod` height)
relativeCoord Moving _ (Coord (a, b)) (Coord (x, y)) = Coord ((x+a + width `div` 2) `mod` width, (y+b + height `div` 2) `mod` height)
relativeCoord FPV d (Coord (a, b)) (Coord (x, y)) = Coord ((a + round (fromIntegral x' * cos d + fromIntegral y' * sin d)) `mod` width,
                                                                (b + round (fromIntegral y' * cos d - fromIntegral x' * sin d)) `mod` height)
  where
    y' = y - height `div` 2
    x' = x - width `div` 2

drawCell :: Cell -> Widget Name
drawCell SnakeHead    = withAttr snakeHeadAttr cw
drawCell SnakeHead2    = withAttr snakeHead2Attr cw
drawCell Snake        = withAttr snakeAttr cw
drawCell Snake2       = withAttr snake2Attr cw
drawCell Empty        = withAttr emptyAttr cw
drawCell MagentaSnake = withAttr snakeAttrMagenta cw
drawCell CyanSnake    = withAttr snakeAttrCyan cw
drawCell WhiteSnake   = withAttr snakeAttrWhite cw
drawCell FoodA        = withAttr foodAttr (cf 'a')
drawCell FoodB        = withAttr foodAttr (cf 'b')


cw :: Widget Name
cw = str "  "

cf :: Char -> Widget Name
cf 'a' = str "🍑 \x08"
cf 'b' = str "💩 \x08"

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (snakeAttr, V.blue `on` V.blue),
      (snake2Attr, V.green `on` V.green),
      (foodAttr, V.red `on` V.red),
      (gameOverAttr, fg V.red `V.withStyle` V.bold),
      (snakeAttrMagenta, V.magenta `on` V.magenta),
      (snakeAttrCyan, V.cyan `on` V.cyan),
      (snakeAttrWhite, V.white `on` V.white),
      (snakeHeadAttr, V.brightRed `on` V.brightRed),
      (snakeHead2Attr, V.brightBlack `on` V.brightBlack),
      (gameOverAttr, fg V.red `V.withStyle` V.bold),
      (cameraAttr, fg V.yellow `V.withStyle` V.bold),
      (titleAttr,            fg V.green `V.withStyle` V.bold),
      (B.hBorderAttr,         fg V.yellow),
      (B.vBorderAttr,         fg V.yellow),
      (B.borderAttr,         fg V.yellow),
      (D.dialogAttr, V.white `on` V.blue),
      (D.buttonAttr, V.black `on` V.white),
      (D.buttonSelectedAttr, fg V.yellow),
      (foodAttr, fg V.red)
    ]



snakeAttr, snakeHeadAttr, snake2Attr, emptyAttr, gameOverAttr, cameraAttr, foodAttr :: AttrName
snakeAttr = attrName "snakeAttr"
snake2Attr = attrName "snake2Attr" -- player2 snake
snakeAttrMagenta = attrName "snakeAttrMagenta"
snakeAttrCyan = attrName "snakeAttrCyan"
snakeAttrWhite = attrName "snakeAttrWhite"
snakeHeadAttr = attrName "snakeHeadAttr"
snakeHead2Attr = attrName "snakeHead2Attr"
emptyAttr = attrName "emptyAttr"
gameOverAttr = attrName "gameOver"
cameraAttr = attrName "cameraAttr"
titleAttr = attrName "titleAttr"
foodAttr = attrName "foodAttr"

