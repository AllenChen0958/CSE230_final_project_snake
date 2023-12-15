testTurn2 :: Test
testTurn2 = TestCase $ do
    
    let 
        
        xm = 20 `div` 4
        ym = 20 `div` 4
        xm2 = 20 - xm
        ym2 = 20 - ym
        initialGame = Game
          { _snake = S.fromList [Coord (xm, ym), Coord (xm, ym-1), Coord (xm, ym-2), Coord (xm, ym-3), Coord (xm, ym-4)], -- snake at the center
            _food = Coord (xm, ym),
            _foods = Coord (xm, ym),
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
            _rng = rand_gen
          }
    let command = CLeft

    let 
        xm = 20 `div` 4
        ym = 20 `div` 4
        xm2 = 20 - xm
        ym2 = 20 - ym
        expectedGame = Game
          { _snake = S.fromList [Coord (xm, ym), Coord (xm, ym-1), Coord (xm, ym-2), Coord (xm, ym-3), Coord (xm, ym-4)], -- snake at the center
            _food = Coord (xm, ym),
            _foods = Coord (xm, ym),
            _foodType = t,
            _foodTypes = ts,
            _foodUpdateCounter = 0,
            _score = 0,
            _dir =  North,
            _dead = False,
            _paused = False,
            _locked = True,
            _twoPlayer = False,
            _snake2 = S.fromList [Coord (xm2, ym2), Coord (xm2, ym2-1), Coord (xm2, ym2-2), Coord (xm2, ym2-3), Coord (xm2, ym2-4)],
            _dir2 = West,
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
            _rng = rand_gen
          }
    let resultGame = turn2 command initialGame

    assertEqual "turn2 should update the game correctly" expectedGame resultGame
