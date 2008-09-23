-- Contains a variety of HCL test programs

import HCL


-- Play the guess a number game
playGame :: IO ()
playGame = execReq gameReq

-- Demonstrates filling out a simple form.
data Form = Form Name Age Children (Maybe Color)
type Name = String
type Age = Integer
type Children = Maybe Integer
data Color = Red | Green | Blue
  deriving (Show, Read)

-- Request used to gather form information.
formReq :: Request Form
formReq =
  do
    name <- prompt "Please enter your name: " reqResp
    age <- prompt "Please enter your age: " reqInteger
    numChildren <- reqIf (promptAgree "Any children? " (Just True) reqResp)
        (do { result <- prompt "Number of children: " reqInteger; return $Just result})
        (return Nothing) 
    color <- prompt "Please enter your favorite color: " (reqRead reqResp)
    return $ Form name age numChildren color

  
{-
fillForm :: IO ()
fillForm =
  do
    reqMaybe formReq
      (putStrLn "You don't have to answer ... this time.")
      printForm
  where
    printForm (Form name age numChildren color) =
      do
        putStrLn $ "Your name is " ++ name ++
          " and you are " ++ show age ++ " years old."
        maybe 
          (putStrLn "You don't have a favorite color?")
          (\c -> putStrLn $ "Your favorite color is " ++ show c)
          color
        maybe
          (putStrLn "You don't have any children.")
          (\n -> putStrLn $ "You have " ++ show n ++ " children.")
          numChildren
-}

-- Actual request used to play the game.
gameReq :: Request Bool 
gameReq =
  do
    target <- reqIO $ getStdRandom (randomR (100::Integer,200))
    game target
  where
    game :: Integer -> Request Bool
    game target =
        reqUntil $ 
          do
            let reqCompare = reqLift2 (compare)
            cmp <- reqCompare
                    (reqConst target) (prompt "Enter a number: " reqInteger)
            case cmp of
              LT -> do { reqIO $ putStrLn "Too high!"; return False }
              GT -> do { reqIO $ putStrLn "Too low!"; return False }
              EQ -> do { reqIO $ putStrLn "You win!"; return True } 

-- An annoying example.
annoying =
  do
    val <- runRequest $ reqUntil $ promptAgree "Answer yes or I won't go away." (Just False) reqResp 
    case val of
      Nothing -> putStrLn "Foo"
      Just True -> putStrLn "Bar"

-- Demonstrates that short circuting works with `orReq`. If the
-- first two inputs are equal, further requests aren't presented. However,
-- if they are not, the second two values are asked for.
compareInputs =
  do
    v <- runRequest prog
    putStrLn (show v)
  where
    reqEq = reqLift2 (==)
    reqNEq = reqLift2 (/=)
    boolRequest = (reqEq reqInteger reqInteger) `orReq` (reqNEq reqInteger reqInteger)
    prog = reqIf (prompt "Enter up to 4 numbers (separated by newlines)" boolRequest)
      (reqConst "equal") (reqConst "not equal")

data Google = Calc | Map | Search
  deriving (Show, Eq)

googleMenu :: Request Google
googleMenu =
    promptChoices googleChoices reqInt
  where
    googleChoices = [("Calculator", Calc), ("Maps", Map), ("Search", Search)]

doGoogle =
  do
    choice <- runRequest googleMenu
    case choice of
      Just Calc -> putStrLn "Google calculator."
      Just Map -> putStrLn "Google map"
      
taskMenu :: Request (IO ())
taskMenu =
    promptChoices taskChoices reqInt
  where
    taskChoices :: [(String, IO ())]
    taskChoices = [("Current Time", currentTimeTask), ("Uptime", uptimeTask), ("Other Tasks", otherTasks)]
    currentTimeTask :: IO ()
    currentTimeTask = do { putStrLn "FooBar"; return () }
    uptimeTask :: IO ()
    uptimeTask = do { putStrLn "FooBaz"; return () }
    otherTasks :: IO ()
    otherTasks = do { result <- runRequest taskMenu2; maybe (return ()) (\r -> do { r; return ()} ) result }

taskMenu2 :: Request (IO ())
taskMenu2 = 
    promptChoices taskChoices reqInt
  where
    taskChoices = [("Current Date", currentDateTask), ("Random Numbe", randomNumberTask)]
    currentDateTask = do { putStrLn "Current Date"; return () }
    randomNumberTask = do { putStrLn "Random Number"; return () }

doTasks =
  do
    choice <- runRequest taskMenu
    maybe (return ()) (\v -> do { v; return () }) choice 
