import HCL
import System.Random

guess_num_boring =
    do
      num <- prompt "Enter your guess between 1 - 100: " reqInt
      if num == 50
        then reqIO $ putStrLn "You win!"
        else reqIO $ putStrLn "Too bad!"

guess_num_fun =
      do
        target <- reqIO $ getStdRandom (randomR (1::Integer,100))
        let guessed val =
              case compare target val of
                GT -> do { reqIO $ putStrLn "Too low!"; return False }
                LT -> do { reqIO $ putStrLn "Too high!"; return False }
                EQ -> do { reqIO $ putStrLn "You win!"; return True }
        reqUntil guessed (prompt "Enter a number between 1 and 100: " reqInteger)

guess_num_cont =
    reqCont guess_num_fun confirm
  where
    confirm =
      reqIf (promptAgree "Are you sure you want to quit? " (Just False) reqResp)
        reqFail
        guess_num_cont

guess_num_confirm =
    reqConfirm confirm guess_num_fun 
  where
    confirm = promptAgree "Are you sure you want to quit? " (Just False) reqResp
        
play_game game = execReq game

