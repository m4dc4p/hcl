import System.Console.HCL
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import System.Random (getStdRandom, randomR)
import Control.Exception (bracket)
import System.IO (openFile, hClose, IOMode (ReadMode), hGetContents, hFileSize)
import Data.List (genericDrop)
import qualified Paths_HCL as Path -- Necessary because of cabal packaging.

-- Stores info for the hangman game, including
-- the word being guessed, letters used so far, and
-- correctly guessed letters (and positions) so far
data Hangman = Hangman {
  numTurns :: Int,
  word :: String,
  lettersUsed :: Set Char,
  turn :: Int }

-- Make an initial hangman value.
makeHangman turns goal = Hangman turns goal Set.empty 0

-- Select a random word from those in the file "2of12.txt"
-- Tries to be fancy and lazy by potentially reading past the end
-- of the fail and scaling back.
selectWord :: IO String
selectWord =
  do
    wordFile <- Path.getDataFileName "hangman/2of12.txt"
    bracket (openFile wordFile ReadMode) hClose getWord
  where
    -- Select a word from the file. Start with file size / 2
    -- as our estimate of number of words
    getWord f =
      do
        sz <- hFileSize f 
        wordList <- hGetContents f
        pickLine (sz `div` 2) . lines $ wordList
    pickLine max words = 
      do
        whichLine <- getStdRandom (randomR (0, max))
        if null $ genericDrop (whichLine - 1) words
          then pickLine (max `div` 2) words
          -- On UNIX platforms, \r at end of each word is not
          -- filtered, thus takeWhile
          else return $! (takeWhile isAlpha . head . genericDrop (whichLine - 1) $ words)

-- Add a guess to the hangman game.
addGuess :: Hangman -> Char -> Hangman
addGuess h@(Hangman {lettersUsed = letters, turn = currTurn, word = currWord}) char =
  if char `elem` currWord
    then h { lettersUsed = (Set.insert char letters) }
    else h { lettersUsed = (Set.insert char letters), turn = (currTurn + 1) }

hangManPic :: Bool -> [String]
hangManPic final = 
    "    _______    " :
    "   /      |     " :
    "   |     ___     " :
    eyes :
    "   |    |   |     " :
    "   |     \\_/      " :
    "   |      |      " :
    "   |     /|\\      " :
    "   |    / | \\     " :
    "   |      |       " :
    "   |      |      " :
    "   |      |       " :
    "   |     / \\       " :
    "   |    /   \\     " :
    "   |             " :
    "----------------" : []
  where
    eyes :: String
    eyes =
      if final
      then "   |    /x x\\     " 
      else "   |    /   \\     " 

-- Print current state of Hangman game.
printHangMan :: Hangman -> IO ()
printHangMan h =
  do
    putStrLn ""
    putStrLn $ formatHangman h
    putStrLn $ "Turns remaining: " ++ show ((numTurns h) - (turn h))
    putStrLn $ "Letters used: " ++ (formatLettersUsed h)
    putStrLn $ "Guess: " ++ (formatGuess h)
  where
    formatLettersUsed :: Hangman -> String
    formatLettersUsed h = foldr (\c rest -> c : " " ++ rest) "" (Set.elems $ lettersUsed h)
    formatGuess :: Hangman -> String
    formatGuess h =
        foldr (\l r -> l : ' ' : r) "" formatGuess 
      where
        formatGuess = map (\c -> if letterGuessed c then c else '_') (word h)
        letterGuessed c = c `Set.member` (lettersUsed h)    
    formatHangman :: Hangman -> String
    formatHangman (Hangman numTurns _ _ turn) =
      let
          picture = hangManPic False
          picLength = toRational $ length picture
          picRatio = picLength / (toRational numTurns)
          amount = (toRational turn) * picRatio
      in
        -- Display last amount lines, since we want to show hangman from bottom up
        unlines $ drop (floor (picLength - amount)) picture
  
-- Plays a round of hangman.
playRound :: Hangman -> Request Hangman
playRound hangMan = 
  do
    nextTurn hangMan
    guess <- reqGuess
    return (addGuess hangMan guess)
  where
    -- Get the next guess from the user. If a letter is entered which already exists,
    -- the user is asked to guess again.
    reqGuess = reqUntil newGuess reqLetter
    -- Test which determines if the character entered has been guessed before.
    newGuess char = return (not $ char `Set.member` (lettersUsed hangMan))
    -- Gets a single letter from the user. If a line of text is entered, only
    -- the first value is used. Loops until valid input is entered. Return/spaces will
    -- cancel though.
    reqLetter :: Request Char
    reqLetter =
      reqLift head $
        reqUntil validGuess (prompt ("Guess a letter: ") reqResp)
    -- Test which determines if the character entered is valid.
    validGuess [] = return False
    validGuess (s:str) = return (isLetter s)
    -- Processing for the next turn of hangman.
    nextTurn hangMan =
      do
        if wordGuessed hangMan
         then do { reqIO $ putStrLn ("You win! The word was " ++ word hangMan); reqFail }
         else
           if (turn hangMan >= numTurns hangMan)
             then do { reqIO $ sequence [putStrLn $ unlines (hangManPic True), putStrLn ("You lose! The word was " ++ word hangMan)]; reqFail }
             else reqIO $ printHangMan hangMan 
    -- Determine if the word has been guessed yet.    
    wordGuessed :: Hangman -> Bool
    wordGuessed h = and (map (`Set.member` (lettersUsed h)) (word h))

-- Picks a word and starts the hangman game.
main :: IO ()
main =
  do
    putStrLn "Hangman uses the excellent word list 2of12.txt compiled by Alan Beale (biljir@pobox.com). Let's play!"
    word <- selectWord 
    execReq $ reqIterate playRound (makeHangman 10 word)
     