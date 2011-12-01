import System.IO
import GameEngine
import Data.List(intersperse)
import System.Random 


main = hSetBuffering stdout NoBuffering >> do
  rand   <- newStdGen
  
  let (num,_) = next rand
  let ww = "ABCDEFGHIJK" !! (num `mod` 11)
  let village = aWerewolf10Villagers ww
  
  input <- getLine
  let (cont, (o, v'))  = startup village [input]
  putStrLn (concat $ intersperse "\n" o)
  
  if ww == 'A' then 
    playWerewolf rand stdout v' cont
  else
    playVillager rand stdout v' cont

playWerewolf rand out v (E name f) = do 
  input <- getLine
  let (cont, (o, v'))    = f v [input]
  
  nextStep rand out playWerewolf v' cont o 
  
playVillager rand out v (E name f) = do 
  let (eaten, rand')     = werewolfEatsAVillager v rand
  let (cont, (o, v'))    = f v [[eaten]]
  hPutStrLn out $ concat o
  
  nextStep rand out playVillager v' cont o
  
nextStep rand out f v' cont o  = do
  input <- getLine
  let (rand', killed) = villagersVote (head input) v' rand
  let (cont', (o', v'')) = run cont v' [[killed]]
  continue f v'' o' rand' out  v'' cont'

continue f v' o' rand'' out v'' cont' =   
  case endGame v' of
    None       -> hPutStrLn out (concat $ intersperse "\n" o') >> 
                  f rand'' out v'' cont'
    Werewolves -> hPutStrLn out $ "the werewolves win"
    Villagers  -> hPutStrLn out $ "the villagers win"


-- TESTS
