import System.IO
import GameEngine
import Data.List(intersperse)
import System.Random 

main = hSetBuffering stdout NoBuffering >> do
  rand   <- newStdGen
  
  let (num,_) = next rand
 --let ww = "ABCDEFGHIJK" !! (num `mod` 11)
  let ww = 'A'
  let village = aWerewolf10Villagers ww
  
  input <- getLine
  let (cont, (o, v'))  = startup village [input]
  putStrLn (concat $ intersperse "\n" o)
  
  if ww == 'A' then 
    playWerewolf rand stdout v' cont
  else
    playVillager rand stdout v' cont

playWerewolf rand out v (E name f) = do 
  eaten <- getVillager
  let (cont, (o, v'))    = f v [[eaten]]
  hPutStrLn out $ concat o
  
  nextStep rand out playWerewolf v' cont
  
playVillager rand out v (E name f) = do 
  let (eaten, rand')     = werewolfEatsAVillager v rand
  
  let (cont, (o, v'))    = f v [[eaten]]
  hPutStrLn out $ concat o
  
  nextStep rand out playVillager v' cont
  
nextStep rand out f v' cont = do
  villager <- getVillager
  let (rand', killed) = villagersVote villager v' rand
  let (cont', (o', v'')) = run cont v' [[killed]]
  continue f v'' o' rand' out  v'' cont'

getVillager = getLine >>= return . head
  
continue f v' o' rand'' out v'' cont' =   
  case endGame v' of
    None       -> hPutStrLn out (concat $ intersperse "\n" o') >> 
                  f rand'' out v'' cont'
    Werewolves -> hPutStrLn out $ "the werewolves win"
    Villagers  -> hPutStrLn out $ "the villagers win"

-- TESTS
