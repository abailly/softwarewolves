import System.IO
import GameEngine
import Data.List(intersperse)
import System.Random 


main = hSetBuffering stdout NoBuffering >> do
  rand   <- newStdGen
  let (num,_) = next rand
  let ww = "ABCDEFGHIJK" !! (num `mod` 11)
  if ww == 'A' then 
    playWerewolf stdout (aWerewolf10Villagers ww) (E "startup" (\v -> startup v))
  else
    playVillager rand stdout (aWerewolf10Villagers ww) (E "startup" (\v -> startup v))

playWerewolf out v (E name f) = do 
  input <- hGetLine out 
  let (cont, (o, v')) = f v [input]
  case endGame v' of
    None       -> hPutStrLn out (concat $ intersperse "\n" o) >> 
                  playWerewolf out v' cont
    Werewolves -> hPutStrLn out $ "the werewolves win"
    Villagers  -> hPutStrLn out $ "the villagers win"

playVillager rand out v (E name f) = do 
  input <- getLine
  let (cont, (o, v')) = f v [input]
  let (eaten, rand') = werewolfEatsAVillager v' rand
  let (cont', (o', v'')) = run cont v' [eaten:[]]
  case endGame v' of
    None       -> hPutStrLn out (concat $ intersperse "\n" o) >> 
                  playVillager rand' out v'' cont'
    Werewolves -> hPutStrLn out $ "the werewolves win"
    Villagers  -> hPutStrLn out $ "the villagers win"

-- TESTS
