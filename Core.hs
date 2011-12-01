import System.IO
import GameEngine
import Data.List(intersperse)

main = hSetBuffering stdout NoBuffering >> 
       playgame oneWerewolf10Villagers (E "startup" (\v -> startup))

playgame v (E name f) = do input <- getLine 
                           let (cont, (o, v')) = f v [input]
                           case endGame v' of
                             None -> putStrLn ((concat $ intersperse "\n" o)) >> playgame v' cont
                             Werewolves  -> putStrLn $ "the werewolves win"
                             Villagers  -> putStrLn $ "the villagers win"
