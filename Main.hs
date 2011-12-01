import System.IO
import GameEngine
import Data.List(intersperse)

main = playgame startup 

playgame e = do input <- getLine 
                let (E cont, (o, v)) = e [input]
                case endGame v of
                  None -> putStrLn ((concat $ intersperse "\n" o) ++ " -> " ++ show v) >> playgame (cont v)
                  win  -> putStrLn $ show win
