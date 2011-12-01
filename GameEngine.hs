module GameEngine where
import Test.HUnit
import IO(stderr)
import System.Exit
import System.IO
import Data.List

data Village = Village {
  alive :: String,
  dead  :: String,
  werewolves :: String
  } deriving (Eq,Show,Read)
             
data GameStatus = Villagers |
                  Werewolves |
                  None 
                  deriving (Eq, Show, Read)

newtype Engine = E { run :: [String] -> (Engine, [String]) }

-- CODE 
eat villager (Village alive dead ww) = Village (delete villager alive) (villager : dead) ww

hang = eat

endGame (Village a d w) | all (`elem` w) a = Werewolves
                        | all (`elem` d) w = Villagers
                        | otherwise        = None

oneWerewolf10Villagers = (Village "ABCDEFGHIJK" "" "A")

numberOfVillagers num = oneWerewolf10Villagers

engine :: [String] -> (Engine, [String])
engine input = (E engine,["A","A"])

-- TESTS

gameEngineTests = TestList [
  "when a player is designated for eating, it is removed from the villagers and added to dead" ~:
  TestList [
    eat 'B' (Village "BCD" "" "A") ~?= (Village "CD" "B" "A"),
    eat 'C' (Village "BCD" "E" "A") ~?= (Village "BD" "CE" "A")  
    ],
  
  "when a player is designated for hanging, it is removed from the villagers and added to dead" ~:
  TestList [
    hang 'C' (Village "BCD" "" "A") ~?= (Village "BD" "C" "A"),
    hang 'D' (Village "BCD" "F" "A") ~?= (Village "BC" "DF" "A")  
    ],
  
  "game ends with villagers victory iff all werewolves are dead" ~: 
  endGame (Village "BCD" "A" "A") ~?= Villagers,
  
  "game ends with werewolves victory when all alive villagers left are werewolves" ~: 
  endGame (Village "A" "BCD" "A") ~?= Werewolves,

  "game does not end when some villagers are still alive" ~: 
  endGame (Village "AE" "BCD" "A") ~?= None,

  "game does not end when some werewolves are still alive" ~: 
  endGame (Village "AB" "CD" "B") ~?= None,

  "reading number of villagers always creates the same village" ~: TestList [
    numberOfVillagers "5" ~?= oneWerewolf10Villagers,
    numberOfVillagers "12" ~?= oneWerewolf10Villagers
    ],
  
  "engine first reads number of villagers then output werewolves" ~: 
  snd (engine ["5"]) ~?= ["A","A"]
  ]
                  
newtype Tests = T {unT :: Test}

data TestCount = TestCount Int Test

tests = T $ test [
  gameEngineTests
                 ]
        
runAllTests tests = do putStrLn "Running test suite: "
                       putStrLn (show tests)
                       counts <- runTest (unT tests)
                       case (errors counts + failures counts) of
                         0 -> return ExitSuccess
                         n -> return (ExitFailure n)

instance Show Tests where
  show t = show' "" t
  
show'  indent (T (TestCase _))    = ""
show'  indent (T (TestList ts))   = concat $ (map (show' (' ':indent) . T) ts)
show'  indent (T (TestLabel l t)) = indent ++ l ++ ":\n" ++ (show' indent (T t))

runTest :: Test -> IO Counts
runTest  t = do (counts, _) <- runTestText (putTextToHandle stderr False) t
		return counts



