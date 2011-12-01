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

data Engine = E { name :: String,
                  run :: Village -> [String] -> (Engine, ([String], Village)) }

-- CODE 
eat villager (Village alive dead ww) = Village (delete villager alive) (villager : dead) ww

hang = eat

endGame (Village a d w) | length (a `intersect` w) >=  
                          length (a \\ w) 
                                           = Werewolves
                        | all (`elem` d) w = Villagers
                        | otherwise        = None

oneWerewolf10Villagers = (Village "ABCDEFGHIJK" "" "A")

numberOfVillagers num = oneWerewolf10Villagers

startup :: [String] -> (Engine, ([String], Village))
startup input = (nightTurn,(["A","A"], oneWerewolf10Villagers))

nightTurn = E "night" night
dayTurn   = E "day" day

night :: Village -> [String] -> (Engine, ([String], Village))
night v [killed] = (dayTurn, ([killed], eat (head killed) v))

day :: Village -> [String] -> (Engine, ([String], Village))
day v [killed] = (nightTurn, ([killed], eat (head killed) v))

engine :: ([String] -> (Engine, ([String], Village))) -> [String] -> [String]
engine e []       = []
engine e (s:rest) = let (cont, (o, v)) = e [s]
                    in o ++ engine (run cont v) rest  

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
  
  "game does not ends when nobody is dead" ~: 
  endGame (Village "BCD" "" "A") ~?= None,
  
  "game ends with werewolves victory when number of alive villagers left is equal to number of alive werewolves" ~: 
  endGame (Village "AE" "BCD" "A") ~?= Werewolves,
  
  "game does not end when enough villagers are still alive" ~: 
  endGame (Village "AFE" "BCD" "A") ~?= None,

  "game does not end when some werewolves are still alive" ~: 
  endGame (Village "BEF" "CD" "AB") ~?= None,

  "reading number of villagers always creates the same village" ~: TestList [
    numberOfVillagers "5" ~?= oneWerewolf10Villagers,
    numberOfVillagers "12" ~?= oneWerewolf10Villagers
    ],
  
  "startup first reads number of villagers then output werewolves" ~: 
  (fst. snd. startup) ["5"] ~?= ["A","A"],
  
  "playing night turn reads villager killed and output villager killed and updated Village" ~:
  (snd.night oneWerewolf10Villagers) ["B"] ~?= (["B"],eat 'B' oneWerewolf10Villagers),
  
  "playing day turn reads villager hanged and output villager killed and updated Village" ~:
  (snd.day oneWerewolf10Villagers) ["B"] ~?= (["B"],hang 'B' oneWerewolf10Villagers),
  
  "engine takes alternating night and day inputs when user plays werewolves" ~: TestList [
    engine startup (map (:[]) "5BCDEFGHIJK") ~?= map (:[]) "AABCDEFGHIJK",
    engine startup (map (:[]) "5DEFGHIJKBC") ~?= map (:[]) "AADEFGHIJKBC"
  ]
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



