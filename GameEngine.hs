module GameEngine where
import Test.HUnit
import IO(stderr)
import System.Exit
import System.IO
import Data.List

-- CODE 
eat villager (alive, dead) = (delete villager alive, villager : dead)
hang = eat

-- TESTS

gameEngineTests = TestList [
  "when a player is designated for eating, it is removed from the villagers and added to dead" ~:
  TestList [
    eat 'B' ("BCD","") ~?= ("CD","B"),
    eat 'C' ("BCD","E") ~?= ("BD","CE")  
    ],
  "when a player is designated for hanging, it is removed from the villagers and added to dead" ~:
  TestList [
    hang 'C' ("BCD","") ~?= ("BD","C"),
    hang 'D' ("BCD","F") ~?= ("BC","DF")  
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



