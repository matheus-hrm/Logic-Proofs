module Util where

import Types

displayHelp :: IO ()
displayHelp = do
  putStrLn "\nAvailable commands:"
  putStrLn "  mp  <line1> <line2>    - Apply Modus Ponens"
  putStrLn "  mt  <line1> <line2>    - Apply Modus Tollens"
  putStrLn "  hsg <line1> <line2>    - Apply Hypothetical Syllogism"
  putStrLn "  dne <line>             - Apply Double Negation Elimination"
  putStrLn "  conj <line1> <line2>   - Apply Conjunction"
  putStrLn "  simp <line>            - Apply Simplification"
  putStrLn "  add  <line1> <line2>   - Apply Addition"
  putStrLn "  show                   - Display current proof state"
  putStrLn "  help                   - Display this help message"
  putStrLn "  quit                   - Exit the program\n"

displayProofState :: ProofState -> IO ()
displayProofState state = do
  putStrLn "\nCurrent Proof State:"
  putStrLn "Steps:"
  mapM_ (putStrLn . formatStep) (steps state)
  putStrLn "Goal:"
  print (goal state)
  putStrLn ""

formatStep :: Step -> String
formatStep Step {line = l, proposition = p, rule = r, premises = prems} =
  show l
    ++ ". "
    ++ show p
    ++ " ["
    ++ show r
    ++ (if null prems then "" else ", from lines: " ++ show prems)
    ++ "]"
