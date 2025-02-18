module Main where

import Core
import Types
import Util

main :: IO ()
main = do
  let initialPremises =
        [ Step 1 (Or (Atom "p") (And (Atom "q") (Atom "r"))) Assumption [],
          Step 2 (Impl (Atom "p") (Atom "s")) Assumption [],
          Step 3 (Impl (Atom "s") (Atom "r")) Assumption []
        ]
      goalProp = Just (Atom "r")
      initialState = ProofState initialPremises goalProp
  putStrLn "Initial Premises:"
  putStrLn "\nLogic Proof Assistant"
  putStrLn "==================="
  putStrLn "\nInitial State:"
  displayProofState initialState
  putStrLn "Type 'help' for available commands."
  prover initialState
