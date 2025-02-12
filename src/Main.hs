module Main where

import Core
import Types
import Util

main :: IO ()
main = do
  let initialPremises =
        [ Step 1 (Not (Not (Atom "p"))) Assumption [],
          Step 2 (Impl (Atom "p") (Atom "q")) Assumption [],
          Step 3 (Impl (Atom "q") (Atom "r")) Assumption []
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
