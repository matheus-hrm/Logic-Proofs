module Core where

import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Rules
import Types
import Util

ruleConfigs :: [(Rule, RuleConfig)]
ruleConfigs =
  [ (ModusPonens, RuleConfig 2 $ \[p1, p2] -> modusPonens p1 p2),
    (ModusTollens, RuleConfig 2 $ \[p1, p2] -> modusTollens p1 p2),
    (HypoteticalSyllogism, RuleConfig 2 $ \[p1, p2] -> hypSyllogism p1 p2),
    (DisjunctionSyllogism, RuleConfig 2 $ \[p1, p2] -> disjSyllogism p1 p2),
    (DisjunctionElimination, RuleConfig 3 $ \[p1, p2, p3] -> elimDisjunction p1 p2 p3),
    (Conjunction, RuleConfig 2 $ \[p1, p2] -> conjunction p1 p2),
    ( ConjunctionElimination,
      RuleConfig 1 $ \[p1] -> do
        (p2, p3) <- elimConjunction p1
        return (And p2 p3)
    ),
    (ConstructiveDillema, RuleConfig 3 $ \[p1, p2, p3] -> consDillema p1 p2 p3),
    (Simplification, RuleConfig 1 $ \[p1] -> simplification p1),
    (Addition, RuleConfig 2 $ \[p1, p2] -> addition p1 p2),
    (Absortion, RuleConfig 1 $ \[p1] -> absortion p1),
    (BiconditionalElimination, RuleConfig 2 $ \[p1, p2] -> elimBiconditional p1 p2),
    (BiconditionalIntroduction, RuleConfig 2 $ \[p1, p2] -> introBiconditional p1 p2),
    (DoubleNegationElimination, RuleConfig 1 $ \[p] -> doubleNegation p)
  ]

createStep :: Int -> Prop -> Rule -> [Int] -> Step
createStep lineNum prop rule premiseLines =
  Step
    { line = lineNum,
      proposition = prop,
      rule = rule,
      premises = premiseLines
    }

applyStep :: Rule -> [Step] -> Proof -> Maybe Step
applyStep rule premises proof = do
  config <- lookup rule ruleConfigs
  let requiredCount = requiredPremises config
  if length premises /= requiredCount
    then Nothing
    else do
      let props = map proposition premises
      result <- validator config props
      return $ createStep (length proof + 1) result rule (map line premises)

validatePremises :: Rule -> [Int] -> [Step] -> Maybe [Step]
validatePremises rule lineNums steps = do
  config <- lookup rule ruleConfigs
  let premises = filter (\s -> line s `elem` lineNums) steps
  if length premises == requiredPremises config
    then Just premises
    else Nothing

proverCommand :: String -> [String] -> ProofState -> IO ()
proverCommand cmd args state = case (cmd, args) of
  ("mp", [l1, l2]) -> applyRuleCommand ModusPonens [read l1, read l2] state
  ("mt", [l1, l2]) -> applyRuleCommand ModusTollens [read l1, read l2] state
  ("hsg", [l1, l2]) -> applyRuleCommand HypoteticalSyllogism [read l1, read l2] state
  ("dne", [l]) -> applyRuleCommand DoubleNegationElimination [read l] state
  ("conj", [l1, l2]) ->
    applyRuleCommand Conjunction [read l1, read l2] state
  ("simp", [l]) ->
    applyRuleCommand Simplification [read l] state
  ("add", [l1, l2]) ->
    applyRuleCommand Addition [read l1, read l2] state
  ("ds", [l1, l2]) ->
    applyRuleCommand DisjunctionSyllogism [read l1, read l2] state
  _ -> do
    putStrLn $ "Invalid command: " ++ cmd ++ ". Type 'help' for available commands."
    prover state

applyRuleCommand :: Rule -> [Int] -> ProofState -> IO ()
applyRuleCommand rule lineNums state = case validatePremises rule lineNums (steps state) of
  Nothing -> do
    putStrLn $ "Invalid " ++ show rule
    prover state
  Just premises -> case applyStep rule premises (steps state) of
    Just step -> do
      putStrLn "New Step:"
      print step
      let newState = state {steps = steps state ++ [step]}
      if Just (proposition step) == goal state
        then putStrLn "Proof Complete"
        else prover newState
    Nothing -> do
      putStrLn $ "Invalid " ++ show rule
      prover state

prover :: ProofState -> IO ()
prover state = do
  putStr "> "
  hFlush stdout
  input <- words <$> getLine
  case input of
    [] -> do
      putStrLn "Empty Command"
      prover state
    (cmd : args) -> case cmd of
      "quit" -> return ()
      "help" -> do
        displayHelp
        prover state
      "show" -> do
        displayProofState state
        prover state
      _ -> do
        proverCommand cmd args state
