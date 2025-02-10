import Control.Applicative (Alternative ((<|>)))
import Control.Monad.State (StateT)
import Data.Maybe (listToMaybe)
import GHC.Base (VecElem (Int16ElemRep))
import GHC.IO.Handle (hFlush)
import System.IO

data Prop
  = Atom String
  | Not Prop -- ¬
  | And Prop Prop -- ∧
  | Or Prop Prop -- ∨
  | Impl Prop Prop -- ->
  | Yields Prop Prop -- ⊢
  | Bicdt Prop Prop -- ↔
  deriving (Eq, Show)

data Step = Step
  { line :: Int,
    proposition :: Prop,
    rule :: Rule,
    premises :: [Int]
  }
  deriving (Show)

data Rule
  = Assumption
  | ModusPonens
  | ModusTollens
  | NegationElimination
  | HypoteticalSyllogism
  | DoubleNegationElimination
  | ImplicationIntroduction
  deriving (Eq, Show)

data ProofState = ProofState
  { steps :: [Step],
    goal :: Maybe Prop
  }
  deriving (Show)

type Proof = [Step]

type ProofSession = StateT ProofState IO

-- Conditionals

modusPonens :: Prop -> Prop -> Maybe Prop
modusPonens (Impl p q) p' | p == p' = Just q
modusPonens _ _ = Nothing

modusTollens :: Prop -> Prop -> Maybe Prop
modusTollens (Impl p q) (Not q') | q == q' = Just (Not p)
modusTollens _ _ = Nothing

deduction :: Prop -> Maybe Prop
deduction (Yields p q) = Just (Impl p q)
deduction _ = Nothing

hypSyllogism :: Prop -> Prop -> Maybe Prop
hypSyllogism (Impl p q) (Impl q' r) | q == q' = Just (Impl p r)
hypSyllogism _ _ = Nothing

absortion :: Prop -> Maybe Prop
absortion (Impl p q) = Just (Impl p (And p q))

-- Disjunctions

disjSyllogism :: Prop -> Prop -> Maybe Prop
disjSyllogism (Or p q) (Not p') | p == p' = Just q
disjSyllogism _ _ = Nothing

elimDisjunction :: Prop -> Prop -> Prop -> Maybe Prop
elimDisjunction (Or p q) (Impl p' r) (Impl q' r')
  | q == q' && p == p' && r == r' = Just r
elimDisjunction _ _ _ = Nothing

consDillema :: Prop -> Prop -> Prop -> Maybe Prop
consDillema (Impl p r) (Impl q s) (Or p' q')
  | p == p' && q == q' = Just (Or r s)
consDillema _ _ _ = Nothing

addition :: Prop -> Prop -> Maybe Prop
addition p q = Just (Or p q)

-- Conjunctions

conjunction :: Prop -> Prop -> Maybe Prop
conjunction (Atom p) (Atom q) = Just (And (Atom p) (Atom q))
conjunction _ _ = Nothing

elimConjunction :: Prop -> Maybe (Prop, Prop)
elimConjunction (And p q) = Just (q, p)
elimConjunction _ = Nothing

simplification :: Prop -> Maybe Prop
simplification (And p q) = Just p
simplification _ = Nothing

doubleNegation :: Prop -> Maybe Prop
doubleNegation (Not (Not p)) = Just p
doubleNegation p = Just (Not (Not p))

-- Biconditionals

introBiconditional :: Prop -> Prop -> Maybe Prop
introBiconditional (Impl p q) (Impl q' p')
  | p == p' && q == q' =
      Just (Bicdt p q)
introBiconditional _ _ = Nothing

elimBiconditional :: Prop -> Prop -> Maybe Prop
elimBiconditional (Bicdt p q) p' | p == p' = Just q
elimBiconditional (Bicdt p q) q' | q == q' = Just p
elimBiconditional (Bicdt p q) (Not p') | p == p' = Just (Not q)
elimBiconditional (Bicdt p q) (Not q') | q == q' = Just (Not p)
elimBiconditional (Bicdt p q) (Or p' q')
  | p == p' && q == q' = Just (And p q)
elimBiconditional (Bicdt p q) (Or (Not p') (Not q'))
  | p == p' && q == q' = Just (And (Not p) (Not q))

applyRule :: [(Rule, [Prop] -> Maybe Prop)]
applyRule =
  [ (ModusPonens, \[p1, p2] -> modusPonens p1 p2 <|> modusPonens p2 p1),
    (ModusTollens, \[p1, p2] -> modusTollens p1 p2 <|> modusTollens p2 p1),
    (HypoteticalSyllogism, \[p1, p2] -> hypSyllogism p1 p2 <|> hypSyllogism p2 p1),
    (DoubleNegationElimination, \[p] -> doubleNegation p)
  ]

applyStep :: Rule -> [Step] -> Proof -> Maybe Step
applyStep rule premises proof =
  case lookup rule applyRule of
    Just validator -> do
      let props = map proposition premises
      result <- validator props
      return $ Step (length proof + 1) result rule (map line premises)
    Nothing -> Nothing

prover :: ProofState -> IO ()
prover state = do
  input <- words <$> getLine
  case input of
    ["mp", l1, l2] -> do
      let n1 = read l1 :: Int
          n2 = read l2 :: Int
          premises = filter (\s -> line s `elem` [n1, n2]) (steps state)
      if length premises /= 2
        then do
          putStrLn "Invalid Modus Ponens"
          prover state
        else case applyStep ModusPonens premises (steps state) of
          Just step -> do
            putStrLn "New Step:"
            print step
            prover $ ProofState (steps state ++ [step]) (goal state)
          Nothing -> do
            putStrLn "Invalid Modus Ponens"
            prover state
    ["hsg", l1, l2] -> do
      let n1 = read l1 :: Int
          n2 = read l2 :: Int
          premises = filter (\s -> line s `elem` [n1, n2]) (steps state)
      if length premises /= 2
        then do
          putStrLn "Invalid Hypotetical Syllogism"
          prover state
        else case applyStep HypoteticalSyllogism premises (steps state) of
          Just step -> do
            putStrLn "New Step:"
            print step
            prover $ ProofState (steps state ++ [step]) (goal state)
          Nothing -> do
            putStrLn "Invalid Hypotetical Syllogism"
            prover state
    ["dne", l] -> do
      let n = read l :: Int
          premises = filter (\s -> line s == n) (steps state)
      if null premises
        then do
          putStrLn "Invalid line number"
          prover state
        else case applyStep DoubleNegationElimination premises (steps state) of
          Just step -> do
            putStrLn "New Step:"
            print step
            let newState = state {steps = steps state ++ [step]}
            if Just (proposition step) == goal state
              then putStrLn "Proof Complete"
              else prover newState
          Nothing -> do
            putStrLn "Invalid Double Negation Elimination"
            prover state
    ["quit"] -> return ()
    _ -> do
      putStrLn "Invalid Command"
      prover state

main :: IO ()
main = do
  let premise1 = Step 1 (Not (Not (Atom "p"))) Assumption []
      premise2 = Step 2 (Impl (Atom "p") (Atom "q")) Assumption []
      premise3 = Step 3 (Impl (Atom "q") (Atom "r")) Assumption []
      intialState = ProofState [premise1, premise2, premise3] (Just (Atom "r"))
  putStrLn "Initial Premises:"
  mapM_ print (steps intialState)
  putStrLn "Goal:"
  print (goal intialState)
  prover intialState
