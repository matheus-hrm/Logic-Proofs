module Types where

import Control.Monad.State (StateT (StateT))

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
  | DoubleNegationElimination
  | NegationElimination
  | HypoteticalSyllogism
  | DisjunctionSyllogism
  | DisjunctionElimination
  | ConstructiveDillema
  | Addition
  | Absortion
  | Conjunction
  | ConjunctionElimination
  | Simplification
  | BiconditionalIntroduction
  | BiconditionalElimination
  | Deduction
  | ImplicationIntroduction
  deriving (Eq, Show)

data ProofState = ProofState
  { steps :: [Step],
    goal :: Maybe Prop
  }
  deriving (Show)

data RuleConfig = RuleConfig
  { requiredPremises :: Int,
    validator :: RuleValidator
  }

type Proof = [Step]

type ProofSession = StateT ProofState IO

type RuleValidator = [Prop] -> Maybe Prop
