module Rules where

import Types

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

destDillema :: Prop -> Prop -> Prop -> Maybe Prop
destDillema (Impl p q) (Impl r s) (Or (Not q') (Not s'))
  | q == q' && s == s' = Just (Or (Not p) (Not r))
destDillema _ _ _ = Nothing

addition :: Prop -> Prop -> Maybe Prop
addition p q = Just (Or p q)

-- Conjunctions

conjunction :: Prop -> Prop -> Maybe Prop
conjunction (Atom p) (Atom q) = Just (And (Atom p) (Atom q))
conjunction _ _ = Nothing

elimConjunction :: Prop -> Maybe (Prop, Prop)
elimConjunction (And p q) = Just (q, p)
elimConjunction _ = Nothing

simplification :: Prop -> Int -> Maybe Prop
simplification (And p q) n = case n of
  1 -> Just p
  2 -> Just q
  _ -> Nothing
simplification (Or p q) n = case n of
  1 -> Just p
  2 -> Just q
  _ -> Nothing
simplification _ _ = Nothing

--TODO: Change Double Negation to be able to aply on single element of the proposition

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
elimBiconditional _ _ = Nothing

-- Equivalence rules

deMorgan :: Prop -> Maybe Prop
deMorgan (Not (And p q)) = Just (Or (Not p) (Not q))
deMorgan (Not (Or p q)) = Just (And (Not p) (Not q))
deMorgan _ = Nothing

condEq :: Prop -> Maybe Prop
condEq (Impl p q) = Just (Or (Not p) q)
condEq _ = Nothing

contraPositive :: Prop -> Maybe Prop
contraPositive (Impl (Not p) (Not q)) = Just (Impl q p)
contraPositive (Impl p q) = Just (Impl (Not q) (Not p))
contraPositive _ = Nothing

idempot :: Prop -> Maybe Prop
idempot (And p p') | p == p' = Just p
idempot p = Just (Or p p)

comut :: Prop -> Maybe Prop
comut (Or p q) = Just (Or q p)
comut (And p q) = Just (And q p)
comut _ = Nothing

exportImport :: Prop -> Maybe Prop
exportImport (Impl (And p q) r) = Just (Impl p (Impl q r))
exportImport (Impl p (Impl q r)) = Just (Impl (And p q) r)
exportImport _ = Nothing

association :: Prop -> Maybe Prop
association (And p (And q r)) = Just (And (And p q) r)
association (Or p (Or q r)) = Just (Or (Or p q) r)

distribuition :: Prop -> Maybe Prop
distribuition (And p (Or q r)) = Just (Or (And p q) (And p r))
distribuition (Or p (And q r)) = Just (And (Or p q) (Or p r))
distribuition _ = Nothing
