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
