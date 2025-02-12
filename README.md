# Logic Proof Checker

A Haskell program for checking logical proofs using natural deduction rules.

## Prerequisites

- GHC (Glasgow Haskell Compiler)
- Cabal

## Installation

1. Clone the repository
2. Run `cabal build`

## Usage

Run the program:
```bash
cabal run
```

# Project structure
```
src/
├── Rules/
│   ├── Conditionals.hs
│   ├── Conjunctions.hs
│   ├── Disjunctions.hs
│   └── Biconditionals.hs
├── Types.hs
├── ProofEngine.hs
├── UI.hs
└── Main.hs
```
