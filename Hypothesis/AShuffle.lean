import Mathlib.Data.List.Basic
import Mathlib.Data.Finset.Basic
import Mathlib.Tactic.LibrarySearch

open List

/-- An a-shuffle (generalised riffle shuffle with a piles).
    If print = true it prints the cut positions and the resulting piles.
    Returns the shuffled deck. -/
def AShuffle {α : Type*} (a : Nat) (deck : List α) (print : Bool := false) : List α :=
  if h : a = 0 then [] else
  have n := deck.length
  -- 1. Choose a-1 random cut positions in 0 .. n (inclusive)
  let cuts : List Nat :=
    (List.range (a-1)).map (fun _ => System.Random.genBounded (a := 0) (b := n))
  let cuts := cuts.insertionSort (· ≤ ·)
  if print then
    println! "cutpositions = {cuts}"

  -- 2. Split the deck into a piles according to the cuts
  --    pile i gets cards from after cut i-1 up to (and including) cut i
  --    (with cut₀ = 0 and cut_a = n)
  let allCuts := 0 :: cuts ++ [n]
  let piles : List (List α) :=
    (List.range a).map fun i =>
      let start := allCuts.get! i
      let stop  := allCuts.get! (i+1)
      deck.drop start |>.take (stop - start)

  if print then
    println! "piles = {piles}"

  -- 3. Create a list that tells, for every remaining card, which pile it belongs to
  let mut pileIndex : List Nat := []
  for i in [:a] do
    pileIndex := pileIndex ++ (List.replicate piles.get! i |>.length i)

  -- 4. Deal the cards one by one, randomly choosing a non-empty pile each time
  let mut currentPiles := piles
  let mut result : List α := []
  for _ in [:n] do
    let nonEmpty := (List.range a).filter fun i => (currentPiles.get! i).length > 0
    let chosenPile := nonEmpty.get! (System.Random.genBounded 0 (nonEmpty.length - 1))
    let card := (currentPiles.get! chosenPile).head!
    result := card :: result
    currentPiles := currentPiles.set! chosenPile (currentPiles.get! chosenPile).tail!

  result.reverse

-- Example usage (exactly the one from the original notebook)
#eval do
  let deck := List.range 10 |>.map (· + 1)   -- [1,2,…,10]
  let shuffled := AShuffle 4 deck true
  IO.println s!"shuffled deck = {shuffled}"
