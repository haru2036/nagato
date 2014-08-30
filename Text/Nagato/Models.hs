module Text.Nagato.Models
( Freqs
  ,Probs
)where
import Data.Map

type Freqs a = Map a Int
type Probs a = Map a Float
