

module Greater where

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data Greater cell a = Greater {-# UNPACK #-} !(cell a, cell a)
  deriving stock (Ord, Eq, Functor, Generic, Show, Read)

greaterCell :: Iso' (cell a, cell a) (Greater cell a)
greaterCell = iso (\(i, o) -> Greater (i, o)) (\(Greater (i, o)) -> (i, o))

data InsideOutside = Inside | Outside
  deriving stock (Show, Read, Eq, Ord)

inside :: Lens' (Greater cell a) (cell a)
inside = lens (\(Greater (i, _)) -> i) (\(Greater (_, o)) i -> Greater (i, o))

outside :: Lens' (Greater cell a) (cell a)
outside = lens (\(Greater (_, o)) -> o) (\(Greater (i, _)) o -> Greater (i, o))

greaterToSubcell :: DrawableCell cell dir rdir => InsideOutside -> dir -> Lens' (Greater cell a) a
greaterToSubcell Inside vn = inside . subcell vn
greaterToSubcell Outside vn = outside . subcell vn