module NestedList where

import Classes ( Invertible(..), Container(..) )
import List ()
import Data.List (intercalate)

{-
    Define the 'NestedList' type, for handling lists with an arbitrary
    level of intricateness, which would be impossible to specify
    using plain Haskell lists. For instance, the String representation
    of such a nested list might be "[1, [2, 3], 4, 5]".

    Instantiate the following classes with the nested list type:
    * 'Show'
    * 'Functor'
    * 'Container'
    * 'Invertible'

    The inversion should be performed DEEPLY i.e., for the elements as well.
-}
data NestedList a = NestedList [NestedList a] | Elem a

instance Show a => Show (NestedList a) where
    show (Elem x) = show x
    show (NestedList xs) = "[" ++ intercalate "," (map show xs)  ++ "]"

instance Functor NestedList where
    fmap f (Elem x) = Elem $ f x
    fmap f (NestedList xs) = NestedList $ map (fmap f) xs

instance Container NestedList where
    contents (Elem x) = [x]
    contents (NestedList xs) = concatMap contents xs

instance Invertible a => Invertible (NestedList a) where
    invert = fmap invert
