module Classes where
import Data.Char (ord, chr, isLower, isUpper)

{-
    A class for container types, which are able to enumerate their elements
    using a Haskell list. What is the kind of 'c'?
-}
class Container c where
    contents :: c a -> [a]

{-
    A class for types with invertible values. What is the kind of 'a'?
    The default invert operation is the identity.
-}
class Invertible a where
    invert :: a -> a
    invert = id

{-
    Primitive types are instances of the 'Invert' class.
    According to the default definition of 'invert', nothing is actually
    performed onto the primitive values.
-}
instance Invertible Char where
    invert c
        | isLower c = chr (ord 'z' - ord c + ord 'a')
        | isUpper c = chr (ord 'Z' - ord c + ord 'A')
        | otherwise = c
instance Invertible Bool where
    invert = not
instance Invertible Int where
    invert = ((-1) *)
instance Invertible Integer where
    invert = ((-1) *)
instance Invertible Float where
    invert = ((-1) *)
instance Invertible Double where
    invert = ((-1) *)

