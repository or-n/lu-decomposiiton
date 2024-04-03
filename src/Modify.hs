module Modify where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST

type ModifyV a = forall s. MV.MVector s a -> ST s ()

modify :: V.Unbox a => ModifyV a -> V.Vector a -> V.Vector a
modify f values = runST $ do
    mut_values <- V.thaw values
    f mut_values
    V.freeze mut_values