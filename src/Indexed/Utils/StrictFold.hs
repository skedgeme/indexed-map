{-# LANGUAGE Safe #-}

module Indexed.Utils.StrictFold (foldlStrict) where

-- | Same as regular 'Data.List.foldl'', but marked INLINE so that it is always
-- inlined. This allows further optimization of the call to f, which can be
-- optimized/specialised/inlined.

foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f = go
  where
    go z []     = z
    go z (x:xs) = let z' = f z x in z' `seq` go z' xs
{-# INLINE foldlStrict #-}
