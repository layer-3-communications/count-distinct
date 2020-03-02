{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language TypeApplications #-}

-- | Approximate the number of distinct values in a collection.
-- The way to use this is to start with @empty@ as the initial @State@
-- and then fold over a collection, taking a 32-bit hash of each element
-- and calling @insert@ to add it to the @State@. After all elements have
-- been added, @count@ computes the estimated cardinality.
--
-- Internally, this uses a variant of the hyperloglog algorithm. The @State@
-- is only 64 bits, which acts as 12 buckets with 5-bit counters. Since the
-- bucket count is not a power of two, we cannot simply take bits from the
-- beginning of the 32-bit word elements. So, we take them from the end and
-- leave them alone. This makes this variant unsuitable for computing
-- cardinalities above 256M. This implementation switches to linear counting
-- for small sets, which is common practice.
module Count.Distinct
  ( State(..)
  , insert
  , empty
  , count
  , fold
  ) where

import Data.Bits
import Data.Word
import Debug.Trace

import qualified Data.List as List

-- | Hyperloglog state. Although this data constructor is exposed, the only
-- meaningful operations on values of this type are @insert@ and @count@.
newtype State = State Word64

-- | The initial state. There are no unique elements.
empty :: State
empty = State 0

-- | Add an element to the state.
insert :: Word32 -> State -> State
insert w (State v) = State (replace w (mod w (fromIntegral @Int @Word32 buckets)) v)

replace :: Word32 -> Word32 -> Word64 -> Word64
replace h r' s =
  let r = fromIntegral @Word32 @Int r'
      pos = r * 5
      shifted = unsafeShiftR s pos
      cleared = (unsafeShiftL (ones (55 - pos)) (pos + 5) .|. ones pos) .&. s
      mag0 = fromIntegral @Word64 @Int (shifted .&. 0b11111)
      mag1 = countLeadingZeros h
   in if mag0 >= mag1
        then s
        else (cleared .|. unsafeShiftL (fromIntegral @Int @Word64 mag1) pos)

-- Returns the requested number of ones. For example, ones 3 = 0b0111
ones :: Int -> Word64
ones i = unsafeShiftL 1 i - 1

upper :: Word64
upper = 0xFFFFFFFFFFFFFFE0

-- | Compute the estimate count of distinct elements.
count :: State -> Int
count (State w)
  | w == 0 = 0
  | numberOfZeroes == 0 =
      floor ((fromIntegral (buckets * buckets) * alpha) * traceShowId (expHarmonicMean ys))
  | otherwise = floor
      ( fromIntegral @Int @Double buckets
        *
        logBase 2 (fromIntegral buckets / fromIntegral numberOfZeroes)
      )
  where
  ys = stateToList 0 w []
  numberOfZeroes = countZeroes 0 ys

stateToList :: Int -> Word64 -> [Int] -> [Int]
stateToList !ix !w !acc = if ix == buckets
  then acc
  else stateToList (ix + 1) (unsafeShiftR w 5) (fromIntegral @Word64 @Int (w .&. 0b11111) : acc)

expHarmonicMean :: [Int] -> Double
expHarmonicMean xs = 1 / (sum (map (\x -> 2 ** fromIntegral @Int @Double (negate (x + 1))) xs))

countZeroes :: Int -> [Int] -> Int
countZeroes !acc [] = acc
countZeroes !acc (a : as) = case a of
  0 -> countZeroes (acc + 1) as
  _ -> countZeroes acc as

alpha :: Double
alpha = 0.67

buckets :: Int
buckets = 12

-- | Fold some hashable elements into a 'State'.
fold :: Foldable t => (a -> Word32) -> t a -> State
fold f = foldr (insert . f) empty
{-# inlineable fold #-}
