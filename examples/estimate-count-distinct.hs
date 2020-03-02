{-# language BangPatterns #-}

import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Bits

import qualified Count.Distinct as Count
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified System.IO as IO

main :: IO ()
main = do
  bytes <- ByteString.hGetContents IO.stdin
  let ws = makeGroups bytes
      !r = Count.count (foldr Count.insert Count.empty ws)
  putStrLn ("Estimated unique numbers: " ++ show r)
  putStrLn ("Actual unique numbers: " ++ show (length (List.group (List.sort ws))))

makeGroups :: ByteString -> [Word32]
makeGroups b = case ByteString.length w of
  4 -> head32 w : makeGroups (ByteString.drop 4 b)
  0 -> []
  _ -> error "estimate-count-distinct: stdin had wrong length"
  where
  w = ByteString.take 4 b

head32 :: ByteString -> Word32
head32 b =
      unsafeShiftL (fromIntegral (ByteString.index b 0)) 24
  .|. unsafeShiftL (fromIntegral (ByteString.index b 1)) 16
  .|. unsafeShiftL (fromIntegral (ByteString.index b 2)) 8
  .|. unsafeShiftL (fromIntegral (ByteString.index b 3)) 0
