-- Jakub Staroń, 2017

module Memory where

import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import Data.Int

type IndexType = Int32
type ValueType = Int32

newtype Memory s = Memory (STUArray s IndexType ValueType)

newMemory :: IndexType -> ST s (Memory s)
newMemory size = do
  array <- newArray_ (0, size)
  return $ Memory array

readMemory :: Memory s -> IndexType -> ST s ValueType
readMemory (Memory array) = readArray array

writeMemory :: Memory s -> IndexType -> ValueType -> ST s ()
writeMemory (Memory array) = writeArray array

getSize :: Memory s -> ST s IndexType
getSize (Memory array) = liftM snd $ getBounds array

main :: IO ()
main =
  let state = do {
    memory <- newMemory (1024 * 1024 * 128);
    writeMemory memory 1 1;
    writeMemory memory 1 2;
    readMemory memory 1; } in
  let value = runST state in
  print value
