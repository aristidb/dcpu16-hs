{-# LANGUAGE RankNTypes #-}
module Memory (Memory, Tri(..), newMemory, readWord, readTri, writeWord, writeTri) where

import           Control.Applicative
import           Data.IORef
import           Data.Word
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Control.Monad
import           Control.Monad.ST
import qualified Data.ByteString             as B

newtype Memory = Mem (UM.IOVector Word16)

data Tri = Tri !Word16 !Word16 !Word16
  deriving (Eq, Ord, Show)

newMemory :: IO Memory
newMemory = do v <- UM.replicate (0x10000+3) 0
               return $ Mem v

readWord :: Memory -> Word16 -> IO Word16
readWord (Mem v) i = UM.read v (fromIntegral i)

readTri :: Memory -> Word16 -> IO Tri
readTri (Mem v) i = do let window = UM.slice (fromIntegral i) 3 v
                       [x, y, z] <- mapM (UM.read window) [0, 1, 2]
                       return $ Tri x y z

writeWord :: Memory -> Word16 -> Word16 -> IO ()
writeWord (Mem v) i x = UM.write v (fromIntegral i) x

writeTri :: Memory -> Word16 -> Tri -> IO ()
writeTri (Mem v) i (Tri x y z) = do let window = UM.slice (fromIntegral i) 3 v
                                    zipWithM_ (UM.write window) [0, 1, 2] [x, y, z]

dumpMemory :: Memory -> IO (U.Vector Word16)
dumpMemory (Mem v) = U.freeze v