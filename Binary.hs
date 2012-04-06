module Binary where

import Model
import Memory
import Data.Word
import Data.Bits
--import Data.Binary.Strict.BitGet

--getOpCode :: BitGet OpCode
--getOpCode = unde

mask :: Int -> Word16
mask n = complement (complement 0 `shiftL` n)

bits :: Word16 -> Int -> Int -> Word16
bits w i n = (w `shiftR` i) .&. mask n

getInstruction :: Tri -> (Instruction, Word16)
getInstruction (Tri op x y) | o == 0    = let (v, n) = getValue b x
                                          in (Instruction (OpExt a) v (REG A), 
                                              if n then 1 else 0)
                            | otherwise = let (v, n1, x2) = case getValue b x of
                                                (t, False) -> (t, 0, x)
                                                (t, True) -> (t, 1, y)
                                              (w, n2) = case getValue b x2 of
                                                (t, False) -> (t, 0)
                                                (t, True) -> (t, 1)
                                          in (Instruction (OpBase o) v w, n1 + n2)
  where o = bits op 0 4
        a = bits op 4 6
        b = bits op 10 6

putInstruction :: Instruction -> [Word16]
putInstruction = undefined

getRegister :: Word16 -> Register
getRegister = toEnum . fromIntegral

getValue :: Word16 -> Word16 -> (Value, Bool)
getValue a x = case a of
                 _ | a <= 0x07 -> (REG (getRegister a), False)
                 _ | a <= 0x0f -> (PtrREG (getRegister (a - 0x8)), False)
                 _ | a <= 0x17 -> (PtrREG_NW x (getRegister (a - 0x10)), True)
                 0x18 -> (POP, False)
                 0x19 -> (PEEK, False)
                 0x1a -> (PUSH, False)
                 0x1b -> (SP, False)
                 0x1c -> (PC, False)
                 0x1d -> (O, False)
                 0x1e -> (PtrNW x, True)
                 0x1f -> (NW x, True)
                 _ -> (Lit (a - 0x20), False)