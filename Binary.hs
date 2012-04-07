module Binary where

import Model
import Memory
import Data.Word
import Data.Bits
import Data.Maybe

mask :: Int -> Word16
mask n = complement (complement 0 `shiftL` n)

bits :: Word16 -> Int -> Int -> Word16
bits w i n = (w `shiftR` i) .&. mask n

unbits :: [(Word16, Int)] -> Word16
unbits [] = 0
unbits [(v, _)] = v
unbits ((v, n) : xs) = v .|. (unbits xs `shiftL` n)

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
putInstruction (Instruction c a b) 
  = case c of
      OpBase bc -> unbits [(bc, 4), (x, 6), (y, 6)] : catMaybes [v, w]
      OpExt ec -> unbits [(0, 4), (ec, 6), (x, 6)] : maybeToList v
  where
    (x, v) = putValue a
    (y, w) = putValue b

getRegister :: Word16 -> Register
getRegister = toEnum . fromIntegral

putRegister :: Register -> Word16
putRegister = fromIntegral . fromEnum

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

putValue :: Value -> (Word16, Maybe Word16)
putValue (REG r) = (putRegister r, Nothing)
putValue (PtrREG r) = (putRegister r + 0x8, Nothing)
putValue (PtrREG_NW nw r) = (putRegister r + 0x10, Just nw)
putValue POP = (0x18, Nothing)
putValue PEEK = (0x19, Nothing)
putValue PUSH = (0x1a, Nothing)
putValue SP = (0x1b, Nothing)
putValue PC = (0x1c, Nothing)
putValue O = (0x1d, Nothing)
putValue (PtrNW nw) = (0x1e, Just nw)
putValue (NW nw) = (0x1f, Just nw)
putValue (Lit a) = (a + 0x20, Nothing)
