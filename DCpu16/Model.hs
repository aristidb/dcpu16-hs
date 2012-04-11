module DCpu16.Model where

import Data.Word
import Data.List

data OpCode = OpBase !Word16 | OpExt !Word16
  deriving (Eq, Show)

data Cardinality = Nullary | Unary | Binary
  deriving (Eq, Show)

data OpInfo = 
  OpInfo {
    name :: !String
  , cardinality :: !Cardinality
  , cycles :: !Int
  , failCycles :: !Int
  , ocode :: !OpCode
  }
  deriving (Eq, Show)

basicOps :: [OpInfo]
basicOps = zipWith ($) ops (map OpBase [0x1 .. 0xf])
  where ops = [ OpInfo "SET" Binary 1 0
              , OpInfo "ADD" Binary 2 0
              , OpInfo "SUB" Binary 2 0
              , OpInfo "MUL" Binary 2 0
              , OpInfo "DIV" Binary 3 0
              , OpInfo "MOD" Binary 3 0
              , OpInfo "SHL" Binary 2 0
              , OpInfo "SHR" Binary 2 0
              , OpInfo "AND" Binary 1 0
              , OpInfo "BOR" Binary 1 0
              , OpInfo "XOR" Binary 1 0
              , OpInfo "IFE" Binary 2 1
              , OpInfo "IFN" Binary 2 1
              , OpInfo "IFG" Binary 2 1
              , OpInfo "IFB" Binary 2 1
              ]

extendedOps :: [OpInfo]
extendedOps = [OpInfo "JSR" Unary 2 0 (OpExt 0x1)]

allOps :: [OpInfo]
allOps = basicOps ++ extendedOps

getOpByName :: String -> Maybe OpInfo
getOpByName s = find (\OpInfo{name=s'} -> s==s') allOps

getOpByCode :: OpCode -> Maybe OpInfo
getOpByCode cd = find (\OpInfo{ocode=cd'} -> cd==cd') allOps

data Register = A | B | C | X | Y | Z | I | J
  deriving (Eq, Ord, Enum, Bounded, Show)

data Value = 
    REG !Register
  | PtrREG !Register
  | PtrREG_NW !Word16 !Register
  | POP
  | PEEK
  | PUSH
  | SP
  | PC
  | O
  | PtrNW !Word16
  | NW !Word16
  | Lit !Word16
  deriving (Eq, Show)

numLit :: Word16 -> Value
numLit w | w < 0x20  = Lit w
         | otherwise = NW w

valueSize :: Value -> Word16
valueSize (PtrREG_NW {}) = 1
valueSize (PtrNW {}) = 1
valueSize (NW {}) = 1
valueSize _ = 0

data Instruction =
  Instruction {
    iCode :: OpCode
  , iA :: Value
  , iB :: Value
  }
  deriving (Eq, Show)
