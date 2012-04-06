module Model where

import Data.Word

data OpCode = OpBase !Word16 | OpExt !Word16
  deriving (Eq, Show)

data OpInfo = 
  OpInfo {
    name :: !String
  , cycles :: !Int
  , failCycles :: !Int
  , ocode :: !OpCode
  }
  deriving (Eq, Show)

basicOps :: [OpInfo]
basicOps = zipWith ($) ops (map OpBase [0x1 .. 0xf])
  where ops = [ OpInfo "SET" 1 0
              , OpInfo "ADD" 2 0
              , OpInfo "SUB" 2 0
              , OpInfo "MUL" 2 0
              , OpInfo "DIV" 3 0
              , OpInfo "MOD" 3 0
              , OpInfo "SHL" 2 0
              , OpInfo "SHR" 2 0
              , OpInfo "AND" 1 0
              , OpInfo "BOR" 1 0
              , OpInfo "XOR" 1 0
              , OpInfo "IFE" 2 1
              , OpInfo "IFN" 2 1
              , OpInfo "IFG" 2 1
              , OpInfo "IFB" 2 1
              ]

extendedOps :: [OpInfo]
extendedOps = [OpInfo "JSR" 2 0 (OpExt 0x1)]

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

data Instruction =
  Instruction {
    icode :: OpCode
  , iA :: Value
  , iB :: Value
  }
  deriving (Eq, Show)
