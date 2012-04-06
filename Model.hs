module Model where

import Data.Word
import Control.Applicative

data OpCode = OpCode { baseCode :: !Word, extCode :: !Word }
  deriving (Eq, Show)

basicCode :: Word -> OpCode
basicCode c = OpCode c 0

data OpInfo = 
  OpInfo {
    name :: !String
  , cycles :: !Word
  , failCycles :: !Word
  , ocode :: !OpCode
  }
  deriving (Eq, Show)

basicOps :: [OpInfo]
basicOps = zipWith ($) ops (map basicCode [0x1 .. 0xf])
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
extendedOps = [OpInfo "JSR" 2 0 (OpCode 0x0 0x1)]

data Register = A | B | C | X | Y | Z | I | J
  deriving (Eq, Ord, Enum, Bounded, Show)

data Value = 
    REG Register
  | PtrREG Register
  | PtrREG_NW Register
  | POP
  | PEEK
  | PUSH
  | SP
  | PC
  | O
  | PtrNW
  | NW
  | Lit Word
  deriving (Eq, Show)

data Instruction =
  Instruction {
    icode :: OpCode
  , iA :: Value
  , iB :: Value
  }
  deriving (Eq, Show)
