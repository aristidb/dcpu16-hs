module Model where

import Data.Word

data BaseOp =
  SET |
  ADD |
  SUB |
  MUL |
  DIV |
  MOD |
  SHL |
  SHR |
  AND |
  BOR |
  XOR |
  IFE |
  IFN |
  IFG |
  IFB

baseCodes :: [(BaseOp, Word)]
baseCodes = [
    (SET, 0x1)
  , (ADD, 0x2)
  , (SUB, 0x3)
  , (MUL, 0x4)
  , (DIV, 0x5)
  , (MOD, 0x6)
  , (SHL, 0x7)
  , (SHR, 0x8)
  , (AND, 0x9)
  , (BOR, 0xa)
  , (XOR, 0xb)
  , (IFE, 0xc)
  , (IFN, 0xd)
  , (IFG, 0xe)
  , (IFB, 0xf)
  ]

baseCycles :: BaseOp -> Word
baseCycles x | x `elem` [SET, AND, BOR, XOR] = 1