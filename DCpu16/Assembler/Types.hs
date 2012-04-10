module DCpu16.Assembler.Types where

import DCpu16.Model

type Label = String

data Sign = Neg | Pos
  deriving (Eq, Ord, Enum, Bounded, Show)

data Form = ShortForm | LongForm
  deriving (Eq, Ord, Enum, Bounded, Show)

data LabelRef = AbsRef Label |
                RelRef Sign Label
  deriving (Eq, Show)

data AsmValue = LitValue Value |
                RefValue (Maybe Form) LabelRef
  deriving (Eq, Show)

asmDummy :: AsmValue
asmDummy = LitValue (REG A)

data AsmInstruction =
  AsmInstruction {
    aiOp :: OpInfo
  , aiA :: AsmValue 
  , aiB :: AsmValue
  }
  deriving (Eq, Show)
