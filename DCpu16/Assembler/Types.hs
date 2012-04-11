module DCpu16.Assembler.Types where

import           DCpu16.Model

import           Data.Sequence (Seq, (<|), (><), ViewL ((:<)))
import qualified Data.Sequence as S
import qualified Data.Map      as M
import           Data.Monoid

type Label = String

data Sign = Neg | Pos
  deriving (Eq, Ord, Enum, Bounded, Show)

data Form = ShortForm | LongForm
  deriving (Eq, Ord, Enum, Bounded, Show)

data LabelRef = AbsRef Label |
                RelRef Sign Label
  deriving (Eq, Show)

data AsmValue = LitValue Value |
                RefValue LabelRef
  deriving (Eq, Show)

data DetAsmValue = DetLitValue Value |
                   DetRefValue Form LabelRef
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

data DetAsmInstruction =
  DetAsmInstruction {
    daiOp :: OpInfo
  , daiA :: DetAsmValue
  , daiB :: DetAsmValue
  }
  deriving (Eq, Show)

data AsmProgram 
  = AsmProgram {
      apAddressableBlocks :: Seq [AsmInstruction]
    , apLabels :: M.Map Label Int
    }
  deriving (Eq, Show)

prependBlock :: [AsmInstruction] -> AsmProgram -> AsmProgram
prependBlock xs (AsmProgram bs lbls) = AsmProgram (xs <| bs) (fmap succ lbls)

instance Monoid AsmProgram where
  mempty = AsmProgram S.empty M.empty
  mappend (AsmProgram as lblA) (AsmProgram bs lblB) = AsmProgram (as >< bs) (M.union (fmap (+ S.length as) lblB) lblB)

prepareAsmProgram :: [Either Label AsmInstruction] -> AsmProgram
prepareAsmProgram xs = let prog@(AsmProgram bs lbls) = go xs
                           (i :< is) = S.viewl bs
                       in case i of
                            [] -> AsmProgram is (fmap pred lbls)
                            _  -> prog
  where
    go :: [Either Label AsmInstruction] -> AsmProgram
    go [] = AsmProgram ([] <| S.empty) M.empty
    go (Left lbl : xs) = let (AsmProgram bs lbls) = go xs
                         in AsmProgram ([] <| bs) (M.insert lbl 1 (fmap succ lbls))
    go (Right instr : xs) = let (AsmProgram bs lbls) = go xs
                                i :< is = S.viewl bs
                            in AsmProgram ((instr : i) <| is) lbls

data DetAsmProgram
  = DetAsmProgram {
      dapAddressableBlocks :: Seq [DetAsmInstruction]
    , dapLabels :: M.Map Label Int
    }
  deriving (Eq, Show)
