module DCpu16.Assembler where

import DCpu16.Model
import DCpu16.Binary
import Text.Parsec (Parsec, ParseError, SourceName)
import qualified Text.Parsec as P
import Control.Applicative
import Data.Maybe

type Label = String

data AsmValue = LitValue Value |
                AbsLabel Label |
                RelLabel Label
  deriving (Eq, Show)

data AsmInstruction =
  AsmInstruction {
    aiCode :: OpCode
  , aiA :: AsmValue 
  , aiB :: AsmValue
  }
  deriving (Eq, Show)

parseAsm :: String -> SourceName -> Either ParseError [Either Label AsmInstruction]
parseAsm src file = P.parse fileParser file src

fileParser :: Parsec String () [Either Label AsmInstruction]
fileParser = filler *> many itemParser <* P.eof

itemParser :: Parsec String () (Either Label AsmInstruction)
itemParser = (fmap Left labelParser <|> fmap Right instructionParser) <* filler

filler :: Parsec String () ()
filler = P.spaces >> P.skipMany (comment >> P.spaces)
  where comment = P.char ';' >> P.skipMany (P.noneOf "\n") -- >> P.char '\n'

instructionParser :: Parsec String () AsmInstruction
instructionParser = do fail "TODO"
                       return AsmInstruction{}

unaryOp :: Parsec String () OpCode
unaryOp = fail "TODO"

binaryOp :: Parsec String () OpCode
binaryOp = fail "TODO"

labelParser :: Parsec String () String
labelParser = do P.char ':'
                 (:) <$> P.letter <*> many P.alphaNum
