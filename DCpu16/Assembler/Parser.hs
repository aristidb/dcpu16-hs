module DCpu16.Assembler.Parser where

import           DCpu16.Assembler.Types
import           DCpu16.Model

import           Control.Applicative
import           Data.Word
import           Numeric                (readHex)
import           Text.Parsec            (Parsec, ParseError, SourceName, (<?>))
import qualified Text.Parsec            as P

parseAsm :: String -> SourceName -> Either ParseError [Either Label AsmInstruction]
parseAsm src file = P.parse fileParser file src

fileParser :: Parsec String () [Either Label AsmInstruction]
fileParser = filler *> many itemParser <* P.eof

itemParser :: Parsec String () (Either Label AsmInstruction)
itemParser = (fmap Left labelParser <|> fmap Right instructionParser) <* filler

sep :: Parsec String () ()
sep = () <$ P.space <|> 
      () <$ P.lookAhead (P.oneOf ",;[]+") <|>
      P.eof 
      <?> "space between tokens"

comment :: Parsec String () ()
comment = P.char ';' >> P.skipMany (P.noneOf "\n") <?> "comment"

filler :: Parsec String () ()
filler = P.spaces >> P.skipMany (comment >> P.spaces)

filler1 :: Parsec String () ()
filler1 = sep >> filler

instructionParser :: Parsec String () AsmInstruction
instructionParser = do oi <- opParser
                       (a, b) <- case cardinality oi of
                                   Nullary -> return (asmDummy, asmDummy)
                                   Unary   -> do a <- valueParser
                                                 return (a, asmDummy)
                                   Binary  -> do a <- valueParser
                                                 P.char ',' >> filler
                                                 b <- valueParser
                                                 return (a, b)
                       return AsmInstruction{ aiOp = oi, aiA = a, aiB = b }

opParser :: Parsec String () OpInfo
opParser = do sym <- word
              case getOpByName sym of
                Nothing -> P.unexpected ("unknown opcode " ++ sym)
                Just oi -> return oi

labelParser :: Parsec String () String
labelParser = do _ <- P.char ':'
                 word

word :: Parsec String () String
word = do w <- (:) <$> P.letter <*> many P.alphaNum
          filler1
          return w

numeric :: Parsec String () Word16
numeric = (unreads =<< P.choice [ P.try (P.string "0x") >> unhex <$> P.many1 P.hexDigit
                               , undec <$> P.many1 P.digit])
          <* filler1
          <?> "numeric constant"
  where unhex, undec :: String -> [(Word16, String)]
        unhex = readHex
        undec = reads

        unreads :: [(Word16, String)] -> Parsec String () Word16
        unreads [(x,"")] = return x
        unreads _ = fail "Invalid number - PARSER BUG"

literal :: String -> Parsec String () String
literal s = P.string s <* filler1

valueParser :: Parsec String () AsmValue
valueParser = fmap LitValue (indirectLitValueParser <|> litValueParser) <|>
              fmap RefValue (relLabelRefParser <|> absLabelRefParser)

litValueParser :: Parsec String () Value
litValueParser = (P.choice . map P.try) [
                   REG <$> registerParser
                 , POP <$ literal "POP"
                 , PEEK <$ literal "PEEK"
                 , PUSH <$ literal "PUSH"
                 , SP <$ literal "SP"
                 , PC <$ literal "PC"
                 , O <$ literal "O"
                 , numLit <$> numeric
                 ]
                 <* filler

indirectLitValueParser :: Parsec String () Value
indirectLitValueParser = do _ <- P.char '['; filler
                            v <- P.choice . map P.try $ [
                                       PtrREG_NW <$> numeric <*> 
                                         (P.char '+' >> filler >> registerParser)
                                     , flip PtrREG_NW <$> (registerParser <* filler) <*> 
                                         (P.char '+' >> filler >> numeric)
                                     , PtrREG <$> registerParser
                                     , POP <$ literal "SP++"
                                     , PEEK <$ literal "SP"
                                     , PUSH <$ literal "--SP"
                                     , PtrNW <$> numeric
                                     ]
                            filler
                            _ <- P.char ']'; filler
                            return v

registerParser :: Parsec String () Register
registerParser = P.choice (zipWith (\c i -> i <$ P.char c) "ABCXYZIJ" [A ..]) <* filler1
                 <?> "register (A/B/C, X/Y/Z or I/J)"

relLabelRefParser :: Parsec String () LabelRef
relLabelRefParser = do sign <- P.choice [Neg <$ P.char '-', Pos <$ P.char '+']
                       filler
                       ref <- word
                       return $ RelRef sign ref
                       
absLabelRefParser :: Parsec String () LabelRef
absLabelRefParser = AbsRef <$> word
