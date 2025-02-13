{-# LANGUAGE OverloadedStrings #-}

module ParseSolidity where

import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (void)
import qualified Text.Megaparsec.Char.Lexer as L

-- Solidity function representation
data SolidityFunction = SolidityFunction
  { fname :: Text
  , params :: [Text]
  , returnType :: Maybe Text
  , modifiesState :: Bool
  } deriving (Eq, Show)

-- Solidity contract representation
data SolidityContract = SolidityContract
  { cname :: Text
  , functions :: [SolidityFunction]
  , stateVariables :: [Text]
  } deriving (Eq, Show)

-- Type alias for parser
type Parser = Parsec Void Text

-- Lexer: Skips whitespace and comments
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

identifier :: Parser Text
identifier = T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)

-- Parses `pragma solidity ^0.8.0;` and skips it
parsePragma :: Parser ()
parsePragma = do
    sc  -- Skip any whitespace and comments before pragma
    _ <- string "pragma" *> space1 *> string "solidity" *> space1
    void $ manyTill anySingle (symbol ";")  -- Consume everything up to the `;`
    sc  -- Skip whitespace/comments after pragma



-- Helper function to separate functions from state variables
separateDecls :: Either SolidityFunction Text -> ([SolidityFunction], [Text]) -> ([SolidityFunction], [Text])
separateDecls (Left func) (funcs, vars) = (func : funcs, vars)  -- Add function to the list
separateDecls (Right var) (funcs, vars) = (funcs, var : vars)   -- Add variable to the list


-- Parse contract name
parseContract :: Parser SolidityContract
parseContract = do
    optional parsePragma  -- ✅ Skips `pragma solidity ...;`
    sc  -- Skip any remaining spaces/comments
    void $ symbol "contract"
    contractName <- identifier
    void $ symbol "{"
    body <- manyTill parseContractBody (symbol "}")
    let (functions, stateVars) = foldr separateDecls ([], []) body
    return $ SolidityContract contractName functions stateVars




-- Parse contract body elements (functions or state variables)
parseContractBody :: Parser (Either SolidityFunction Text)
parseContractBody = try (Left <$> parseFunction) <|> (Right <$> parseStateVar)

-- Parse Solidity state variables
parseStateVar :: Parser Text
parseStateVar = do
    sc
    _ <- some alphaNumChar -- Type (e.g., uint, string)
    varName <- identifier
    void $ optional (symbol "=" >> manyTill anySingle (symbol ";"))
    return varName

-- Parse Solidity function
parseFunction :: Parser SolidityFunction
parseFunction = do
    sc
    void $ symbol "function"
    funcName <- identifier
    params <- between (symbol "(") (symbol ")") (sepBy identifier (symbol ","))
    _ <- optional (symbol "public" <|> symbol "private")
    returnType <- optional (symbol "returns" *> between (symbol "(") (symbol ")") identifier)
    void $ symbol "{"
    void $ manyTill anySingle (symbol "}")
    return $ SolidityFunction funcName params returnType True

-- Parse a Solidity file
parseSolidityFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) SolidityContract)
parseSolidityFile file = do
    content <- TIO.readFile file
    return $ runParser parseContract file content
