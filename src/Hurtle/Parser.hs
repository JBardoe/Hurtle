module Hurtle.Parser where

{- This module defines the parsers that convert the user input to a HogoProgram -}

import Hurtle.Types

-- You'll probably want to refer to https://hackage.haskell.org/package/megaparsec for documentation of the Megaparsec library.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char
import Data.Maybe (catMaybes)
import Control.Monad 

{-
    This is the main parser for the application. It takes the whole program as input and then parses it to a HogoProgram. The first part consumes
    any empty spaces or comments at the start of the program and stops when it reaches a valid line or the end of the file(for an empty file). 
    Then the program continuously parses valid lines or until it reaches the end of the input. As the line parser, returns maybe HogoCode (to 
    account for empty lines) the parser finally uses catMaybes to traverse the list and collect the Just values into a program. manyTill is used 
    as it allows the enforcement of the end conditions which prevents infinite loops of parsing. 
-}
parseHogo :: Parser HogoProgram
parseHogo = do
    _ <- manyTill (space1 <|> parseComment <|> void tab) (lookAhead (void parseLine <|> eof))
    program <- manyTill parseLine eof
    pure $ catMaybes program

{- Tries all the different parsers for the different commands. They are separated out to ensure neat code and clear separation. -}
parseCommand :: Parser HogoCode
parseCommand = try parseForward <|> try parseBack <|> try parseLeft <|> try parseRight <|> try parsePenup <|> try parsePendown <|> try parseClearScreen <|> try parseHome <|> try parseColour <|> try parseRepeat <|> try parseFunctionCall <|> parseFunctionDef

{- 
    Parses a line of valid input. Starts by consuming all space before it which helps them be chained together. It then tries to parse a line
    which then has to be ended by a newline, comment or the end of input. lookAhead ensures the parser does not consume the end of input. 
-}
parseLine :: Parser (Maybe HogoCode)
parseLine = do
    space
    x <- optional parseCommand
    hspace
    _ <- try parseComment <|> try (void eol) <|> lookAhead eof
    pure x

--Parses a forward command by matching the string forward then taking the float value
parseForward :: Parser HogoCode
parseForward = do
    space
    _ <- string' "forward"
    space1
    GoForward <$> parseFloat

--Parses a backwards command by matching the string back then taking the float value
parseBack :: Parser HogoCode
parseBack = do
    space
    _ <- string' "back"
    space1
    GoBackward <$> parseFloat

--Parses a turn left command by matching the string left then taking the float value
parseLeft :: Parser HogoCode
parseLeft = do
    space
    _ <- string' "left"
    space1
    TurnLeft <$> parseFloat

--Parses a turn right command by matching the string right then taking the float value
parseRight :: Parser HogoCode
parseRight = do
    space
    _ <- string' "right"
    space1
    TurnRight <$> parseFloat

--Parse the penup command by matching the string penup
parsePenup :: Parser HogoCode
parsePenup = do
    space
    _ <- string' "penup"
    pure PenUp

--Parse the pendown command by matching the string pendown
parsePendown :: Parser HogoCode
parsePendown = do
    space
    _ <- string' "pendown"
    pure PenDown

--Parse the clear screen command by matching the string clearscreen
parseClearScreen :: Parser HogoCode
parseClearScreen = do
    space
    _ <- string' "clearscreen"
    pure ClearScreen

--Parse the go home command by matching the string home
parseHome :: Parser HogoCode
parseHome = do
    space
    _ <- string' "home"
    pure GoHome

--Parse the change pen colour command by matching the string colour then taking the three float values
parseColour :: Parser HogoCode
parseColour = do
    space
    _ <- string "colour"
    hspace
    red <- parseFloat
    hspace
    green <- parseFloat
    hspace
    PenColour red green <$> parseFloat

--Parses a repeat command by matching the string repeat, then accepting a number and requiring a [. It then parses a smaller HogoProgram. 
parseRepeat :: Parser HogoCode
parseRepeat = do
    space
    _ <- string' "repeat"
    space1
    x <- parseInt
    hspace
    _ <- char '['
    _ <- manyTill (space1 <|> parseComment <|> void tab) (lookAhead $ void parseLine)
    code <- manyTill parseSubCode (char ']')
    hspace
    pure $ Repeat x (catMaybes code)

{- Parses a smaller section of code such as in a function or repeat block. It is similar to the parseLine but has to be ended with a ].
   It is declared on the top level so it can be used in both parseRepeat and parseFunctionDef 
-}
parseSubCode :: Parser (Maybe HogoCode)
parseSubCode = do
    space
    program <- optional parseCommand
    _ <- try parseComment <|> try (void eol) <|> lookAhead (void $ char ']')
    pure program

--Parses a function declaration by matching the string function then taking the name and a [. It then parses a smaller HogoProgram.
parseFunctionDef :: Parser HogoCode
parseFunctionDef = do
    space
    _ <- string' "function"
    space1
    name <- takeWhile1P (Just "functionName") isLetter
    hspace
    _ <- char '['
    _ <- manyTill (space1 <|> parseComment <|> void tab) (lookAhead $ void parseLine)
    code <- manyTill parseSubCode (char ']')
    hspace
    pure $ FunctionDef name (catMaybes code)

--Parses a function call by taking a series of letters followed by () 
parseFunctionCall :: Parser HogoCode
parseFunctionCall = do
    space
    name <- takeWhile1P (Just "functionName") isLetter
    _ <- string "()"
    pure $ FunctionCall name

--Consumes a comment without returning anything
parseComment :: Parser ()
parseComment = do
    space
    _ <- char ';'
    _ <- manyTill anySingle (lookAhead (void eol <|> eof))
    mempty

{- Parses a float value by taking digits and then optionally a decimal point and more digits. By pattern matching on the decimal point,
   if there was one, the latter digits are appended and returned, otherwise only the start is returned.
 -}
parseFloat :: Parser Float
parseFloat = do
    x <- takeWhile1P (Just "digit") isDigit
    isFloat <- optional (char '.') -- Allow optional decimal point
    y <- takeWhileP (Just "digit") isDigit
    case isFloat of
        Just _ -> pure $ read $ x ++ "." ++ y
        Nothing -> pure $ read x

--Parses an integer by taking several digits in a row
parseInt :: Parser Int
parseInt = do
    x <- takeWhile1P (Just "int") isDigit
    pure $ read x


