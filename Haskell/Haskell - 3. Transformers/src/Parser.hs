module Parser where

import ExecutorIntf

import Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec as Meg
import Text.Megaparsec.Char
import Text.Megaparsec.Expr

import Data.Foldable as Fold
import Data.Void as Void

data Statement = Define String Expr
    | Update String Expr
    | Read String
    | Write Expr
    | Pure Expr
    | For String Expr Expr [Statement]
    | Break
    | If Expr String Expr [Statement]
    | While Expr String Expr [Statement]
    deriving Show

newtype Script = Script [Statement] deriving Show
type Parser = Meg.Parsec Void.Void String

reservedWords :: [String]
reservedWords = ["let", "in", "=", "mut", 
                 "for", "{", "}", ";", "break", "until", 
                 "<<", ">>", "if", "while"]

spaces :: Parser ()
spaces = Lex.space space1 (Lex.skipLineComment "//") (Lex.skipBlockComment "/*" "*/")

readSymbol :: String -> Parser String
readSymbol a = Meg.try $ Lex.symbol spaces a

arithmeticOperators :: [[Operator Parser Expr]]
arithmeticOperators = [[InfixL (Mul <$ readSymbol "*"), InfixL (Div <$ readSymbol "/"), 
                            InfixL (Mod <$ readSymbol "%")],
                       [InfixL (Add <$ readSymbol "+"), InfixL (Sub <$ readSymbol "-")]]

parseScriptFromSrc :: Parser Script
parseScriptFromSrc = Meg.try $ 
    Script <$> Meg.between spaces Meg.eof (Meg.many _parseStatements)
    where
        {- 
            May be it was better to do as class bacause
            this id done for incapsulation of support methods
        -}

        _readWord :: String -> Parser ()
        _readWord str = (_readLexeme . Meg.try) 
                        (string str *> Meg.notFollowedBy alphaNumChar)

        _readLexeme :: Parser a -> Parser a
        _readLexeme = Lex.lexeme spaces

        _parseWord :: Parser String
        _parseWord = (:) <$> letterChar <*> Meg.many alphaNumChar

        _readIdentifier :: Parser String
        _readIdentifier = Meg.try $ (_readLexeme . Meg.try) (_parseWord >>= _isReserved)
            where
                _isReserved :: String -> Parser String
                _isReserved name = if name `elem` reservedWords 
                                   then fail $ "keyword " ++ show name ++ " cannot be an identifier"
                                   else return name

        _parseStatements :: Parser Statement
        _parseStatements = Meg.try _parseFOR Meg.<|> _parseWHILE Meg.<|> _parseIF Meg.<|> _parseOPERAITON

        _parseOPERAITON :: Parser Statement
        _parseOPERAITON = Meg.try $ Fold.foldr1 (Meg.<|>) (Prelude.map (Meg.try . _parseSEMI) 
            [_parseDEFINE, _parseUPDATE, _parseREAD, _parseWRITE, _parseBREAK, _parsePURE])
            where
                _parseSEMI a = a >>= \b -> Meg.try $ readSymbol ";" >> return b

        _parseFOR :: Parser Statement
        _parseFOR = do 
            _readWord "for"
            var <- _readIdentifier
            _readWord "="
            from <- _parseEXPR
            _readWord "to"
            to <- _parseEXPR
            _readWord "{"
            statements <- Meg.many _parseStatements
            _readWord "}"
            return (For var from to statements)

        _parseWHILE :: Parser Statement
        _parseWHILE = do 
            _readWord "while"
            left <- _parseEXPR
            cond <- _readIdentifier
            right <- _parseEXPR
            _readWord "{"
            statements <- Meg.many _parseStatements
            _readWord "}"
            return (While left cond right statements)

        _parseIF :: Parser Statement
        _parseIF = do 
            _readWord "if"
            left <- _parseEXPR
            cond <- _readIdentifier
            right <- _parseEXPR
            _readWord "{"
            statements <- Meg.many _parseStatements
            _readWord "}"
            return (If left cond right statements)

        _parseDEFINE :: Parser Statement
        _parseDEFINE = do 
            _readWord "mut"
            name <- _readIdentifier
            _readWord "="
            expr <- _parseEXPR
            return (Define name expr)

        _parseUPDATE :: Parser Statement
        _parseUPDATE = do 
            name <- _readIdentifier
            _readWord "="
            expr <- _parseEXPR
            return (Update name expr)

        _parseREAD :: Parser Statement
        _parseREAD = do 
            _readWord ">>"
            name <- _readIdentifier
            return (Read name)

        _parseWRITE :: Parser Statement
        _parseWRITE = do 
            _readWord "<<"
            expr <- _parseARITHMETICS
            return (Write expr)

        _parseBREAK :: Parser Statement
        _parseBREAK = Meg.try $ const Parser.Break <$> readSymbol "break"

        _parsePURE :: Parser Statement
        _parsePURE = Meg.try $ Pure <$> _parseEXPR

        _parseEXPR :: Parser Expr
        _parseEXPR = Fold.foldr1 (Meg.<|>)
            [_parseARITHMETICS, _parseVAR, _parseLIT, _parseLET, _parsePARENTHS _parseEXPR]

        _parseARITHMETICS :: Parser Expr
        _parseARITHMETICS = makeExprParser (Meg.try _parseAEXPR) arithmeticOperators
            where
                _parseAEXPR :: Parser Expr
                _parseAEXPR = Fold.foldr1 (Meg.<|>)
                    [_parseVAR, _parseLIT, _parseLET, _parsePARENTHS _parseEXPR]

        _parseVAR :: Parser Expr
        _parseVAR = Var <$> _readIdentifier

        _parseLIT :: Parser Expr
        _parseLIT = Lit <$> Meg.try (Lex.signed spaces (Meg.try $ _readLexeme Lex.decimal))

        _parseLET :: Parser Expr
        _parseLET = do 
            _readWord "let"
            name <- _readIdentifier
            _readWord "="
            sub <- _parseEXPR
            _readWord "in"
            expr <- _parseEXPR
            return (Let name sub expr)

        _parsePARENTHS :: Parser a -> Parser a
        _parsePARENTHS = Meg.between (readSymbol "(") (readSymbol ")")