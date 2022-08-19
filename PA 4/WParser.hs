module WParser ( parse,
                 wprogram ) where

    import Data.Char
    import W

    import Control.Applicative (Applicative(..))
    import Control.Monad (liftM, ap)

    -----------------------------
    -- This is the main parser --
    -----------------------------
    wprogram = whitespace >> many stmt >>= \ss -> return (Block ss)
    -- a program is a sequence of statements; the parser returns them
    -- as a single block-statement

    -- only two of the statement types above are supported, the rest are undefined.
    -- please implement them
    stmt = varDeclStmt +++ assignStmt +++ ifStmt +++ whileStmt +++ 
           blockStmt +++ emptyStmt +++ printStmt

    emptyStmt = 
      symbol ";" >>
      return Empty
  
    -- Handles Print Statements
    printStmt = 
      keyword "print" >>
      expr >>= \e ->
      symbol ";" >>
      return (Print e)

    -- Handles variable declaration statements
    varDeclStmt =
      keyword "var" >>
      identifier >>= \s ->
      symbol "=" >>
      expr >>= \e ->
      symbol ";" >>
      return (VarDecl s e)

    -- Handles assignment statements
    assignStmt = 
      identifier >>= \s ->
      symbol "=" >>
      expr >>= \e ->
      symbol ";" >>
      return (Assign s e)

    -- Handles case 1 of if statement
    ifStmt =
      keyword "if" >>
      symbol ("(") >>
      expr >>= \e ->
      symbol (")") >>
      stmt >>= \b1 ->
      keyword "else" >>
      stmt >>= \b2 ->
      return (If e b1 b2)

    whileStmt =
      keyword "while" >>
      symbol ("(") >>
      expr >>= \e ->
      symbol (")") >>
      stmt >>= \b1 ->
      return (While e b1)
    
    -- Returns a list of statements
    blockStmt = symbol ("{") >>
                whitespace >>
                many stmt >>= \s ->
                whitespace >>
                symbol ("}") >>
                whitespace >>
                return (Block(s))

    -- the only kind of expression supported for now is stringLiterals
    -- implement the full expression language of W

    ----------------------
    --    WExp Code     --
    ----------------------

    expr = andSome >>= orSeq

    orSeq left = ( (symbol "||") >>= \s ->
                     addSub >>= \right ->
                     orSeq ((toOp s) left right)
                   ) +++ return left    

    andSome = relational >>= andSeq

    andSeq left = ( (symbol "&&") >>= \s ->
                     addSub >>= \right ->
                     andSeq ((toOp s) left right)
                   ) +++ return left

    relational = addSub >>= relationalSeq

    relationalOp = symbol "==" +++ symbol "!=" +++ symbol "<=" +++ symbol ">=" +++ symbol "<" +++ symbol ">"
    relationalSeq left = ( (relationalOp) >>= \s ->
                     addSub >>= \right ->
                     relationalSeq ((toOp s) left right)
                   ) +++ return left

    addSub = multDiv >>= addSubSeq

    addSubSeq left = ( (symbol "+" +++ symbol "-") >>= \s ->
                     multDiv >>= \right ->
                     addSubSeq ((toOp s) left right)
                   ) +++ return left

    multDiv = factor >>= multDivSeq 

    multDivSeq left = ( (symbol "*" +++ symbol "/") >>= \s ->
                       factor >>= \right ->
                       multDivSeq ((toOp s) left right)
                     ) +++ return left

    notParser =
      symbol ("!") >>
      expr >>= \e ->
      whitespace >>
      return (Not e)

    varParser =
      identifier >>= \s ->
      return (Var s)

    factor = parens expr +++ notParser +++ stringLiteral +++ intParser +++ intParserN +++ boolParser +++ varParser

    --expr = stringLiteral +++ intParser +++ intParserN +++ boolParser

    --toOp "!" = Not
    toOp "+" = Plus
    toOp "-" = Minus
    toOp "*" = Multiplies
    toOp "/" = Divides
    toOp "==" = Equals
    toOp "!=" = NotEqual
    toOp "<" = Less
    toOp ">" = Greater
    toOp "<=" = LessOrEqual
    toOp ">=" = GreaterOrEqual
    toOp "&&" = And
    toOp "||" = Or

    -- stringLiterals can contain \n characters
    stringLiteral = char ('"') >>
                    many stringChar >>= \s ->
                    char ('"') >>
                    whitespace >>
                    return (Val (VString s))

    stringChar = (char '\\' >> char 'n' >> return '\n') 
                 +++ sat (/= '"')

    -- Integer Parser
    intParser = nat >>= \s ->
             return (Val(VInt s))

    -- Negative Integer Parser
    intParserN = char ('-') >>= \_ ->
                 nat >>= \s ->
                 return (Val(VInt(-s)))

    -- Bool Parser
    boolParser =
        (string "True" >>
        return (Val(VBool(True))))
        +++ (string "False" >>
        return (Val(VBool(False))))

    ----------------------
    -- Parser utilities --
    ----------------------

    keywords = words "var if else while"
    isKeyword s = s `elem` keywords

    keyword s = 
      identifier >>= \s' ->
      if s' == s then return s else failure

    newtype Parser a = P (String -> [(a, String)])
    
    parse :: Parser a -> String -> [(a, String)]
    parse (P p) inp = p inp
    
    instance Functor Parser where
        fmap = liftM
     
    instance Applicative Parser where
        pure  = return
        (<*>) = ap
    
    instance Monad Parser where
        -- return :: a -> Parser a
        return v = P $ \inp -> [(v, inp)]
                 
        -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        p >>= q = P $ \inp -> case parse p inp of 
                                [] -> []
                                [(v, inp')] -> let q' = q v in parse q' inp'
    
    failure :: Parser a
    failure = P $ \_ -> []
    
    item :: Parser Char 
    item = P $ \inp -> case inp of 
                         (x:xs) -> [(x, xs)]
                         [] -> []
    
    -- Parse with p or q
    (+++) :: Parser a -> Parser a -> Parser a
    p +++ q = P $ \inp -> case parse p inp of 
                              [] -> parse q inp
                              [(v, inp')] -> [(v, inp')]
    
    
    -- Simple helper parsers
    sat :: (Char -> Bool) -> Parser Char
    sat pred = item >>= \c ->
               if pred c then return c else failure
    
    digit, letter, alphanum :: Parser Char
    digit = sat isDigit
    letter = sat isAlpha
    alphanum = sat isAlphaNum
    
    char :: Char -> Parser Char
    char x = sat (== x)
    
    string = sequence . map char 
    
    many1 :: Parser a -> Parser [a]
    many1 p = p >>= \v ->
              many p >>= \vs ->
              return (v:vs)
    
    many :: Parser a -> Parser [a]
    many p = many1 p +++ return []
    
    -- Useful building blocks
    nat :: Parser Int
    nat = many1 digit >>= \s ->
          whitespace >>
          return (read s)
    
    identifier :: Parser String
    identifier = letter >>= \s ->
                 many alphanum >>= \ss ->
                 whitespace >>
                 return (s:ss)
    
    whitespace :: Parser ()
    whitespace = many (sat isSpace) >> comment
    
    symbol s = 
        string s >>= \s' ->
        whitespace >>
        return s'    
    
    comment = ( string "//" >>
                many (sat (/= '\n')) >>
                whitespace ) +++ return ()
    
    parens p = 
        symbol "(" >> 
        p >>= \res ->
        symbol ")" >>
        return res
