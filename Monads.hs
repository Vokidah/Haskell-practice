import Control.Monad
import Data.Char
import Control.Applicative

-- ============================== PARSER DECLARATION
-- Write code for (>>=) operation  for Parser monad. 
newtype Parser a = Parser { parse::String -> [(a, String)] }

instance Monad(Parser) where
    return a = Parser( \s -> [(a, s)])
    (Parser par) >>= f = Parser (\s -> concat [parse (f a) s' | (a,s') <- parse (Parser par) s])
    fail s = Parser (\s -> [])
    
instance Functor (Parser) where
    fmap = liftM

instance Applicative (Parser) where
    pure = return
    (<*>) = ap    

-- ============================= GENERAL PARSING

-- item: simple parser that chops off first letter of a string.
--   parse item "1234"      = [('1',"234")]
--   parse item "abcd"      = [('a',"234")]
--   parse item ""          = []
item::Parser Char
item = Parser( \s -> if null(s) then [] else [(head s, tail s)] )

-- symb and symbOf: parsers that accept some fixed set of symbols.
--   parse (symb isDigit) "1234"      = [("1","234")]
--   parse (symb isDigit) "abcd"      = []
--   parse (symbOf "+-") "1234"       = []
--   parse (symbOf "+-") "+1234"      = [("+","1234")] 
symb::(Char->Bool)->(Parser String)
symb p = do
    x<-item
    if (p x) then (return [x]) else (fail "")

symbOf::String->(Parser String)
symbOf s = symb (\c -> elem c s)

infixr 5 +++
infixr 9 ***

-- Write code for (+++) operation
-- Combines parsers "in parallel". 
-- Parser a+++b tries to parse first with a, and if it fails then with b
--   parse (symb isDigit +++ symbOf "+-") "12345"       = [("1","2345")]
--   parse (symb isDigit +++ symbOf "+-") "+12345"       = [("+","12345")]
(+++)::(Parser a)->(Parser a)->(Parser a)    
p +++ q = Parser (\x -> case parse p x of
	[] -> parse q x
	[(a, s)] -> [(a,s)])

-- Write code for (***) operation using do notation
-- Combines string parsers consequently. 
-- Parser a***b parses first part of a string with a, parses the rest with b and then concatenates results.
-- If a or b fails, then a***b also fails
--   parse (symbOf "+-" *** symb isDigit) "12345"       = []
--   parse (symbOf "+-" *** symb isDigit) "+12345"       = [("+1","2345")]
(***)::(Parser String)->(Parser String)->(Parser String)    
p *** q = do
	p' <- p
	q' <- q
	return (p' ++ q')


-- Write code for ntimes, startimes and plustimes operations
-- Combines string parsers consequently. 
-- Parser a***b parses first part of a string with a, parses the rest with b and then concatenates results.
-- If a or b fails, then a***b also fails
--   parse (ntimes 2 (symb isDigit)) "12345"       = [("12","345")]
--   parse (ntimes 5 (symb isDigit)) "12345"       = [("12345","")]
--   parse (startimes (symb isDigit)) "12345abc"       = [("12345","abc")]
--   parse (startimes (symb isDigit)) "+12345abc"       = [("","+12345abc")]
--   parse (plustimes (symb isDigit)) "12345abc"       = [("12345","abc")]
--   parse (plustimes (symb isDigit)) "+12345abc"       = []
ntimes::Int -> (Parser String) -> (Parser String)
ntimes 0 p = Parser (\x -> [("", x)]) 
ntimes 1 p = p >>= (\x -> return x)
ntimes n p = p *** ntimes (n-1) p

startimes::(Parser String) -> (Parser String)
startimes p = p *** startimes p +++ return "" 

plustimes::(Parser String) -> (Parser String)    
plustimes p = p *** startimes p *** return ""


-- ================================ DATA PARSING

strToInt::String -> Int
strToInt s  = foldl (\n c->n*10 + digitToInt c ) 0 s

-- Write code for number parser using do notation. Notice, that this is a Parser Int, not Parser String as before.
-- Chops off an integer out of the string. 
--   parse number "12345"       = [(12345,"")]
--   parse number "+12345"       = []
--   parse number "12+34"       = [(12, "+34)]
number::(Parser Int)
number = do 
	x<- plustimes (symb isDigit) 
	return (strToInt x)	



-- Write code for mult and plus parsers
-- Parser mult parses and calculates expression in the form number(+number)* 
-- Parser plus parses and calculates expression in the form mult(+mult)* 
-- If written correctly, plus is able to parse and calculate expressions with sums and multiplication,
-- with precedence of multiplication over sum
--   parse plus "12345"             = [(12345,"")]
--   parse plus "1+2"               = [(3,"")]
--   parse plus "1+2+3"             = [(6,"")]
--   parse plus "1*1+2*2+3*3"       = [(14,"")]
--   parse plus "1+2+"              = [(3,"+")]
--   parse plus "1+2+a*b"           = [(3,"+a*b")]
--   parse plus "abcd"              = []
-- mult = number("*" mult)?
-- plus = mult("+" plus)?

mult = do
	x <- number
	y <- do 
		symbOf "*"
		mult
             +++ return 1
	return (x*y)
plus = do
	x<- mult
	y <- do
		symbOf "+"
		plus
	     +++ return 0
	return (x+y)

	    
eval::(Parser Int)->String->Int
eval p s = 
    let res = (parse p s) in 
        if null(res) then error "Parsing failed" 
            else if not.null.snd.head $ res then error "Parsing incomplete"
                else fst.head $ res




