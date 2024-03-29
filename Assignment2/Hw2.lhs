---
title: Homework #2, Due Friday 2/24/14
---

> {-# LANGUAGE TypeSynonymInstances #-}
> module Hw2 where

> import Control.Applicative hiding (empty, (<|>))
> import Data.Map hiding (foldl, foldr, delete)
> import qualified Data.Map as Map
> import Control.Monad.State hiding (when)
> import Text.Parsec hiding (State, between)
> import Text.Parsec.Combinator hiding (between)
> import Text.Parsec.Char
> import Text.Parsec.Language (haskellDef)
> import qualified Text.Parsec.Token as Token
> import Text.Parsec.String

This week's homework is presented as a literate Haskell file,
just like the lectures. This means that every line beginning with
`>` is interpreted as Haskell code by the compiler, while every other
line is ignored. (Think of this as the comments and code being reversed
from what they usually are.)

You can load this file into `ghci` and compile it with `ghc`
just like any other Haskell file, so long as you remember to save
it with a `.lhs` suffix.

To complete this homework, download [this file as plain text](Hw2.lhs) and
answer each question, filling in code where noted (i.e. where it says `error
"TBD"`).

Your code *must* typecheck against the given type signatures.
Feel free to add your own tests to this file to exercise the functions
you write.  Submit your homework by sending this file, filled in
appropriately, to `cse230@goto.ucsd.edu` with the subject "HW2"; you
will receive a confirmation email after submitting. 

Before starting this assignment:

1. Install `parsec3` via the command `cabal install parsec3`
2. Learn to read the [documentation](http://hackage.haskell.org)
3. Download the test files 
   [test.imp](/static/test.imp),
   [fact.imp](/static/fact.imp), 
   [abs.imp](/static/abs.imp), 
   [times.imp](/static/times.imp).

Problem 0: All About You
========================


Tell us your name, email and student ID, by replacing the respective
strings below

myName  = "Xintian Li"
myEmail = "xil200@eng.ucsd.edu"
mySID   = "A53058364"

> myName  = "Tao Cai"
> myEmail = "taocai@eng.ucsd.edu"
> mySID   = "A53051887"


Problem 1: All About `foldl`
============================

Define the following functions by filling in the "error" portion:

1. Describe `foldl` and give an implementation:
The 'foldl' takes the second argument and the first item in the list and applies the function to them, then feeds the function with the result and the second item from the list, and so on. If the list is empty, then the argument remains the same.

> myFoldl :: (a -> b -> a) -> a -> [b] -> a
> myFoldl f b []       = b
> myFoldl f b (x : xs) = myFoldl f (f b x) xs

2. Using the standard `foldl` (not `myFoldl`), define the list reverse function:

> myReverse :: [a] -> [a]
> myReverse xs = foldl (\arr y -> y : arr) [] xs

3. Define `foldr` in terms of `foldl`:

> myFoldr :: (a -> b -> b) -> b -> [a] -> b
> myFoldr f a xs =  foldl (\g b x -> g (f b x)) id xs a

4. Define `foldl` in terms of the standard `foldr` (not `myFoldr`):

> myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
> myFoldl2 f b xs = foldr (\a g x -> g (f x a)) id xs b

5. Try applying `foldl` to a gigantic list. Why is it so slow?
   Try using `foldl'` (from [Data.List](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#3))
   instead; can you explain why it's faster?

When we apply the 'foldl' to the gigantic list, the function is obviously slow when the size of list increases. And in the extreme cases, the function terminates due to exausting of the memory.

This is because when using the 'foldl', the continuous recursion will require more and more memory, until all memory is exausted. And the recursion and context switch significantly slow the execution of the function.

On the other hand, the 'foldl'' is more optimized. It will avoid unnecessary recursion, and try to reuse the same memory space for subproblems. Therefore by using this case, we can get rid of lots of memory utilization and time for context switch, and the execution of the function is significantly faster.


Part 2: Binary Search Trees
===========================

Recall the following type of binary search trees:

> data BST k v = Emp 
>              | Bind k v (BST k v) (BST k v) 
>              deriving (Show)

Define a `delete` function for BSTs of this type:

> delete :: (Ord k) => k -> BST k v -> BST k v
> delete _ Emp = Emp
> delete k (Bind k' _ Emp Emp) | k == k' = Emp
> delete k (Bind k' _ Emp rc)  | k == k' = rc
> delete k (Bind k' _ lc Emp)  | k == k' = lc
> delete k (Bind k' v' lc rc)
>   | k < k' = Bind k' v' (delete k lc) rc
>   | k > k' = Bind k' v' lc (delete k rc)
>   | otherwise = Bind k'' v'' lc rc'
>                 where (k'', v'') = minNode rc
>                       rc' = delete k'' rc

> minNode :: (BST k v) -> (k, v)
> minNode Emp = error "Emp BST error"
> minNode (Bind k v Emp _) = (k, v)
> minNode (Bind _ _ lc _)  = minNode lc

Part 3: An Interpreter for WHILE 
================================

Next, you will use monads to build an evaluator for
a simple *WHILE* language. In this language, we will
represent different program variables as 

> type Variable = String

Programs in the language are simply values of the type

> data Statement =
>     Assign Variable Expression          -- x = e
>   | If Expression Statement Statement   -- if (e) {s1} else {s2}
>   | While Expression Statement          -- while (e) {s}
>   | Sequence Statement Statement        -- s1; s2
>   | Skip                                -- no-op
>   deriving (Show)

where expressions are variables, constants or 
binary operators applied to sub-expressions

> data Expression =
>     Var Variable                        -- x
>   | Val Value                           -- v 
>   | Op  Bop Expression Expression
>   deriving (Show)

and binary operators are simply two-ary functions

> data Bop = 
>     Plus     -- +  :: Int  -> Int  -> Int
>   | Minus    -- -  :: Int  -> Int  -> Int
>   | Times    -- *  :: Int  -> Int  -> Int
>   | Divide   -- /  :: Int  -> Int  -> Int
>   | Gt       -- >  :: Int -> Int -> Bool 
>   | Ge       -- >= :: Int -> Int -> Bool
>   | Lt       -- <  :: Int -> Int -> Bool
>   | Le       -- <= :: Int -> Int -> Bool
>   deriving (Show)

> data Value =
>     IntVal Int
>   | BoolVal Bool
>   deriving (Show)

We will represent the *store* i.e. the machine's memory, as an associative
map from `Variable` to `Value` 

> type Store = Map Variable Value

**Note:** we don't have exceptions (yet), so if a variable
is not found (eg because it is not initialized) simply return 
the value `0`. In future assignments, we will add this as a 
case where exceptions are thrown (the other case being type errors.)

We will use the standard library's `State` 
[monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2)
to represent the world-transformer.
Intuitively, `State s a` is equivalent to the world-transformer 
`s -> (a, s)`. See the above documentation for more details. 
You can ignore the bits about `StateT` for now.

Expression Evaluator
--------------------

First, write a function 

> evalE :: Expression -> State Store Value

that takes as input an expression and returns a world-transformer that
returns a value. Yes, right now, the transformer doesnt really transform
the world, but we will use the monad nevertheless as later, the world may
change, when we add exceptions and such.

**Hint:** The value `get` is of type `State Store Store`. Thus, to extract 
the value of the "current store" in a variable `s` use `s <- get`.

> evalE (Var x)      = do s <- get
>                         return (Map.findWithDefault (IntVal 0) x s )

> evalE (Val v)      = return v
> evalE (Op o e1 e2) = do v1 <- evalE e1
>                         v2 <- evalE e2
>                         return (bop o v1 v2)

If it is not init, using the zero

> bop :: Bop -> Value -> Value -> Value
> bop _ (BoolVal _) _                           = IntVal 0
> bop _ _ (BoolVal _)                           = IntVal 0
> bop Plus (IntVal value1) (IntVal value2)      = IntVal (value1 + value2)
> bop Minus (IntVal value1) (IntVal value2)     = IntVal (value1 - value2)
> bop Times (IntVal value1) (IntVal value2)     = IntVal (value1 * value2)
> bop Divide (IntVal value1) (IntVal 0)         = error "divied by zero"
> bop Divide (IntVal value1) (IntVal value2)    = IntVal (value1 `div` value2)
> bop Lt (IntVal value1) (IntVal value2)        = BoolVal (value1 < value2)
> bop Le (IntVal value1) (IntVal value2)        = BoolVal (value1 <= value2)
> bop Gt (IntVal value1) (IntVal value2)        = BoolVal (value1 > value2)
> bop Ge (IntVal value1) (IntVal value2)        = BoolVal (value1 >= value2)



Statement Evaluator
-------------------

Next, write a function

> evalS :: Statement -> State Store ()

that takes as input a statement and returns a world-transformer that
returns a unit. Here, the world-transformer should in fact update the input
store appropriately with the assignments executed in the course of
evaluating the `Statement`.

**Hint:** The value `put` is of type `Store -> State Store ()`. 
Thus, to "update" the value of the store with the new store `s'` 
do `put s`.

> evalS w@(While e s)    = evalS $ If e (Sequence s (While e s)) Skip
> evalS Skip             = return ()
> evalS (Sequence s1 s2) = do evalS s1
>                             evalS s2
> evalS (Assign x e )    = do v <- evalE e
>                             s <- get
>                             put $ Map.insert x v s
> evalS (If e s1 s2)     = do rb <- evalE e
>                             case rb of
>                                       (BoolVal True)  -> evalS s1
>                                       (BoolVal False) -> evalS s2
>                                       _               -> return() 

In the `If` case, if `e` evaluates to a non-boolean value, just skip both
the branches. (We will convert it into a type error in the next homework.)
Finally, write a function 

> execS :: Statement -> Store -> Store
> execS = execState . evalS

such that `execS stmt store` returns the new `Store` that results
from evaluating the command `stmt` from the world `store`. 
**Hint:** You may want to use the library function 

~~~~~{.haskell}
execState :: State s a -> s -> s
~~~~~

When you are done with the above, the following function will 
"run" a statement starting with the `empty` store (where no 
variable is initialized). Running the program should print 
the value of all variables at the end of execution.

> run :: Statement -> IO ()
> run stmt = do putStrLn "Output Store:" 
>               putStrLn $ show $ execS stmt empty

Here are a few "tests" that you can use to check your implementation.

> w_test = (Sequence (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign "Y" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Y" (Op Plus (Var "Y") (Var "X"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))))

> w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))

As you can see, it is rather tedious to write the above tests! They
correspond to the code in the files `test.imp` and `fact.imp`. When you are
done, you should get

~~~~~{.haskell}
ghci> run w_test
Output Store:
fromList [("X",IntVal 0),("Y",IntVal 10)]

ghci> run w_fact
Output Store:
fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
~~~~~

Problem 4: A Parser for WHILE 
=============================

It is rather tedious to have to specify individual programs as Haskell
values. For this problem, you will use parser combinators to build a parser
for the WHILE language from the previous problem.

Parsing Constants
-----------------

First, we will write parsers for the `Value` type

> valueP :: Parser Value
> valueP = intP <|> boolP

To do so, fill in the implementations of

> intP :: Parser Value
> intP = do n <- Token.lexeme (Token.makeTokenParser haskellDef) (many1 digit)
>           return (IntVal (read n))

Next, define a parser that will accept a 
particular string `s` as a given value `x`

> constP :: String -> a -> Parser a
> constP s x = do Token.lexeme (Token.makeTokenParser haskellDef) (string s)
>                 return x

and use the above to define a parser for boolean values 
where `"true"` and `"false"` should be parsed appropriately.

> boolP :: Parser Value
> boolP = constP "true" (BoolVal True) <|> constP "false" (BoolVal False)

Continue to use the above to parse the binary operators

> opP :: Parser Bop 
> opP = constP "+" Plus  
>       <|> constP "-" Minus
>       <|> constP "*" Times 
>       <|> constP "/" Divide
>       <|> constP ">" Gt 
>       <|> constP "<" Lt 
>       <|> constP ">=" Ge 
>       <|> constP "<=" Le
 
Parsing Expressions 
-----------------

Next, the following is a parser for variables, where each 
variable is one-or-more uppercase letters. 


> parens = Token.parens (Token.makeTokenParser haskellDef) 
> lexeme = Token.lexeme (Token.makeTokenParser haskellDef) 

> varP :: Parser Variable
> varP =  lexeme (many1 upper)

Use the above to write a parser for `Expression` values

> parseP = varP >>= return . Var
> parseValue = valueP >>= return . Val
> parseExpr = parens exprP
> parseAll = parseP <|> parseValue <|>  parseExpr

> lexemeParser = lexeme $ opP >>= return . Op

> exprP :: Parser Expression
> exprP = lexeme $ chainl1 parseAll lexemeParser

Parsing Statements
------------------

Next, use the expression parsers to build a statement parser

> statementP :: Parser Statement
> statementP = chainl1 stateAll (lexeme $ char ';' >> return Sequence)

> stateAll :: Parser Statement
> stateAll = stateSkip <|> stateAssign <|> stateIf <|> stateWhile <|> statementP

> stateSkip :: Parser Statement
> stateSkip = do lexeme $ string "skip"
>                return Skip

> stateAssign :: Parser Statement
> stateAssign = do v <- varP
>                  lexeme $ string ":="
>                  e <- exprP
>                  return (Assign v e)

> stateIf :: Parser Statement
> stateIf = do lexeme $ string "if"
>              expr <- exprP
>              lexeme $ string "then"
>              s1 <- statementP
>              lexeme $ string "else"
>              s2 <- statementP
>              lexeme $ string "endif"
>              return (If expr s1 s2)

> stateWhile :: Parser Statement
> stateWhile = do lexeme $ string "while"
>                 e <- exprP
>                 lexeme $ string "do"
>                 s <- statementP
>                 lexeme $ string "endwhile"
>                 return (While e s)



When you are done, we can put the parser and evaluator together 
in the end-to-end interpreter function

> runFile s = do p <- parseFromFile statementP s
>                case p of
>                  Left err   -> print err
>                  Right stmt -> run stmt

When you are done you should see the following at the ghci prompt

~~~~~{.haskell}
ghci> runFile "test.imp"
Output Store:
fromList [("X",IntVal 0),("Y",IntVal 10)]

ghci> runFile "fact.imp" 
Output Store:
fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
~~~~~





