This post will walk through the design a simple calulator in Haskell. We want to be able to take expressions such as 3*(4+2) and have our calulator evaluate this expression giving 18 as the result.

Before we get into the details of parsing such expressions let's look at a simpler notational system for arithemetic expressions.

## Reverse Polish Notation

Reverse Polish Notation (RPN) is a system for expressing arithmetic expressions in a postfix form without a need for parentheses. Some examples of RPN expressions include

    3 4 +
    
    4 7 * 9 5 + -
    
    5 1 2 + 4 * + 3 -
    
We can consider an RPN expression as a stack of numbers and operators then to evaluate RPN expressions we will use an additional stack to keep track of intermediate results. Evaluation proceeds as follows:

1. If the element at the top of the expression stack is a number then push it onto the evaluation stack.
2. If the element at the top of the expression stack is an operator then pop the top two elements off of the evaluation stack, apply the operator to them, and then push the result back onto the evalutation stack.
3. If the expression stack is empty, then evaluation has completed, if the evalutation stack is nonempty the top element is the desired result.


Let's work through an example by hand before diving into some code. We'll represent a state of evaluation by a pair (e,s) where e is the remaining expression and s is the current evaluation stack. So to evaluate (5 1 2 + 4 * + 3 -) we start with (5 1 2 + 4 * + 3 -,[])

1. (5 1 2 + 4 * + 3 -,[])
2. Push 5 on the evaluation stack -> (1 2 + 4 * + 3 -,[5])
3. Push 1 on the evaluation stack -> (2 + 4 * + 3 -,[1,5])
4. Push 2 on the evaluation stack -> (+ 4 * + 3 -,[2,1,5])
5. Pop 2 and 1 off the stack, add them, push 3 back on the stack -> (4 * + 3 -,[1+2,5]) -> (4 * + 3 -,[3,5])
6. Push 4 -> (* + 3 -,[4,3,5])
7. Pop and multiply -> (+ 3 -,[12,5])
8. Pop and add -> (3 -,[17])
9. Push 3 (-,[3,17])
10. Pop and subtract -> (,[14])
11. Expression is empty so result is at the top of evaluation stack -> 14

## An Evaluator

We can express the process of evaluating an RPN expression in Haskell quite easily, let's start with a data type to represent the elements that can occur in an RPN expression.

``` haskell
data RPN = Val Integer
         | Plus
         | Minus
         | Times
         | Divide
         deriving(Show)
         
type RPNExpr = [RPN]
```

Now we can easily evaluate an RPNExpr according to the rules above. Wrapping the result in Maybe helps to deal with those cases when eval is passed an improperly formed expression, or something else goes wrong. Things will still go wrong if we try to divide by zero, but let's not worry about that for now.

``` haskell
eval :: RPNExpr -> Maybe Integer
eval e = go e [] where
         go (Val n : es) s = go es (n:s)
         go (Plus : es) (n1:n2:s) = go es (n2 + n1 : s)
         go (Minus : es) (n1:n2:s) = go es (n2 - n1 : s)
         go (Times : es) (n1:n2:s) = go es (n2 * n1 : s)
         go (Divide : es) (n1:n2:s) = go es (n2 `div` n1 : s)
         go [] (n:_) = Just n
         go _ _ = Nothing
```

And we can try it out on one of our examples from above.

``` active haskell
data RPN = Val Integer
         | Plus
         | Minus
         | Times
         | Divide
         deriving(Show)
         
type RPNExpr = [RPN]

eval :: RPNExpr -> Maybe Integer
eval e = go e [] where
         go (Val n : es) s = go es (n:s)
         go (Plus : es) (n1:n2:s) = go es (n2 + n1 : s)
         go (Minus : es) (n1:n2:s) = go es (n2 - n1 : s)
         go (Times : es) (n1:n2:s) = go es (n2 * n1 : s)
         go (Divide : es) (n1:n2:s) = go es (n2 `div` n1 : s)
         go [] (n:_) = Just n
         go _ _ = Nothing
-- show
expr = [Val 5, Val 1, Val 2, Plus, Val 4, Times, Plus, Val 3, Minus]
main = putStrLn $ show $ eval expr
-- /show
```

Running this example we find that we do get the same answer as before.

## An Arithmetic Machine

We found that we can evaluate arithmetic expressions written in RPN easily, and that the evaluation is heavily stack based. From our evaluation of these expressions we can devise a stack based machine for the evaluation of arithmetic expressions. When we were evaluating RPN expressions we had two main (classes of) instructions 

1. Push a value on the stack
2. Operate on the stack
    - Add
    - Subtract
    - Multiply
    - Divide
    
Let's represent these instructions by a Haskell data type.

```haskell
data Op = Push Integer
        | Add
        | Sub
        | Mul
        | Div
        deriving(Show)
```

This will be our "assembly language" for arithmetic expressions. A program in this language is just a list of Ops.

```haskell
type Program = [Op]
```

Remeber the eval program we wrote above, let's use it as a model to write a function, run, which will run a program in our arithmetic assembly language.

```haskell
run :: Program -> Maybe Integer
run p = go p [] where
         go (Push n : es) s = go es (n:s)
         go (Add : es) (n1:n2:s) = go es (n2 + n1 : s)
         go (Sub : es) (n1:n2:s) = go es (n2 - n1 : s)
         go (Mul : es) (n1:n2:s) = go es (n2 * n1 : s)
         go (Div : es) (n1:n2:s) = go es (n2 `div` n1 : s)
         go [] (n:_) = Just n
         go _ _ = Nothing
```

## Representing Arithmetic

Let's take a little step back and remember our goal. We want to create a program that can evaluate arithmetic expressions that look like "3*(2+5)-8/4". Right now we can only evaluate expressions that look like an abstrack version of "3 2 5 + * 8 4 / -". We can't parse such expressions, but we can represent them in as values of a data type in Haskell and then evaluate them. 

Before we can even begin to think about parsing arithmetic expressions we first need to represent them by a type in Haskell. One possible way to accomplish this is 

```haskell
data ArithExpr = ValE Integer
               | AddE ArithExpr ArithExpr
               | SubE ArithExpr ArithExpr
               | MulE ArithExpr ArithExpr
               | DivE ArithExpr ArithExpr
               deriving(Show)
```


This data type resembles a tree that will represent a parsed arithmetic expression. To accomplish our ultimate goal we need to be able to turn this tree representation into a `Program` in our "arithmetic assembly language" that we can `run` on our arithmetic machine.

## Compilation

The step of transforming from the tree representation into an assembly language program is essentially compilation. Let's write a function that compiles `ArithExpr` trees into `Progrsm`s. As before we will use an intermediary list to hold the the assembly program that we will build up from a tree.

```haskell
compile :: ArithExpr -> Program
compile t = go t [] where
            go (ValE n) s = (Push n) : s
            go (AddE l r) s = go l $ go r (Add : s)
            go (SubE l r) s = go l $ go r (Sub : s)
            go (MulE l r) s = go l $ go r (Mul : s)
            go (DivE l r) s = go l $ go r (Div : s)
```

## A Grammar for Arithmetic

We can now compile arithmetic expressions (represented as trees) to run on our arithmetic machine, but we still can't yet deal with expressions that look like "1+2*3". To deal with these expressions we need to parse strings like that into `ArithExpr` trees which we can compile and run.

In order to parse strings that represent `ArithExpr`s we first need a grammar for arithmetic expressions. To save some time fussing over things like operator precedence and ambiguity we will write a parser for the following grammar.

    <Expr>    ::= <Expr> + <Term>
    		   |  <Expr> - <Term>
    		   |  <Term>

    <Term>    ::= <Term> * <Factor>
    		   |  <Term> / <Factor>
    		   |  <Factor>

    <Factor>  ::= ( <Expr> )
               | <Integer>

    <Integer> ::= <Digit>+

    <Digit>   ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
    

## Parser Combinators

We're going to build a parser for arithmetic expressions out of parser combinators, to start with we need a type for parsers.

```haskell
newtype Parser s t = P {runP :: [s] -> [(t,[s])]}
```

This says that a parser is essentially a function which takes a list of tokens such as characters or any other symbol, and returns a list of parses represented by a pair of a parsed value t and the remainder of the string s.


### Parsing Symbols

To parse arithmetic expressions we need to be able parse the single character operators '+','-','*','/'. So we need a parser that will parse single characters (or symbols).

```haskell
pSym :: Eq s => s -> Parser s s
pSym a = P $ \inp -> case inp of 
                     (s:ss) | s == a -> [(s,ss)]
                     otherwise       -> []
```

The `pSym` parser "succeeds" when the symbol it is looking for is at the head of the string it is parsing, otherwise it fails and returns an empty parse. When it succeeds it returns the symbol it was looking for and the remainder of the input string in a pair. Run the following code to see an example of this.

``` active haskell
newtype Parser s t = P {runP :: [s] -> [(t,[s])]}

pSym :: Eq s => s -> Parser s s
pSym a = P $ \inp -> case inp of 
                     (s:ss) | s == a -> [(s,ss)]
                     otherwise       -> []

-- show
pAdd = pSym '+'
main = putStrLn $ show $ runP pAdd $ "+31" 
-- /show
```

### Success and Failure

Sometimes we may have a need for parsers that either always succeed or always fail without consuming any input, we call these parsers `pReturn` and `pFail` respectively.

```haskell
pReturn :: a -> Parser s a
pReturn a = P $ \inp -> [(a,inp)]
```

This parser always suceeds, producing the parsed value a and leaving the input string untouched. 

```haskell
pFail :: Parser s a
pFail = P $ const []
```

The `pFail` parser always fails returning the empty list to signify that there is no successful parse.

## Combining Parsers

Very often we will want ways to combine parsers to create parsers for longer or sequenced expressions. 


### Alternation

Let's start with a simple way to combine two parsers, alternation. Suppose we have two parsers, one that can parse single digits and another that can parse single letters. Call these parsers `pDigit` and `pAlpha` respectively, we'll see how to actually write these later, but for now let's imagine they're given to us. 

Often programming languages allow for variable names to contain either letters or numbers (subject to some constraints we won't worry about right now), so we might want a parser that will succeed if it encounters either a letter or a digit. Instead of writing another parser from scratch, it would be nice if we could somehow combine the parsers `pDigit` and `pAlpha` into a parser that successfully matches either a digit or a letter. 

Recall the definition of our `Parser` type.

```haskell
newtype Parser s t = P {runP :: [s] -> [(t,[s])]}
```

A `Parser` is merely a wrapper around a function that takes a list of tokens as input and returns a list of pairs consisting of a parsed value and the remainder of the input. So if we want to construct a parser which will combine two parsers and succeed whenever one of them does how should we accomplish that?

Given an input, let's try to run both parsers on it. Suppose we have the string "foo", what happens when we run `pDigit` and `pAlpha` on it?

```active haskell
import Data.Char

newtype Parser s a = P {runP :: [s] ->[(a,[s])]}

pSatisfy :: (a -> Bool) -> Parser a a
pSatisfy p = P $ \inp -> case inp of
                        (s:ss) | p s -> [(s,ss)]
                        otherwise -> []
pDigit = pSatisfy isDigit
pAlpha = pSatisfy isAlpha

-- show
p1 = runP pDigit $ "foo"
p2 = runP pAlpha $ "foo"

main = do
    putStrLn $ show p1
    putStrLn $ show p2
-- /show
```
As expected `pDigit` returns an empty list since it failed to parse the string, and `pAlpha` successfully parsed the letter `'f'` giving `[('f',"oo")]` as its result. If we were to combine these parsers into one which accepts either a letter or a digit then we should probably expect `[('f',"oo")]` as a result again since only the letter `'f'` will be recognized.

It might seem obvious then to create a parser (from two parsers) that suceeds when at least one of them does, we should just run both and concatenate the resulting lists of parses. The Haskell code for this is essentially a transliteration of the english statement:

```haskell
infixr 3 <|>
(<|>) :: Parser s a -> Parser s a -> Parser s a
P p1 <|> P p2 = P $ \inp -> p1 inp ++ p2 inp
```

Let's try the above example again with our combined parser this time.

```active haskell
import Data.Char

newtype Parser s a = P {runP :: [s] ->[(a,[s])]}

infixr 3 <|>
(<|>) :: Parser s a -> Parser s a -> Parser s a
P p1 <|> P p2 = P $ \inp -> p1 inp ++ p2 inp

pSatisfy :: (a -> Bool) -> Parser a a
pSatisfy p = P $ \inp -> case inp of
                        (s:ss) | p s -> [(s,ss)]
                        otherwise -> []
pDigit = pSatisfy isDigit
pAlpha = pSatisfy isAlpha

-- show
pAlphaOrDigit = pAlpha <|> pDigit

p = runP pAlphaOrDigit $ "foo"

main = putStrLn $ show p
-- /show
```
That should give us the result we expected: `[('f',"oo")]`

If we want to choose between many parsers, or a list of parsers we can implement such a function easily using the `<*>` operator.

```haskell
pChoice :: [Parser s a] -> Parser s a
pChoice ps = foldr (<*>) pFail ps
```

Remember that `pFail` is the parser that always fails and returns an empty list, `pChoice` will try each parser in the list `ps` in turn building up a list of successes, and if they all fail then the parse result will be the empty list `[]`.

## Sequencing

Our goal is to parse complex arithmetic expressions, but in order to understand how to seqeuence parsers together let's simplify the arithmetic expressions we wish to parse down to the following grammar:

    <Expr> = <Digit> '+' <Digit>
    <Digit> = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0'
    
That is expressions which are just the a single digit followed by a '+' and another single digit.

A Haskell data type to represent the Abstract Syntax Tree for this grammar could be

```haskell
data PlusExpr = Plus Digit Digit
data Digit = Digit Int
```

Somehow we wish to turn an expression such as "1+7" into `Plus (Digit 1) (Digit 7)`.

### Satisfying Parsers

Let's sidestep for a second to reanalyze the problem of parsing digits. We have a parser that can parse a single character `pSym` and we could build a parser that matches a number of characters using `pChoice` so one implementation of `pDigit` could be 

```haskell
pDigit = pChoice $ map pSym ['0','1','2','3','4','5','6','7','8','9']
```

That would work, but is a pretty ugly implementation, in `Data.Char` there is a function `isDigit` which returns True when applied to a character representing a digit. It would be nice if we could write a parser that turns such a predicate into a parser for the values it recognizes. Recall the definition of `pSym`

```haskell
pSym :: (Eq s) => s -> Parser s s
pSym a = P $ \inp -> case inp of
                     (s:ss) | s == a -> [(s,ss)]
                     otherwise       -> []
```

`pSym` succeeds if the input starts with an `a`, if we replace the equality check with a call to our predicate then we would have a parser that succeeds when the start of the input satisfies a condition, such as being a digit.

```haskell
pSatisfy :: (a -> Bool) -> Parser a a
pSatisfy p = P $ \inp -> case inp of
                         (s:ss) | p s -> [(s,ss)]
                         otherwise    -> []
```

Then a better defintion for `pDigit` using the <hoogle>isDigit</hoogle> function from `Data.Char` would be 

```haskell
import Data.Char
pDigit = pSatisfy isDigit
```

Let's run the example we had before and make sure it still works leveraging our new `pSatisfy` tool.

```active haskell
-- show
import Data.Char
-- /show
newtype Parser s a = P {runP :: [s] ->[(a,[s])]}

infixr 3 <|>
(<|>) :: Parser s a -> Parser s a -> Parser s a
P p1 <|> P p2 = P $ \inp -> p1 inp ++ p2 inp

pSatisfy :: (a -> Bool) -> Parser a a
pSatisfy p = P $ \inp -> case inp of
                        (s:ss) | p s -> [(s,ss)]
                        otherwise -> []

-- show
pDigit = pSatisfy isDigit
pAlpha = pSatisfy isAlpha

pAlphaOrDigit = pAlpha <|> pDigit

p = runP pAlphaOrDigit $ "foo"

main = putStrLn $ show p
-- /show
```

### From Strings to Data Types

So far so good, we can easily parse single digits and '+', so we just need to figure out how to sequence these parsers in order to parse our simplified arithemtic expressions.

Our Haskell representation of digits is 
```haskell
data Digit = Digit Integer
```
And if we try to parse a string such as `"1"` with `pDigit` what we get back is `[('1',"")]`, this isn't quite what we want. If we want to somehow get a `Digit` back as a result we need to somehow transform the parse result from a `Char` into a `Digit`, in other words a function of type `Char -> Digit`.

```haskell
charToDigit :: Char -> Digit
charToDigit c = undefined
```

If we could turn the `Char` `c` into a `String` we could leverage Haskell's builtin `read` function to convert it to the `Integer` required by the `Digit` constructor. Fortunately it's easy to turn a `Char` into a string.

```haskell
charToDigit :: Char -> Digit
charToDigit c = Digit $ read [c]
```

This function will have problems if c isn't actually a digit, but our parser for digits should prevent it from being called on a character that is not a digit.

