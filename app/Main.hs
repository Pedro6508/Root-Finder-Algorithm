module Main where

import Text.Read.Lex (Lexeme(String, Number, Char))
import Data.Char (toTitle)
import Text.ParserCombinators.ReadP (string, get)
import Text.Printf (printf, formatString)
import Data.Foldable (Foldable(toList))
import Data.Functor.Classes (eq2)

-- eps -> Epsilon 
eps = 0.01

-- coef -> Slope of Line AB
coef f a b = (f b - f a)/(b - a)

-- derv -> Derivative Approximation
derv f x = coef f x (x + eps) 

-- linearEq -> Line AB Equation
linearEq f a b x = coef f a b *(x - a) + f a  

-- sq(x) -> xˆ2
sq x = x*x

-- eq2nd -> 2nd Degree Equation
eq2nd a b x = sq (x - a) - b

-- linearRoot -> root-finder for linear equation
linearRoot :: Fractional a => (a -> a) -> a -> a -> a
linearRoot f a b = do
  let h = linearEq f a b
  (- h a)/coef h a b + a

-- mid -> midpoint between AB and more 
mid f a b = do
  let tg = coef f a b
  let g x = tg * f x 
  let mdot = (b + a)/2

  if g mdot > 0 then
    (g, a, mdot)
  else 
    (g, mdot, b)

findRoot f a b = rfindRoot f (min a b) (max a b)

-- { f(b) > 0 > f(a); b > a; f is continuous in (a, b) ; eps > 0 }
rfindRoot :: (Double -> Double) -> Double -> Double -> IO ()
rfindRoot f a b = do 
  let (g, x, y) = mid f a b
  let r = (y + x)/2
  let diff = abs (y-x)

  print (x, y, r)
  if (diff < eps) || isNaN r then 
    print (Just r)
  else 
    rfindRoot g x y
-- { f(b) > 0 > f(a); b > a; g is continuous in (a, b) ; |r - x0| < eps  }
main = do
  putStrLn " Root-finding Algorithm"
  putStrLn "- f(x) = (x - a)ˆ2 - b"
  putStrLn "- g(x) = (x - c)ˆ2 - d"
  putStrLn "- Eq -> f(g(x))"
  putStrLn "a = "
  raw_a <- getLine
  putStrLn "b = "
  raw_b <- getLine 
  putStrLn "c = "
  raw_c <- getLine 
  putStrLn "d = "
  raw_d <- getLine 
  let a = read raw_a :: Double
  let b = read raw_b :: Double
  let c = read raw_c :: Double
  let d = read raw_d :: Double
  let f x = eq2nd c d (eq2nd a b x) 
  findRoot f 0.5 4.25 

  
