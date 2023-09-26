module Main where

import Text.Read.Lex (Lexeme(String, Number, Char))
import Data.Char (toTitle)
import Text.ParserCombinators.ReadP (string)
import Text.Printf (printf, formatString)
import Data.Foldable (Foldable(toList))

eps = 0.01
coef f a b = (f b - f a)/(b - a)
derv f x = coef f x (x + eps) 

linearEq f a b x = coef f a b *(x - a) + f a  

inverLinearEq f a b y = (y - f a)/coef f a b + a

sq x = x*x
eq2ndDeg a b x = sq (x - a) - b
-- s = linearEq eq2nddeg 2 4

linearRoot :: Fractional a => (a -> a) -> a -> a -> a
linearRoot f a b = do
  let h = linearEq f a b
  (- h a)/coef h a b + a

mid f a b = do
  let tg = coef f a b
  let s x = tg*(x - a) + f a
  let g x = tg * f x 
  let mdot = (b + a)/2

  if g mdot > 0 then
    (g, a, mdot)
  else 
    (g, mdot, b)

findRoot f a b = rfindRoot f (min a b) (max a b)

-- { f(b) > 0 > f(a); b > a; f is continuous in (a, b) ; eps > 0 }
rfindRoot f a b = do 
  let (g, x, y) = mid f a b
  let r = (y + x)/2
  let diff = abs (y-x)

  print (x, y, r)
  if (diff < eps) || isNaN r then 
    print (Just r)
  else 
    rfindRoot g x y

main = do
  putStrLn "\tRoot Finder Algorithm"
  let 
  findRoot eq2ndDeg 1.4 6 

  
