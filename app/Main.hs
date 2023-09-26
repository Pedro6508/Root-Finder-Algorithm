module Main where

import Text.Read.Lex (Lexeme(String, Number, Char))
import Data.Char (toTitle)
import Text.ParserCombinators.ReadP (string)
import Text.Printf (printf, formatString)
import Data.Foldable (Foldable(toList))

eps :: Double
eps = 0.01
coef f a b = (f b - f a)/(b - a)
derv f x = coef f x (x + eps) 

linearEq f a b x = coef f a b *(x - a) + f a  

inverLinearEq f a b y = (y - f a)/coef f a b + a

sq x = x*x
eq a b x = sq (x - a) - b
func = eq 2 4 
-- s = linearEq func 2 4

linearRoot :: Fractional a => (a -> a) -> a -> a -> a
linearRoot f a b = do
  let h = linearEq f a b
  (- h a)/coef h a b + a

-- { f(b) > 0 > f(a); b > a; f is continuous in (a, b) ; eps > 0 }


anyFuncRoot f g a b = do 
  let inverS = inverLinearEq g a b
  let zx = inverS 0
  let zy = g zx
  let sc = abs (coef f a b)
  let fs x = f x * sc
  let a1 = (b + zx)/2
  let b1 = (zx + a)/2
  print (zx, zy, a, b)

  if (eps > zy && zy > -eps) || isNaN zy then 
    print (Just zx)
  else if zy < 0 then
    anyFuncRoot f fs zx b
  else 
    anyFuncRoot f fs a zx

main = do
  putStrLn "\tLinearEq between (a, f(a)) & (b, f(b))"
  putStrLn "Number of attempts:"
  print (linearRoot func 3 5)
  let test x = sq (sq x - x) + 2*sq (x-2) - x
  anyFuncRoot test test 1.4 2 
  -- times <- getLine
  -- let 
  --   loop 0 = return ()
  --   loop i = do
  --     raw_x <- getLine
  --     let x = (read raw_x :: Double)
  --     putStr ("f'(" ++ raw_x ++ ") = ")
  --     let result = s x
  --     print result
  --     loop(i-1)
  -- loop(read times :: Integer)
  
  
