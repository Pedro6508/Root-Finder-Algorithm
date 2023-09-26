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
sq x = x*x
eq a b x = sq (x - a) - b
func = eq 2 4 
s = linearEq func 2 4

linearRoot :: Fractional a => (a -> a) -> a -> a -> a
linearRoot f a b = do
  let h = linearEq f a b
  (- h a)/coef h a b + a

-- { f(b) > 0 > f(a); b > a; f is continuous in (a, b) ; eps > 0 }

anyFuncRoot :: (Double -> Double) -> Double -> Double -> IO ()
anyFuncRoot f a b = do 
  -- let fs x = (f x) * ((linearEq f a b) x)
  let zx = linearRoot f a b
  let zy = f zx
  -- let sc = coef f a b
  -- let p = (1 + f a)/(2*sc)
  -- let q = (- f a)/(2*sc)
  print (zx, zy, a, b)

  if (eps > zy && zy > -eps) || isNaN zy then 
    print (Just zx)
  else if zy > 0 then
    anyFuncRoot f zx b
  else 
    anyFuncRoot f a zx

main = do
  putStrLn "\tLinearEq between (a, f(a)) & (b, f(b))"
  putStrLn "Number of attempts:"
  print (linearRoot func 3 5)
  let test x = sq (sq x - x) - x  
  anyFuncRoot test 5 3
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
  
  
