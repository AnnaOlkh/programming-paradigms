module Main where

n :: Double
n = 14.0


safeLog10 :: Double -> Maybe Double
safeLog10 value
  | value > 0 = Just (logBase 10 value)
  | otherwise = Nothing


safeSqrt :: Double -> Maybe Double
safeSqrt value
  | value >= 0 = Just (sqrt value)
  | otherwise = Nothing


f8 :: Double -> Maybe Double
f8 x =
  safeLog10 (x - 1 / n)

f9 :: Double -> Maybe Double
f9 x =
  safeSqrt (x - 1 / n)

f4 :: Double -> Maybe Double
f4 x
  | x + n > 0 = Just (1 / sqrt (x + n))
  | otherwise = Nothing


-- f8(f9(f4(x)))

superpositionDo :: Double -> Maybe Double
superpositionDo x = do
  valueAfterF4 <- f4 x
  valueAfterF9 <- f9 valueAfterF4
  f8 valueAfterF9


superpositionBind :: Double -> Maybe Double
superpositionBind x =
  f4 x >>= f9 >>= f8

------------------------------------

f4Bin :: Double -> Double -> Maybe Double
f4Bin x nArg
  | x + nArg > 0 = Just (1 / sqrt (x + nArg))
  | otherwise = Nothing

-- f4Bin (f8(x)) (f9(x))

binarySuperpositionDo :: Double -> Maybe Double
binarySuperpositionDo x = do
  firstValue <- f8 x
  secondValue <- f9 x
  f4Bin firstValue secondValue

binarySuperpositionBind :: Double -> Maybe Double
binarySuperpositionBind x =
  f8 x >>= \firstValue ->
  f9 x >>= \secondValue ->
  f4Bin firstValue secondValue


printTest :: String -> Maybe Double -> IO ()
printTest name result =
  putStrLn (name ++ " = " ++ show result)


main :: IO ()
main = do
  putStrLn "Unary functions with constant n:"
  printTest "f8 1.0" (f8 1.0)
  printTest "f8 0.1" (f8 0.1)

  printTest "f9 1.0" (f9 1.0)
  printTest "f9 0.05" (f9 0.05)

  printTest "f4 0.0" (f4 0.0)
  printTest "f4 (-14.0)" (f4 (-14.0))

  putStrLn ""
  putStrLn "Superposition f8(f9(f4 x)):"
  printTest "superpositionDo 0.0" (superpositionDo 0.0)
  printTest "superpositionBind 0.0" (superpositionBind 0.0)
  printTest "superpositionDo (-14.0)" (superpositionDo (-14.0))
  printTest "superpositionBind (-14.0)" (superpositionBind (-14.0))

  putStrLn ""
  putStrLn "Binary function:"
  printTest "f4Bin 1.0 14.0" (f4Bin 1.0 14.0)
  printTest "f4Bin (-14.0) 14.0" (f4Bin (-14.0) 14.0)

  putStrLn ""
  putStrLn "Binary superposition f4Bin (f8 x) (f9 x):"
  printTest "binarySuperpositionDo 2.0" (binarySuperpositionDo 2.0)
  printTest "binarySuperpositionBind 2.0" (binarySuperpositionBind 2.0)
  printTest "binarySuperpositionDo 0.1" (binarySuperpositionDo 0.1)
  printTest "binarySuperpositionBind 0.1" (binarySuperpositionBind 0.1)