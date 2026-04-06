{-Розбити список на два списки відповідно до умови – “бути чи не бути факторіалом деякого числа”.-}
splitFactorials :: [Int] -> ([Int], [Int])
splitFactorials [] = ([], [])
splitFactorials (x:xs) =
  let (factorials, others) = splitFactorials xs
  in if isFactorial x 2
       then (x:factorials, others)
       else (factorials, x:others)
  where
    isFactorial :: Int -> Int -> Bool
    isFactorial 1 k = True
    isFactorial n k
      | n <= 0 = False
      | n < k = False
      | mod n k == 0 = isFactorial (div n k) (k + 1)
      | otherwise = False
main :: IO ()
main = do
    putStrLn "--- Test 1 ---"
    putStrLn "Input: [1, 2, 3, 4, 5, 6, 24, 25]"
    putStrLn "Expected: ([1, 2, 6, 24],[3, 4, 5, 25])"
    putStrLn "Result:"
    print (splitFactorials [1, 2, 3, 4, 5, 6, 24, 25])

    putStrLn "\n--- Test 2 ---"
    putStrLn "Input: [2, 6, 24, 120]"
    putStrLn "Expected: ([2, 6, 24, 120],[])"
    putStrLn "Result:"
    print (splitFactorials [2, 6, 24, 120])

    putStrLn "\n--- Test 3  ---"
    putStrLn "Input: [-5, 0, 4, 10]"
    putStrLn "Expected: ([],[-5, 0, 4, 10])"
    putStrLn "Result:"
    print (splitFactorials [-5, 0, 4, 10])

    putStrLn "\n--- Test 4 ---"
    putStrLn "Input: []"
    putStrLn "Expected: ([],[])"
    putStrLn "Result:"
    print (splitFactorials [])
{-
    putStrLn "Enter list of numbers:"
    input <- getLine
    
    let numbers = map read (words input) :: [Int]
    putStrLn "\nResult:"
    print (splitFactorials numbers)
-}