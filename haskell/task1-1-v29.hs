{-Залишити у першому списку елементи, що входять у другий список по одному разу-}
onlyElements :: [Int] -> [Int] -> [Int]
onlyElements [] _ = []
onlyElements (x:xs) ys =
    if count x ys == 1
    then x : onlyElements xs ys
    else onlyElements xs ys
  where
    count :: Int -> [Int] -> Int
    count _ [] = 0
    count a (y:ys) =
        if a == y
        then 1 + count a ys
        else count a ys

main :: IO ()
main = do
    putStrLn "Enter elements of the first list (e.g., 1 2 3):"
    line1 <- getLine
    putStrLn "Enter elements of the second list (e.g., 2 4 1):"
    line2 <- getLine
    
    let list1 = map read (words line1) :: [Int]
    let list2 = map read (words line2) :: [Int]
    let result = onlyElements list1 list2
    
    putStrLn "\nResult:"
    print result
    {-
    putStrLn "Test 1:"
    print (onlyElements [1, 2, 3, 4] [1, 1, 2, 4, 5])

    putStrLn "\nTest 2:"
    print (onlyElements [10, 20, 30] [30, 20, 10, 40])

    putStrLn "\nTest 3:"
    print (onlyElements [5, 6, 7] [5, 5, 8, 9])

    putStrLn "\nTest 4:"
    print (onlyElements [1, 2, 3] [])-}