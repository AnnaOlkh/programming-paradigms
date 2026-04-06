{-Знайти всі “нижні піки” списку та підрахувати їх кількість. Елемент списку є нижнім піком, якщо він менший
за своїх наявних сусідів. Вважати, що список складається з різних елементів. Наприклад, у списку
[5,4,2,8,3,1,6,9,5] нижні піки та їх позиції такі: (2,3), (1,6), (5,9).-}
lowerPeaks :: [Int] -> ([(Int, Int)], Int)
lowerPeaks xs =
    let peaks = getPeaks Nothing xs 1
    in (peaks, countList peaks)
  where
    getPeaks :: Maybe Int -> [Int] -> Int -> [(Int, Int)]
    getPeaks _ [] _ = []
    getPeaks Nothing [_] _ = []
    
    -- останній елемент
    getPeaks (Just left) [x] pos =
        [(x, pos) | x < left]
        
    -- перший елемент
    getPeaks Nothing (x:y:xs) pos =
        let rest = getPeaks (Just x) (y:xs) (pos + 1)
        in if x < y
           then (x, pos) : rest
           else rest
           
    -- інші
    getPeaks (Just left) (x:y:xs) pos =
        let rest = getPeaks (Just x) (y:xs) (pos + 1)
        in if x < left && x < y
           then (x, pos) : rest
           else rest
    countList :: [a] -> Int
    countList [] = 0
    countList (_:ts) = 1 + countList ts

main :: IO ()
main = do
    putStrLn "--- Test 1 ---"
    putStrLn "Input: [5, 4, 2, 8, 3, 1, 6, 9, 5]"
    putStrLn "Expected: ([(2,3),(1,6),(5,9)],3)"
    putStrLn "Result:"
    print (lowerPeaks [5, 4, 2, 8, 3, 1, 6, 9, 5])

    putStrLn "\n--- Test 2 ---"
    putStrLn "Input: [10, 8, 6, 4]"
    putStrLn "Expected: ([(4,4)],1)"
    putStrLn "Result:"
    print (lowerPeaks [10, 8, 6, 4])

    putStrLn "\n--- Test 3  ---"
    putStrLn "Input: [2, 5, 8, 12]"
    putStrLn "Expected: ([(2,1)],1)"
    putStrLn "Result:"
    print (lowerPeaks [2, 5, 8, 12])

    putStrLn "\n--- Test 4 ---"
    putStrLn "Input: []"
    putStrLn "Expected: ([],0)"
    putStrLn "Result:"
    print (lowerPeaks [])

    putStrLn "\n--- Test 5 ---"
    putStrLn "Input: [1]"
    putStrLn "Expected: ([],0)"
    putStrLn "Result:"
    print (lowerPeaks [1])

    putStrLn "\n--- Test 6 ---"
    putStrLn "Input: [7, 3]"
    putStrLn "Expected: ([(3,2)],1)"
    putStrLn "Result:"
    print (lowerPeaks [7, 3])

    putStrLn "\n--- Test 7 ---"
    putStrLn "Input: [3, 8, 10, 6, 2]"
    putStrLn "Expected: ([(3,1), (2,5)],2)"
    putStrLn "Result:"
    print (lowerPeaks [3, 8, 10, 6, 2])

{-
main :: IO ()
main = do
    putStrLn "Enter list of different numbers (e.g. 5 4 2 8 3 1 6 9 5):"
    input <- getLine
    
    let numbers = map read (words input) :: [Int]
    
    putStrLn "\nResult (Peaks, Count):"
    print (lowerPeaks numbers)
-}