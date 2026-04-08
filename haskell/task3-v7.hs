import Data.List (nub)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S

type State = Int
type Symbol = Char
type InputWord = String
type Transition = ((State, Symbol), [State])

data Automaton = Automaton
  { states :: [State]
  , alphabet :: [Symbol]
  , startState :: State
  , finalStates :: [State]
  , transitions :: [Transition]
  }

data Result = Result
  { xPart :: String
  , wPart :: String
  , yPart :: String
  , wholeWord :: String
  }
instance Show Result where
  show r = "xwy = \"" ++ wholeWord r ++ "\" (x=\"" ++ xPart r ++ "\", w=\"" ++ wPart r ++ "\", y=\"" ++ yPart r ++ "\")"
getNextStates :: Automaton -> State -> Symbol -> [State]
getNextStates automaton q c =
  fromMaybe [] (lookup (q, c) (transitions automaton))

runWord :: Automaton -> [State] -> InputWord -> [State]
runWord _ current [] = nub current
runWord automaton current (c:cs) =
  let next = nub
        [ q'
        | q <- current
        , q' <- getNextStates automaton q c
        ]
  in runWord automaton next cs

reachableFromStartWithWord :: Automaton -> [(State, String)]
reachableFromStartWithWord automaton =
  bfs [(startState automaton, "")] S.empty
  where
    bfs :: [(State, String)] -> S.Set State -> [(State, String)]
    bfs [] _ = []
    bfs ((q, path):queue) visited
      | S.member q visited = bfs queue visited
      | otherwise =
          let visited' = S.insert q visited
              next =
                [ (q', path ++ [c])
                | c <- alphabet automaton
                , q' <- getNextStates automaton q c
                , not (S.member q' visited')
                ]
          in (q, path) : bfs (queue ++ next) visited'

getIncomingTransitions :: Automaton -> State -> [(State, Symbol)]
getIncomingTransitions automaton q =
  [ (from, c)
  | ((from, c), to) <- transitions automaton
  , q `elem` to
  ]

reachableToFinalWithWord :: Automaton -> [(State, String)]
reachableToFinalWithWord automaton =
  bfs [ (f, "") | f <- finalStates automaton ] S.empty
  where
    bfs :: [(State, String)] -> S.Set State -> [(State, String)]
    bfs [] _ = []
    bfs ((q, path):queue) visited
      | S.member q visited = bfs queue visited
      | otherwise =
          let visited' = S.insert q visited
              next =
                [ (prevState, c : path)
                | (prevState, c) <- getIncomingTransitions automaton q
                , not (S.member prevState visited')
                ]
          in (q, path) : bfs (queue ++ next) visited'

solveXWY :: Automaton -> InputWord -> Maybe Result
solveXWY automaton w =
  listToMaybe
    [ Result
        { xPart = x
        , wPart = w
        , yPart = y
        , wholeWord = x ++ w ++ y
        }
    | (q, x) <- reachableFromStartWithWord automaton
    , qAfter <- runWord automaton [q] w
    , Just y <- [lookup qAfter (reachableToFinalWithWord automaton)]
    ]

--Тестова частина    
--Основний тестовий автомат
exampleAutomaton :: Automaton
exampleAutomaton =
  Automaton
    { states = [0, 1, 2, 3]
    , alphabet = ['a', 'b']
    , startState = 0
    , finalStates = [3]
    , transitions =
        [ ((0, 'a'), [1])
        , ((0, 'b'), [0])
        , ((1, 'a'), [1])
        , ((1, 'b'), [2])
        , ((2, 'a'), [2])
        , ((2, 'b'), [3])
        , ((3, 'a'), [3])
        , ((3, 'b'), [3])
        ]
    }
--Автомат з одним станом
epsilonAutomaton :: Automaton
epsilonAutomaton =
  Automaton
    { states = [0]
    , alphabet = ['a']
    , startState = 0
    , finalStates = [0]
    , transitions =
        [ ((0, 'a'), [0]) ]
    }
--Лінійний автомат, що приймає виключно слово "cab"
prefixAutomaton :: Automaton
prefixAutomaton =
  Automaton
    { states = [0, 1, 2, 3]
    , alphabet = ['a', 'b', 'c']
    , startState = 0
    , finalStates = [3]
    , transitions =
        [ ((0, 'c'), [1])
        , ((1, 'a'), [2])
        , ((2, 'b'), [3])
        ]
    }
--Лінійний автомат, що приймає виключно слово "abc"
suffixAutomaton :: Automaton
suffixAutomaton =
  Automaton
    { states = [0, 1, 2, 3]
    , alphabet = ['a', 'b', 'c']
    , startState = 0
    , finalStates = [3]
    , transitions =
        [ ((0, 'a'), [1])
        , ((1, 'b'), [2])
        , ((2, 'c'), [3])
        ]
    }
--Автомат із двома ізольованими частинами. Зі стартового стану (0) неможливо потрапити у фінальний (1).
rejectAutomaton :: Automaton
rejectAutomaton =
  Automaton
    { states = [0, 1]
    , alphabet = ['a', 'b']
    , startState = 0
    , finalStates = [1]
    , transitions =
        [ ((0, 'a'), [0])
        , ((0, 'b'), [0])
        , ((1, 'a'), [1])
        , ((1, 'b'), [1])
        ]
    }

-- Автомат для перевірки роботи в нескінченному циклі
loopAutomaton :: Automaton
loopAutomaton =
  Automaton
    { states = [0]
    , alphabet = ['a']
    , startState = 0
    , finalStates = [0]
    , transitions =
        [ ((0, 'a'), [0]) ]
    }

-- Автомат, де немає шляху до фіналу
deadEndAutomaton :: Automaton
deadEndAutomaton =
  Automaton
    { states = [0, 1]
    , alphabet = ['a']
    , startState = 0
    , finalStates = [0]
    , transitions =
        [ ((0, 'a'), [1]) ]
    }

-- Автомат із коротким шляхом, де w довше за сам автомат
shortPathAutomaton :: Automaton
shortPathAutomaton =
  Automaton
    { states = [0, 1]
    , alphabet = ['a']
    , startState = 0
    , finalStates = [1]
    , transitions =
        [ ((0, 'a'), [1]) ]
    }
main :: IO ()
main = do
  putStrLn "===== Test 1 ====="
  putStrLn "Automaton: exampleAutomaton"
  putStrLn "w = \"ab\""
  putStrLn "Expected: there exists at least one word of the form xwy"
  print (solveXWY exampleAutomaton "ab")

  putStrLn "\n===== Test 2 ====="
  putStrLn "Automaton: epsilonAutomaton"
  putStrLn "w = \"\""
  putStrLn "Expected: the empty word should work, result like Just Result {xPart = \"\", wPart = \"\", yPart = \"\", wholeWord = \"\"}"
  print (solveXWY epsilonAutomaton "")

  putStrLn "\n===== Test 3 ====="
  putStrLn "Automaton: prefixAutomaton"
  putStrLn "w = \"ab\""
  putStrLn "Expected: a non-empty prefix x is needed, for example the word \"cab\""
  print (solveXWY prefixAutomaton "ab")

  putStrLn "\n===== Test 4 ====="
  putStrLn "Automaton: suffixAutomaton"
  putStrLn "w = \"ab\""
  putStrLn "Expected: a non-empty suffix y is needed, for example the word \"abc\""
  print (solveXWY suffixAutomaton "ab")

  putStrLn "\n===== Test 5 ====="
  putStrLn "Automaton: rejectAutomaton"
  putStrLn "w = \"ab\""
  putStrLn "Expected: no solution exists, so the result should be Nothing"
  print (solveXWY rejectAutomaton "ab")

  putStrLn "\n===== Test 6====="
  putStrLn "Automaton: exampleAutomaton"
  putStrLn "w = \"c\""
  putStrLn "Expected: the symbol is not in the alphabet and there are no transitions, so the result should be Nothing"
  print (solveXWY exampleAutomaton "c")
  putStrLn "\n===== Test 7 ====="
  putStrLn "Automaton: loopAutomaton"
  putStrLn "w = \"aa\""
  putStrLn "Expected: short result despite the loop"
  print (solveXWY loopAutomaton "aa")

  putStrLn "\n===== Test 8 ====="
  putStrLn "Automaton: deadEndAutomaton"
  putStrLn "w = \"a\""
  putStrLn "Expected: w can be read, but we get stuck in state 1 with no path to final state 0. Result: Nothing"
  print (solveXWY deadEndAutomaton "a")

  putStrLn "\n===== Test 9 ====="
  putStrLn "Automaton: shortPathAutomaton"
  putStrLn "w = \"aaa\""
  putStrLn "Expected: the word w is longer than any path in the automaton. Result: Nothing"
  print (solveXWY shortPathAutomaton "aaa")