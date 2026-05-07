module Main where

import Data.Foldable (forM_)
import Data.List (foldl', intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type NonTerminal = String
type Terminal = Char
type TerminalWord = String

data Symbol
  = T Terminal
  | NT NonTerminal
  deriving (Eq, Ord, Show)

data Rule = Rule NonTerminal [Symbol]
  deriving (Eq, Ord, Show)

data Grammar = Grammar
  { nonTerminals :: [NonTerminal]
  , terminals :: [Terminal]
  , startSymbol :: NonTerminal
  , rules :: [Rule]
  }
  deriving (Eq, Show)

type MinLengths = Map.Map NonTerminal Int
type MinWords = Map.Map NonTerminal (Set.Set TerminalWord)

data Result = Result
  { resultMinLength :: Maybe Int
  , resultMinWords :: Set.Set TerminalWord
  }
  deriving (Eq, Show)

-- Repeats calculation until the state stops changing.
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f state =
  let nextState = f state
  in if nextState == state
       then state
       else fixpoint f nextState

-- minlength(A)

symbolLength :: MinLengths -> Symbol -> Maybe Int
symbolLength _ (T _) =
  Just 1

symbolLength lengths (NT nt) =
  Map.lookup nt lengths

rhsLength :: MinLengths -> [Symbol] -> Maybe Int
rhsLength lengths rhs =
  fmap sum (traverse (symbolLength lengths) rhs)

stepMinLengths :: Grammar -> MinLengths -> MinLengths
stepMinLengths grammar currentLengths =
  foldl' update currentLengths (rules grammar)
  where
    update acc (Rule lhs rhs) =
      case rhsLength currentLengths rhs of
        Nothing ->
          acc

        Just candidate ->
          Map.insertWith min lhs candidate acc

computeMinLengths :: Grammar -> MinLengths
computeMinLengths grammar =
  fixpoint (stepMinLengths grammar) Map.empty

-- words_minlen(A)

symbolWords :: MinWords -> Symbol -> Set.Set TerminalWord
symbolWords _ (T c) =
  Set.singleton [c]

symbolWords wordsMap (NT nt) =
  Map.findWithDefault Set.empty nt wordsMap

concatWordSets :: [Set.Set TerminalWord] -> Set.Set TerminalWord
concatWordSets =
  foldl' combine (Set.singleton "")
  where
    combine acc current =
      Set.fromList
        [ left ++ right
        | left <- Set.toList acc
        , right <- Set.toList current
        ]

rhsWords :: MinWords -> [Symbol] -> Set.Set TerminalWord
rhsWords wordsMap rhs =
  concatWordSets (map (symbolWords wordsMap) rhs)

stepMinWords :: Grammar -> MinLengths -> MinWords -> MinWords
stepMinWords grammar lengths currentWords =
  foldl' update currentWords (rules grammar)
  where
    update acc (Rule lhs rhs) =
      case (Map.lookup lhs lengths, rhsLength lengths rhs) of
        (Just lhsLength, Just rhsLengthValue)
          | lhsLength == rhsLengthValue ->
              let generatedWords = rhsWords currentWords rhs
              in if Set.null generatedWords
                   then acc
                   else Map.insertWith Set.union lhs generatedWords acc

        _ ->
          acc

computeMinWords :: Grammar -> MinLengths -> MinWords
computeMinWords grammar lengths =
  fixpoint (stepMinWords grammar lengths) Map.empty

-------------------------------------------------
solve :: Grammar -> Map.Map NonTerminal Result
solve grammar =
  Map.fromList
    [ (nt, Result lengthValue wordsValue)
    | nt <- nonTerminals grammar
    , let lengthValue = Map.lookup nt lengths
    , let wordsValue = Map.findWithDefault Set.empty nt wordsMap
    ]
  where
    lengths = computeMinLengths grammar
    wordsMap = computeMinWords grammar lengths


formatLength :: Maybe Int -> String
formatLength Nothing =
  "does not exist"

formatLength (Just value) =
  show value

formatWord :: TerminalWord -> String
formatWord "" =
  "eps"

formatWord word =
  word

formatWords :: Set.Set TerminalWord -> String
formatWords wordsSet
  | Set.null wordsSet = "{}"
  | otherwise =
      "{ " ++ intercalate ", " (map formatWord (Set.toList wordsSet)) ++ " }"

printResults :: Grammar -> Map.Map NonTerminal Result -> IO ()
printResults grammar results =
  forM_ (nonTerminals grammar) $ \nt ->
    case Map.lookup nt results of
      Just (Result minLen minWords) -> do
        putStrLn $ nt ++ ":"
        putStrLn $ "  minlength(" ++ nt ++ ") = " ++ formatLength minLen
        putStrLn $ "  words_minlen(" ++ nt ++ ") = " ++ formatWords minWords
        putStrLn ""

      Nothing ->
        pure ()

--- Example grammar

exampleGrammar :: Grammar
exampleGrammar =
  Grammar
    { nonTerminals = ["S", "A", "B", "C", "D", "E", "F"]
    , terminals = ['a', 'b', 'c', 'd', 'e', 'x', 'y']
    , startSymbol = "S"
    , rules =
        [ Rule "S" [NT "A", NT "B"]
        , Rule "S" [NT "C"]
        , Rule "S" [T 'x', NT "D"]

        , Rule "A" [T 'a']
        , Rule "A" [T 'b']
        , Rule "A" [NT "E", T 'c']

        , Rule "B" [T 'd']
        , Rule "B" [T 'e']
        , Rule "B" [NT "C"]

        , Rule "C" [NT "A", NT "D"]
        , Rule "C" [T 'y']

        -- D -> eps
        , Rule "D" []
        , Rule "D" [T 'd', NT "D"]

        , Rule "E" [T 'a']
        , Rule "E" [T 'b']

        -- F is non-productive.
        , Rule "F" [NT "F"]
        ]
    }

main :: IO ()
main = do
  let results = solve exampleGrammar

  putStrLn "Grammar:"
  putStrLn $ "  non-terminals = " ++ show (nonTerminals exampleGrammar)
  putStrLn $ "  terminals = " ++ show (terminals exampleGrammar)
  putStrLn $ "  start symbol = " ++ startSymbol exampleGrammar
  putStrLn ""

  putStrLn "Results:"
  printResults exampleGrammar results