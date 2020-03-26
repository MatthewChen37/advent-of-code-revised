import System.IO
import Data.List.Split
import Data.Sequence
import Data.Foldable
import Prelude hiding (drop)
main = do
    handle <- openFile "2019day2.in" ReadMode
    contents <- hGetContents handle

    let parsed_values = splitOn "," contents --gets a list of stringed-integers
    let parsed_integers = map parse parsed_values --gets a list of parsed integers from the strings
    let correct_sequence = search parsed_integers

    print (show correct_sequence)

parse :: String -> Int
parse x = read x :: Int


addInitialCondition :: Seq Int -> Int -> Int -> Seq Int
addInitialCondition sequence noun verb =
    update 1 noun (update 2 verb sequence)

processEntire :: Seq Int -> Seq Int
processEntire x = process x x

process :: Seq Int -> Seq Int -> Seq Int
process writing_copy reading_copy
    |opCode == 1 = add writing_copy (drop 1 reading_copy)
    |opCode == 2 = multiply writing_copy (drop 1 reading_copy)
    |opCode == 99 = writing_copy
    where opCode = index reading_copy 0

add :: Seq Int -> Seq Int -> Seq Int
add writing_copy reading_copy =
    process (update (index reading_copy 2) sum writing_copy) (drop 3 reading_copy)
    where sum = index writing_copy (index reading_copy 0) + index writing_copy (index reading_copy 1)

multiply :: Seq Int -> Seq Int -> Seq Int
multiply writing_copy reading_copy =
    process (update (index reading_copy 2) product writing_copy) (drop 3 reading_copy)
    where product = index writing_copy (index reading_copy 0) * index writing_copy (index reading_copy 1)


search :: [Int] -> [Int]
search parsed_integers = do
    let parsed_sequence = fromList parsed_integers
    let solution = [ input parsed_sequence noun verb | noun <-[0..99], verb <-[0..99], head (input parsed_sequence noun verb) == 19690720]
    head solution

input :: Seq Int -> Int -> Int -> [Int]
input sequence noun verb = toList (processEntire (addInitialCondition sequence noun verb))
