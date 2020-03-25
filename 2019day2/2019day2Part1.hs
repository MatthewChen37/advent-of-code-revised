import System.IO
import Data.List.Split
import Data.Sequence
import Prelude hiding (drop)
main = do
    handle <- openFile "2019day2.in" ReadMode
    contents <- hGetContents handle
    let parsed_values = splitOn "," contents --gets a list of stringed-integers
    let parsed_integers = map parse parsed_values --gets a list of parsed integers from the strings
    let parsed_sequence = fromList parsed_integers
    print (show (process_entire (add_initial_condition parsed_sequence)))





parse :: String -> Int
parse x = read x :: Int


add_initial_condition :: Seq Int -> Seq Int
add_initial_condition sequence =
    update 1 12 (update 2 2 sequence)



process_entire :: Seq Int -> Seq Int
process_entire x = process x x 0

process :: Seq Int -> Seq Int -> Int -> Seq Int
process writing_copy reading_copy index
    |opCode == 1 = add writing_copy (drop 1 reading_copy) (index + 1)
    |opCode == 2 = multiply writing_copy (drop 1 reading_copy) (index + 1)
    |opCode == 99 = writing_copy
    where opCode = index reading_copy 0


add :: Seq Int -> Seq Int -> Int -> Seq Int
add writing_copy reading_copy index =
    process (update (index * 4) sum writing_copy) (drop 3 reading_copy) index
    where sum = index writing_copy (index reading_copy 0) + index writing_copy (index reading_copy 1)


multiply :: Seq Int -> Seq Int -> Int -> Seq Int
multiply writing_copy reading_copy index =
    process (update (index * 4) product writing_copy) (drop 3 reading_copy) index
    where product = index writing_copy (index reading_copy 0) * index writing_copy (index reading_copy 1)
