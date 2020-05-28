import System.IO
import Prelude
import Data.List.Split

main = do
    handle <- openFile "2019day3.in" ReadMode
    contents <- hGetContents handle
    let paths = lines contents
    -- IO that splits the input file into two different arrays
    let first_path = splitOn "," (head paths)
    let second_path = splitOn "," (paths !! 1)

    let first_path_points = generatePath first_path
    print first_path_points


generatePath :: [String] -> [(Int,Int)]
generatePath path =
    generatePoints path [(0,0)]
    --list of points is of type [[(Int, Int)]]
    --TODO: break down the lists of lists into a continuous list

generatePoints :: [String] -> [(Int, Int)] -> [(Int,Int)]
generatePoints input list_of_points
    |direction == 'U' = goUp input list_of_points
    |direction == 'D' = goDown input list_of_points
    |direction == 'L' = goLeft  input list_of_points
    |direction == 'R' = goRight input list_of_points
    |direction == '\n' = list_of_points
    where direction = head (head input)


goUp :: [String] -> [(Int, Int)] -> [(Int, Int)]
goUp string_path list_of_points =
    let parsed_int = parseCurrent string_path
        new_list_of_points = tupleUp parsed_int list_of_points
    in generatePoints (drop 1 string_path) new_list_of_points

tupleUp :: Int -> [(Int, Int)] -> [(Int, Int)]
tupleUp 0 list_of_points = list_of_points
tupleUp increment list_of_points =
    let last_point = last list_of_points
        new_point = (fst last_point, snd last_point + 1)
        new_list_of_points = list_of_points ++ [new_point]
    in tupleUp (increment - 1) new_list_of_points

goDown :: [String] -> [(Int, Int)] -> [(Int, Int)]
goDown string_path list_of_points =
    let parsed_int = parseCurrent string_path
        new_list_of_points = tupleDown parsed_int list_of_points
    in generatePoints (drop 1 string_path) new_list_of_points

tupleDown :: Int -> [(Int, Int)] -> [(Int, Int)]
tupleDown 0 list_of_points = list_of_points
tupleDown increment list_of_points =
    let last_point = last list_of_points
        new_point = (fst last_point, snd last_point - 1)
        new_list_of_points = list_of_points ++ [new_point]
    in tupleDown (increment - 1) new_list_of_points

goLeft :: [String] -> [(Int, Int)] -> [(Int, Int)]
goLeft string_path list_of_points =
    let parsed_int = parseCurrent string_path
        new_list_of_points = tupleLeft parsed_int list_of_points
    in generatePoints (drop 1 string_path) new_list_of_points

tupleLeft :: Int -> [(Int, Int)] -> [(Int, Int)]
tupleLeft 0 list_of_points = list_of_points
tupleLeft increment list_of_points =
    let last_point = last list_of_points
        new_point = (fst last_point - 1, snd last_point)
        new_list_of_points = list_of_points ++ [new_point]
    in tupleLeft (increment - 1) new_list_of_points

goRight :: [String] -> [(Int, Int)] -> [(Int, Int)]
goRight string_path list_of_points =
    let parsed_int = parseCurrent string_path
        new_list_of_points = tupleRight parsed_int list_of_points
    in generatePoints (drop 1 string_path) new_list_of_points

tupleRight :: Int -> [(Int, Int)] -> [(Int, Int)]
tupleRight 0 list_of_points = list_of_points
tupleRight increment list_of_points =
    let last_point = last list_of_points
        new_point = (fst last_point + 1, snd last_point)
        new_list_of_points = list_of_points ++ [new_point]
    in tupleRight (increment - 1) new_list_of_points

parseCurrent :: [String] -> Int
parseCurrent path =
    read (tail (head path)) :: Int


-- go through each string
-- Cartesian coordinates: Record every point that the wire goes through
-- and then check which points are similar between the two
-- wires' paths
