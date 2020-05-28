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

    let first_path_points = generate_path first_path

    print first_path_points


generate_path :: [String] -> [(Int,Int)]
generate_path path =
    generate_points path [(0,0)]
    --list of points is of type [[(Int, Int)]]
    --TODO: break down the lists of lists into a continuous list

generate_points :: (Int,Int) -> [String] -> [(Int, Int)] -> [(Int,Int)]
generate_points input list_of_points
    |direction == 'U' = go_up input list_of_points
    |direction == 'D' = go_down input list_of_points
    |direction == 'L' = go_left  input list_of_points
    |direction == 'R' = go_right input list_of_points
    |direction == '\n' = list_of_points
    where direction = head (input !! 0)

go_up string_path list_of_points =
    let parsed_int = parse_current string_path
    in tuple_up parsed_int list_of_points

tuple_up :: Int -> [String] -> [(Int, Int)]

tuple_up 0 list_of_points = list_of_points
tuple_up increment list_of_points =
    let last_point = tail list_of_points
        new_point = ((fst last_point), (snd last_point) + 1)
        new_list_of_points = list_of_points ++ new_point
    in tuple_up (increment - 1) new_list_of_points

go_down :: [String] -> [(Int, Int)]
go_down string_number list_of_points =
    let parsed_int = parse_current string_path

go_left :: [String] -> [(Int, Int)]
go_left string_number list_of_points =
    let parsed_int = parse_current string_path

go_right :: [String] -> [(Int, Int)]
go_right string_number list_of_points =
    let parsed_int = parse_current string_path



parse_current :: [String] -> Int
parse_current path =
    read (tail (path !! 0)) :: Int


-- go through each string
-- Cartesian coordinates: Record every point that the wire goes through
-- and then check which points are similar between the two
-- wires' paths
