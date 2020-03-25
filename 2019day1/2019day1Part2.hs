import System.IO

main = do
    handle <- openFile "2019day1.in" ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
    let parsed_num = map parse linesOfFiles
    let fuel = map calculate parsed_num
    let total_fuel = sum fuel
    print (show total_fuel)



parse :: String -> Int
parse x = read x :: Int


calculate :: Int -> Int
calculate x
    | curr_fuel <= 0 = 0
    | otherwise = curr_fuel + calculate curr_fuel
    where curr_fuel = (x `div` 3) - 2
