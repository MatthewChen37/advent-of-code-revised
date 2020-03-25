import System.IO
main = do
    handle <- openFile "2019day1.in" ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
    let fuel = map calculate linesOfFiles
    let total_fuel = sum fuel
    print (show total_fuel)
calculate x = ((read x :: Int) `div` 3)  - 2
