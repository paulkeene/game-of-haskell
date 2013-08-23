module Game where

import Control.Concurrent(threadDelay)

data Cell = Alive Integer | Dead Integer deriving (Eq)

type Board = [Cell]

startingBoardStr :: String
startingBoardStr = concat ["      ...    .      ",
                           "  . ..     ......   ",
                           " ..        .. ..    ",
                           "     .  .     .     ",
                           ".    .              ",
                           "...  .              ",
                           ".             .     ",
                           " .    ..            ",
                           " . .  . .           ",
                           " .    ...   .       ",
                           " .    ...   .  .    ",
                           " . .  . .  .        ",
                           " .     ..   .  .    ",
                           " .     .     .      ",
                           " .    ...   ..  .   ",
                           " .    . .           ",
                           " .    ...           ",
                           " .    ...   . .     ",
                           " .    .             ",
                           "...                 "]

startingBoard :: Board
startingBoard = createBoard startingBoardStr

createBoard :: String -> Board
createBoard s = map toCell (indexedList)
    where toCell (i, ' ') = Dead (fromIntegral i)
          toCell (i, '.') = Alive (fromIntegral i)
          indexedList = zip [0..length s - 1] s

board :: [Board]
board = iterate gameStep startingBoard

boardHeight :: Board -> Integer
boardHeight = floor . sqrt . fromIntegral . length

neighbors :: Board -> Cell -> [Cell]
neighbors b c = map ((b !!) . fromIntegral) . neighborIndeces c $ boardHeight b

isAlive :: Cell -> Bool
isAlive (Alive _) = True
isAlive _ = False

isDead :: Cell -> Bool
isDead = not . isAlive

getIndex :: Cell -> Integer
getIndex (Alive i) = i
getIndex (Dead i) = i

neighborIndeces :: Cell -> Integer -> [Integer]
neighborIndeces c h = absoluteNonNegative
    where relative = [1, h, (h + 1), (h - 1)]
          allRelative = relative ++ (map negate relative)
          absoluteNonNegative = filter (\x -> x >= 0 && x < (h*h)) $
                                map (+ i) allRelative
          i = getIndex c

gameStep :: Board -> Board
gameStep b = map (cellStep b) b

liveCount :: [Cell] -> Integer
liveCount = fromIntegral . length . filter isAlive

cellStep :: Board -> Cell -> Cell
cellStep b c | (isAlive c && (numLiveNeighbors == 2 ||
                              numLiveNeighbors == 3)) = Alive i
             | (isDead c && numLiveNeighbors == 3) = Alive i
             where numLiveNeighbors = liveCount $ neighbors b c
                   i = getIndex c
cellStep _ c = (Dead $ getIndex c)

showBoard :: Board -> String
showBoard b = intersperse' h boardStr '\n'
    where boardStr = concat $ map showCell b
          h = boardHeight b

intersperse' :: Integer -> [a] -> a -> [a]
intersperse' _ [] _ = []
intersperse' i l e | length l < (fromIntegral i) = l
                   | otherwise = (take (fromIntegral i) l) ++
                     e:(intersperse' i (drop (fromIntegral i) l) e)

showCell :: Cell -> String
showCell (Alive _) = "*"
showCell (Dead _) = " "

main :: IO ()
main =
    printLoop board
    where printLoop b = do
            putStrLn $ replicate (fromIntegral $ boardHeight $ head board) '\n'
            putStrLn . showBoard $ head b
            threadDelay 100000
            printLoop $ drop 1 b
