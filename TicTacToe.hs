import Prelude hiding (replicate)
import Control.Applicative ((<|>))
import Data.Char (digitToInt)
import Data.Functor ((<$>))
import Data.Sequence hiding (length)
import Data.Foldable (toList)
import Data.Monoid (mappend)
import Data.Maybe (mapMaybe)
import System.IO (hFlush, hSetBuffering, stdin, BufferMode (NoBuffering))


type BoardRow = Seq Cell
type Board = Seq BoardRow

data Symbol = Cross | Circle deriving (Show, Eq)
type Cell = Maybe Symbol


createBlankBoard :: (Int, Int) -> Board
createBlankBoard (column, row) = replicate row $ replicate column Nothing

initialBoardSize :: Int
initialBoardSize = 3

initialBoardRowCount :: Int
initialBoardRowCount = initialBoardSize

initialBoardColumnCount :: Int
initialBoardColumnCount = initialBoardSize

initialBoard :: Board
initialBoard = createBlankBoard (initialBoardRowCount, initialBoardColumnCount)

showSymbol :: Symbol -> Char
showSymbol Cross = 'X'
showSymbol Circle = 'O'

showCell :: Cell -> Char
showCell = maybe '.' showSymbol

readCell :: Char -> Cell
readCell '.' = Nothing
readCell 'X' = Just Cross
readCell 'O' = Just Circle

inverseSymbol :: Symbol -> Symbol
inverseSymbol Cross = Circle
inverseSymbol Circle = Cross

showBoard :: Board -> String
showBoard = unlines . map (map showCell . toList) . toList

readBoard :: String -> Board
readBoard = fromList . map (fromList . map readCell) . lines

updateBoard :: Symbol -> Int -> Board -> Board
updateBoard symbol location = adjust (update columnIndex (Just symbol)) rowIndex
	where rowIndex = location `div` initialBoardColumnCount
	      columnIndex = location `mod` initialBoardColumnCount

getCell :: Board -> (Int, Int) -> Cell
getCell board (columnIndex, rowIndex) = index (index board rowIndex) columnIndex

fullOfSameSymbol :: Symbol -> Board -> [(Int, Int)] -> Bool
fullOfSameSymbol symbol board locations = all (== Just symbol) cells
	where cells = getCell board <$> locations

fullOfSymbols :: Board -> [(Int, Int)] -> Cell
fullOfSymbols board locations
	| fullOfSameSymbol Circle board locations = Just Circle
	| fullOfSameSymbol Cross board locations = Just Cross
	| otherwise = Nothing

checkWinByRow :: Board -> Cell
checkWinByRow board = foldl1 (<|>) $ map (fullOfSymbols board) locations
	where locations = [wholeRow rowIndex | rowIndex <- [0..initialBoardRowCount - 1]]
		where wholeRow rowIndex = [(columnIndex, rowIndex) | columnIndex <- [0..initialBoardColumnCount - 1]]

checkWinByColumn :: Board -> Cell
checkWinByColumn board = foldl1 (<|>) $ map (fullOfSymbols board) locations
	where locations = [wholeColumn columnIndex | columnIndex <- [0..initialBoardColumnCount - 1]]
		where wholeColumn columnIndex = [(columnIndex, rowIndex) | rowIndex <- [0..initialBoardRowCount - 1]]

checkWinByDiagonal :: Board -> Cell
checkWinByDiagonal board = fullOfSymbols board leftDiagnalLocations <|> fullOfSymbols board rightDiagonalLocations
	where leftDiagnalLocations = [(cellIndex, cellIndex) | cellIndex <- [0..initialBoardSize - 1]]
	      rightDiagonalLocations = [(cellIndex, initialBoardSize - cellIndex - 1) | cellIndex <- [0..initialBoardSize - 1]]

checkWin :: Board -> Cell
checkWin board = checkWinByRow board <|> checkWinByColumn board <|> checkWinByDiagonal board

isLegalInput :: Char -> Bool
isLegalInput ch = ch >= '1' && ch <= '9'

getLocation :: IO Int
getLocation = do
	line <- getLine
	if length line == 1 && isLegalInput (head line) then
		return $ digitToInt (head line) - 1
	else do
		putStrLn "Error input! Again: "
		getLocation

nextTurn :: Symbol -> Board -> IO ()
nextTurn symbol board = do
	hSetBuffering stdin NoBuffering
	putStrLn $ '\n' : showBoard board
	putStrLn $ "Your turn, " ++ [showSymbol symbol] ++ " (1-9):"
	location <- getLocation
	let newBoard = updateBoard symbol location board
	    winCell = checkWin newBoard
	case winCell of
		Nothing -> nextTurn (inverseSymbol symbol) newBoard
		Just symbol -> do
			putStrLn $ '\n' : showBoard newBoard
			putStrLn (showSymbol symbol : " Wins!!!")

main :: IO ()
main = nextTurn Circle initialBoard