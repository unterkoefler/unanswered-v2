module WordCloud where

import System.Random
import Data.Function
import Data.Maybe
import Data.List
import Safe

data Datum = Datum {
    count :: Int,
    label :: String,
    fontSize :: Int,
    width :: Int,
    height :: Int
}

data Position = Position {
    x :: Double,
    y :: Double
}

data PlacedWord = PlacedWord {
    word :: Datum,
    position :: Position
}

wordCloud :: [(String, Int)] -> [PlacedWord]
wordCloud tuples =
    let sortedData = tuples & sortOn snd & reverse
        maxD = sortedData & headMay & fromMaybe ("", 1) & snd
    in
    sortedData
        & map (datumFromTuple maxD)
        & placeWords
        & reverse

maxFontSize = 48

datumFromTuple :: Int -> (String, Int) -> Datum
datumFromTuple maxCount tuple =
    let (label, count) = tuple
        normalizedValue = (fromIntegral count) / (fromIntegral maxCount)
        fontSize = normalizedValue * (fromIntegral maxFontSize)
        fontCharWidth = fontSize * 0.73-- TODO: check this
    in
    Datum {
        count = count,
        label = label,
        fontSize = floor fontSize,
        width = floor ((fromIntegral (length label)) * fontCharWidth),
        height = floor fontSize
    }

placeWords :: [Datum] -> [PlacedWord]
placeWords ds =
    placeWordsHelp ds [] 0

placeWordsHelp :: [Datum] -> [PlacedWord] -> Int -> [PlacedWord]
placeWordsHelp [] placedWords _ = placedWords
placeWordsHelp (x:xs) placedWords index =
    let (placedWord, nextIndex) = placeWord x placedWords index
    in
    placeWordsHelp
        xs
        (placedWord:placedWords)
        nextIndex

placeWord :: Datum -> [PlacedWord] -> Int -> ( PlacedWord, Int )
placeWord datum placedWords index =
    let g = mkStdGen (3 * index)
        x = spiralX (fromIntegral index) & nudge g
        y = spiralY (fromIntegral index)
        placedWord = PlacedWord { word = datum, position = Position { x = x, y = y } }
    in
    if any (overlap placedWord) placedWords then
        placeWord datum placedWords (index + 1)
    else
        (placedWord, index + 1)

nudge :: StdGen -> Double -> Double
nudge g p =
    let (delta :: Double) = (randomRs (-60.0, 60.0) g :: [Double]) & headMay & fromMaybe 0
    in p + delta

overlap :: PlacedWord -> PlacedWord -> Bool
overlap pwA pwB =
    let wordA = word pwA
        posA = position pwA
        wordB = word pwB
        posB = position pwB
        leftA = x posA
        rightA = leftA + (fromIntegral $ width wordA)
        topA = y posA
        bottomA = topA + (fromIntegral $ height wordA)
        leftB = x posB
        rightB = leftB + (fromIntegral $ width wordB)
        topB = y posB
        bottomB = topB + (fromIntegral $ height wordB)
    in
        leftA < rightB && leftB < rightA
        && topA < bottomB && topB < bottomA

spiralX :: Double -> Double
spiralX i =
    let a = 0.01
    in a * i * cos i

spiralY :: Double -> Double
spiralY i =
    let b = 0.01
    in b * i * sin i
