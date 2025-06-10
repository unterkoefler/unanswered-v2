{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Options.Applicative
import Diagrams.Prelude
import Diagrams.Backend.SVG
import System.IO
import qualified WordCloud as WC
import qualified Data.Map
import qualified Data.Char as Char
import qualified Data.List
import Data.Set (Set, fromList)
import NLP.Tokenize.String
import Control.Monad

data MyOptions = MyOptions
    { optCommand :: Command
    }

data Command
    = WordCloud WordCloudOptions
    | TimeChart TimeChartOptions

data WordCloudOptions = WordCloudOptions
    { wordCloudInputPath :: String
    }

data TimeChartOptions = TimeChartOptions
    { timeChartInputPath :: String
    }

inputPathParser :: Parser String
inputPathParser = strOption
    ( long "input"
    <> short 'i'
    <> help "input path of text file to analyze" )

wordCloudParser :: Parser MyOptions
wordCloudParser = MyOptions <$> WordCloud <$>  WordCloudOptions
    <$> inputPathParser

timeChartParser :: Parser MyOptions
timeChartParser = MyOptions <$> TimeChart <$> TimeChartOptions
    <$> inputPathParser

options :: Parser MyOptions
options = hsubparser
    ( command "wordcloud" (info wordCloudParser (progDesc "generate wordcloud"))
    <> command "timechart" (info timeChartParser (progDesc "generate timechart"))
    )

-- DIAGRAMS --

theCircle :: Diagram B
theCircle = circle 20

someText :: Diagram B
someText =
    position [(p2 (0, 0), hello), (p2 (0, -5), svg)]
    where hello = text "Hello" # fontSize (local 5)
          svg = text "SVG" # fontSize (local 5)


cloud :: [String]-> Diagram B
cloud words =
    position (map mkPair placedWords)
        # bgFrame 5 white
    where placedWords = WC.wordCloud . Data.Map.toList . Data.Map.filter (\count -> count > 2) $ frequency words
          mkPair WC.PlacedWord { WC.position = p, WC.word = d } = (p2 (WC.x p, WC.y p), textBox d)

textBox :: WC.Datum -> Diagram B
textBox d =
    rect (fromIntegral $ WC.width d) (fromIntegral $ WC.height d) # lw none <> text (WC.label d) # fontSize (local (fromIntegral $ WC.fontSize d))

testData :: [(String, Int)]
testData = [("hello", 16), ("goodbye", 8), ("yooooo", 3), ("hi", 1)]

testData' = (take 16 $ repeat "hello") ++ (take 8 $ repeat "goodbye") ++ (take 3 $ repeat "yooooo") ++ ["hi"]

frequency :: [String] -> Data.Map.Map String Int
frequency = foldr (\x -> Data.Map.insertWith (+) x 1) Data.Map.empty

filterWords :: [String] -> [String]
filterWords =
    Data.List.filter (\w -> (not $ all Char.isPunctuation w) && notElem w boringWords)

boringWords :: Set String
boringWords =
    Data.Set.fromList
        [ "and"
        , "the"
        , "in"
        , "'s"
        , "'ve"
        , "'d"
        , "'ll"
        , "a"
        ]


myTokenizer :: Tokenizer
myTokenizer =     whitespace
              >=> uris
              >=> punctuation
              >=> myPunctuation
              >=> letters
              >=> contractions

letters :: Tokenizer
letters = finalLetters >=> initialLetters

-- | Split off word-final non-letters
finalLetters :: Tokenizer
finalLetters x = NLP.Tokenize.String.E . filter (not . null . unwrap) $
    case span (not . Char.isLetter) . reverse $ x of
      ([],w) -> [Right . reverse $ w]
      (ps,w) -> [Right . reverse $ w, Right . reverse $ ps]

-- | Split off word-initial non-letters
initialLetters :: Tokenizer
initialLetters x = NLP.Tokenize.String.E . filter (not . null . unwrap) $
    case span (not . Char.isLetter) $ x of
      ([],w) -> [Right w]
      (ps,w) -> [Right ps, Right w]

myPunctuation :: Tokenizer
myPunctuation = NLP.Tokenize.String.E . map Right
                          . Data.List.groupBy (\a b -> isMyPunctuation a == isMyPunctuation b)

isMyPunctuation :: Char.Char -> Bool
isMyPunctuation '.' = True
isMyPunctuation '-' = True
isMyPunctuation _ = False

unwrap (Left x) = x
unwrap (Right x) = x

renderWordCloud :: String -> IO ()
renderWordCloud inputFile = do
    putStrLn $ "making word cloud for: " ++ inputFile
    inh <- openFile inputFile ReadMode
    inputStr <- hGetContents inh
    let allWords = filterWords . run myTokenizer $ map Char.toLower inputStr
    putStrLn $ show $ frequency allWords
    renderSVG "wordcloud.svg" (dims2D 4000 2000)
        (cloud allWords)

main :: IO ()
main = runCommand =<< execParser opts
    where
        opts = info (options <**> helper)
            ( fullDesc
            <> progDesc "Produce svg charts to analyze blog stats"
            <> header "stats" )

runCommand :: MyOptions -> IO ()
runCommand (MyOptions (WordCloud (WordCloudOptions input))) = renderWordCloud input
runCommand (MyOptions (TimeChart (TimeChartOptions input))) = putStrLn $ "making timechart for: " ++ input
