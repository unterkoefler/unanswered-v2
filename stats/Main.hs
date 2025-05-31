{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Options.Applicative
import Diagrams.Prelude
import Diagrams.Backend.SVG

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
theCircle = circle 1

renderWordCloud :: String -> IO ()
renderWordCloud inputFile = do
    putStrLn $ "making word cloud for: " ++ inputFile
    renderSVG "wordcloud.svg" (dims2D 100 100) theCircle



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
