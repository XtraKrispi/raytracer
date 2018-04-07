{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Control.Monad
import Data.Word
import Codec.BMP
import Data.ByteString hiding (concatMap, replicate)
import qualified Data.Yaml as Y
import Options.Applicative
import Data.Semigroup ((<>))

import Types
import Parsing

width :: Int
width = 1024

height :: Int
height = 768

backgroundColor :: Color 
backgroundColor = (0, 255, 0)

-- | Drawing related

renderScene :: Scene -> Bitmap
renderScene (Scene objs l c) = 
  let fullData = replicate height . replicate width $ backgroundColor
  in  fullData

convertToWords :: Color -> [Word8]
convertToWords (r, g, b) = [r, g, b, 1]

writeToFile :: FilePath -> Bitmap -> IO ()
writeToFile filePath bitmap = do
  let convert = packRGBA32ToBMP width height . pack . concatMap convertToWords . join
  writeBMP filePath $ convert bitmap

runApp :: Config -> IO ()
runApp Config{..} = do
  decoded <- Y.decodeFileEither sceneFile
  case decoded of
    Right scene -> 
      writeToFile outputFile $ renderScene scene
    Left err -> return ()   

main :: IO ()
main = runApp =<< execParser opts
  where 
    opts = info (config <**> helper)
            (fullDesc 
          <> progDesc "Raytracer for simple scenes"
          <> header   "raytracer - A simple raytracer" )

