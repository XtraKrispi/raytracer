{-# LANGUAGE OverloadedStrings #-}
module Parsing where
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Yaml  ((.:))
import Control.Monad
import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as HMS
import Data.Aeson hiding (Object)
import Data.Aeson.Types hiding (Object, Parser)
  
import Types

config :: Parser Config
config = Config 
     <$> strOption
         ( long "scene"
        <> short 's'
        <> metavar "FILE PATH"
        <> help "Path to the scene YAML file" )
     <*> strOption
         ( long "output"
        <> short 'o'
        <> metavar "FILE PATH"
        <> help "Path to the output BMP file" )

instance Y.FromJSON Vector3D where
  parseJSON = Y.withObject "Pos" $ \v -> Vector3D
    <$> v .: "x"
    <*> v .: "y"
    <*> v .: "z"

instance Y.FromJSON Light where
  parseJSON = Y.withObject "Light" $ \l -> Light
    <$> l .: "position"
    <*> l .: "brightness"

instance Y.FromJSON Camera where
  parseJSON = Y.withObject "Camera" $ \c -> Camera
    <$> c .: "position"
    <*> c .: "lookingAt"

instance Y.FromJSON Object where
  parseJSON (Y.Object o) = 
    case HMS.lookup "type" o of
      Just (String "cube") -> 
          Cube <$> o .: "pos"
      Just (String "sphere") ->
          Sphere <$> o .: "pos"               
      _ -> mzero

instance Y.FromJSON Scene where
  parseJSON = Y.withObject "Scene" $ \s -> Scene
    <$> s .: "camera"
    <*> s .: "light"
    <*> s .: "objects"