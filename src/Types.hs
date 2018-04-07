module Types where
import Data.Word

data Vector3D = 
  Vector3D { vX :: Int, vY :: Int, vZ :: Int }
  deriving (Show)

type Pos = Vector3D

mkPos :: Int -> Int -> Int -> Pos
mkPos = Vector3D

type Coord2D = (Int, Int)

type Color = (Word8, Word8, Word8)

data Object = 
    Cube Pos 
  | Sphere Pos
  deriving (Show)

data Light = Light { 
   lPos :: Pos
  ,lBrightness :: Double
  }
  deriving (Show)
 

data Camera = Camera {
   cPos :: Pos
  ,cLookingAt :: Pos
  }
  deriving (Show)

data Scene = Scene {
   sCamera  :: Camera
  ,sLight   :: Light
  ,sObjects :: [Object]
  }
  deriving (Show)

type Bitmap = [[Color]]

data Config = Config {
   sceneFile :: FilePath
  ,outputFile :: FilePath
  }
  deriving (Show)

