module Engine.ModelLoader(loadModel) where

import Graphics.Rendering.OpenGL

import qualified Linear as L
import Text.ParserCombinators.Parsec



import Model.Object


type VertexCoordinate   = L.V4 GLfloat
type VertexUVCoordinate = L.V2 GLfloat
type VertexNormal       = L.V3 GLfloat
type FaceElement        = L.V3 GLuint
type ModelOutput = ([VertexCoordinate], [VertexUVCoordinate], [VertexNormal], [FaceElement])


loadModel :: ObjectResource -> IO ModelOutput
loadModel oR@(ObjectResource _ fp format)
    | format == ModelFormat'OBJ = loadOBJModel fp
    | otherwise = error $ "unsupported format: " ++ show oR

loadOBJModel :: FilePath -> IO ModelOutput
loadOBJModel fp = do
  contents <- readFile fp
  return $ parseOBJModel contents


parseOBJModel :: String -> ModelOutput
parseOBJModel contents =
    case parse objData "objData" contents of
      Left err -> error $ show err ++ show contents
      Right mo -> mo

objData :: GenParser Char st ModelOutput
objData =
    do _     <- manyTill anyChar $ try $ string "\nv "
       vCs   <- manyTill vertexCoordinate $ try $ string "\nvt "
       vUVcs <- manyTill vertexUVCoordinate $ try $ string "\nvn "
       vNs   <- manyTill vertexNormal $ try $ string "\ns "

       _     <- manyTill anyChar $ try $ string "\nf " -- s is shader? i dunno

       fEs   <- manyTill faceElements $ try nlEOF

       return (vCs, vUVcs, vNs, concat fEs)

    where
      nlEOF =
          do _ <- char '\n'
             eof

      skipSpace = skipMany space

      nonFloatParser = skipMany $ noneOf "1234567890-."
      floatParser = many $ oneOf "1234567890-."

      vertexCoordinate :: GenParser Char st VertexCoordinate
      vertexCoordinate =
          do nonFloatParser
             x <- floatParser
             skipSpace
             y <- floatParser
             skipSpace
             z <- floatParser
             let rx = read x
                 ry = read y
                 rz = read z
                 n = 1.0
             return $ L.V4 rx ry rz n

      vertexUVCoordinate :: GenParser Char st VertexUVCoordinate
      vertexUVCoordinate =
          do nonFloatParser
             x <- floatParser
             skipSpace
             y <- floatParser
             let rx = read x
                 ry = read y
             return $ L.V2 rx ry

      vertexNormal :: GenParser Char st VertexNormal
      vertexNormal =
          do nonFloatParser
             x <- floatParser
             skipSpace
             y <- floatParser
             skipSpace
             z <- floatParser
             let rx = read x
                 ry = read y
                 rz = read z
             return $ L.V3 rx ry rz

      nonFaceElement = skipMany $ noneOf "/1234567890"

      faceElements :: GenParser Char st [FaceElement]
      faceElements =
          do nonFaceElement
             a <- faceElement
             skipSpace
             b <- faceElement
             skipSpace
             c <- faceElement
             return [a, b,c]

      faceElement :: GenParser Char st FaceElement
      faceElement =
          do a <- many digit
             skipMany1 $ char '/'
             b <- many digit
             skipMany1 $ char '/'
             c <- many digit
             let ra = read a
                 rb = read b
                 rc = read c

             return $ L.V3 ra rb rc
