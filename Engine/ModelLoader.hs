module Engine.ModelLoader(loadModel) where

import Graphics.Rendering.OpenGL

import qualified Linear as L
import Text.ParserCombinators.Parsec



import Model.Object


type VectorIndex        = GLuint

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

       is    <- manyTill faceElements $ try nlEOF


       let (vert_indices, uv_indices, norm_indices) = unzip3 is
           (vs, uvs, ns, is') = rectifyOBJFaceIndexing is
       return (vCs, vUVcs, vNs, vert_indices)

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


      faceElements :: GenParser Char st (FaceElement, FaceElement, FaceElement)
      faceElements =
          do nonFaceElement
             (v1, u1, n1) <- faceElement
             skipSpace
             (v2, u2, n2) <- faceElement
             skipSpace
             (v3, u3, n3) <- faceElement
             return $ (L.V3 v1 v2 v3, L.V3 u1 u2 u3, L.V3 n1 n2 n3)

      faceElement :: GenParser Char st (VectorIndex, VectorIndex, VectorIndex)
      faceElement =
          do vertexIndex <- many digit
             skipMany1 $ char '/'
             uvIndex <- many digit
             skipMany1 $ char '/'
             normalIndex <- many digit
             let r_vec  = (read vertexIndex) - 1 -- OBJ files are 1 indexed.
                 r_uv   = (read uvIndex) - 1
                 r_norm = (read normalIndex) - 1

             return (r_vec, r_uv, r_norm)


rectifyOBJFaceIndexing ::
