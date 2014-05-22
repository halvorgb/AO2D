module Engine.ModelLoader(loadModel) where

import Graphics.Rendering.OpenGL

import qualified Linear as L
import Text.ParserCombinators.Parsec
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set


import Model.Object

type Index              = GLuint -- "polymorphic"

type VertexIndex        = GLuint
type UVIndex            = GLuint
type NormalIndex        = GLuint

type VertexCoordinate   = L.V4 GLfloat
type VertexUVCoordinate = L.V2 GLfloat
type VertexNormal       = L.V3 GLfloat
type ElementIndex       = L.V3 GLuint
type ModelOutput = ([VertexCoordinate], [VertexUVCoordinate], [VertexNormal], [ElementIndex])


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


       let nofFaces = length is
           (vert_indices, uv_indices, norm_indices) = unzip3 is
           vert_indices' = concat vert_indices
           uv_indices'   = concat uv_indices
           norm_indices' = concat norm_indices
           elems = indicesToElemVectors vert_indices'

           vData = expandVertexData vert_indices' vCs

       error $ show vData ++ "............................... " ++ show vCs
--           vUVcs' = reorderUVCoordinates vert_indices' uv_indices' vUVcs
--       error $ show vUVcs'
       return (vCs, vUVcs, vNs, elems)
--       return (vertexCoordinates, uvCoordinates, normalVectors, elems)


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


      faceElements :: GenParser Char st ([VertexIndex], [UVIndex], [NormalIndex])
      faceElements =
          do nonFaceElement
             (v1, u1, n1) <- faceElement
             skipSpace
             (v2, u2, n2) <- faceElement
             skipSpace
             (v3, u3, n3) <- faceElement
             return $ ([v1, v2, v3], [u1, u2, u3], [n1, n2, n3])

      faceElement :: GenParser Char st (VertexIndex, UVIndex, NormalIndex)
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

{-
reorder :: [VertexIndex] -> [VertexCoordinate] ->
           [UVIndex]     -> [VertexUVCoordinate] ->
           [NormalIndex] -> [VertexNormal] ->
           ([VertexCoordinate], [VertexUVCoordinate], [VertexNormal], [VertexIndex])
reorder v_i v uv_i uv n_i n = (rVCs, rUVCs, rVNs, v_i)
    where
      rVCs   = map (reorder' v) v_i
      rUVCs = map (reorder' uv) uv_i
      rVNs   = map (reorder' n) n_i
      reorder' l i = l !! (fromIntegral i)
-}
indicesToElemVectors :: [VertexIndex]-> [ElementIndex]
indicesToElemVectors [] = []
indicesToElemVectors (i1:i2:i3:r) =
    (L.V3 i1 i2 i3):indicesToElemVectors r
indicesToElemVectors _ = error "indices not divisible by 3..."


reorderUVCoordinates :: [VertexIndex] -> [UVIndex] -> [VertexUVCoordinate] -> [VertexUVCoordinate]
reorderUVCoordinates vert_indices uv_indices vUVcs =
    buildUVCoordinateList uvCoordinateOrder uvMap
    where
      uv_indices' = List.nub uv_indices

      uvMap = Map.fromList $ zip uv_indices' vUVcs

      uvCoordinateOrder = map snd $ onlyFirstKey $ List.sort $ zip vert_indices uv_indices

      buildUVCoordinateList :: [UVIndex] -> Map.Map UVIndex VertexUVCoordinate -> [VertexUVCoordinate]
      buildUVCoordinateList [] _ = []
      buildUVCoordinateList (uvi:uvis) m = case Map.lookup uvi m of
                                      Just uvCoord -> uvCoord:buildUVCoordinateList uvis m
                                      Nothing -> error "error in reorderUVCoordinates, possible parse trouble."

onlyFirstKey :: [(VertexIndex, UVIndex)] -> [(VertexIndex, UVIndex)]
onlyFirstKey [] = []
onlyFirstKey ((vi, uvi):is) = (vi,uvi):onlyFirstKey' vi is
    where onlyFirstKey' :: VertexIndex -> [(VertexIndex, UVIndex)] -> [(VertexIndex, UVIndex)]
          onlyFirstKey' _ [] = []
          onlyFirstKey' prev ((vi', uvi'):is')
              | vi' == prev = onlyFirstKey' prev is'
              | otherwise  = (vi', uvi'):onlyFirstKey' vi' is'



expandVertexData :: [VertexIndex] -> [VertexCoordinate] -> ([VertexIndex], [VertexCoordinate])
expandVertexData vis vcs = expandVertexData' vis m Set.empty []
    where
      m :: Map.Map VertexIndex VertexCoordinate
      m = Map.fromList $ map (\vi -> (vi, vcs !! fromIntegral vi)) vis



expandVertexData' :: [VertexIndex] -> Map.Map VertexIndex VertexCoordinate -> Set.Set VertexIndex -> [VertexIndex] -> ([VertexIndex], [VertexCoordinate])
expandVertexData' []       m _ ordered_vis = (reverse ordered_vis, reverse $ Map.fold (\vc vcs -> vc:vcs) [] m)
expandVertexData' (vi:vis) m s ordered_vis
    | Set.member vi s = let (maxK, _) = Map.findMax m
                            vi' = maxK + 1
                            ordered_vis' = vi':ordered_vis
                            vc = fromMapToJust m vi "expandVertexData'"
                            m' = Map.insert vi' vc m

                        in expandVertexData' vis m' s' ordered_vis' -- create a duplicate vertex

    | otherwise       = let ordered_vis' = vi:ordered_vis
                        in expandVertexData' vis m s' ordered_vis'
    where s' = Set.insert vi s



fromMapToJust :: Ord a => Map.Map a b -> a -> String -> b
fromMapToJust m k err = case Map.lookup k m of
                      Just v -> v
                      Nothing -> error $ "Error in fromMapToJust, called by: " ++ err
