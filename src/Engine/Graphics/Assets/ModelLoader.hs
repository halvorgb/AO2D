module Engine.Graphics.Assets.ModelLoader(loadModel) where

--import Graphics.Rendering.OpenGL

import Model.Types

import qualified Linear as L
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Array as Array
import qualified Data.List as List

import Model.Resources

type VertexIndex        = GLuint
type UVIndex            = GLuint
type NormalIndex        = GLuint

type VertexCoordinate   = L.V3 GLfloat
type VertexUVCoordinate = L.V2 GLfloat
type VertexNormal       = L.V3 GLfloat

type ModelOutput = ([VertexCoordinate], [VertexUVCoordinate], [VertexNormal], [VertexIndex], [VertexIndex])

loadModel :: GeometryResource -> IO ModelOutput
loadModel gR@(GeometryResource _ format fp)
    | format == ModelFormat'OBJ = loadOBJModel fp
    | otherwise = error $ "unsupported format: " ++ show gR

loadOBJModel :: FilePath -> IO ModelOutput
loadOBJModel fp = do
  contents <- readFile fp

--  return $ parseOBJModel contents
  return $ parseOBJModel' $ lines contents


data LineOutput = Indices ([VertexIndex], [UVIndex], [NormalIndex])
                | VXC VertexCoordinate
                | UVC VertexUVCoordinate
                | VXN VertexNormal
                | NoParse

type QuadrupleList = ([([VertexIndex], [UVIndex], [NormalIndex])],
                      [VertexCoordinate],
                      [VertexUVCoordinate],
                      [VertexNormal])
parseOBJModel' ::  [String] -> ModelOutput
parseOBJModel' contents = (vData, uvData', normData', triElems, triAdjElems)
    where (is, vs, uvs, ns) = concatedLists
          (vert_indices, uv_indices, norm_indices) = unzip3 is
          vert_indices' = concat vert_indices
          uv_indices'   = concat uv_indices
          norm_indices' = concat norm_indices

          (vert_indices'', vData)    = expandVertexData vert_indices' vs
          (uv_indices''  , uvData)   = expandVertexData uv_indices'   uvs
          (norm_indices'', normData) = expandVertexData norm_indices' ns

          uvData'   = reorderCoordinates vert_indices'' uv_indices'' uvData
          normData' = map L.normalize $ reorderCoordinates vert_indices'' norm_indices'' normData

          triElems     = vert_indices''
          triAdjElems = makeAdjacencyList'' vData vert_indices''

          --
          parseRes = map parseLine contents

          parseLine :: String -> LineOutput
          parseLine l = case parse line "parseLine" l of
                          Left err -> error $ show err
                          Right p -> p

          concatedLists :: QuadrupleList
          concatedLists = foldl concatQuadruples emptyQuadruple $ reverse $
                          map toQuadrupleList parseRes
              where toQuadrupleList :: LineOutput -> QuadrupleList
                    toQuadrupleList (Indices i)  = ([i],  [],    [],    [])
                    toQuadrupleList (VXC vxc)    = ([],   [vxc], [],    [])
                    toQuadrupleList (UVC uvc)    = ([],   [],    [uvc], [])
                    toQuadrupleList (VXN vxn)    = ([],   [],    [],    [vxn])
                    toQuadrupleList _                  = emptyQuadruple

                    emptyQuadruple = ([],   [],    [],    [])

                    concatQuadruples :: QuadrupleList ->
                                        QuadrupleList -> QuadrupleList
                    concatQuadruples (a,b,c,d) (a', b', c', d') =
                        (a'++a, b'++b, c'++c, d'++d)

line :: GenParser Char st LineOutput
line = try vertexCoordinate <|>
       try vertexUVCoordinate <|>
       try vertexNormal <|>
       try faceElements <|>
       return NoParse
    where

      skipSpace = skipMany space

      nonFloatParser = skipMany $ noneOf "1234567890-."
      floatParser = many $ oneOf "1234567890-."


      vertexCoordinate :: GenParser Char st LineOutput
      vertexCoordinate =
          do _ <- string "v "
             nonFloatParser
             x <- floatParser
             skipSpace
             y <- floatParser
             skipSpace
             z <- floatParser
             let rx = read x
                 ry = read y
                 rz = read z

             return $ VXC $ L.V3 rx ry rz

      vertexUVCoordinate :: GenParser Char st LineOutput
      vertexUVCoordinate =
          do _ <- string "vt "
             nonFloatParser
             x <- floatParser
             skipSpace
             y <- floatParser
             let rx = read x
                 ry = read y
             return $ UVC $ L.V2 rx ry

      vertexNormal :: GenParser Char st LineOutput
      vertexNormal =
          do _ <- string "vn "
             nonFloatParser
             x <- floatParser
             skipSpace
             y <- floatParser
             skipSpace
             z <- floatParser
             let rx = read x
                 ry = read y
                 rz = read z
             return $ VXN $ L.V3 rx ry rz

      nonFaceElement = skipMany $ noneOf "/1234567890"


      faceElements :: GenParser Char st LineOutput
      faceElements =
          do _ <- string "f "
             nonFaceElement
             (v1, u1, n1) <- faceElement
             skipSpace
             (v2, u2, n2) <- faceElement
             skipSpace
             (v3, u3, n3) <- faceElement
             return $ Indices ([v1, v2, v3], [u1, u2, u3], [n1, n2, n3])

      faceElement :: GenParser Char st (VertexIndex, UVIndex, NormalIndex)
      faceElement =
          do vertexIndex <- many digit
             skipMany1 $ char '/'
             uvIndex <- many digit
             skipMany1 $ char '/'
             normalIndex <- many digit
             let r_vec  = read vertexIndex - 1 -- OBJ files are 1 indexed.
                 r_uv   = read uvIndex - 1
                 r_norm = read normalIndex - 1

             return (r_vec, r_uv, r_norm)

--expandVertexData :: [Index] -> [VertexCoordinate] -> ([Index], [VertexCoordinate])
expandVertexData :: Integral a => [a] -> [b] -> ([a],[b])
expandVertexData vis vcs = expandVertexData' vis m Set.empty []
    where
--      m :: Map.Map Index VertexCoordinate
      m = Map.fromList $ map (\vi -> (vi, vcs !! fromIntegral vi)) vis




--expandVertexData :: [Index] -> Map.Map Index VertexCoordinate -> Set.Set Index -> [Index] -> ([Index], [VertexCoordinate])
expandVertexData' :: (Ord a, Num a) => [a] -> Map.Map a b -> Set.Set a -> [a] -> ([a], [b])
expandVertexData' []       m _ ordered_vis = (reverse ordered_vis, Map.fold (:) [] m)
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
fromMapToJust m k err =
    Maybe.fromMaybe
    (error $ "Error in fromMapToJust, called by: " ++ err)
    (Map.lookup k m)


--reorderCoordinates :: [VertexIndex] -> [UVIndex] -> [VertexUVCoordinate] -> [VertexUVCoordinate]
reorderCoordinates :: (Ord a, Integral b) =>
                      [a] -> [b] -> [c] -> [c]
reorderCoordinates vis uvis cds = reorderCoordinates' vis uvis m
    where m = Map.fromList $ map (\i -> (i, cds !! fromIntegral i)) uvis

--reorderCoordinates' :: [VertexIndex] -> [UVIndex] -> Map.Map UVIndex VertexUVCoordinate -> [VertexUVCoordinate]
reorderCoordinates' :: (Ord a, Integral b) =>
                       [a] -> [b] -> Map.Map b c -> [c]
reorderCoordinates' vis uvis uvi2uvmap = map snd $ Map.toList vi2uvimap
    where vi2uvimap = List.foldl' addToTempMap Map.empty $ zip vis uvis

--          addToTempMap :: Map.Map VertexIndex VertexUVCoordinate -> (VertexIndex, UVIndex) -> Map.Map VertexIndex VertexUVCoordinate
          addToTempMap m' (vi, uvi) = Map.insert vi (fromMapToJust uvi2uvmap uvi "addToTempMap") m'




type VertCIMap = Map.Map VertexCoordinate VertexIndex
type EdgeVMap  = Map.Map (VertexIndex, VertexIndex) [VertexIndex]

makeAdjacencyList'' :: [VertexCoordinate] -> [VertexIndex] -> [VertexIndex]
makeAdjacencyList'' vertCoords vertIndices = adjacentVertIndices
  where

      -- 0 make a vertexcoordinate array for constant lookup times.
      vertArray :: Array.Array Int VertexCoordinate
      vertArray = Array.listArray (0, length vertCoords - 1) vertCoords

      -- 1. create a map between vertexCoords and the first vertexIndex that refers to it.
      vertCoordToFirstIndexMap :: VertCIMap
      vertCoordToFirstIndexMap = Map.fromList $ zip (fastNub vertCoords) [0..]


      -- 2. create a map between edges and uniquqe vertex indices
      edgeVertexMap :: EdgeVMap
      edgeVertexMap = mkEVMap Map.empty vertIndices
        where mkEVMap :: EdgeVMap -> [VertexIndex] -> EdgeVMap
              mkEVMap evMap []            = evMap
              mkEVMap evMap (v0:v2:v4:vr) = mkEVMap evMap' vr
                where e02 = mkEdge v0 v2
                      e24 = mkEdge v2 v4
                      e40 = mkEdge v4 v0
                      edges = [e02, e24, e40]
                      notVs = map getUniqueIndex [v4, v0, v2]

                      evMap' :: EdgeVMap
                      evMap' = List.foldl' (\m (k,v) ->
                                             Map.insertWith (++) k [v] m)
                               evMap $ zip edges notVs
              mkEVMap _ _ = error "something went terribly wrong in mkEVMap"



      adjacentVertIndices :: [VertexIndex]
      adjacentVertIndices = concat $ embellishVertices vertIndices
        where embellishVertices :: [VertexIndex] -> [[VertexIndex]]
              embellishVertices [] = []
              embellishVertices (v0:v2:v4:vr) =  [v0, v1, v2, v3, v4, v5]:embellishVertices vr
                where e02 = mkEdge v0 v2
                      v1 = oppositeEdge e02 v4
                      e24 = mkEdge v2 v4
                      v3 = oppositeEdge e24 v0
                      e40 = mkEdge v4 v0
                      v5 = oppositeEdge e40 v2
              embellishVertices _ = error "something went terribly wrong in embellishVertices"


      mkEdge :: VertexIndex -> VertexIndex -> (VertexIndex, VertexIndex)
      mkEdge v1 v2
        | uv1 > uv2 = (uv1, uv2)
        | otherwise = (uv2, uv1)
        where uv1 = getUniqueIndex v1
              uv2 = getUniqueIndex v2


      oppositeEdge :: (VertexIndex, VertexIndex) -> VertexIndex -> VertexIndex
      oppositeEdge e notV
        | v1 == notVUnique = v2
        | otherwise        = v1
        where [v1, v2] = edgeVertexMap Map.! e
              notVUnique = getUniqueIndex notV


      getUniqueIndex :: VertexIndex -> VertexIndex
      getUniqueIndex vi = vertCoordToFirstIndexMap Map.! viVert
        where viVert = vertArray Array.! fromIntegral vi

fastNub :: Ord a => [a] -> [a]
fastNub l = fastNub' l Set.empty
  where  fastNub' :: Ord a => [a] -> Set.Set a -> [a]
         fastNub' [] _     = []
         fastNub' (x:xs) s
           | Set.member x s = fastNub' xs s
           | otherwise = x:fastNub' xs s'
           where s' = Set.insert x s
