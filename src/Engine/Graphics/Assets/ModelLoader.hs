module Engine.Graphics.Assets.ModelLoader(loadModel) where

import Graphics.Rendering.OpenGL

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
type ElementIndex       = L.V3 GLuint

type ModelOutput = ([VertexCoordinate], [VertexUVCoordinate], [VertexNormal], [VertexIndex], [VertexIndex])

loadModel :: GeometryResource -> IO ModelOutput
loadModel gR@(GeometryResource _ format fp)
    | format == ModelFormat'OBJ = loadOBJModel fp
    | otherwise = error $ "unsupported format: " ++ show gR

loadOBJModel :: FilePath -> IO ModelOutput
loadOBJModel fp = do
  contents <- readFile fp
  return $ parseOBJModel contents

{- meh: boring.
parseObjModel' :: String -> ModelOutput
parseObjModel' contents = undefined
    where a = map parseLine $ lines contents

parseLine :: String -> ModelOutput
parseLine l
    | t2 == "v "  = (v d2, [],    [],    [])
    | t3 == "vt " = ([],   uv d3, [],    [])
    | t3 == "vn " = ([],   [],    vn d3, [])
    | t2 == "f "  = ([],   [],    [],    f d2)
    | otherwise   = ([],   [],    [],    [])
    where t2 = take 2 l
          d2 = words $ drop 2 l

          t3 = take 3 l
          d3 = words $ drop 3 l

          v :: [String] -> [VertexCoordinate]
          v [x,y,z] = [L.V4 (read x) (read y) (read z) 1.0]
          v err = error $ "parseError: v! " ++ show err

          uv :: [String] -> [VertexUVCoordinate]
          uv [x,y] = [L.V2 (read x) (read y)]
          uv err = error $ "parseError: uv! " ++ show err

          vn :: [String] -> [VertexNormal]
          vn [x,y,z] = [L.V3 (read x) (read y) (read z)]
          vn err = error $ "parseError: vn! " ++ show err

          f :: [String] -> [ElementIndex]
          f [a,b,c] = []
              where [v1, uv1, vn1] = splitStringAndParse a []
                    [v2, uv2, vn2] = splitStringAndParse b []
                    [v3, uv3, vn3] = splitStringAndParse c []

                    splitStringAndParse :: String -> String -> [GLuint]
                    splitStringAndParse [] mem = [read mem]
                    splitStringAndParse (s:sx) mem
                        | s == '\\' = read mem : splitStringAndParse sx []
                        | otherwise = splitStringAndParse sx (mem ++ [s])

-}


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
           vert_indices' = concat vert_indices
           uv_indices'   = concat uv_indices
           norm_indices' = concat norm_indices
           (vert_indices'', vData)    = expandVertexData vert_indices' vCs
           (uv_indices''  , uvData)   = expandVertexData uv_indices'   vUVcs
           (norm_indices'', normData) = expandVertexData norm_indices'  vNs


           uvData'   = reorderCoordinates vert_indices'' uv_indices'' uvData
           normData' = map L.normalize $ reorderCoordinates vert_indices'' norm_indices'' normData

           --           elems = indicesToElemVectors vert_indices''
           triElems     = vert_indices''
           triAdjElems = makeAdjacencyList'' vData vert_indices''


       return (vData, uvData', normData', triElems, triAdjElems) -- normals not done


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

             return $ L.V3 rx ry rz

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
             return ([v1, v2, v3], [u1, u2, u3], [n1, n2, n3])

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


indicesToElemVectors :: [VertexIndex]-> [ElementIndex]
indicesToElemVectors [] = []
indicesToElemVectors (i1:i2:i3:r) =
    L.V3 i1 i2 i3:indicesToElemVectors r
indicesToElemVectors _ = error "indices not divisible by 3..."

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
  -- error $ show vertIndices ++
  --                                            "      ~      " ++
  --                                            show vertCoords ++
  --                                            "      ~      " ++
  --                                            show vertCoordToFirstIndexMap ++
  --                                                  "      ~      " ++
  --                                            show edgeVertexMap
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



      mkEdge :: VertexIndex -> VertexIndex -> (VertexIndex, VertexIndex)
      mkEdge v1 v2
        | uv1 > uv2 = (uv1, uv2)
        | otherwise = (uv2, uv1)
        where uv1 = getUniqueIndex v1
              uv2 = getUniqueIndex v2


      oppositeEdge :: (VertexIndex, VertexIndex) -> VertexIndex -> VertexIndex
      oppositeEdge e notV
        | v1 == notV = v2
        | otherwise  = v1
        where [v1, v2] = edgeVertexMap Map.!e


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
