module Engine.Graphics.Assets.Resources(loadResources) where

import qualified Codec.Picture                      as PNG
import qualified Data.List                          as L
import qualified Data.Map                           as M
import qualified Data.Yaml                          as Yaml
import           Engine.Graphics.Assets.ImageLoader
import           Engine.Graphics.Assets.ModelLoader
import           Engine.Graphics.Common
import qualified Graphics.GLUtil                    as GLUtil
import           Graphics.Rendering.OpenGL
import qualified Linear                             as L
import           Model.Collision
import           Model.Configuration
import qualified Model.Configuration.Material       as MatR
import qualified Model.Configuration.Model          as ModR
import qualified Model.Configuration.Shader         as ShaR
import           Model.Entity
import           Model.GameState
import           Model.Geometry
import           Model.Material
import           Model.Object
import           Model.World

loadResources :: InitialState -> IO World
loadResources ((gs, is), ulObjs, ulEnts) =
  do cfg <- readCfg
     shaderMap   <- buildIOMap loadShader   $ shaders cfg
     checkError "loadShader"

     materialMap <- buildIOMap loadMaterial $ materials cfg
     checkError "loadMaterial"

     geometryMap <- buildIOMap loadGeometry $ models cfg
     checkError "loadGeometry"



     let entityMap   = L.foldl' (loadEntity materialMap geometryMap) M.empty ulEnts
         objects     = map (loadObject entityMap) ulObjs

     return ( gs { gsObjects = objects
                 , gsShaderPrograms = shaderMap}, is
            )


readCfg :: IO ResourceConfig
readCfg =
  do cfg_res <- Yaml.decodeFileEither resource_config_file
     case cfg_res of
       Left err -> error $ show err
       Right cfg -> return cfg

--------------------------------------
-- Load Objects using loaded entities:
--------------------------------------
loadObject :: M.Map String Entity ->
              ObjectUnloaded ->
              Object
loadObject entityMap ou = obj
  where ents = map (entityMap M.!) $ ouEntityNames ou

        obj = Object { oPosition    = ouPosition ou
                     , oRotation    = ouRotation ou
                     , oScale       = ouScale ou
                     , oEntities    = ents
                     }



-------------------------------------
-- Load Entities using loaded assets:
-------------------------------------
loadEntity :: M.Map String Material ->
              M.Map String Geometry ->
              M.Map String Entity ->
              EntityUnloaded ->
              M.Map String Entity
loadEntity materialMap geometryMap entMap eu =
  M.insert un ent entMap
  where un = euUniqueName eu

        mn = euMaterialName eu
        gn = euGeometryName eu

        m = materialMap M.! mn
        g = geometryMap M.! gn


        ent = Entity { eRelativePos = euRelativePos eu
                     , eRelativeRot = euRelativeRot eu
                     , eBoundingBox = gBoundingBox g
                     , eScale       = euScale eu
                     , eAmbOverride = euAmbOverride eu
                     , eGeometry    = g
                     , eMaterial    = m
                     }

-------------------
-- Load raw assets:
-------------------
buildIOMap :: (r -> IO (String, l)) -> [r] -> IO (M.Map String l)
buildIOMap loadFunc = L.foldl' loadFunc' (return M.empty)

  where --loadFunc' :: IO (M.Map String l) -> r -> IO (M.Map String l)
        loadFunc' loadedMap r
          = do m <- loadedMap
               (k, l) <- loadFunc r
               return $ M.insert k l m



loadShader :: ShaR.ShaderResource -> IO (String, GLUtil.ShaderProgram)
loadShader sr = do
  sp <- GLUtil.loadShaderProgram $ map (\(st, fp) -> (st, shader_file_path fp)) $ [v,f] ++ g

  return (un, sp)

  where v = (VertexShader, vert)
        f = (FragmentShader, frag)
        g = maybe [] (\geomFP -> [(GeometryShader, geomFP)]) geom

        un   = ShaR.name sr
        vert = ShaR.vert sr
        frag = ShaR.frag sr
        geom = ShaR.geom sr


loadGeometry :: ModR.ModelResource -> IO (String, Geometry)
loadGeometry mr = do
  (vertices, uvs, normals, triElements, triAdjElements) <- loadModel
                                                           mr { ModR.file_name = model_file_path $ ModR.file_name mr
                                                              }

  [vao] <- genObjectNames 1
  bindVertexArrayObject $= Just vao
  verts       <- GLUtil.fromSource ArrayBuffer        vertices
  uvCds       <- GLUtil.fromSource ArrayBuffer        uvs
  norms       <- GLUtil.fromSource ArrayBuffer        normals
  triElems    <- GLUtil.fromSource ElementArrayBuffer triElements
  triAdjElems <- GLUtil.fromSource ElementArrayBuffer triAdjElements
  bindVertexArrayObject $= Nothing

  let nofTris = length triElements
      nofAdjs = length triAdjElements
      geometry = Geometry { gVertices    = verts
                          , gUVCoords    = uvCds
                          , gNormals     = norms
                          , gTriElems    = triElems
                          , gTriAdjElems = triAdjElems
                          , gNOFTris     = fromIntegral nofTris
                          , gNOFAdjs     = fromIntegral nofAdjs
                          , gVAO         = vao
                          , gBoundingBox = createBoundingBox vertices
                          }

  return (un, geometry)
  where un = ModR.name mr

        createBoundingBox :: [L.V3 GLfloat] -> BoundingBox
        createBoundingBox vs = combineBBs $ map (\v -> BoundingBox v v) vs


loadMaterial :: MatR.MaterialResource -> IO (String, Material)
loadMaterial mr = do
  -- load image:
  diff_img <- loadImage diffuseFilePath

  let diff_w    = fromIntegral $ PNG.imageWidth diff_img
      diff_h    = fromIntegral $ PNG.imageHeight diff_img
      diff_data = PNG.imageData diff_img

      diff_texInfo = GLUtil.TexInfo diff_w diff_h GLUtil.TexRGBA diff_data


  diff_texObject <- GLUtil.loadTexture diff_texInfo

  textureBinding Texture2D    $= Just diff_texObject
  textureFilter Texture2D     $= ((Linear',Just Linear'), Linear')
  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)
  generateMipmap' Texture2D
  textureBinding Texture2D    $= Nothing

  let material =
          Material { mDiffuseMap = diff_texObject
                   }
  return (un, material)
  where un = MatR.name mr
        diffuseFilePath = material_file_path $ MatR.diffuse mr
