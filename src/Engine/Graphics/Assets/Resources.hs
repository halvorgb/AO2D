module Engine.Graphics.Assets.Resources(loadResources) where

import qualified Graphics.GLUtil as GLUtil
import Graphics.Rendering.OpenGL

import qualified Data.Map as M
import qualified Data.List as L
import qualified Codec.Picture as PNG

import Engine.Graphics.Common
import Engine.Graphics.Assets.ImageLoader
import Engine.Graphics.Assets.ModelLoader

import Model.Resources
import Model.Object
import Model.Entity
import Model.ShaderPrograms

import Model.Geometry
import Model.Material

import Model.World

import Model.GameState


loadResources :: InitialState -> IO World
loadResources ((gs, is), rs, ulObjs, ulEnts, ulSPs) = do
  shaderMap   <- buildIOMap loadShader   $ rShaderRs rs
  checkError "loadShader"

  materialMap <- buildIOMap loadMaterial $ rMaterialRs rs
  checkError "loadMaterial"

  geometryMap <- buildIOMap loadGeometry $ rGeometryRs rs
  checkError "loadGeometry"



  let entityMap   = L.foldl' (loadEntity materialMap geometryMap) M.empty ulEnts
      objects     = map (loadObject entityMap) ulObjs
      shaderProgs = addShaderProgs shaderMap ulSPs

  return (gs {gsObjects = objects,
              gsShaderPrograms = shaderProgs},
          is)

-------------------------------------
-- Add loaded shader programs.......
-------------------------------------
addShaderProgs :: M.Map String GLUtil.ShaderProgram ->
                  ShaderPrograms'Unloaded ->
                  ShaderPrograms
addShaderProgs shaderMap spsU =
    ShaderPrograms {
  spSilhouette = shaderMap M.! spSilhouetteName spsU,
  spLight = shaderMap M.! spLightName spsU
}

--------------------------------------
-- Load Objects using loaded entities:
--------------------------------------
loadObject :: M.Map String Entity ->
              Object'Unloaded ->
              Object
loadObject entityMap ou = obj
    where
      ents = map (entityMap M.!) $ ouEntityNames ou

      obj = Object {
              oPosition = ouPosition ou,
              oRotation = ouRotation ou,
              oScale    = ouScale ou,

              oBBT      = undefined,

              oEntities = ents
            }



-------------------------------------
-- Load Entities using loaded assets:
-------------------------------------
loadEntity :: M.Map String Material ->
              M.Map String Geometry ->
              M.Map String Entity ->
              Entity'Unloaded ->
              M.Map String Entity
loadEntity materialMap geometryMap entMap eu =
    M.insert un ent entMap
    where
      un = euUniqueName eu

      mn = euMaterialName eu
      gn = euGeometryName eu

      m = materialMap M.! mn
      g = geometryMap M.! gn


      ent = Entity {
              eRelativePos = euRelativePos eu,
              eRelativeRot = euRelativeRot eu,
              eScale       = euScale eu,

              eAmbOverride = euAmbOverride eu,

              eGeometry    = g,
              eMaterial    = m
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



loadShader :: ShaderResource -> IO (String, GLUtil.ShaderProgram)
loadShader sr = do
  sp <- GLUtil.loadShaderProgram $
        (v:g)++f


  return (un, sp)

      where
        v = (VertexShader, vert)
        g = maybe [] (\geomFP -> [(GeometryShader, geomFP)]) geom
        f = maybe [] (\fragFP -> [(FragmentShader, fragFP)]) frag

        un   = srUniqueName sr
        vert = srVertShaderFP sr
        geom = srGeomShaderFP sr
        frag = srFragShaderFP sr


loadGeometry :: GeometryResource -> IO (String, Geometry)
loadGeometry gr = do
  (vertices, uvs, normals, triElements, triAdjElements) <- loadModel gr

  [vao] <- genObjectNames 1
  bindVertexArrayObject $= Just vao
  verts       <- GLUtil.fromSource ArrayBuffer        vertices
  uvCds       <- GLUtil.fromSource ArrayBuffer        uvs
  norms       <- GLUtil.fromSource ArrayBuffer        normals
  triElems    <- GLUtil.fromSource ElementArrayBuffer triElements
  triAdjElems <- GLUtil.fromSource ElementArrayBuffer triAdjElements
  bindVertexArrayObject $= Nothing

  let nofTris = length triAdjElements
      geometry = Geometry {
                    gVertices    = verts,
                    gUVCoords    = uvCds,
                    gNormals     = norms,
                    gTriElems    = triElems,
                    gTriAdjElems = triAdjElems,
                    gNOFTris     = div (fromIntegral nofTris) 2,
                    gVAO         = vao }

  return (un, geometry)
    where un = grUniqueName gr


loadMaterial :: MaterialResource -> IO (String, Material)
loadMaterial mr = do
  -- load image:
  diff_img <- loadImage diffuseFilePath

  let diff_w    = fromIntegral $ PNG.imageWidth diff_img
      diff_h    = fromIntegral $ PNG.imageHeight diff_img
      diff_data = PNG.imageData diff_img

      diff_texInfo = GLUtil.TexInfo diff_w diff_h GLUtil.TexRGBA diff_data


  diff_texObject <- GLUtil.loadTexture diff_texInfo

  textureBinding Texture2D $= Just diff_texObject
  textureFilter Texture2D $= ((Linear',Just Linear'), Linear')
  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)
  generateMipmap' Texture2D
  textureBinding Texture2D $= Nothing

  let material =
          Material {
        mDiffuseMap = diff_texObject
      }
  return (un, material)
    where
      un = mrUniqueName mr
      diffuseFilePath = mrDiffuseFP mr
