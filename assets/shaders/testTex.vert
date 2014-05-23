#version 330 core

layout(location = 0) in vec4 v_position;
layout(location = 1) in vec2 v_UV;
layout(location = 2) in vec3 v_norm;


out vec2 f_UV;
out vec3 pos_worldspace;
out vec3 norm_camspace;
out vec3 eyedir_camspace;
out vec3 lightdir_camspace;


uniform mat4 MVP;
uniform mat4 M;
uniform mat4 V;
uniform vec3 lightpos_worldspace;

// based on https://code.google.com/p/opengl-tutorial-org/source/browse/tutorial08_basic_shading/StandardShading.vertexshader
void main(){
  mat4 MV = V * M;


  gl_Position    =  MVP * v_position;

  pos_worldspace = (M * v_position).xyz;

  vec3 pos_camspace = (MV * v_position).xyz;
  eyedir_camspace   = vec3(0,0,0) - pos_camspace;

  vec3 lightpos_camspace = (V * vec4(lightpos_worldspace, 1)).xyz;
  lightdir_camspace      = lightpos_camspace + eyedir_camspace;

  // wrong since M scales.
  norm_camspace = (V * inverse(M) * vec4(v_norm, 0)).xyz;



  f_UV = v_UV;
}
