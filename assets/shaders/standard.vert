#version 330 core

layout(location = 0) in vec3 v_position;
//layout(location = 1) in vec2 v_UV;
//layout(location = 2) in vec3 v_norm;

//out vec2 f_UV;
//out vec3 pos_worldspace;
//out vec3 norm_camspace;
//out vec3 eyedir_camspace;

out vec3 WorldPos0;
uniform mat4 MVP;
//uniform mat4 M;
//uniform mat4 V;

// based on https://code.google.com/p/opengl-tutorial-org/source/browse/tutorial08_basic_shading/StandardShading.vertexshader
void main(){
  //  mat4 MV = V * M;

  vec4 v4_position = vec4(v_position, 1);
  gl_Position    =  MVP * v4_position;
  WorldPos0 = (MVP * v4_position).xyz;
  //  pos_worldspace = (M * v4_position).xyz;

  //  vec3 pos_camspace = (MV * v4_position).xyz;
  //  eyedir_camspace   = vec3(0,0,0) - pos_camspace;

  //  norm_camspace = (V * inverse(M) * vec4(v_norm, 0)).xyz;

  //  f_UV = v_UV;

}
