#version 330 core

// Input vertex data, different for all executions of this shader.
layout(location = 0) in vec4 v_position;
layout(location = 1) in vec2 v_UV;
// Output data ; will be interpolated for each fragment.
out vec2 f_UV;
// Values that stay constant for the whole mesh.
uniform mat4 MVP;

void main(){

  // Output position of the vertex, in clip space : MVP * position
  gl_Position =  MVP * v_position;

  // UV of the vertex. No special space for this one.
  f_UV = v_UV;
}
