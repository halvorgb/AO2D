#version 330 core

layout(location = 0) in vec3 v_position;
layout(location = 1) in vec2 v_UV;
layout(location = 2) in vec3 v_norm;


out vec2 f_UV;
out vec3 f_pos;
out vec3 f_norm;

uniform mat4 MVP;
uniform mat4 M;

void main(){
  gl_Position    =  MVP * vec4(v_position, 1.0);
  f_UV = v_UV;
  f_norm = (M * vec4(v_norm, 0)).xyz;
  f_pos  = (M * vec4(v_position, 1)).xyz;

}
