#version 330 core

layout(location = 0) in vec3 v_position;

uniform mat4 MVP;

void main(){
  vec4 v4_pos    = vec4(v_position, 1.0);
  gl_Position    =  MVP * v4_pos;
}
