#version 330 core

layout(location = 0) in vec3 v_position;

out vec3 pos_worldspace;

uniform mat4 MVP;
uniform mat4 M;


void main() {
  vec4 v4_position = vec4(v_position, 1);

  gl_Position =  MVP * v4_position;

  pos_worldspace   = (M * v4_position).xyz;
}
