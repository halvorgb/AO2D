#version 130


in vec4 coord3d;
in vec4 v_color;
uniform mat4 mvp;

out vec4 f_color;


void main(void) {
  gl_Position = mvp * coord3d;

  f_color = v_color;
}
