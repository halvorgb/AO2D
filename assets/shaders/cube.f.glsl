#version 130

in vec4 f_color;

void main(void) {
  gl_FragColor = vec4(f_color.x, f_color.y, f_color.z, 0.2);
}
