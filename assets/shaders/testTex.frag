#version 330 core

// Interpolated values from the vertex shaders
in vec2 f_UV;

// Ouput data
out vec4 color;

// Values that stay constant for the whole mesh.
uniform sampler2D diffuse;

void main(){

    // Output color = color of the texture at the specified UV
  color = texture(diffuse, f_UV) + vec4(0.0, 0.0, 0.0, 1.0);
}
