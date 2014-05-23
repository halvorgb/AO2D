#version 330 core

in vec2 f_UV;
in vec3 pos_worldspace;
in vec3 norm_camspace;
in vec3 eyedir_camspace;
in vec3 sundir_camspace;

out vec4 color;

uniform sampler2D diffuse;
uniform vec3 sunpos_worldspace;
uniform float sunpower;
uniform vec3 suncolor;
uniform vec3 color_override;

void main(){

  vec3 textureColor = color_override * texture2D(diffuse, f_UV).rgb;

  // material colors
  vec3 matDiffColor    = textureColor;
  vec3 matAmbiColor    = vec3(0.1, 0.1, 0.1) * matDiffColor;
  vec3 matSpecColor    = vec3(1.0, 0.0, 0.0);

  float distance_to_light = length(sunpos_worldspace - pos_worldspace);

  vec3 n = normalize(norm_camspace);
  vec3 l = normalize(sundir_camspace);

  float cosTheta = clamp(dot(n,l), 0, 1);


  vec3 e = normalize(eyedir_camspace);
  vec3 r = reflect(-l,n);

  float cosAlpha = clamp(dot(e, r), 0, 1);

  vec3 colorRGB =
    matAmbiColor +
    matDiffColor * suncolor * sunpower * cosTheta / (distance_to_light*distance_to_light); +
    matSpecColor * suncolor * sunpower * pow(cosAlpha,5) / (distance_to_light*distance_to_light);

  color = vec4(colorRGB, 1.0);

}
