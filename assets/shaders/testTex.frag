#version 330 core

in vec2 f_UV;
in vec3 pos_worldspace;
in vec3 norm_camspace;
in vec3 eyedir_camspace;
in vec3 lightdir_camspace;

out vec4 color;

uniform sampler2D diffuse;
uniform vec3 lightpos_worldspace;
//uniform vec4 global_color;
void main(){
  // temp for testing
  vec3 lightColor = vec3(1,1,1);
  float lightPower = 50;

  // material colors
  vec3 matDiffColor    = texture2D(diffuse, f_UV).rgb;
  vec3 matAmbiColor = vec3(0.1, 0.1, 0.1) + matDiffColor;
  vec3 matSpecColor    = vec3(0.3, 0.3, 0.3);

  float distance_to_light = length(lightpos_worldspace - pos_worldspace);

  vec3 n = normalize (norm_camspace);
  vec3 l = normalize (lightdir_camspace);

  float cosTheta = clamp(dot(n,l), 0, 1);


  vec3 e = normalize(eyedir_camspace);
  vec3 r = reflect(-l,n);

  float cosAlpha = clamp(dot(e, r), 0, 1);

  vec3 colorRGB =
    matAmbiColor +
    matDiffColor * lightColor * lightPower * cosTheta / (distance_to_light*distance_to_light) +
    matSpecColor * lightColor * lightPower * pow(cosAlpha,5) / (distance_to_light*distance_to_light);

  color = vec4(colorRGB, 1.0);

}
