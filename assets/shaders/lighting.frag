#version 330 core

in vec2 f_UV;
in vec3 pos_worldspace;
in vec3 norm_camspace;
in vec3 eyedir_camspace;

out vec4 color;

uniform mat4 V;
uniform sampler2D diffuse;
uniform float ambianceIntensity;
uniform float diffuseIntensity;

uniform vec3 lightPosition;
uniform vec3 lightColor;
uniform float lightStrength;


void main(){

  vec3 diffuseColor_RAW =  texture2D(diffuse, f_UV*16).rgb;
  vec3 diffuseColor = diffuseIntensity * diffuseColor_RAW;
  vec3 specularColor = vec3(0.15, 0.15, 0.15);


  vec3 ambianceColor = ambianceIntensity * diffuseColor_RAW;


  float distance_to_light = length(lightPosition - pos_worldspace);

  vec3 lightPosition_camspace  = (V * vec4(lightPosition, 1)).xyz;

  vec3 lightDirection_camspace = lightPosition_camspace + eyedir_camspace;

  vec3 n = normalize(norm_camspace);
  vec3 l = normalize(lightDirection_camspace);

  float cosTheta = clamp(dot(n,l),0,1);

  vec3 diffuseReflection  =
    diffuseColor * lightColor * lightStrength * cosTheta /(distance_to_light*distance_to_light);




  vec3 e = normalize(eyedir_camspace);
  vec3 r = reflect(-l,n);

  float cosAlpha = clamp(dot(e, r), 0, 1);

  vec3 specularReflection =
    specularColor * lightColor * lightStrength * pow(cosAlpha, 5) / (distance_to_light*distance_to_light);




  vec3 totalColor = max(ambianceColor, diffuseReflection + specularReflection);

  color = vec4(totalColor, 1.0);

}
