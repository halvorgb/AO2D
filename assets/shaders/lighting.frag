#version 330 core

in vec2 f_UV;
in vec3 f_norm;
in vec3 f_pos;

out vec4 color;

uniform sampler2D diffuse;
uniform float ambientIntensity;
uniform float diffuseIntensity;

uniform vec3 eyepos_worldspace;
uniform vec3 plPositions;
uniform vec3 plColors;

struct VSOutput
{
  vec2 TexCoord;
  vec3 Normal;
  vec3 WorldPos;
};

struct Attenuation
{
  float Constant;
  float Linear;
  float Exp;
};

struct PointLight
{
  vec3 position;
  vec3 color;
  Attenuation Atten;
};

vec4 calcLightInternal(PointLight pl, vec3 LightDirection, VSOutput In)
{
  float specularPower = 0.0;
  float specularIntensity = 0.0;

  vec4 AmbientColor = vec4(pl.color, 1.0) * ambientIntensity;
  float DiffuseFactor = dot(In.Normal, -LightDirection);

  vec4 DiffuseColor  = vec4(0, 0, 0, 0);
  vec4 SpecularColor = vec4(0, 0, 0, 0);

  if (DiffuseFactor > 0.00001) {
    DiffuseColor = vec4(pl.color, 1.0) * diffuseIntensity * DiffuseFactor;

    vec3 VertexToEye = normalize(eyepos_worldspace - In.WorldPos);
    vec3 LightReflect = normalize(reflect(LightDirection, In.Normal));
    float SpecularFactor = dot(VertexToEye, LightReflect);
    SpecularFactor = pow(SpecularFactor, specularPower);
    if (SpecularFactor > 0.0) {
      SpecularColor = vec4(pl.color, 1.0) *
	specularIntensity * SpecularFactor;
    }
  }

  return (AmbientColor + DiffuseColor + SpecularColor);
}


vec4 calcPointLight(PointLight pl, VSOutput In)
{
  vec3 LightDirection = In.WorldPos - pl.position;
  float Distance = length(LightDirection);
  LightDirection = normalize(LightDirection);

  vec4 Color = calcLightInternal(pl, LightDirection, In);
  float att =  pl.Atten.Constant +
    pl.Atten.Linear * Distance +
    pl.Atten.Exp * Distance * Distance;

  return Color / att;

}

void main() {
  VSOutput In;
  In.TexCoord = f_UV*16;
  In.Normal   = normalize(f_norm);
  In.WorldPos = f_pos;

  vec4 totalLight = vec4(0, 0, 0, 1);
  if (ambientIntensity == 1) {
    totalLight = vec4(1,1,1,1);
  } else {
    Attenuation defaultAtten = Attenuation(0, 0.25, 0);
    PointLight pl = PointLight(plPositions,
                               plColors,
                               defaultAtten);
    totalLight += calcPointLight(pl, In);
  }

  color = texture(diffuse, In.TexCoord.xy) * totalLight;

}
