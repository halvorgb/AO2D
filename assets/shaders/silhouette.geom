#version 330 core

layout (triangles_adjacency) in;
layout (line_strip, max_vertices = 6) out;

in vec3 pos_worldspace[];

void EmitLine(int StartIndex, int EndIndex)
{
    gl_Position = gl_in[StartIndex].gl_Position;
    EmitVertex();

    gl_Position = gl_in[EndIndex].gl_Position;
    EmitVertex();

    EndPrimitive();
}

uniform vec3 lightPosition;

void main()
{
    vec3 e1 = pos_worldspace[2] - pos_worldspace[0];
    vec3 e2 = pos_worldspace[4] - pos_worldspace[0];
    vec3 e3 = pos_worldspace[1] - pos_worldspace[0];
    vec3 e4 = pos_worldspace[3] - pos_worldspace[2];
    vec3 e5 = pos_worldspace[4] - pos_worldspace[2];
    vec3 e6 = pos_worldspace[5] - pos_worldspace[0];

    vec3 Normal = cross(e1,e2);
    vec3 LightDir = lightPosition - pos_worldspace[0];

    if (dot(Normal, LightDir) > 0.00001) {

        Normal = cross(e3,e1);

        if (dot(Normal, LightDir) <= 0) {
            EmitLine(0, 2);
        }

        Normal = cross(e4,e5);
        LightDir = lightPosition - pos_worldspace[2];

        if (dot(Normal, LightDir) <=0) {
            EmitLine(2, 4);
        }

        Normal = cross(e2,e6);
        LightDir = lightPosition - pos_worldspace[4];
	if (dot(Normal, LightDir) <= 0) {
            EmitLine(4, 0);
        }
    }
}
