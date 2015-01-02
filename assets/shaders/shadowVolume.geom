#version 330

layout (triangles_adjacency) in;
layout (triangle_strip, max_vertices = 18) out;

in vec3 pos_worldspace[];

uniform vec3 lightPosition;
uniform mat4 VP;

float EPSILON = 0.01;

void EmitQuad(int StartIndex, vec3 StartVertex, int EndIndex, vec3 EndVertex)
{
    vec3 LightDir = normalize(StartVertex - lightPosition);
    vec3 l = LightDir * EPSILON;
    gl_Position = VP * vec4((StartVertex + l), 1.0);
    EmitVertex();

    gl_Position = VP * vec4(LightDir, 0.0);
    EmitVertex();

    LightDir = normalize(EndVertex - lightPosition);
    l = LightDir * EPSILON;
    gl_Position = VP * vec4((EndVertex + l), 1.0);
    EmitVertex();

    gl_Position = VP * vec4(LightDir, 0.0);
    EmitVertex();

    EndPrimitive();
}


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

    if (dot(Normal, LightDir) > 0.000001) {

        Normal = cross(e3,e1);

        if (dot(Normal, LightDir) <= 0) {
            vec3 StartVertex = pos_worldspace[0];
            vec3 EndVertex = pos_worldspace[2];
            EmitQuad(0, StartVertex, 2, EndVertex);
        }

        Normal = cross(e4,e5);
        LightDir = lightPosition - pos_worldspace[2];

        if (dot(Normal, LightDir) <= 0) {
            vec3 StartVertex = pos_worldspace[2];
            vec3 EndVertex = pos_worldspace[4];
            EmitQuad(2, StartVertex, 4, EndVertex);
        }

        Normal = cross(e2,e6);
        LightDir = lightPosition - pos_worldspace[4];

        if (dot(Normal, LightDir) <= 0) {
            vec3 StartVertex = pos_worldspace[4];
            vec3 EndVertex = pos_worldspace[0];
            EmitQuad(4, StartVertex, 0, EndVertex);
        }
	// reeder front cap:
        vec3 LightDir = (normalize(pos_worldspace[0] - lightPosition)) * EPSILON;
        gl_Position = VP * vec4((pos_worldspace[0] + LightDir), 1.0);
        EmitVertex();

        LightDir = (normalize(pos_worldspace[2] - lightPosition)) * EPSILON;
        gl_Position = VP * vec4((pos_worldspace[2] + LightDir), 1.0);
        EmitVertex();

        LightDir = (normalize(pos_worldspace[4] - lightPosition)) * EPSILON;
        gl_Position = VP * vec4((pos_worldspace[4] + LightDir), 1.0);
        EmitVertex();
        EndPrimitive();

	// render the back cap
        LightDir = pos_worldspace[0] - lightPosition;
        gl_Position = VP * vec4(LightDir, 0.0);
        EmitVertex();

        LightDir = pos_worldspace[4] - lightPosition;
        gl_Position = VP * vec4(LightDir, 0.0);
        EmitVertex();

        LightDir = pos_worldspace[2] - lightPosition;
        gl_Position = VP * vec4(LightDir, 0.0);
        EmitVertex();
    }
}
