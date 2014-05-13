
#version 330

layout (std140) uniform Matrices {
	mat4 pvm;
} ;

in vec4 position;
in vec4 color;
out vec4 pass_Color;  

void main()
{
	gl_Position = pvm * position;
	pass_Color = color;
} 
