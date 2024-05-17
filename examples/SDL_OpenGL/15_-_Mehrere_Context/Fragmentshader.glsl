#version 450 core

uniform vec3 uCol;

layout (location = 0) out vec4 fColor;

void main()
{
  fColor = vec4(uCol, 1.0);
}
