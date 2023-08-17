out vec4 FragColor;

in vec3 vecColor;
in vec2 vecCoord;

uniform sampler2D vecTexture;

void main()
{
    FragColor = texture(vecTexture, vecCoord);
}