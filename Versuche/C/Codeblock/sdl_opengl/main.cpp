#include <SDL3/SDL.h>
#include <SDL3/SDL_opengl.h>
#include <SDL3/SDL_vulkan.h>
//#include <SDL3/SDL_opengl_glext.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <GL/glew.h>
//#include <GL/gl.h>
    ////# include <GL/glu.h>
//#include <GL/glext.h>

//#include <stdbool.h>
//#define CODE(...) #__VA_ARGS__

extern GLenum glewInit (void);


int main(){

TVkInstance

    unsigned int width = 320 * 2;
    unsigned int height = 180 * 2;

    // create window
    SDL_Init(SDL_INIT_VIDEO);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
    SDL_Window* window = SDL_CreateWindow("Modern Opengl", width, height, SDL_WINDOW_RESIZABLE | SDL_WINDOW_OPENGL);
    SDL_GLContext context = SDL_GL_CreateContext(window);

		printf("OpenGL 3.3\n");

//	GLenum glew_status;
	//glew_status = glewInit();

	// (GLEW_OK != glew_status) {
//fprintf(stderr, "Error: %s\n", glewGetErrorString(glew_status));
		//intf("fdsfsfd\n");
		//return 1;
	//

	// (!GLEW_VERSION_2_0) {
//fprintf(stderr, "No support for OpenGL 2.0 found\n");
		//return 1;
	//




    SDL_GL_SetSwapInterval(1);
    glViewport(0, 0, width, height);

    // triangle data
    float vertices[] = {
        -0.5f, -0.5f, 0.0f,
        0.5f, -0.5f, 0.0f,
        0.0f,  0.5f, 0.0f
    };

    // create a vertex array object
    unsigned int vao;
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);

    // create a vertex buffer object
    unsigned int vbo;
    glGenBuffers(1, &vbo);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);

    // add the vertex data to the vertex buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GL_FLOAT), NULL);
    glEnableVertexAttribArray(0);

    // vertex shader
    const GLchar* vs_code = "#version 330 core\n" CODE(
        layout (location = 0) in vec3 aPos;
        void main(){
            gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
        }
    );

    unsigned int vs = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vs, 1, &vs_code, NULL);
    glCompileShader(vs);

    int success;
    char infolog[512];
    glGetShaderiv(vs, GL_COMPILE_STATUS, &success);
    if(!success){
        glGetShaderInfoLog(vs, 512, NULL, infolog);
        printf("%s\n", infolog);
    }

    // fragment shader
    const GLchar* fs_code = "#version 330 core\n" CODE(
        out vec4 color;
        void main()
        {
            color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
        }
    );

    unsigned int fs = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fs, 1, &fs_code, NULL);
    glCompileShader(fs);

    glGetShaderiv(fs, GL_COMPILE_STATUS, &success);
    if(!success){
        glGetShaderInfoLog(fs, 512, NULL, infolog);
        printf("%s\n", infolog);
    }

    // create program
    unsigned int program = glCreateProgram();
    glAttachShader(program, vs);
    glAttachShader(program, fs);
    glLinkProgram(program);
    glGetProgramiv(program, GL_LINK_STATUS, &success);

    if(!success){
        glGetProgramInfoLog(program, 512, NULL, infolog);
         printf("%s\n", infolog);
    }

    glDeleteShader(vs);
    glDeleteShader(fs);

    // start render loop
    bool quit = false;
    while(!quit){
        SDL_Event event;
        while(SDL_PollEvent(&event)){
            if(event.type == SDL_EVENT_QUIT)
                quit = true;
        }
        glClearColor(1.0, 0.3, 0.3, 1.0);
        glClear(GL_COLOR_BUFFER_BIT);
        glUseProgram(program);
        glBindVertexArray(vao);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        SDL_GL_SwapWindow(window);
    }
    glDeleteProgram(program);
    SDL_GL_DeleteContext(context);
    SDL_DestroyWindow(window);
    SDL_Quit();
}
