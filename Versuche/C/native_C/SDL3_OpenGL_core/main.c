/*
  Linux:
  gcc main.c opengl.c -o main -lGL -lSDL3


  Windows:
  x86_64-w64-mingw32-gcc main.c opengl.c -o main.exe -lopengl32 -lSDL3 -I/usr/local/include -L/usr/local/bin
*/

#include <SDL3/SDL.h>
#include <SDL3/SDL_opengl.h>

#include "opengl.h"

const float SQUARE[] = {
    -1.0f,  1.0f,
    -1.0f, -1.0f,
     1.0f,  1.0f,
     1.0f, -1.0f
};

static bool
opengl_shader_compile_source(GLuint shader, const GLchar* source)
{
    glShaderSource(shader, 1, &source, NULL);
    glCompileShader(shader);

    GLint success;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
    if (!success) {
        GLint info_log_length;
        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &info_log_length);

        GLchar* info_log = SDL_malloc(info_log_length);
        glGetShaderInfoLog(shader, info_log_length, NULL, info_log);

        SDL_LogError(SDL_LOG_PRIORITY_ERROR, "failed to compile shader:\n%s", info_log);
        SDL_free(info_log);

        return false;
    }

    return true;
}

static bool
opengl_shader_link_program(GLuint program, GLuint vertex_shader, GLuint fragment_shader)
{
    glAttachShader(program, vertex_shader);
    glAttachShader(program, fragment_shader);
    glLinkProgram(program);

    GLint success;
    glGetProgramiv(program, GL_LINK_STATUS, &success);
    if (success != GL_TRUE) {
        GLint info_log_length;
        glGetProgramiv(program, GL_INFO_LOG_LENGTH, &info_log_length);

        GLchar* info_log = SDL_malloc(info_log_length);
        glGetProgramInfoLog(program, info_log_length, NULL, info_log);

        SDL_LogError(SDL_LOG_PRIORITY_ERROR, "failed to link program:\n%s", info_log);
        SDL_free(info_log);

        glDetachShader(program, vertex_shader);
        glDetachShader(program, fragment_shader);

        return false;
    }

    glDetachShader(program, vertex_shader);
    glDetachShader(program, fragment_shader);
    return false;
}

int main(int argc, char* argv[])
{
    bool fullscreen = false;
    bool vsync = false;

    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        SDL_LogError(SDL_LOG_PRIORITY_ERROR, "failed to init SDL2: %s", SDL_GetError());
        return 1;
    }

    SDL_Log("Platform:        %s", SDL_GetPlatform());
    SDL_Log("CPU Count:       %d", SDL_GetCPUCount());
    SDL_Log("System RAM:      %d MB", SDL_GetSystemRAM());
    SDL_Log("Supports SSE:    %s", SDL_HasSSE() ? "true" : "false");
    SDL_Log("Supports SSE2:   %s", SDL_HasSSE2() ? "true" : "false");
    SDL_Log("Supports SSE3:   %s", SDL_HasSSE3() ? "true" : "false");
    SDL_Log("Supports SSE4.1: %s", SDL_HasSSE41() ? "true" : "false");
    SDL_Log("Supports SSE4.2: %s", SDL_HasSSE42() ? "true" : "false");

    // Request at least 32-bit color
    SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8);

    // Request a double-buffered, OpenGL 3.3 (or higher) core profile
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);

    unsigned long flags = SDL_WINDOW_OPENGL;
//    if (fullscreen) flags |= SDL_WINDOW_FULLSCREEN_DESKTOP;

    SDL_Window* window = SDL_CreateWindow("SDL2 OpenGL Demo", 640, 640, flags);

    if (window == NULL) {
        SDL_LogError(SDL_LOG_PRIORITY_ERROR, "failed to create SDL2 window: %s", SDL_GetError());
        SDL_Quit();
        return 1;
    }

    // SDL_GLContext is an alias for "void*"
    SDL_GLContext context = SDL_GL_CreateContext(window);
    if (context == NULL) {
        SDL_LogError(SDL_LOG_PRIORITY_ERROR, "failed to create OpenGL context: %s", SDL_GetError());
        SDL_DestroyWindow(window);
        SDL_Quit();
        return 1;
    }

    SDL_Log("OpenGL Vendor:   %s", glGetString(GL_VENDOR));
    SDL_Log("OpenGL Renderer: %s", glGetString(GL_RENDERER));
    SDL_Log("OpenGL Version:  %s", glGetString(GL_VERSION));
    SDL_Log("GLSL Version:    %s", glGetString(GL_SHADING_LANGUAGE_VERSION));

    // Enable v-sync (set 1 to enable, 0 to disable)
    SDL_GL_SetSwapInterval(vsync ? 1 : 0);

    // Load the modern OpenGL funcs
    opengl_load_functions();


    // Do modern OpenGL stuff
    const GLchar *vs_source =
        "#version 330\n"
        "layout(location = 0) in vec2 point;\n"
        "uniform float angle;\n"
        "void main() {\n"
        "    mat2 rotate = mat2(cos(angle), -sin(angle),\n"
        "                       sin(angle), cos(angle));\n"
        "    gl_Position = vec4(0.75 * rotate * point, 0.0, 1.0);\n"
        "}\n";

    const GLchar *fs_source =
        "#version 330\n"
        "out vec4 color;\n"
        "void main() {\n"
        "    color = vec4(1, 0.15, 0.15, 0);\n"
        "}\n";

    GLuint vs = glCreateShader(GL_VERTEX_SHADER);
    GLuint fs = glCreateShader(GL_FRAGMENT_SHADER);
    opengl_shader_compile_source(vs, vs_source);
    opengl_shader_compile_source(fs, fs_source);

    GLuint prog = glCreateProgram();
    opengl_shader_link_program(prog, vs, fs);

    GLint uniform_angle = glGetUniformLocation(prog, "angle");

    glDeleteShader(fs);
    glDeleteShader(vs);


    GLuint vbo;
    glGenBuffers(1, &vbo);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(SQUARE), SQUARE, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    GLuint vao;
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
    glEnableVertexAttribArray(0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);

    long frame_count = 0;
    unsigned long last_frame = 0;
    unsigned long last_second = 0;
    double angle = 0.0;

    bool running = true;
    while (running) {
        SDL_Event event = { 0 };
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_EVENT_QUIT) running = false;
            if (event.type == SDL_EVENT_KEY_UP) {
                SDL_Keycode key = event.key.keysym.sym;
                if (key == SDLK_q || key == SDLK_ESCAPE) running = false;
            }
        }

        glClearColor(0.15f, 0.15f, 0.15f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT);

        glUseProgram(prog);
        glUniform1f(uniform_angle, angle);
        glBindVertexArray(vao);
        glDrawArrays(GL_TRIANGLE_STRIP, 0, sizeof(SQUARE) / sizeof(*SQUARE) / 2);
        glBindVertexArray(0);
        glUseProgram(0);

        unsigned long now = SDL_GetTicks();
        unsigned long diff = now - last_frame;
        last_frame = now;

        angle += diff / 1000.0;
        if (angle > 2 * SDL_PI_F) angle -= 2 * SDL_PI_F;

        if (now - last_second >= 1000) {
            SDL_Log("FPS: %ld\n", frame_count);
            frame_count = 0;
            last_second = now;
        }

        frame_count++;
        SDL_GL_SwapWindow(window);
    }

    // Cleanup OpenGL resources
    glDeleteVertexArrays(1, &vao);
    glDeleteBuffers(1, &vbo);
    glDeleteProgram(prog);

    // Cleanup SDL2 resources
    SDL_GL_DeleteContext(context);
    SDL_DestroyWindow(window);
    SDL_Quit();

    return 0;
}

