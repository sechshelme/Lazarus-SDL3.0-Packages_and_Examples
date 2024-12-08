/*
  Linux:
  gcc main.c -o main -lSDL3 -lGL

*/

#define SDL_MAIN_USE_CALLBACKS

#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>
#include <SDL3/SDL_opengles2.h>

struct AppContext {
    SDL_Window* window;
    SDL_GLContext glcontext;
    SDL_bool app_quit;
} appContext;

const char *vertexShaderSource =
    "attribute vec2 aPosition;\n"
    "void main()"
    "{\n"
    "   gl_Position = vec4(aPosition, 0.0, 1.0);\n"
    "}\n";

const char *fragmentShaderSource =
    "void main()"
    "{\n"
    "    gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);\n"
    "}\n";

GLuint createShader(const char *shaderSource, int shaderType)
{
    GLuint shader = glCreateShader(shaderType);
    glShaderSource(shader, 1, &shaderSource, NULL);
    glCompileShader(shader);
    GLint status;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
    if (status == GL_FALSE)
    {
        GLint maxLength = 0;
        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &maxLength);

        GLchar *errorLog;
        errorLog = SDL_malloc(maxLength);
        glGetShaderInfoLog(shader, maxLength, &maxLength, errorLog);
        glDeleteShader(shader);
        SDL_Log("%s", errorLog);
        SDL_free(errorLog);
    }
    return shader;
}

GLuint createShaderProgram()
{
    GLuint program = glCreateProgram();
    GLuint vShader = createShader(vertexShaderSource, GL_VERTEX_SHADER);
    GLuint fShader = createShader(fragmentShaderSource, GL_FRAGMENT_SHADER);

    glAttachShader(program, vShader);
    glAttachShader(program, fShader);
    glLinkProgram(program);
    glUseProgram(program);

    return program;
}

void initVertexBuffers(GLuint program)
{
    float vertPositions[] = {
        -0.5f, -0.5f,
        0.5f, -0.5f,
        0.f, 0.5f
    };
    GLuint vertPosBuffer;
    glGenBuffers(1, &vertPosBuffer);
    glBindBuffer(GL_ARRAY_BUFFER, vertPosBuffer);
    int amount = sizeof(vertPositions) / sizeof(vertPositions[0]);
    glBufferData(GL_ARRAY_BUFFER, amount * sizeof(GLfloat), vertPositions, GL_STATIC_DRAW);
    GLint aPositionLocation = glGetAttribLocation(program, "aPosition");
    glVertexAttribPointer(aPositionLocation, 2, GL_FLOAT, GL_FALSE, 0, 0);
    glEnableVertexAttribArray(aPositionLocation);
}

int SDL_Fail()
{
    SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, "Error %s", SDL_GetError());
    return -1;
}

int SDL_AppInit(void **appstate, int argc, char *argv[])
{
    if (SDL_Init(SDL_INIT_VIDEO))
    {
        return SDL_Fail();
    }

    SDL_Window *window = SDL_CreateWindow("OpenGL Window", 352, 430,
        SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE);
    if (!window)
    {
        return SDL_Fail();
    }

    SDL_GLContext glcontext = SDL_GL_CreateContext(window);

    GLuint program = createShaderProgram();
    initVertexBuffers(program);

    SDL_ShowWindow(window);

    appContext.window = window;
    appContext.glcontext = glcontext;
    appContext.app_quit = SDL_FALSE;
    *appstate = &appContext;
    SDL_Log("Application started successfully!");

    return 0;
}

int SDL_AppEvent(void *appstate, const SDL_Event *event)
{
    struct AppContext *app = appstate;

    if (event->type == SDL_EVENT_QUIT) {
        app->app_quit = SDL_TRUE;
    }

    return 0;
}

int SDL_AppIterate(void *appstate)
{
    struct AppContext *app = appstate;

    glClearColor(0.1, 0.3, 0.2, 1.f);
    glClear(GL_COLOR_BUFFER_BIT);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    SDL_GL_SwapWindow(app->window);

    return app->app_quit;
}

void SDL_AppQuit(void *appstate)
{
    struct AppContext *app = appstate;
    if (app)
    {
        SDL_GL_DeleteContext(app->glcontext);
        SDL_DestroyWindow(app->window);
    }

    SDL_Quit();
    SDL_Log("Application quit successfully!");
}
