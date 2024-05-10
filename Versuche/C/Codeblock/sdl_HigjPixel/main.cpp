#include <SDL3/SDL.h>

#include <stdio.h>

int main() {
    SDL_SetHint("SDL_WINDOWS_DPI_AWARENESS", "permonitorv2");

    SDL_Init(SDL_INIT_VIDEO);

    SDL_Window* window = SDL_CreateWindow("Test", 500, 500, SDL_WINDOW_HIGH_PIXEL_DENSITY | SDL_WINDOW_OPENGL);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
    SDL_GLContext context = SDL_GL_CreateContext(window);
    SDL_GL_SetSwapInterval(1);
    SDL_Event event;


    bool should_quit = false;
    while (!should_quit) {
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_EVENT_QUIT)
                should_quit = true;
        }
        int w, h;
        SDL_GetWindowSize(window, &w, &h);
        printf("Window size: %d x %d\n", w, h);

        SDL_GetWindowSizeInPixels(window, &w, &h);
        printf("Window size in pixels: %d x %d\n", w, h);

        float scale = SDL_GetWindowDisplayScale(window);
        printf("Window display scale: %f\n", scale);

        float pixel_density = SDL_GetWindowPixelDensity(window);
        printf("Window pixel density: %f\n", pixel_density);

        SDL_GL_SwapWindow(window);
    }

    SDL_DestroyWindow(window);
    SDL_GL_DeleteContext(context);
    SDL_Quit();
}
