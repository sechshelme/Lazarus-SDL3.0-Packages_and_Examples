#include <SDL3/SDL.h>

int main()
{
        uint32_t initFlags = SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_EVENTS;
        SDL_Init(initFlags);
        uint32_t windowFlags = SDL_WINDOW_HIDDEN | SDL_WINDOW_TRANSPARENT | SDL_WINDOW_BORDERLESS;
        SDL_Surface * shapeSurface = SDL_CreateSurface(800, 600, SDL_PIXELFORMAT_RGBA8888);
        SDL_Rect r;
        r = {0, 0, 400, 300};
        SDL_FillSurfaceRect(shapeSurface, &r, 0x00000080);
        r = {400, 300, 400, 300};
        SDL_FillSurfaceRect(shapeSurface, &r, 0x000000FF);

        SDL_Window * win = SDL_CreateWindow("Shapes", 800, 600, windowFlags);
        SDL_Delay(2000);
        SDL_Renderer * screen = SDL_CreateRenderer(win, 0);
        SDL_SetWindowShape(win, shapeSurface);
        SDL_ShowWindow(win);

        SDL_SetRenderDrawColor(screen, 255, 255, 25, SDL_ALPHA_OPAQUE);
        SDL_RenderClear(screen);
        SDL_RenderPresent(screen);

        SDL_Delay(5000);
        SDL_DestroyWindow(win);
        SDL_Quit();
}
