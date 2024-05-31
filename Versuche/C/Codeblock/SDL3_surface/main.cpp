#include <SDL3/SDL.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    SDL_Init(SDL_INIT_VIDEO);
    SDL_Window *win = SDL_CreateWindow("Test", 320, 200, SDL_WINDOW_RESIZABLE);
    SDL_Surface *winSurface;

    SDL_bool loopShouldStop = SDL_FALSE;
    while (!loopShouldStop)
    {
        SDL_Event event;
        while (SDL_PollEvent(&event))
        {
            switch (event.type)
            {
                case SDL_EVENT_QUIT:
                    loopShouldStop = SDL_TRUE;
                    break;
            }
        }
        winSurface = SDL_GetWindowSurface(win);

        SDL_FillSurfaceRect(winSurface, nullptr, 0x80);
        static int x = 0;
        SDL_Rect r =  {x, 20, 100, 100};
        x += 1;
        if ( x > 500) { x = 0; };
        SDL_FillSurfaceRect(winSurface, &r, rand() % 0xFFFFFF);

        SDL_UpdateWindowSurface(win);
    }

    SDL_DestroyWindow(win);
    SDL_DestroySurface(winSurface);
    SDL_Quit();

    return 0;
}
