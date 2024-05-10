#include <SDL3/SDL.h>

int main(int argc, char *argv[])
{
    SDL_Init(SDL_INIT_VIDEO);

    SDL_Window * win;
    win = SDL_CreateWindow("Hello World", 640, 480, SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIDDEN) ;

    SDL_Renderer  * renderer;
    renderer = SDL_CreateRenderer(win, NULL, SDL_RENDERER_PRESENTVSYNC);

    SDL_bool quit = SDL_FALSE;


    SDL_ShowWindow(win);
    while (!quit)
    {
        SDL_Event event;
        while (SDL_PollEvent(&event))
        {
            switch (event.type)
            {
                case SDL_EVENT_QUIT:
                    quit = SDL_TRUE;
                    break;
            }
        }
        SDL_RenderClear(renderer);
        SDL_RenderPresent(renderer);
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(win);
    SDL_Quit();
    return 0;
}
