#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>

#include <SDL3/SDL.h>

// extern  void  SDL_Log(const char * fmt, ...);

int main(int argc, char *argv[])
{
    SDL_bool loopShouldStop = SDL_FALSE;
    SDL_Init(SDL_INIT_VIDEO);

    SDL_Window *win = SDL_CreateWindow("Keyboard", 320, 200, SDL_WINDOW_RESIZABLE);

        SDL_Event event;

    SDL_Log("Differenz: %i", (long) (&event.key.keysym.sym) - ((long) &event));
    SDL_Log("Differenz: %i", (long) (&event.key.keysym) - ((long) &event));
    SDL_Log("Differenz: %i", (long) (&event.key) - ((long) &event));

    SDL_Log("Differenz: %i", (long) (&event.key.keysym) - ((long) &event));


    while (!loopShouldStop)
    {
        while (SDL_PollEvent(&event))
        {
            SDL_Log("event: %i", event.type); // neu
            switch (event.type)
            {
                case SDL_EVENT_KEY_DOWN:
                    SDL_Log("Key: %i", event.key.keysym.sym);
                    switch (event.key.keysym.sym)
                    {
                       case SDLK_ESCAPE:
                       loopShouldStop = SDL_TRUE;
                       break;
                    }
                    break;
                case SDL_EVENT_QUIT:
                    loopShouldStop = SDL_TRUE;
                    break;
            }
        }
    }

    SDL_DestroyWindow(win);

    SDL_Quit();

    return 0;
}
