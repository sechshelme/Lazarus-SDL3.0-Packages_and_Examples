#include <SDL3/SDL.h>
#include <SDL3_image/SDL_image.h>

int main()
{
        uint32_t initFlags = SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_EVENTS;
        SDL_Init(initFlags);
        IMG_Init(IMG_INIT_PNG);
        uint32_t windowFlags = SDL_WINDOW_HIDDEN | SDL_WINDOW_TRANSPARENT | SDL_WINDOW_BORDERLESS;
        SDL_Surface * shapeSurface = IMG_Load("project1.png");
SDL_Log("x: %i  y: %i", shapeSurface->w, shapeSurface->h);
//        SDL_Window * win = SDL_CreateWindow("title", shapeSurface->w, shapeSurface->h, windowFlags);
        SDL_Window * win = SDL_CreateWindow("titlvve", 800, 600, windowFlags);
        SDL_Renderer * screen = SDL_CreateRenderer(win, 0, SDL_RENDERER_PRESENTVSYNC);
        SDL_SetWindowShape(win, shapeSurface);
        SDL_ShowWindow(win);

        SDL_SetRenderDrawColor(screen, 255, 255, 26, SDL_ALPHA_OPAQUE);
        bool run = true;
        while(run)
        {
                SDL_Event ev;
                while(SDL_PollEvent(&ev))
                {
                        switch(ev.type)
                        {
                                case SDL_EVENT_KEY_DOWN:
                                        switch(ev.key.keysym.sym)
                                        {
                                                case SDLK_ESCAPE:
                                                        run = false;
                                                        break;
                                        }
                                        break;
                                case SDL_EVENT_QUIT:
                                        run = false;
                                        break;
                        }
                }
                SDL_RenderClear(screen);
                SDL_RenderPresent(screen);
        }
        SDL_DestroyWindow(win);
        IMG_Quit();
        SDL_Quit();
}
