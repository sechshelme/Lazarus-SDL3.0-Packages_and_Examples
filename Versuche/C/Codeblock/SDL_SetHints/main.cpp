#include <SDL3/SDL.h>
int main()
{
        SDL_Init(SDL_INIT_VIDEO);
        SDL_Window * win = SDL_CreateWindow("Tablet Events", 400, 400, SDL_WINDOW_RESIZABLE);
        SDL_Renderer * screen = SDL_CreateRenderer(win, 0, 0);

        SDL_SetHint(SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH, "1");

        bool run = true;
        while(run)
        {
                SDL_Event ev;
                while(SDL_PollEvent(&ev))
                {
                        switch(ev.type)
                        {
                                case SDL_EVENT_WINDOW_FOCUS_GAINED:
                                        SDL_Log("Gained focus");
                                        break;
                                case SDL_EVENT_MOUSE_BUTTON_DOWN:
                                        // click off window, then back on, and this is not caught.
                                        SDL_Log("I caught this click.");
                                        break;
                                case SDL_EVENT_MOUSE_BUTTON_UP:
                                        // same, this is not caught until click #2
                                        SDL_Log("Now button up.");
                                        break;
                                case SDL_EVENT_QUIT:
                                        run = false;
                                        break;
                        }
                }

                SDL_RenderClear(screen);
                SDL_RenderPresent(screen);
                SDL_Delay(60);
        }
        SDL_DestroyWindow(win);
        SDL_Quit();
}
