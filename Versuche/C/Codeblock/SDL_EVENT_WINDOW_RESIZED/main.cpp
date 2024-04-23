#include <SDL3/SDL.h>

int main(int argc, char *argv[])
{

  SDL_bool quit = SDL_FALSE;
  SDL_Window *window = SDL_CreateWindow("SDL_EVENT_WINDOW_RESIZED Example", 800, 600, SDL_WINDOW_RESIZABLE);

  SDL_Renderer *renderer = SDL_CreateRenderer(window, NULL, SDL_RENDERER_PRESENTVSYNC);


   while (!quit) {
   SDL_Event ev;
     while (SDL_PollEvent(&ev) != 0) {
      switch(ev.type) {
        case SDL_EVENT_QUIT:
          quit = SDL_TRUE;
        break;
        case SDL_EVENT_WINDOW_RESIZED:
          char * s;
          SDL_asprintf(&s, "X: %i   Y: %i", ev.window.data1, ev.window.data2);
          SDL_SetWindowTitle(window, s);
          SDL_free(s);
        break;
      }
    }
    SDL_SetRenderDrawColor(renderer, 128, 0, 0, 255);
    SDL_RenderClear(renderer);

    SDL_RenderPresent(renderer);
  }

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;
}
