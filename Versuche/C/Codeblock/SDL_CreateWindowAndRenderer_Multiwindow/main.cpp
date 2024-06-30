#include <SDL3/SDL.h>

#define winCount   4

int main(int argc, char *argv[])
{
  SDL_FRect rDest = {150, 150, 256, 256};

  SDL_Init(SDL_INIT_VIDEO);

  SDL_Window *win[winCount];
  SDL_Renderer *renderer[winCount];

  for (int i=0; i < winCount; i++) {
    win[i] = SDL_CreateWindow("Hello World", 640, 480, SDL_WINDOW_RESIZABLE ) ;
    renderer[i] = SDL_CreateRenderer(win[i], NULL);
  }

  SDL_bool quit = SDL_FALSE;
  while (!quit)
  {
    SDL_Event event;
    while (SDL_PollEvent(&event))
    {
      switch (event.type)
      {
        case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
             quit = SDL_TRUE;
             break;
      }
    }

    rDest.x += 0.1;
    if (rDest.x > 300) {
      rDest.x = 0.0;
    }

    for (int i=0; i < winCount; i++) {
      SDL_SetRenderDrawColorFloat(renderer[i], 0.5, 1.0, 1.0, SDL_ALPHA_OPAQUE);
      SDL_RenderClear(renderer[i]);

      SDL_SetRenderDrawColorFloat(renderer[i], 1.0, 0.5, 0.5, SDL_ALPHA_OPAQUE);
      SDL_RenderFillRect(renderer[i], &rDest);

      SDL_RenderPresent(renderer[i]);
    }
  }

  for (int i=0; i < winCount; i++) {
    SDL_DestroyRenderer(renderer[i]);
    SDL_DestroyWindow(win[i]);
  }
  SDL_Quit();
  return 0;
}
