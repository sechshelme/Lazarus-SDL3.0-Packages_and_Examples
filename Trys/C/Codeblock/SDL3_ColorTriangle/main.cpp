#include <SDL3/SDL.h>

#define  SDL_RENDERER_ACCELERATED 0x00000002

int main(int argc, char *argv[])
{
  bool quit = false;
  SDL_Window *window = SDL_CreateWindow("Triangle Example", 800, 600, 0);

  SDL_Renderer *renderer = SDL_CreateRenderer(window, NULL);

  SDL_Vertex vert[3]= {{400, 150, 1.0, 0.0, 0.0, 1.0}, {200, 450, 0.0, 0.0, 1.0, 1.0}, {600 ,450, 0.0, 1.0, 0.0, 1.0}};

  while (!quit) {
    SDL_Event ev;
    int64_t startTick = SDL_GetTicksNS();
    while (SDL_PollEvent(&ev) != 0) {
      switch(ev.type) {
        case SDL_EVENT_QUIT:
          quit = true;
        break;
        case SDL_EVENT_KEY_DOWN:
          if (ev.key.scancode == SDL_SCANCODE_T) {
            SDL_Log("press T");
          }
        break;
      }
    }
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);

    SDL_RenderGeometry(renderer, NULL, vert, 3, NULL, 0);

    SDL_RenderPresent(renderer);
    //int tickEnd = SDL_GetTicks();
    //SDL_Log("Tick: %i", (SDL_GetTicksNS() - startTick) / 10000);

  }

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;

  bool b =0;
}

SDL_GLContextState
