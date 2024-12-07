#include <SDL3/SDL.h>

int main(int argc, char *argv[])
{
  SDL_Window *window = SDL_CreateWindow("test", 800, 600, 0);
  SDL_Renderer *renderer = SDL_CreateRenderer(window, NULL);

  SDL_SetRenderDrawColorFloat(renderer, 0, 0, 1, SDL_ALPHA_OPAQUE);
  SDL_RenderClear(renderer);

  SDL_SetRenderDrawColor(renderer, 255, 255, 255, SDL_ALPHA_OPAQUE);
  SDL_RenderDebugText(renderer, 0, 20, "Hello World 1");
  SDL_SetRenderDrawColorFloat(renderer, 1.0, 1.0, 1.0, SDL_ALPHA_OPAQUE);
  SDL_RenderDebugText(renderer, 0, 40, "Hello World 2");

  SDL_RenderPresent(renderer);
  SDL_Delay(3000);

  SDL_DestroyWindow(window);
  SDL_DestroyRenderer(renderer);

  SDL_Quit();
  return 0;
}
