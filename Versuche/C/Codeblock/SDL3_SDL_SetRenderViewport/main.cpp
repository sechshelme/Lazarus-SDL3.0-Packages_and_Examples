#include <SDL3/SDL.h>

int main(int argc, char *argv[])
{
  SDL_Init(SDL_INIT_VIDEO);

  SDL_Window *window = SDL_CreateWindow("RenderViewport Example", 800, 600, 0);
  SDL_Renderer *renderer = SDL_CreateRenderer(window, NULL);

  SDL_Rect VPrect = {100, 100, 100, 100};
  SDL_SetRenderViewport(renderer, &VPrect);

  SDL_SetRenderDrawColorFloat(renderer, 1.0, 0.0, 0.0, 1.0);
  SDL_RenderFillRect(renderer, nullptr);

  SDL_SetRenderDrawColorFloat(renderer, 0.0, 1.0, 1.0, 1.0);
  for (int i = 0; i < 8; i++) {
    SDL_FRect rect = {i * 4 , i * 4, 40, 40};
    SDL_RenderRect(renderer, &rect);
  }

  SDL_RenderPresent(renderer);

  SDL_Delay(5000);

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;
}
